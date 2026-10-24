//
//  SrlDecoder.m
//  Sereal
//
//  Created by Andrea Guzzo on 8/8/13.
//  Copyright (c) 2013 Andrea Guzzo. All rights reserved.
//

#import "SrlDecoder.h"
#import "SrlObject.h"

#include "srl_protocol.h"
#include "csnappy.h"

typedef struct __srl_decoder_t {
    char    *hdr;
    ssize_t  len;
    ssize_t *ofx;
    ssize_t  bdy;
    BOOL     cpy;
    BOOL     b2s;
    CFTypeRef td;
    NSUInteger depth;
    NSUInteger maxdepth;
} srl_decoder_t;

typedef id (^srl_decoder)(srl_decoder_t *);

static srl_decoder decoders[128];

// lower than the Perl decoder's 10000: each level costs ~100-300 bytes of C stack, so 10000
// levels overflow the 512KB stack of a secondary thread, which kills the process uncatchably
#define SRL_DEFAULT_MAX_RECURSION_DEPTH 1000

#define ADVANCE_OFFSET(__dec, __size) {\
    *__dec->ofx += __size;\
    if (*__dec->ofx >= __dec->len) {\
        @throw [NSException exceptionWithName:@"BufferOverrun"\
                                       reason:[NSString stringWithFormat:@"%s (%s:%d) : %@",\
                                                __FUNCTION__, __FILE__, __LINE__, @"Buffer overrun (truncated?)"]\
                                     userInfo:nil];\
    }\
}

// throws unless __size more bytes follow the current offset
#define CHECK_REMAINING(__dec, __size) {\
    if ((__size) < 0 || (unsigned long long)(__size) > (unsigned long long)(__dec->len - *__dec->ofx - 1)) {\
        @throw [NSException exceptionWithName:@"BufferOverrun"\
                                       reason:[NSString stringWithFormat:@"%s (%s:%d) : %@",\
                                                __FUNCTION__, __FILE__, __LINE__, @"Buffer overrun (truncated?)"]\
                                     userInfo:nil];\
    }\
}

static long long read_varint(srl_decoder_t *dec)
{
    unsigned lshift = 0;
    int maxbits = sizeof(long long) * 8;

    ADVANCE_OFFSET(dec, 1);
    unsigned long long number = 0;
    while (*dec->ofx < dec->len) {
        number |= ((unsigned long long)(dec->hdr[*dec->ofx] & 0x7f)) << lshift;
        lshift += 7;
        if (!(dec->hdr[*dec->ofx] & 0x80))
            break;
        if (lshift >= maxbits) {
            @throw [NSException exceptionWithName:@"VarintOverflow"
                                           reason:@"Varint is bigger than possible representation on this architecture"
                                         userInfo:nil];
            break;
        }
        ADVANCE_OFFSET(dec, 1);
    }
    return number;
}

static id srl_decode(srl_decoder_t *dec)
{
    NSMutableDictionary *trackingDictionary = (__bridge NSMutableDictionary *)(dec->td);

    id obj = nil;
    if (++dec->depth > dec->maxdepth)
        @throw [NSException exceptionWithName:@"RecursionLimit"
                                       reason:[NSString stringWithFormat:@"Reached recursion limit (%lu) during deserialization", (unsigned long)dec->maxdepth]
                                     userInfo:nil];
    ADVANCE_OFFSET(dec, 1);
    // NOTE: the tracking flag will be ignored if we are decoding a copy command
    BOOL track = (dec->hdr[*dec->ofx] & SRL_HDR_TRACK_FLAG && !dec->cpy);
    ssize_t ofx = *dec->ofx;
    char hdr = dec->hdr[*dec->ofx] & ~SRL_HDR_TRACK_FLAG;
    srl_decoder decoder = decoders[hdr];
    if (decoder) {
        obj = decoder(dec);
        if (obj && track)
            [trackingDictionary setObject:obj forKey:[NSNumber numberWithLongLong:ofx]];
    } else {
        @throw [NSException exceptionWithName:@"DecodeError"
                                       reason:[NSString stringWithFormat:@"Can't decode: Unknown header %02x", hdr]
                                     userInfo:nil];
    }
    dec->depth--;
    return obj;
}

// reads the offset of a COPY/OBJECTV/OBJECTV_FREEZE tag sitting at __tag and returns the
// position the copy decoder starts from; the target must be inside the body and precede the tag
static ssize_t read_backref_offset(srl_decoder_t *dec, ssize_t tag)
{
    unsigned long long ofs = read_varint(dec);
    if (ofs == 0 || ofs >= (unsigned long long)(tag - dec->bdy))
        @throw [NSException exceptionWithName:@"BadOffset"
                                       reason:[NSString stringWithFormat:@"Offset %llu to copied data does not precede its tag", ofs]
                                     userInfo:nil];
    return dec->bdy + (ssize_t)ofs - 1;
}

// the copied item must end before the tag that refers to it
#define COPY_DECODER(__src, __ofx, __tag) \
{\
    .hdr = __src->hdr,\
    .len = __tag,\
    .ofx = &__ofx,\
    .bdy = __src->bdy,\
    .cpy = YES,\
    .b2s = __src->b2s,\
    .depth = __src->depth,\
    .maxdepth = __src->maxdepth\
}\

static inline void fill_array(NSMutableArray *array, int items, srl_decoder_t *dec)
{
    for (int i = 0; i < items; i++) {
        id obj = srl_decode(dec);
        [array addObject:obj];
    }
}

static inline void fill_dictionary(NSMutableDictionary *dict, int items, srl_decoder_t *dec)
{
    for (int i = 0; i < items; i++) {
        id key = srl_decode(dec);
        id value = srl_decode(dec);
        if (value)
            [dict setObject:value forKey:key];
    }
}

static void create_decoders_lookup()
{
    memset(decoders, 0, sizeof(decoders));

    for (int i = 0; i < SRL_HDR_VARINT; i++) {
        decoders[i] = ^id (srl_decoder_t *dec) {
            char hdr = dec->hdr[*dec->ofx];
            int val = hdr & 0x0f;
            NSNumber *number = [NSNumber numberWithInt:(hdr < 16) ? val : (val - 16)];

            return number;
        };
    }
    
    decoders[SRL_HDR_VARINT] = ^id (srl_decoder_t *dec) {
        unsigned long long varint = read_varint(dec);
        NSNumber *number = [NSNumber numberWithUnsignedLongLong:varint];
        return number;
    };
    
    decoders[SRL_HDR_ZIGZAG] = ^id (srl_decoder_t *dec) {
        long long varint = read_varint(dec);
        varint = (varint >> 1) ^ -(varint & 0x01); // -(1 + (varint >> 1)); // unzigzag
        NSNumber *number = [NSNumber numberWithLongLong:varint];
        return number;
    };
    
    decoders[SRL_HDR_FLOAT] = ^id (srl_decoder_t *dec) {
        float flNumber;
        CHECK_REMAINING(dec, sizeof(float));
        memcpy(&flNumber, dec->hdr + *dec->ofx + 1, sizeof(float));
        *dec->ofx += sizeof(float);
        NSNumber *number = [NSNumber numberWithFloat:flNumber];
        return number;
    };
    
    decoders[SRL_HDR_DOUBLE] = ^id (srl_decoder_t *dec) {
        double dNumber;
        CHECK_REMAINING(dec, sizeof(double));
        memcpy(&dNumber, dec->hdr + *dec->ofx + 1, sizeof(double));
        *dec->ofx += sizeof(double);
        NSNumber *number = [NSNumber numberWithDouble:dNumber];
        return number;
    };
    
    decoders[SRL_HDR_LONG_DOUBLE] = ^id (srl_decoder_t *dec) {
        long double ldNumber;
        CHECK_REMAINING(dec, sizeof(long double));
        memcpy(&ldNumber, dec->hdr + *dec->ofx + 1, sizeof(long double));
        *dec->ofx += sizeof(long double);
        NSNumber *number = [NSNumber numberWithDouble:ldNumber];
        return number;
    };
    
    decoders[SRL_HDR_UNDEF] = ^id (srl_decoder_t *dec) {
        return [NSNull null];
    };
    
    decoders[SRL_HDR_BINARY] = ^id (srl_decoder_t *dec) {
        long long bytes = read_varint(dec);
        CHECK_REMAINING(dec, bytes);
        NSData *data = [NSData dataWithBytes:(dec->hdr + *dec->ofx + 1) length:bytes];
        *dec->ofx += bytes;
        if (dec->b2s) {
            NSString *string = [[NSString alloc] initWithData:data encoding:NSISOLatin1StringEncoding];
            if (string && strlen(string.UTF8String) == bytes)
                return string;
        }
        return data;
    };
    
    decoders[SRL_HDR_STR_UTF8] = ^id (srl_decoder_t *dec) {
        long long bytes = read_varint(dec);
        CHECK_REMAINING(dec, bytes);
        NSString *str = [[NSString alloc] initWithBytes:dec->hdr + *dec->ofx + 1
                                                 length:bytes
                                               encoding:NSUTF8StringEncoding];
        *dec->ofx += bytes;
        return str;
    };
    
    decoders[SRL_HDR_REFN] = ^id (srl_decoder_t *dec) {
        id obj = srl_decode(dec);
        return obj;
    };
    
    decoders[SRL_HDR_REFP] = ^id (srl_decoder_t *dec) {
        NSMutableDictionary *trackingDictionary = (__bridge NSMutableDictionary *)(dec->td);
        long long offset = read_varint(dec) + dec->bdy;
        if (offset >= *dec->ofx)
            @throw [NSException exceptionWithName:@"BadRefp"
                                           reason:@"Offset to reference data is past the current offset"
                                         userInfo:nil];

        return [trackingDictionary objectForKey:[NSNumber numberWithLongLong:offset]];
    };
    
    decoders[SRL_HDR_ALIAS] = decoders[SRL_HDR_REFP];
    
    decoders[SRL_HDR_WEAKEN] = ^id (srl_decoder_t *dec) {
        // TODO - implement
        return srl_decode(dec);
    };
    
    decoders[SRL_HDR_COPY] = ^id (srl_decoder_t *dec) {
        if (dec->cpy) {
            // TODO - we are already inside a copy ... we need to throw an exception
            @throw [NSException exceptionWithName:@"nested_copy" reason:@"Can't nest COPY headers" userInfo:nil];
        }
        ssize_t tag = *dec->ofx;
        ssize_t offset = read_backref_offset(dec, tag);
        srl_decoder_t copy_dec = COPY_DECODER(dec, offset, tag);
        return srl_decode(&copy_dec);
    };
    
    decoders[SRL_HDR_HASH] = ^id (srl_decoder_t *dec) {
        long long count = read_varint(dec);
        NSMutableDictionary *dict = [[NSMutableDictionary alloc] initWithCapacity:count];
        fill_dictionary(dict, (int)count, dec);
        return dict;
    };
    
    decoders[SRL_HDR_ARRAY] = ^id (srl_decoder_t *dec) {
        long long count = read_varint(dec);
        NSMutableArray *array = [[NSMutableArray alloc] initWithCapacity:count];
        fill_array(array, (int)count, dec);
        return array;
    };
    
    decoders[SRL_HDR_OBJECT_FREEZE] = ^id (srl_decoder_t *dec) {
        NSString *className = srl_decode(dec);
        id object = srl_decode(dec);
        if ([object isKindOfClass:[NSData class]]) {
            id unarchivedObject = nil;
            @try {
                unarchivedObject = [NSUnarchiver unarchiveObjectWithData:object];
            }
            @catch (NSException *exception) {
                NSLog(@"Can't unarchive data for object of type: %@", className);
            }
            @finally {
                return unarchivedObject;
            }
            
        }
        // TODO - Error Messages
        return NULL;
    };
    
    decoders[SRL_HDR_OBJECTV_FREEZE] = ^id (srl_decoder_t *dec) {
        if (dec->cpy)
            @throw [NSException exceptionWithName:@"nested_copy" reason:@"Can't nest OBJECTV_FREEZE inside COPY headers" userInfo:nil];
        ssize_t tag = *dec->ofx;
        ssize_t offset = read_backref_offset(dec, tag);
        srl_decoder_t copy_dec = COPY_DECODER(dec, offset, tag);
        NSString *className = srl_decode(&copy_dec);
        id object = srl_decode(dec);
        if ([object isKindOfClass:[NSData class]]) {
            id unarchivedObject = nil;
            @try {
                unarchivedObject = [NSUnarchiver unarchiveObjectWithData:object];
            }
            @catch (NSException *exception) {
                NSLog(@"Can't unarchive data for object of type: %@", className);
            }
            @finally {
                return unarchivedObject;
            }
            
        }
        // TODO - Error Messages
        return NULL;
    };
    
    decoders[SRL_HDR_OBJECT] = ^id (srl_decoder_t *dec) {
        NSString *className = srl_decode(dec);
        id object = srl_decode(dec);
        return [SrlObject srlObject:className data:object];
    };
    
    decoders[SRL_HDR_OBJECTV] = ^id (srl_decoder_t *dec) {
        if (dec->cpy)
            @throw [NSException exceptionWithName:@"nested_copy" reason:@"Can't nest OBJECTV inside COPY headers" userInfo:nil];
        ssize_t tag = *dec->ofx;
        ssize_t offset = read_backref_offset(dec, tag);
        srl_decoder_t copy_dec = COPY_DECODER(dec, offset, tag);
        NSString *className = srl_decode(&copy_dec);
        id object = srl_decode(dec);
        return [SrlObject srlObject:className data:object];
    };
    
    decoders[SRL_HDR_REGEXP] = ^id (srl_decoder_t *dec) {
        id patternVal = srl_decode(dec);
        id modifiersVal = srl_decode(dec);
        NSString *pattern = nil;
        NSString *modifiers = nil;
        if ([patternVal isKindOfClass:[NSString class]]) {
            pattern = patternVal;
        } else if ([patternVal isKindOfClass:[NSData class]]) {
            pattern = [[NSString alloc] initWithBytes:[(NSData *)patternVal bytes]
                                               length:[(NSData *)patternVal length]
                                             encoding:NSUTF8StringEncoding];
        } else {
            @throw [NSException exceptionWithName:@"BadRegexpData"
                                           reason:@"Bad regexp pattern"
                                         userInfo:nil];
        }
        
        if ([modifiersVal isKindOfClass:[NSString class]]) {
            modifiers = modifiersVal;
        } else if ([modifiersVal isKindOfClass:[NSData class]]) {
            modifiers = [[NSString alloc] initWithBytes:[(NSData *)modifiersVal bytes]
                                                 length:[(NSData *)modifiersVal length]
                                               encoding:NSUTF8StringEncoding];
        } else {
            @throw [NSException exceptionWithName:@"BadRegexpData"
                                           reason:@"Bad regexp modifiers"
                                         userInfo:nil];
        }
        
        return [NSString stringWithFormat:@"/%@/%@", pattern, modifiers];
    };
    
    decoders[SRL_HDR_FALSE] = ^id (srl_decoder_t *dec) {
        return [NSNumber numberWithBool:NO];
    };
    
    decoders[SRL_HDR_TRUE] = ^id (srl_decoder_t *dec) {
        return [NSNumber numberWithBool:YES];
    };
    
    decoders[SRL_HDR_PAD] = ^id (srl_decoder_t *dec) {
        return srl_decode(dec);
    };
    
    decoders[SRL_HDR_RESERVED_6] = decoders[SRL_HDR_PAD];
    
    // make the reserved headers behave like padding (they are not used yet ... so we just skip over)
    for (char i = SRL_HDR_RESERVED_LOW; i <= SRL_HDR_RESERVED_HIGH; i++) {
        decoders[i] = decoders[SRL_HDR_PAD];
    }
    
    for (int i = SRL_HDR_ARRAYREF; i <= SRL_HDR_ARRAYREF+SRL_MASK_ARRAYREF_COUNT; i++) {
        decoders[i] = ^id (srl_decoder_t *dec) {
            int nItems = dec->hdr[*dec->ofx]&SRL_MASK_ARRAYREF_COUNT;
            NSMutableArray *array = [[NSMutableArray alloc] initWithCapacity:nItems];
            fill_array(array, nItems, dec);
            return array;
        };
    }
    
    for (int i = SRL_HDR_HASHREF; i <= SRL_HDR_HASHREF+SRL_MASK_HASHREF_COUNT; i++) {
        decoders[i] = ^id (srl_decoder_t *dec) {
            int nItems = dec->hdr[*dec->ofx]&SRL_MASK_HASHREF_COUNT;
            NSMutableDictionary *dict = [[NSMutableDictionary alloc] initWithCapacity:nItems];
            fill_dictionary(dict, nItems, dec);
            return dict;
        };
    }
    
    for (int i = SRL_HDR_SHORT_BINARY; i <= SRL_HDR_SHORT_BINARY+SRL_MASK_SHORT_BINARY_LEN; i++) {
        decoders[i] = ^id (srl_decoder_t *dec) {
            int nBytes = dec->hdr[*dec->ofx]&SRL_MASK_SHORT_BINARY_LEN;
            if (dec->len < *dec->ofx + nBytes + 1) {
                @throw [NSException exceptionWithName:@"BadBinaryData"
                                               reason:@"Buffer shorter than declared length"
                                             userInfo:nil];
            }
            ADVANCE_OFFSET(dec, 1);
            NSData *data = [NSData dataWithBytes:(dec->hdr + *dec->ofx) length:nBytes];
            ADVANCE_OFFSET(dec, nBytes - 1);
            if (dec->b2s) {
                NSString *string = [[NSString alloc] initWithData:data encoding:NSISOLatin1StringEncoding];
                if (string && strlen(string.UTF8String) == nBytes)
                    return string;
            }
            return data;
        };
    }
}

static NSError *decode_error(NSString *errorMessage)
{
    return [NSError errorWithDomain:@"SrlDecoder"
                               code:-1
                           userInfo:[NSDictionary dictionaryWithObject:errorMessage
                                                                forKey:NSLocalizedDescriptionKey]];
}

@implementation SrlDecoder

@synthesize error;
@synthesize maxRecursionDepth;

- (id)init
{
    self = [super init];
    if (self)
        maxRecursionDepth = SRL_DEFAULT_MAX_RECURSION_DEPTH;
    return self;
}

+ (void)initialize
{
    create_decoders_lookup();
}

- (id)decode:(NSData *)someData error:(NSError **)err
{
    id obj = [self decode:someData];
    if (err)
        *err = self.error;
    return obj;
}

- (id)decode:(NSData *)someData
{
    ssize_t index = 0;
    char *bytes = (char *)[someData bytes];
    
    error = nil;
    if (someData.length < 5) {
        NSLog(@"Buffer too short");
        error = decode_error(@"Buffer too short");
        return nil;
    }
    uint32 magic = *((uint32 *)bytes);
    if (magic != SRL_MAGIC_STRING_UINT_LE) {
        NSLog(@"Bad SRL Magic, skipping packet");
        error = decode_error(@"Bad SRL Magic");
        return nil;
    }
    
    index +=4;
    char vtype = bytes[index];
    
    char version = vtype&0x0f;
    
    char type = (vtype&0xf0) >> 4;
    if (type != 0 && type != 2) {
        NSLog(@"Compression type %d not supported, only 0 and 2 are supported in this implementation", type);
        error = decode_error([NSString stringWithFormat:@"Compression type %d not supported", type]);
        return nil;
    }
    
    srl_decoder_t dec = {
        .hdr = bytes,
        .len = someData.length,
        .ofx = &index,
        .bdy = 0,
        .cpy = NO,
        .b2s = self.binaryStrings,
        .maxdepth = self.maxRecursionDepth,
        .td  = CFBridgingRetain([[NSMutableDictionary alloc] init])
    };
    
    id object = nil;
    @try {
        // header suffix
        long long suffixSize = read_varint(&dec);
        CHECK_REMAINING((&dec), suffixSize);
        
        // skip the optional suffix for now;
        index += suffixSize;
        if (type == 2) {
            ssize_t blen = read_varint(&dec);
            if (blen) {
                uint32_t ulen = 0;
                index++;
                int ret = csnappy_get_uncompressed_length(bytes+index, blen, &ulen);
                if (ret == CSNAPPY_E_HEADER_BAD) {
                    NSLog(@"Malformed csnappy header");
                    return nil;
                }
                char *uncompressed = malloc(ulen);
                
                ret = csnappy_decompress(bytes+index, blen, uncompressed, ulen);
                
                if (ret != CSNAPPY_E_OK) {
                    NSLog(@"csnappy_decompress returned an error: %d", ret);
                    return nil;
                }
                
                dec.hdr = uncompressed;
                dec.len = ulen;
                index = -1;
            } else {
                NSLog(@"Can't read the length for the compressed buffer");
                return nil;
            }
        }
        
        if (version > 1)
            dec.bdy = index;

        // from here bytes points to the uncompressed body
        object = srl_decode(&dec);
    }
    @catch (NSException *exception) {
        error = decode_error([NSString stringWithFormat:@"Can't decode: %@", exception]);
    }
    @finally {
        CFBridgingRelease(dec.td);
    }
    return object;
}

@end
