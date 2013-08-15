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
    ssize_t len;
    ssize_t *ofx;
    ssize_t bdy;
    BOOL    cpy;
} srl_decoder_t;

typedef id (^srl_decoder)(srl_decoder_t *);

static srl_decoder decoders[128];
static NSMutableDictionary *trackingDictionary = nil;

static long long read_varint(srl_decoder_t *dec)
{
    unsigned lshift = 0;
    ssize_t index = (*dec->ofx)++;
    unsigned long long number = (dec->hdr[index] & 0x7f);
    
    while (index < dec->len - 1 && dec->hdr[index] & 0x80) {
        index = (*dec->ofx)++;
        lshift += 7;
        int maxbits = sizeof(long long) * 8;
        if (lshift > maxbits) {
//            NSLog(@"Varint is bigger than possible representation on this architecture. "
//                  @"Truncating to %d bits.", maxbits);
            @throw [NSException exceptionWithName:@"VarintOverflow"
                                           reason:@"Varint is bigger than possible representation on this architecture"
                                         userInfo:nil];
            break;
        }
        number |= (dec->hdr[index] & 0x7f) << lshift;
    }
    return number;
}

static id srl_decode(srl_decoder_t *dec)
{
    id obj = nil;
    // NOTE: the tracking flag will be ignored if we are decoding a copy command
    BOOL track = (dec->hdr[*dec->ofx] & SRL_HDR_TRACK_FLAG && !dec->cpy);
    ssize_t ofx = *dec->ofx;
    if (*dec->ofx >= dec->len) {
        @throw [NSException exceptionWithName:@"BufferOverrun"
                                       reason:@"Buffer overrun (truncated?)"
                                     userInfo:nil];
    }
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
    return obj;
}

#define COPY_DECODER(__src, __ofx) \
{\
    .hdr = __src->hdr,\
    .len = __src->len,\
    .ofx = &__ofx,\
    .bdy = __src->bdy,\
    .cpy = YES\
}\

#define FILL_ARRAY(__array, __items, __dec) \
for (int i = 0; i < __items; i++) {\
    id obj = srl_decode(__dec);\
    [__array addObject:obj];\
}\

#define FILL_DICTIONARY(__dict, __items, __dec) \
for (int i = 0; i < __items; i++) {\
    id key = srl_decode(__dec);\
    id value = srl_decode(__dec);\
    if (value)\
        [__dict setObject:value forKey:key];\
}

static void create_decoders_lookup()
{
    memset(decoders, 0, sizeof(decoders));

    for (int i = 0; i < SRL_HDR_VARINT; i++) {
        decoders[i] = ^id (srl_decoder_t *dec) {
            char hdr = dec->hdr[(*dec->ofx)++];
            int val = hdr & 0x0f;
            NSNumber *number = [NSNumber numberWithInt:(hdr < 16) ? val : -val];

            return number;
        };
    }
    
    decoders[SRL_HDR_VARINT] = ^id (srl_decoder_t *dec) {
        (*dec->ofx)++;
        long long varint = read_varint(dec);
        NSNumber *number = [NSNumber numberWithLongLong:varint];
        return number;
    };
    
    decoders[SRL_HDR_ZIGZAG] = ^id (srl_decoder_t *dec) {
        
        (*dec->ofx)++;
        long long varint = read_varint(dec);
        varint = -(1 + (varint >> 1)); // unzigzag
        NSNumber *number = [NSNumber numberWithLongLong:varint];
        return number;
    };
    
    decoders[SRL_HDR_FLOAT] = ^id (srl_decoder_t *dec) {
        (*dec->ofx)++;
        Float32 flNumber = ((Float32)*(dec->hdr + *dec->ofx));
        *dec->ofx += sizeof(Float32);
        NSNumber *number = [NSNumber numberWithFloat:flNumber];
        return number;
    };
    
    decoders[SRL_HDR_DOUBLE] = ^id (srl_decoder_t *dec) {
        (*dec->ofx)++;
        double dNumber = ((double)*(dec->hdr + *dec->ofx));
        *dec->ofx += sizeof(double);
        NSNumber *number = [NSNumber numberWithDouble:dNumber];
        return number;
    };
    
    decoders[SRL_HDR_LONG_DOUBLE] = ^id (srl_decoder_t *dec) {
        (*dec->ofx)++;
        long double ldNumber = ((long double)*(dec->hdr + ++(*dec->ofx)));
        *dec->ofx += sizeof(long double);
        NSNumber *number = [NSNumber numberWithLongLong:ldNumber];
        return number;
    };
    
    decoders[SRL_HDR_UNDEF] = ^id (srl_decoder_t *dec) {
        (*dec->ofx)++;
        return [NSNull null];
    };
    
    decoders[SRL_HDR_BINARY] = ^id (srl_decoder_t *dec) {
        (*dec->ofx)++;
        long long bytes = read_varint(dec);
        NSData *data = [NSData dataWithBytes:(dec->hdr + *dec->ofx) length:bytes];
        (*dec->ofx) += bytes;
        return data;
    };
    
    decoders[SRL_HDR_STR_UTF8] = ^id (srl_decoder_t *dec) {
        (*dec->ofx)++;
        long long bytes = read_varint(dec);
        NSString *str = [[NSString alloc] initWithBytes:dec->hdr + *dec->ofx
                                                 length:bytes
                                               encoding:NSUTF8StringEncoding];
        (*dec->ofx) += bytes;
        return str;
    };
    
    decoders[SRL_HDR_REFN] = ^id (srl_decoder_t *dec) {
        (*dec->ofx)++;
        id obj = srl_decode(dec);
        return obj;
    };
    
    decoders[SRL_HDR_REFP] = ^id (srl_decoder_t *dec) {
        (*dec->ofx)++;
        long long offset = read_varint(dec) + dec->bdy - 1;
        if (offset >= *dec->ofx)
            @throw [NSException exceptionWithName:@"BadRefp"
                                           reason:@"Offset to reference data is past the current offset"
                                         userInfo:nil];

        return [trackingDictionary objectForKey:[NSNumber numberWithLongLong:offset]];
    };
    
    decoders[SRL_HDR_ALIAS] = decoders[SRL_HDR_REFP];
    
    decoders[SRL_HDR_WEAKEN] = ^id (srl_decoder_t *dec) {
        (*dec->ofx)++;
        // TODO - implement
        return srl_decode(dec);
    };
    
    decoders[SRL_HDR_COPY] = ^id (srl_decoder_t *dec) {
        if (dec->cpy) {
            // TODO - we are already inside a copy ... we need to throw an exception
            @throw [NSException exceptionWithName:@"nested_copy" reason:@"Can't nest COPY headers" userInfo:nil];
        }
        (*dec->ofx)++;
        ssize_t offset = read_varint(dec) + dec->bdy - 1;
        srl_decoder_t copy_dec = COPY_DECODER(dec, offset);
        return srl_decode(&copy_dec);
    };
    
    decoders[SRL_HDR_HASH] = ^id (srl_decoder_t *dec) {
        (*dec->ofx)++;
        long long count = read_varint(dec);
        NSMutableDictionary *dict = [[NSMutableDictionary alloc] initWithCapacity:count];
        FILL_DICTIONARY(dict, (int)count, dec);
        return dict;
    };
    
    decoders[SRL_HDR_ARRAY] = ^id (srl_decoder_t *dec) {
        (*dec->ofx)++;
        long long count = read_varint(dec);
        NSMutableArray * array = [[NSMutableArray alloc] initWithCapacity:count];
        FILL_ARRAY(array, (int)count, dec);
        return array;
    };
    
    decoders[SRL_HDR_OBJECT] = ^id (srl_decoder_t *dec) {
        (*dec->ofx)++;
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
        return [SrlObject srlObject:className data:object];
    };
    
    decoders[SRL_HDR_OBJECTV] = ^id (srl_decoder_t *dec) {
        (*dec->ofx)++;
        ssize_t offset = read_varint(dec) + dec->bdy - 1;
        srl_decoder_t copy_dec = COPY_DECODER(dec, offset);
        NSString *className = srl_decode(&copy_dec);
        id object = srl_decode(dec);
        return [SrlObject srlObject:className data:object];
    };
    
    decoders[SRL_HDR_REGEXP] = ^id (srl_decoder_t *dec) {
        (*dec->ofx)++;
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
        (*dec->ofx)++;
        return [NSNumber numberWithBool:NO];
    };
    
    decoders[SRL_HDR_TRUE] = ^id (srl_decoder_t *dec) {
        (*dec->ofx)++;
        return [NSNumber numberWithBool:YES];
    };
    
    decoders[SRL_HDR_PAD] = ^id (srl_decoder_t *dec) {
        (*dec->ofx)++;
        return srl_decode(dec);
    };
    
    decoders[SRL_HDR_MANY] = ^id (srl_decoder_t *dec) {
        @throw [NSException exceptionWithName:@"Unimplemented" reason:@"Unimplemented" userInfo:nil];
        return nil;
    };
    
    // make the reserved headers behave like padding (they are not used yet ... so we just skip over)
    for (char i = SRL_HDR_RESERVED_LOW; i <= SRL_HDR_RESERVED_HIGH; i++) {
        decoders[i] = decoders[SRL_HDR_PAD];
    }
    
    for (int i = SRL_HDR_ARRAYREF; i <= SRL_HDR_ARRAYREF+SRL_MASK_ARRAYREF_COUNT; i++) {
        decoders[i] = ^id (srl_decoder_t *dec) {
            int nItems = dec->hdr[(*dec->ofx)++]&SRL_MASK_ARRAYREF_COUNT;
            NSMutableArray *array = [[NSMutableArray alloc] initWithCapacity:nItems];
            FILL_ARRAY(array, nItems, dec);
            return array;
        };
    }
    
    for (int i = SRL_HDR_HASHREF; i <= SRL_HDR_HASHREF+SRL_MASK_HASHREF_COUNT; i++) {
        decoders[i] = ^id (srl_decoder_t *dec) {
            int nItems = dec->hdr[(*dec->ofx)++]&SRL_MASK_HASHREF_COUNT;
            NSMutableDictionary *dict = [[NSMutableDictionary alloc] initWithCapacity:nItems];
            FILL_DICTIONARY(dict, nItems, dec);
            return dict;
        };
    }
    
    for (int i = SRL_HDR_SHORT_BINARY; i < SRL_HDR_SHORT_BINARY+SRL_MASK_SHORT_BINARY_LEN; i++) {
        decoders[i] = ^id (srl_decoder_t *dec) {
            int nBytes = dec->hdr[(*dec->ofx)++]&SRL_MASK_SHORT_BINARY_LEN;
            if (dec->len < *dec->ofx + nBytes + 1) {
                @throw [NSException exceptionWithName:@"BadBinaryData"
                                               reason:@"Buffer shorter than declared length"
                                             userInfo:nil];
            }
            NSString *string = [[NSString alloc] initWithBytes:&dec->hdr[*dec->ofx] length:nBytes encoding:NSUTF8StringEncoding]; // Note: bytes are copied here
            (*dec->ofx) += nBytes;
            return string;
        };
    }
}

@implementation SrlDecoder

+ (void)initialize
{
    create_decoders_lookup();
}

- (id)decode:(NSData *)someData
{
    ssize_t index = 0;
    char *bytes = (char *)[someData bytes];
    
    if (someData.length < 5) {
        NSLog(@"Buffer too short");
        return nil;
    }
    uint32 magic = *((uint32 *)bytes);
    if (magic != SRL_MAGIC_STRING_LILIPUTIAN) {
        NSLog(@"Bad SRL Magic, skipping packet");
        return nil;
    }
    
    index +=4;
    char vtype = bytes[index++];
    
    char version = vtype&0x0f;
    
    char type = (vtype&0xf0) >> 4;
    if (type != 0 && type != 2) {
        NSLog(@"Compression type %d not supported, only 0 and 2 are supported in this implementation", type);
        return nil;
    }
    
    srl_decoder_t dec = {
        .hdr = bytes,
        .len = someData.length,
        .ofx = &index,
        .bdy = 0,
        .cpy = NO
    };
    
    // header suffix
    long long suffixSize = read_varint(&dec);
    
    // skip the optional suffix for now;
    index += suffixSize;
    if (type == 2) {
        ssize_t blen = read_varint(&dec);
        if (blen) {
            uint32_t ulen = 0;
            
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
            index = 0;
        } else {
            NSLog(@"Can't read the length for the compressed buffer");
            return nil;
        }
    }
    
    if (version > 1)
        dec.bdy = index;
    // from here bytes points to the uncompressed body
    if (!trackingDictionary)
        trackingDictionary = [[NSMutableDictionary alloc] init];
    [trackingDictionary removeAllObjects];
    
    id object = nil;
    @try {
        object = srl_decode(&dec);
    }
    @catch (NSException *exception) {
        NSLog(@"Can't decode: %@", exception);
    }
    @finally {
        
    }
    return object;
}

@end
