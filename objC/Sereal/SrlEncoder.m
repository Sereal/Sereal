//
//  SrlEncoder.m
//  Sereal
//
//  Created by Andrea Guzzo on 8/8/13.
//  Copyright (c) 2013 Andrea Guzzo. All rights reserved.
//

#import "SrlEncoder.h"
#import "SrlObject.h"

#include "srl_protocol.h"
#include "csnappy.h"

#define ENCODING_BUFFER_THRESHOLD 512 // in bytes
typedef struct __srl_encoder_t {
    char      *hdr; // header
    ssize_t    len; // len
    ssize_t   *ofx; // offset
    BOOL       shk; // strict hash keys
    BOOL        bs; // binary strings
    CFTypeRef   es; // encoded strings
    CFTypeRef   eb; // encoded binary data
    CFTypeRef   ei; // encoded instances
} srl_encoder_t;

static void srl_encode(srl_encoder_t *enc, id obj);

#define EXPAND_BUFFER(__enc, __size) {\
    int __new_size = *__enc->ofx + __size;\
    if (__new_size > __enc->len) {\
        __enc->len = MAX(__enc->len + ENCODING_BUFFER_THRESHOLD, __new_size);\
        __enc->hdr = realloc(__enc->hdr, __enc->len);\
    }\
}

static void encode_varint(char *buf, ssize_t *size, unsigned long long value)
{
    int offset = 0;
    *buf = 0;
    
    do {
        buf[offset] =  value & 0x7f;
        value >>= 7;
        if (value)
            buf[offset] |= 0x80;
        offset++;
    } while (value && offset < *size);

    *size = offset + 1;
}

static void append_varint(srl_encoder_t *enc, unsigned long long value)
{
    ssize_t len = 10;
    char buf[len];
    encode_varint(buf, &len, value);
    EXPAND_BUFFER(enc, len);
    memcpy(enc->hdr + *enc->ofx, buf, len);
    (*enc->ofx) += len - 1;
}

static BOOL reuse_object(srl_encoder_t *enc, id obj)
{
    NSMutableDictionary *encodedInstances = (__bridge NSMutableDictionary *)(enc->ei);
    NSNumber *encodedKey = [NSNumber numberWithInt:(int)obj];
    NSNumber *encodedOffset = [encodedInstances objectForKey:encodedKey];
    if (encodedOffset) {
        ssize_t prevOffset = [encodedOffset longLongValue] - 1;
        char prevHeader = enc->hdr[prevOffset] & ~SRL_HDR_TRACK_FLAG;
        EXPAND_BUFFER(enc, 1);
        if ((prevHeader&0xf0) == SRL_HDR_ARRAYREF ||
            (prevHeader&0xf0) == SRL_HDR_HASHREF)
        {
            enc->hdr[(*enc->ofx)++] = SRL_HDR_ALIAS;
        } else {
            enc->hdr[(*enc->ofx)++] = SRL_HDR_REFP;
        }
        append_varint(enc, [encodedOffset longLongValue]);
        enc->hdr[prevOffset] |= SRL_HDR_TRACK_FLAG;
        return YES;
    } else if (obj != [NSNull null] && ![obj isKindOfClass:[NSNumber class]] &&
               !(enc->shk && [obj isKindOfClass:[NSString class]]))
    {
        ssize_t offset = *enc->ofx + 1;
        
        if (([obj isKindOfClass:[NSDictionary class]] || [obj isKindOfClass:[NSArray class]]) && [obj count] >= 16)
        {
            // skip the REFN encoded with bigger arrays/dictionaries otherwise it will be a reference to a reference
            offset++;
        }
        
        encodedOffset = [NSNumber numberWithLongLong:offset];
        [encodedInstances setObject:encodedOffset forKey:encodedKey];
    }
    return NO;
}

static void encode_dictionary(srl_encoder_t *enc, NSDictionary *obj)
{
    if ([obj count] < 16) {
        EXPAND_BUFFER(enc, 1);
        enc->hdr[(*enc->ofx)++] = SRL_HDR_HASHREF_LOW | [obj count];
    } else {
        EXPAND_BUFFER(enc, 2);
        enc->hdr[(*enc->ofx)++] = SRL_HDR_REFN;
        enc->hdr[(*enc->ofx)++] = SRL_HDR_HASH;
        append_varint(enc, [obj count]);
    }
    for (id key in [obj allKeys]) {
        if (enc->shk) {
            srl_encode(enc, [NSString stringWithFormat:@"%@", key]);
        } else {
            srl_encode(enc, key);
        }
        id value = [obj objectForKey:key];
        srl_encode(enc, value);
    }
}

static void encode_array(srl_encoder_t *enc, NSArray *obj)
{
    if ([obj count] < 16) {
        EXPAND_BUFFER(enc, 1);
        enc->hdr[(*enc->ofx)++] = SRL_HDR_ARRAYREF_LOW | [obj count];
    } else {
        EXPAND_BUFFER(enc, 2);
        enc->hdr[(*enc->ofx)++] = SRL_HDR_REFN;
        enc->hdr[(*enc->ofx)++] = SRL_HDR_ARRAY;
        append_varint(enc, [obj count]);
    }
    for (id value in obj) {
        srl_encode(enc, value);
    }
}

static void encode_number(srl_encoder_t *enc, NSNumber *obj)
{
    // NOTE: objective C doesn't support long double encoding
    // ( https://developer.apple.com/library/mac/documentation/Cocoa/Conceptual/ObjCRuntimeGuide/Articles/ocrtTypeEncodings.html )
    const char *ctype = [obj objCType];
    if (strcmp(ctype, @encode(float)) == 0) {
        EXPAND_BUFFER(enc, 1);
        enc->hdr[(*enc->ofx)++] = SRL_HDR_FLOAT;
        EXPAND_BUFFER(enc, sizeof(float));
        float fval = [obj floatValue];
        memcpy(enc->hdr + *enc->ofx, &fval, sizeof(float));
        *enc->ofx += sizeof(float);
    } else if (strcmp(ctype, @encode(double)) == 0) {
        EXPAND_BUFFER(enc, 1);
        enc->hdr[(*enc->ofx)++] = SRL_HDR_DOUBLE;
        EXPAND_BUFFER(enc, sizeof(double));
        double fval = [obj doubleValue];
        memcpy(enc->hdr + *enc->ofx, &fval, sizeof(double));
        *enc->ofx += sizeof(double);
    } else if (strcmp(ctype, @encode(unsigned long long)) == 0) {
        unsigned long long value = [obj longLongValue];
        EXPAND_BUFFER(enc, 1);
        enc->hdr[(*enc->ofx)++] = SRL_HDR_VARINT;
        append_varint(enc, value);
    } else {
        long long value = [obj longLongValue];
        if (value >= -16 && value < 16) {
            EXPAND_BUFFER(enc, 1);
            if (value >= 0) {
                enc->hdr[(*enc->ofx)++] = SRL_HDR_POS | value;
            } else {
                enc->hdr[(*enc->ofx)++] = SRL_HDR_NEG | (16 + value);
            }
        } else {
            EXPAND_BUFFER(enc, 1);
            if (value >= 0) {
                enc->hdr[(*enc->ofx)++] = SRL_HDR_VARINT;
            } else {
                enc->hdr[(*enc->ofx)++] = SRL_HDR_ZIGZAG;
                value = (value << 1) ^ (value >> 63); // zigzag
            }
            append_varint(enc, value);
        }
    }
}

static void encode_string(srl_encoder_t *enc, NSString *obj)
{
    NSMutableDictionary *encodedStrings = (__bridge NSMutableDictionary *)(enc->es);

    NSNumber *offset = ([obj length] > 5) ? [encodedStrings objectForKey:obj] : nil;
    if (offset) {
        EXPAND_BUFFER(enc, 1);
        enc->hdr[(*enc->ofx)++] = SRL_HDR_COPY;
        append_varint(enc, [offset longLongValue]);
    } else {
        EXPAND_BUFFER(enc, 1);
        NSInteger currentOffset = *enc->ofx;
        enc->hdr[(*enc->ofx)++] = SRL_HDR_STR_UTF8;
        int length = [obj lengthOfBytesUsingEncoding:NSUTF8StringEncoding];
        append_varint(enc, length);
        EXPAND_BUFFER(enc, length);
        memcpy(enc->hdr + *enc->ofx, [obj UTF8String], length);
        (*enc->ofx) += length;
        if ([obj length] > 5)
            [encodedStrings setObject:[NSNumber numberWithInteger:currentOffset+1] forKey:obj];
    }
}

static void encode_binary(srl_encoder_t *enc, id obj)
{
    NSMutableDictionary *encodedStrings = (__bridge NSMutableDictionary *)(enc->es);
    NSMutableDictionary *encodedBinaryData = (__bridge NSMutableDictionary *)(enc->eb);

    NSData *data = obj;
    BOOL copied = NO;
    // in perl compatibility mode (or if binaryStrings is true)
    // the binary encoder is used for strings as well
    if ([obj isKindOfClass:[NSString class]]) {
        
        if (![obj canBeConvertedToEncoding:NSISOLatin1StringEncoding])
            return encode_string(enc, obj);
        
        NSCharacterSet *notAsciiSet =
        [[NSCharacterSet alphanumericCharacterSet] invertedSet];
        
         BOOL notAscii = [obj rangeOfCharacterFromSet:notAsciiSet].location == NSNotFound
                       ? NO
                       : YES;
        if (notAscii)
            return encode_string(enc, obj);

        NSNumber *offset = ([obj length] > 5) ? [encodedStrings objectForKey:obj] : nil;
        if (offset) {
            EXPAND_BUFFER(enc, 1);
            enc->hdr[(*enc->ofx)++] = SRL_HDR_COPY;
            append_varint(enc, [offset longLongValue]);
            copied = YES;
        } else {
            if ([obj length] > 5)
                [encodedStrings setObject:[NSNumber numberWithInteger:(*enc->ofx)+1] forKey:obj];
        }
        data = [obj dataUsingEncoding:NSISOLatin1StringEncoding];
    } else {
        // check if we already encoded the same binary payload earlier in the message
        NSNumber *offset = ([obj length] > 5) ? [encodedBinaryData objectForKey:obj] : nil;
        if (offset) {
            EXPAND_BUFFER(enc, 1);
            enc->hdr[(*enc->ofx)++] = SRL_HDR_COPY;
            append_varint(enc, [offset longLongValue]);
            copied = YES;
        } else {
            if ([obj length] > 5)
                [encodedBinaryData setObject:[NSNumber numberWithInteger:(*enc->ofx)+1] forKey:obj];
        }
    }
    if (!copied) {
        int length = [data length];
        
        if (length < 16) {
            EXPAND_BUFFER(enc, 1);
            enc->hdr[(*enc->ofx)++] = SRL_HDR_SHORT_BINARY | length;
        } else {
            EXPAND_BUFFER(enc, 1);
            enc->hdr[(*enc->ofx)++] = SRL_HDR_BINARY;
            append_varint(enc, length);
        }
        EXPAND_BUFFER(enc, length);
        memcpy(enc->hdr + *enc->ofx, [data bytes], length);
        (*enc->ofx) += length;
    }
}

static void encode_object(srl_encoder_t *enc, id obj)
{
    NSMutableDictionary *encodedStrings = (__bridge NSMutableDictionary *)(enc->es);

    NSData *data = nil;
    NSString *className = nil;
    
    BOOL frozen = NO;
    
    if ([obj isKindOfClass:[SrlObject class]]) {
        data = ((SrlObject *)obj).objData;
        className = ((SrlObject *)obj).className;
    } else {
        data = [NSArchiver archivedDataWithRootObject:obj];
        className = [obj className];
        frozen = YES;
    }
    NSNumber *offset = [encodedStrings objectForKey:className];
    if (offset) {
        EXPAND_BUFFER(enc, 1);
        enc->hdr[(*enc->ofx)++] = frozen ? SRL_HDR_OBJECTV_FREEZE : SRL_HDR_OBJECTV;
        append_varint(enc, [offset longLongValue]);
    } else {
        EXPAND_BUFFER(enc, 1);
        enc->hdr[(*enc->ofx)++] = frozen ? SRL_HDR_OBJECT_FREEZE : SRL_HDR_OBJECT;
        srl_encode(enc, [obj className]);
    }
    if ([data isKindOfClass:[NSData class]]) {
        EXPAND_BUFFER(enc, 1);
        enc->hdr[(*enc->ofx)++] = SRL_HDR_REFN;
        EXPAND_BUFFER(enc, 1);
        enc->hdr[(*enc->ofx)++] = SRL_HDR_BINARY;
        NSInteger length = [data length];
        append_varint(enc, length);
        EXPAND_BUFFER(enc, length);
        memcpy(enc->hdr + *enc->ofx, [data bytes], length);
        (*enc->ofx) += length;
    } else {
        if (!([data isKindOfClass:[NSDictionary class]] || [data isKindOfClass:[NSArray class]])) {
            // perl requires object data always to be a reference so unless it's going to be an hashref or an arrayref
            // we need to reference the object data (which could also be a string or a number)
            EXPAND_BUFFER(enc, 1);
            enc->hdr[(*enc->ofx)++] = SRL_HDR_REFN;
        }
        srl_encode(enc, data);
    }
}

static void srl_encode(srl_encoder_t *enc, id obj)
{
    // check if we already encoded an instance of obj
    // which we can reuse as REFP/ALIAS
    if (reuse_object(enc, obj))
        return;
    
    if (obj == [NSNull null]) {
        EXPAND_BUFFER(enc, 1);
        enc->hdr[(*enc->ofx)++] = SRL_HDR_UNDEF;
    } else if ([obj isKindOfClass:[NSDictionary class]]) {
        encode_dictionary(enc, obj);
    } else if ([obj isKindOfClass:[NSArray class]]) {
        encode_array(enc, obj);
    } else if ([obj isKindOfClass:[NSNumber class]]) {
        encode_number(enc, obj);
    } else if (!enc->bs && [obj isKindOfClass:[NSString class]]) {
        encode_string(enc, obj);
    } else if ([obj isKindOfClass:[NSData class]] || (enc->bs && [obj isKindOfClass:[NSString class]])) {
        encode_binary(enc, obj);
    } else {
        encode_object(enc, obj);
    }
}

@implementation SrlEncoder

@synthesize error;

- (NSData *)encode:(id)obj error:(NSError **)err
{
    NSData *data = [self encode:obj];
    if (err)
        *err = self.error;
    return data;
}

- (NSData *)encode:(id)obj
{
    NSData *data = nil;
    ssize_t offset = 0;
    ssize_t encoder_offset = 0;
    ssize_t buffer_length = ENCODING_BUFFER_THRESHOLD; // base header length
    char *output_buffer = nil;
    ssize_t output_length = 0;
    srl_encoder_t enc = {
        .hdr = calloc(1, buffer_length),
        .len = buffer_length,
        .ofx = &encoder_offset,
        .shk = self.strictHashKeys,
        .bs  = self.binaryStrings,
        .es = CFBridgingRetain([[NSMutableDictionary alloc] init]),
        .ei = CFBridgingRetain([[NSMutableDictionary alloc] init]),
        .eb = CFBridgingRetain([[NSMutableDictionary alloc] init]),
    };
    
    @try {
        srl_encode(&enc, obj);
        enc.len = *enc.ofx;
        char vtype = 0x02; // version 2 no compression
        // check if it's worth using compression
        if (self.compress && enc.len > self.compressionThreshold) {
            uint32_t compressed_length = csnappy_max_compressed_length(enc.len);
            char *compressed_buffer = malloc(compressed_length);
            char *working_memory = malloc(1 << 12);
            csnappy_compress(enc.hdr, enc.len, compressed_buffer, &compressed_length, working_memory, 12);
            
            // don't compress the output if thre is no benefit (read: the uncompressed buffer would be shorter)
            if (compressed_length < enc.len) {
                free(enc.hdr);
                enc.hdr = compressed_buffer;
                enc.len = compressed_length;
                vtype |= 0x02 << SRL_PROTOCOL_VERSION_BITS; // enable compression
            } else {
                free(compressed_buffer);
            }
            free(working_memory);
        }
        
        output_length = 6;
        ssize_t vsize = 5;
        char varint[vsize];
        BOOL is_compressed = vtype & SRL_PROTOCOL_ENCODING_MASK;

        if (is_compressed) {
            encode_varint(varint, &vsize, enc.len);
            output_length += vsize;
        }
        
        output_length += enc.len;
        output_buffer = calloc(1, output_length);
        *((uint32_t *)output_buffer) = SRL_MAGIC_STRING_UINT_LE;
        offset += 4;
        output_buffer[offset++] = vtype;
        output_buffer[offset++] = 0x00; // no extra header options
        
        if (is_compressed) {
            memcpy(output_buffer + offset, varint, vsize);
            offset += vsize - 1;
        }
        memcpy(output_buffer + offset, enc.hdr, enc.len);
        error = nil;
        data = [NSData dataWithBytesNoCopy:output_buffer length:output_length freeWhenDone:YES];
    }
    @catch (NSException *exception) {
        NSString *errorMessage = [NSString stringWithFormat:@"Can't encode obj %@ : %@", obj, exception];
        error = [NSError errorWithDomain:@"SrlEncoder"
                                    code:-1
                                userInfo:[NSDictionary dictionaryWithObject:errorMessage
                                                                     forKey:NSLocalizedDescriptionKey]];
    }
    CFBridgingRelease(enc.ei);
    CFBridgingRelease(enc.es);
    CFBridgingRelease(enc.eb);
    free(enc.hdr);
    return data;
}

@end
