//
//  SrlEncoder.m
//  Sereal
//
//  Created by Andrea Guzzo on 8/8/13.
//  Copyright (c) 2013 Andrea Guzzo. All rights reserved.
//

#import "SrlEncoder.h"

#include "srl_protocol.h"
#include "csnappy.h"

#define ENCODING_BUFFER_THRESHOLD 512 // in bytes
typedef struct __srl_encoder_t {
    char    *hdr;
    ssize_t len;
    ssize_t *ofx;
} srl_encoder_t;

static NSMutableDictionary *encodedStrings = nil;
static NSMutableDictionary *encodedInstances = nil;

#define EXPAND_BUFFER(__enc, __size) {\
    int __new_size = *__enc->ofx + __size;\
    if (__new_size > __enc->len) {\
        __enc->len = MAX(__enc->len + ENCODING_BUFFER_THRESHOLD, __new_size);\
        __enc->hdr = realloc(__enc->hdr, __enc->len);\
    }\
}

static void encode_varint(char *buf, ssize_t *size, long long value)
{
    int offset = 0;
    *buf = 0;

    while (value) {
        buf[offset] =  value & 0x7f;
        value >>= 7;
        if (value)
            buf[offset] |= 0x80;
        offset++;
    }
    *size = offset + 1;
}

static void append_varint(srl_encoder_t *enc, long long value)
{
    ssize_t len = 5;
    char buf[len];
    encode_varint(buf, &len, value);
    EXPAND_BUFFER(enc, len);
    memcpy(enc->hdr + *enc->ofx, buf, len);
    (*enc->ofx) += len - 1;
}

static void srl_encode(srl_encoder_t *enc, id obj)
{
    NSNumber *encodedKey = [NSNumber numberWithInt:(int)obj];
    NSNumber *encodedOffset = [encodedInstances objectForKey:encodedKey];
    if (encodedOffset) {
        enc->hdr[(*enc->ofx)++] = SRL_HDR_REFP;
        append_varint(enc, [encodedOffset longLongValue]);
        enc->hdr[[encodedOffset longLongValue] - 1] |= SRL_HDR_TRACK_FLAG;
        return;
    } else {
        encodedOffset = [NSNumber numberWithLongLong:*enc->ofx + 1];
        [encodedInstances setObject:encodedOffset forKey:encodedKey];
    }
    
    if ([obj isKindOfClass:[NSDictionary class]]) {
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
            srl_encode(enc, key);
            id value = [obj objectForKey:key];
            srl_encode(enc, value);
        }
    } else if ([obj isKindOfClass:[NSArray class]]) {
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
    } else if ([obj isKindOfClass:[NSNumber class]]) {
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
                    value = (value << 1) ^ (value >> 63);
                }
                append_varint(enc, value);
            }
        }
    } else if ([obj isKindOfClass:[NSString class]]) {
        NSNumber *offset = [encodedStrings objectForKey:obj];
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
            [encodedStrings setObject:[NSNumber numberWithInteger:currentOffset+1] forKey:obj];
        }
    } else {
        NSData *data = [NSArchiver archivedDataWithRootObject:obj];
        NSString *className = [obj className];
        NSNumber *offset = [encodedStrings objectForKey:className];
        if (offset) {
            EXPAND_BUFFER(enc, 1);
            enc->hdr[(*enc->ofx)++] = SRL_HDR_OBJECTV;
            append_varint(enc, [offset longLongValue]);
        } else {
            EXPAND_BUFFER(enc, 1);
            enc->hdr[(*enc->ofx)++] = SRL_HDR_OBJECT;
            srl_encode(enc, [obj className]);
        }
        EXPAND_BUFFER(enc, 1);
        enc->hdr[(*enc->ofx)++] = SRL_HDR_BINARY;
        NSInteger length = [data length];
        append_varint(enc, length);
        EXPAND_BUFFER(enc, length);
        memcpy(enc->hdr + *enc->ofx, [data bytes], length);
        (*enc->ofx) += length;
    }
}

@implementation SrlEncoder

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
        .ofx = &encoder_offset
    };
    
    if (!encodedStrings) {
        encodedStrings = [[NSMutableDictionary alloc] init];
    } else {
        [encodedStrings removeAllObjects];
    }
    
    if (!encodedInstances) {
        encodedInstances = [[NSMutableDictionary alloc] init];
    } else {
        [encodedInstances removeAllObjects];
    }
    
    @try {
        srl_encode(&enc, obj);
        enc.len = *enc.ofx + 1;
        char vtype = 0x02; // version 2 no compression
        // check if it's worth using compression
        if (!self.skipCompression && enc.len > self.compressionThreshold) {
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
        *((uint32_t *)output_buffer) = SRL_MAGIC_STRING_LILIPUTIAN;
        offset += 4;
        output_buffer[offset++] = vtype;
        output_buffer[offset++] = 0x00; // no extra header options
        
        if (is_compressed) {
            memcpy(output_buffer + offset, varint, vsize);
            offset += vsize - 1;
        }
        memcpy(output_buffer + offset, enc.hdr, enc.len);
    }
    @catch (NSException *exception) {
        NSLog(@"Can't encode obj %@ : %@", obj, exception);
    }
    @finally {
        data = [NSData dataWithBytesNoCopy:output_buffer length:output_length freeWhenDone:YES];
    }
    
    free(enc.hdr);
    return data;
}

@end
