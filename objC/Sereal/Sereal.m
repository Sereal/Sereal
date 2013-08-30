//
//  Sereal.m
//  Sereal
//
//  Created by Andrea Guzzo on 8/9/13.
//  Copyright (c) 2013 Andrea Guzzo. All rights reserved.
//

#import "Sereal.h"
#import "SrlEncoder.h"
#import "SrlDecoder.h"

@implementation Sereal

@synthesize encoder, decoder, error;

- (id)init {
    self = [super init];
    if (self) {
        encoder = [[SrlEncoder alloc] init];
        decoder = [[SrlDecoder alloc] init];
    }
    return self;
}

- (NSData *)encode:(id)obj
{
    NSData *data = [encoder encode:obj];
    error = encoder.error;
    return data;
}

- (id)decode:(NSData *)data
{
    id obj = [decoder decode:data];
    error = decoder.error;
    return obj;
}

- (NSData *)encode:(id)obj error:(NSError **)err
{    
    return [encoder encode:obj error:err];
}

- (id)decode:(NSData *)data error:(NSError **)err
{
    return [decoder decode:data error:err];
}

- (BOOL)compress
{
    return encoder.compress;
}

- (void)setCompress:(BOOL)compress
{
    encoder.compress = compress;
}

- (NSUInteger)compressionThreshold
{
    return encoder.compressionThreshold;
}

- (void)setCompressionThreshold:(NSUInteger)compressionThreshold
{
    encoder.compressionThreshold = compressionThreshold;
}

- (BOOL)decodeBinaryAsLatin1
{
    return decoder.decodeBinaryAsLatin1;
}

- (void)setDecodeBinaryAsLatin1:(BOOL)decodeBinaryAsLatin1
{
    decoder.decodeBinaryAsLatin1 = decodeBinaryAsLatin1;
}

@end
