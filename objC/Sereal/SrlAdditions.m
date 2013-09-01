//
//  SrlAdditions.m
//  Sereal
//
//  Created by Andrea Guzzo on 8/8/13.
//  Copyright (c) 2013 Andrea Guzzo. All rights reserved.
//

#import "SrlAdditions.h"
#import "Sereal.h"

static Sereal *__sereal = nil;

@implementation NSObject (SrlAdditions)
+ (Sereal *)sereal
{
    if (!__sereal)
        __sereal = [[Sereal alloc] init];
    return __sereal;
}

+ (void)setBinaryStrings:(BOOL)binaryStrings
{
    [[self sereal] setBinaryStrings:binaryStrings];
}

+ (void)setStrictHashKeys:(BOOL)strictHashKeys
{
    [[self sereal] setStrictHashKeys:strictHashKeys];
}

+ (void)setPerlCompatible:(BOOL)perlCompatible
{
    [[self sereal] setPerlCompatible:perlCompatible];

}

+ (void)setCompress:(BOOL)compress
{
    [[self sereal] setCompress:compress];
}

+ (void)setCompressionThreshold:(NSUInteger)compressionThreshold
{
    [[self sereal] setCompressionThreshold:compressionThreshold];
}

+ (id)decodeSrl:(NSData *)data
{
    return [[self sereal] decode:data];
}

- (NSData *)encodeSrl
{
    return [[NSObject sereal] encode:self];
}

@end

@implementation NSData (SrlAdditions)

- (id)decodeSrl
{
    return [[NSObject sereal] decode:self];
}

@end

@implementation NSNumber (SrlAdditions)

@end