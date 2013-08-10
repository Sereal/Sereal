//
//  SrlAdditions.m
//  Sereal
//
//  Created by Andrea Guzzo on 8/8/13.
//  Copyright (c) 2013 Andrea Guzzo. All rights reserved.
//

#import "SrlAdditions.h"

@implementation NSObject (SrlAdditions)

- (NSData *)encodeSrl
{
    return nil;
}

- (id)decodeSrl:(NSData *)data
{
    return self;
}

- (id)initWithSrlData:(NSData *)data
{
    return self;
}

@end



@implementation NSNumber (SrlAdditions)

+ (id)numberWithSrlData:(NSData *)data
{
    return [[self alloc] initWithSrlData:data];
}

@end


@implementation NSString (SrlAdditions)

+ (id)stringWithSrlData:(NSData *)data
{
    return [[self alloc] initWithSrlData:data];
}

@end