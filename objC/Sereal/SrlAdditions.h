//
//  SrlAdditions.h
//  Sereal
//
//  Created by Andrea Guzzo on 8/8/13.
//  Copyright (c) 2013 Andrea Guzzo. All rights reserved.
//

#import <Foundation/Foundation.h>

@protocol Sereal
@required
- (NSData *)encodeSrl;
- (id)decodeSrl:(NSData *)data;
- (id)initWithSrlData:(NSData *)data;
@end

@interface NSObject (SrlAdditions) <Sereal>

@end


@interface NSNumber (SrlAdditions) <Sereal>

+ (id)numberWithSrlData:(NSData *)data;

@end

@interface NSString (SrlAdditions) <Sereal>

+ (id)stringWithSrlData:(NSData *)data;

@end

@interface NSDictionary (SrlAdditions) <Sereal>

+ (id)dictionaryWithSrlData:(NSData *)data;

@end

@interface NSArray (SrlAdditions) <Sereal>

+ (id)arrayWithSrlData:(NSData *)data;

@end