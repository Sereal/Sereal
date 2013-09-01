//
//  SrlAdditions.h
//  Sereal
//
//  Created by Andrea Guzzo on 8/8/13.
//  Copyright (c) 2013 Andrea Guzzo. All rights reserved.
//

#import <Foundation/Foundation.h>



@interface NSObject (SrlAdditions)
+ (void)setBinaryStrings:(BOOL)binaryStrings;
+ (void)setStrictHashKeys:(BOOL)strictHashKeys;
+ (void)setPerlCompatible:(BOOL)perlCompatible;
+ (void)setCompress:(BOOL)compress;
+ (void)setCompressionThreshold:(NSUInteger)compressionThreshold;

- (NSData *)encodeSrl;
+ (id)decodeSrl:(NSData *)data;
@end

@interface NSData (SrlAdditions)
- (id)decodeSrl;
@end
