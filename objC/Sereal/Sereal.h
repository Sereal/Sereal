//
//  Sereal.h
//  Sereal
//
//  Created by Andrea Guzzo on 8/9/13.
//  Copyright (c) 2013 Andrea Guzzo. All rights reserved.
//

#import <Foundation/Foundation.h>

@class SrlEncoder;
@class SrlDecoder;

@interface Sereal : NSObject

@property (assign) BOOL compress;
@property (nonatomic, assign) BOOL decodeBinaryAsLatin1;
@property (nonatomic, assign) NSUInteger compressionThreshold;

@property (readonly) SrlEncoder *encoder;
@property (readonly) SrlDecoder *decoder;
@property (readonly) NSError *error;

- (NSData *)encode:(id)obj;
- (NSData *)encode:(id)data error:(NSError **)err;

- (id)decode:(NSData *)obj;
- (id)decode:(NSData *)data error:(NSError **)err;

@end
