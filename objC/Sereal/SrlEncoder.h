//
//  SrlEncoder.h
//  Sereal
//
//  Created by Andrea Guzzo on 8/8/13.
//  Copyright (c) 2013 Andrea Guzzo. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface SrlEncoder : NSObject

- (NSData *)encode:(id)obj error:(NSError **)err;
- (NSData *)encode:(id)obj;

@property (nonatomic, readonly) NSError *error;

@property (nonatomic, assign) BOOL compress;
@property (nonatomic, assign) NSUInteger compressionThreshold;

// force hash keys to be string
@property (nonatomic, assign) BOOL strictHashKeys;
// assume binary/shortbinary contains strings (perl compatibility)
@property (nonatomic, assign) BOOL binaryStrings;

@end
