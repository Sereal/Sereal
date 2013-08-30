//
//  SrlDecoder.h
//  Sereal
//
//  Created by Andrea Guzzo on 8/8/13.
//  Copyright (c) 2013 Andrea Guzzo. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface SrlDecoder : NSObject

- (id)decode:(NSData *)someData;
- (id)decode:(NSData *)someData error:(NSError **)err;

@property (nonatomic, readonly) NSError *error;
@property (nonatomic, assign) BOOL decodeBinaryAsLatin1; // for compatibility with strings encoded from perl
@end
