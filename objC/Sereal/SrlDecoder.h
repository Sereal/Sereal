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

// assume binary/shortbinary contains strings (perl compatibility)
@property (nonatomic, assign) BOOL binaryStrings;
@end
