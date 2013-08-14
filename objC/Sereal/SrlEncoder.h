//
//  SrlEncoder.h
//  Sereal
//
//  Created by Andrea Guzzo on 8/8/13.
//  Copyright (c) 2013 Andrea Guzzo. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface SrlEncoder : NSObject

- (NSData *)encode:(id)obj;

@property (nonatomic, assign) BOOL skipCompression;

@end
