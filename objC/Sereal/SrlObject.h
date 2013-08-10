//
//  SrlObject.h
//  Sereal
//
//  Created by xant on 8/10/13.
//  Copyright (c) 2013 Andrea Guzzo. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface SrlObject : NSObject <NSCopying>

+ (id)srlObject:(NSString *)className data:(id)data;
- (id)initWithName:(NSString *)className data:(id)data;

@property (nonatomic, readonly) NSString *className;
@property (nonatomic, readonly) id objData;

@end
