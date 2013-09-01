//
//  SerealTest.m
//  SerealTest
//
//  Created by Andrea Guzzo on 8/15/13.
//  Copyright (c) 2013 Andrea Guzzo. All rights reserved.
//

#import "SerealTest.h"
#import "Sereal.h"
#import "SrlObject.h"
#import "SrlEncoder.h"
#import <Foundation/Foundation.h>

static Sereal *sereal = nil;

/* encoded buffer generated from perl running:
 ---
 use Sereal::Encoder qw(encode_sereal);
 
 
 my $encoder = Sereal::Encoder->new({ snappy => 0, snappy_threshold => 0 });
 $hash = { a => 1, b => [2,3,4,5], c => undef, d => "CIAO" };
 
 my $out = $encoder->encode([$hash, { %$hash, c => "kakka", m => qr/some\s+regexp\s+pattern/i }, $encoder]);
 print $out;
 ---
 */
static char encoded_test[] = {
    0x3d, 0x73, 0x72, 0x6c, 0x22, 0x00, 0xf2, 0x00, 0x73, 0xf0, 0x40, 0x43, 0x28, 0x2a, 0x04, 0x61,
    0x63, 0x25, 0x61, 0x61, 0x01, 0x61, 0x62, 0x28, 0xab, 0x04, 0x02, 0x03, 0x04, 0x05, 0x61, 0x64,
    0x64, 0x43, 0x49, 0x41, 0x4f, 0x28, 0x2a, 0x05, 0x2f, 0x05, 0x65, 0x6b, 0x61, 0x6b, 0x6b, 0x61,
    0x2f, 0x08, 0x01, 0x2f, 0x0b, 0x29, 0x0e, 0x61, 0x6d, 0x2c, 0x66, 0x52, 0x65, 0x67, 0x65, 0x78,
    0x70, 0x28, 0x31, 0x77, 0x73, 0x6f, 0x6d, 0x65, 0x5c, 0x73, 0x2b, 0x72, 0x05, 0x10, 0x34, 0x5c,
    0x73, 0x2b, 0x70, 0x61, 0x74, 0x74, 0x65, 0x72, 0x6e, 0x61, 0x69, 0x2f, 0x14, 0x05, 0x3f, 0x64,
    0x2c, 0x6f, 0x53, 0x65, 0x72, 0x65, 0x61, 0x6c, 0x3a, 0x3a, 0x45, 0x6e, 0x63, 0x6f, 0x64, 0x65,
    0x72, 0x28, 0x20, 0xd0, 0xe4, 0x85, 0xc6, 0x8c, 0xf4, 0x1f
};

@implementation SerealTest

- (void)setUp
{
    [super setUp];
    sereal = [[Sereal alloc] init];
    // Set-up code here.
}

- (void)tearDown
{
    // Tear-down code here.
    
    [super tearDown];
}

- (void)testEncoding
{
    NSDate *date = [NSDate date];
    NSDictionary *dict = [NSDictionary dictionaryWithObjectsAndKeys:
                          @"CIAO", @"key",
                          @"1", @"key2",
                          [@"CIAO" mutableCopy], @"copy",
                          @"CIAO", @"refp",
                          date, @"date",
                          [NSArray arrayWithObjects:@"BLAH", @"DIOKANE", date, nil], @"array",
                          nil];
    NSData *data = [dict encodeSrl];//[sereal encode:dict];

    STAssertNotNil(data, @"Can't encode a simple structure");
}

- (void)testDecoding
{
    NSData *data = [NSData dataWithBytesNoCopy:encoded_test length:sizeof(encoded_test) freeWhenDone:NO];
    
    [NSData setPerlCompatible:YES]; // sereal.perlCompatible = YES;
    id obj = [data decodeSrl]; // [sereal decode:data];
    [NSData setPerlCompatible:NO]; // sereal.perlCompatible = NO;

    STAssertNotNil(obj, @"Can't decode a simple message");
    STAssertTrue([obj isKindOfClass:[NSArray class]], @"Decoded object is not an array");
    STAssertTrue([obj count] == 3, @"Array count doesn't match");
    STAssertTrue([[obj objectAtIndex:2] isKindOfClass:[SrlObject class]], @"Last element is not an SrlObject");
    NSDictionary *dict1 = [obj objectAtIndex:0];
    STAssertTrue([dict1 isKindOfClass:[NSDictionary class]], @"Decoded object is not a dictionary");

    NSDictionary *dict2 = [obj objectAtIndex:1];
    STAssertTrue([dict2 isKindOfClass:[NSDictionary class]], @"Decoded object is not a dictionary");
    
    NSArray *ar1 = [dict1 objectForKey:@"b"];
    STAssertTrue([ar1 isKindOfClass:[NSArray class]], @"Decoded object is not an array");

    NSArray *ar2 = [dict2 objectForKey:@"b"];
    STAssertTrue([ar2 isKindOfClass:[NSArray class]], @"Decoded object is not an array");
    
    STAssertTrue(ar1 == ar2, @"The two arrays are not the same instance");
    
    STAssertTrue([dict1 objectForKey:@"c"] == [NSNull null], @"'c' is not null in the first dictionary");
    
    STAssertTrue([[dict2 objectForKey:@"c"] isEqual:@"kakka"], @"'c' is not what expected in the second dictionary");
}

- (void)testRoundTrip
{
    NSDate *date = [NSDate date];
    NSString *ciao = @"CIAO";
    char bytes[6] = { 'a', 'b', 'c', 'd', 'e', 'f' };
    NSData *dataref = [NSData dataWithBytes:bytes length:6];
    NSDictionary *dict = [NSDictionary dictionaryWithObjectsAndKeys:
                          ciao, @"key",
                          @"1", @"key2",
                          [ciao mutableCopy], @"copy",
                          ciao, @"refp",
                          date, @"date",
                          dataref, @"data1",
                          dataref, @"data2",
                          [dataref mutableCopy], @"data3",
                          [NSNumber numberWithFloat:34.5], @"float",
                          [NSNumber numberWithDouble:45.7], @"double",
                          [NSArray arrayWithObjects:@"BLAH", @"DIOKANE", date, nil], @"array",
                          [NSArray arrayWithObjects:@-17, @-16, @-15, @-14, @-13, @-12, @-11, @-10, @-9, @-8, @-7, @-6, @-5, @-4, @-3, @-2, @-1,
                           @0, @1, @2, @3, @4, @5, @6, @7, @8, @9, @10, @11, @12, @13, @14, @15, @16, @17, nil], @"array2",
                          nil];
    NSData *data = [dict encodeSrl];//[sereal encode:dict];
    
    id obj = [data decodeSrl];//[sereal decode:data];
    NSLog(@"%@", obj);

    STAssertTrue([obj isKindOfClass:[NSDictionary class]], @"Didn't get back a dictionary");
    STAssertTrue([obj count] == [dict count], @"Didn't get the same amount of items in the decoded dictionary");

    NSString *ciaostr = [obj objectForKey:@"key"];
    STAssertEqualObjects(ciaostr, ciao, @"Didn't get the same value for 'key'");
    NSString *ciaoref = [obj objectForKey:@"refp"];
    //STAssertEquals(ciaoref, ciao, @"Didn't get the same instance for 'refp'");
    STAssertTrue(ciaostr == ciaoref, @"Didn't get the same instance for 'refp'");

    NSString *ciaocopy = [obj objectForKey:@"copy"];
    STAssertFalse(ciaostr == ciaocopy, @"Got the same instance for 'copy'");
    STAssertEqualObjects(ciaostr, ciaocopy, @"Didn't get the same value for 'copy'");
    
    id dateObj = [obj objectForKey:@"date"];
    STAssertTrue([dateObj isKindOfClass:[NSDate class]], @"Can't get back an object for 'date'");
    STAssertEqualObjects(dateObj, date, @"Date objects differ");
    
    id arrayObj = [obj objectForKey:@"array"];
    STAssertTrue([arrayObj isKindOfClass:[NSArray class]], @"Can't get back an array object");
    STAssertTrue([arrayObj count] == 3, @"Didn't get the same amount of items inside the array object");
    
    id dateRef = [arrayObj lastObject];
    STAssertTrue([dateRef isKindOfClass:[NSDate class]], @"Can't get a date object as last element of the array");
    STAssertTrue(dateRef == dateObj, @"Didn't get the same instance for second date object (should be a refp)");

    
    id floatObj = [obj objectForKey:@"float"];
    STAssertTrue([floatObj isKindOfClass:[NSNumber class]], @"Can't get a number for 'float'");
    STAssertTrue(strcmp([floatObj objCType], @encode(float)) == 0, @"Can't get a float back");
    STAssertTrue(34.5 == [floatObj floatValue], @"Can't get the correct float value back");
    
    id doubleObj = [obj objectForKey:@"double"];
    STAssertTrue([doubleObj isKindOfClass:[NSNumber class]], @"Can't get a number for 'double'");
    STAssertTrue(strcmp([doubleObj objCType], @encode(double)) == 0, @"Can't get a double back");
    STAssertTrue(45.7 == [doubleObj doubleValue], @"Can't get the correct double value back");

    NSArray *array2 = [obj objectForKey:@"array2"];
    STAssertTrue([array2 isKindOfClass:[NSArray class]], @"Can't get back an array object");
    STAssertTrue([array2 count] == 35, @"Didn't get the same amount of items inside the array object");
    for (int i = -17; i <= 17; i++) {
        NSNumber *num = [array2 objectAtIndex:(i + 17)];
        STAssertTrue([num isKindOfClass:[NSNumber class]], @"Can't get a number from array2");
        STAssertTrue([num intValue] == i, @"Wrong number found in array2");
    }
    
    NSData *data1 = [obj objectForKey:@"data1"];
    STAssertTrue([data1 isKindOfClass:[NSData class]], @"Can't get back a NSData object");

    NSData *data2 = [obj objectForKey:@"data2"];
    STAssertTrue([data2 isKindOfClass:[NSData class]], @"Can't get back a NSData object");
    STAssertTrue(data1 == data2, @"NSData objects are not the same instances");
    
    NSData *data3 = [obj objectForKey:@"data3"];
    STAssertTrue([data3 isKindOfClass:[NSData class]], @"Can't get back a NSData object");
    STAssertTrue(data1 != data3, @"NSData objects are not the same instances");
    STAssertEqualObjects(data1, data3, @"NSData objects differ");

    // TODO - test encoding/decoding of SrlObjects VS native objects
}

//- (void)testProcessEvents
//{
//    NSError *err = nil;
//    sereal.perlCompatible = YES;
//    NSString *eventsDir = @"/Users/xant/src/events";
//    NSFileManager *fm = [NSFileManager defaultManager];
//    NSArray *files = [fm contentsOfDirectoryAtPath:eventsDir error:&err];
//    if (!files && err) {
//        STFail(@"Can't open eventsDir %@ : %@",eventsDir, err);
//    }
//    for (NSString *fName in files) {
////        if (![fName isEqualToString:@"test_000816.in"])
////            continue;
//        NSString *fullPath = [NSString stringWithFormat:@"%@/%@", eventsDir, fName];
//        if (![fullPath hasSuffix:@".in"])
//            continue;
//        NSData *data = [fm contentsAtPath:fullPath];
//        id obj = [sereal decode:data error:&err];
//        if (!obj && err) {
//            STFail(@"Can't decode: %@", err);
//        }
//        NSData *encodedData = [sereal encode:obj error:&err];
//        if (!encodedData && err) {
//            STFail(@"Can't encode: %@", err);
//        }
//        
//        NSString *newFileName = [fName stringByReplacingCharactersInRange:NSMakeRange(fName.length - 3, 3) withString:@".out"];
//        [encodedData writeToFile:[NSString stringWithFormat:@"%@/%@", eventsDir, newFileName] atomically:YES];
//    }
//}
@end
