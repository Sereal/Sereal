//
//  SerealTest.m
//  SerealTest
//
//  Created by Andrea Guzzo on 8/15/13.
//  Copyright (c) 2013 Andrea Guzzo. All rights reserved.
//

#import "SerealTest.h"
#import "SrlEncoder.h"
#import "SrlDecoder.h"
#import "SrlObject.h"

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
    
    // Set-up code here.
}

- (void)tearDown
{
    // Tear-down code here.
    
    [super tearDown];
}

- (void)testEncoding
{
    SrlEncoder *encoder = [[SrlEncoder alloc] init];
    NSDate *date = [NSDate date];
    NSDictionary *dict = [NSDictionary dictionaryWithObjectsAndKeys:
                          @"CIAO", @"key",
                          @"1", @"key2",
                          [@"CIAO" mutableCopy], @"copy",
                          @"CIAO", @"refp",
                          date, @"date",
                          [NSArray arrayWithObjects:@"BLAH", @"DIOKANE", date, nil], @"array",
                          nil];
    NSData *data = [encoder encode:dict];

    STAssertNotNil(data, @"Can't encode a simple structure");
}

- (void)testDecoding
{
    NSData *data = [NSData dataWithBytesNoCopy:encoded_test length:sizeof(encoded_test) freeWhenDone:NO];
    SrlDecoder *decoder = [[SrlDecoder alloc] init];
    id obj = [decoder decode:data];
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
    SrlEncoder *encoder = [[SrlEncoder alloc] init];
    NSDate *date = [NSDate date];
    NSString *ciao = @"CIAO";
    NSDictionary *dict = [NSDictionary dictionaryWithObjectsAndKeys:
                          ciao, @"key",
                          @"1", @"key2",
                          [ciao mutableCopy], @"copy",
                          ciao, @"refp",
                          date, @"date",
                          [NSNumber numberWithFloat:34.5], @"float",
                          [NSNumber numberWithDouble:45.7], @"double",
                          [NSArray arrayWithObjects:@"BLAH", @"DIOKANE", date, nil], @"array",
                          nil];
    NSData *data = [encoder encode:dict];
    
    SrlDecoder *decoder = [[SrlDecoder alloc] init];
    id obj = [decoder decode:data];
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
}

@end
