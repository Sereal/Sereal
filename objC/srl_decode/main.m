//
//  main.c
//  srl_decode
//
//  Created by Andrea Guzzo on 8/9/13.
//  Copyright (c) 2013 Andrea Guzzo. All rights reserved.
//

#include <stdio.h>
#include <sys/stat.h>

#import "SrlDecoder.h"

int main(int argc, const char * argv[])
{
    
    if (argc < 2) {
        printf("Usage: srl_decode <path/to/serialized_data_file>");
        exit(-1);
    }
    char *filename = (char *)argv[1];
    FILE *in = fopen(filename, "r");
    if (!in) {
        printf("Can't open file %s : %s", filename, strerror(errno));
        exit(-1);
    }
    
    struct stat fstat;
    if (stat(filename, &fstat)) {
        printf("Can't read file %s : %s\n", filename, strerror(errno));
        exit(-1);
    }
    
    char *data = malloc(fstat.st_size);
    fread(data, 1, fstat.st_size, in);

    SrlDecoder *decoder = [[SrlDecoder alloc] init];
    id obj = [decoder decode:[NSData dataWithBytes:data length:fstat.st_size]];
    NSLog(@"%@\n--\n%@\n--\n", decoder, obj);
    return 0;
}

