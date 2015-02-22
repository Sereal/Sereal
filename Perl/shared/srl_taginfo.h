#ifndef SRL_TAGINFO_H
#define SRL_TAGINFO_H

#define SRL_TAG_NAME(tag) (tag_name[(tag) & 127])

/*

=for autoupdater start

* NOTE this section is autoupdated by author_tools/update_from_header.pl
*/

static const char * const tag_name[] = {
	"POS_0",             /*        0 0x00 0b00000000 */
	"POS_1",             /*        1 0x01 0b00000001 */
	"POS_2",             /*        2 0x02 0b00000010 */
	"POS_3",             /*        3 0x03 0b00000011 */
	"POS_4",             /*        4 0x04 0b00000100 */
	"POS_5",             /*        5 0x05 0b00000101 */
	"POS_6",             /*        6 0x06 0b00000110 */
	"POS_7",             /* "\a"   7 0x07 0b00000111 */
	"POS_8",             /* "\b"   8 0x08 0b00001000 */
	"POS_9",             /* "\t"   9 0x09 0b00001001 */
	"POS_10",            /* "\n"  10 0x0a 0b00001010 */
	"POS_11",            /*       11 0x0b 0b00001011 */
	"POS_12",            /* "\f"  12 0x0c 0b00001100 */
	"POS_13",            /* "\r"  13 0x0d 0b00001101 */
	"POS_14",            /*       14 0x0e 0b00001110 */
	"POS_15",            /*       15 0x0f 0b00001111 */
	"NEG_16",            /*       16 0x10 0b00010000 */
	"NEG_15",            /*       17 0x11 0b00010001 */
	"NEG_14",            /*       18 0x12 0b00010010 */
	"NEG_13",            /*       19 0x13 0b00010011 */
	"NEG_12",            /*       20 0x14 0b00010100 */
	"NEG_11",            /*       21 0x15 0b00010101 */
	"NEG_10",            /*       22 0x16 0b00010110 */
	"NEG_9",             /*       23 0x17 0b00010111 */
	"NEG_8",             /*       24 0x18 0b00011000 */
	"NEG_7",             /*       25 0x19 0b00011001 */
	"NEG_6",             /*       26 0x1a 0b00011010 */
	"NEG_5",             /* "\e"  27 0x1b 0b00011011 */
	"NEG_4",             /*       28 0x1c 0b00011100 */
	"NEG_3",             /*       29 0x1d 0b00011101 */
	"NEG_2",             /*       30 0x1e 0b00011110 */
	"NEG_1",             /*       31 0x1f 0b00011111 */
	"VARINT",            /* " "   32 0x20 0b00100000 */
	"ZIGZAG",            /* "!"   33 0x21 0b00100001 */
	"FLOAT",             /* "\""  34 0x22 0b00100010 */
	"DOUBLE",            /* "#"   35 0x23 0b00100011 */
	"LONG_DOUBLE",       /* "\$"  36 0x24 0b00100100 */
	"UNDEF",             /* "%"   37 0x25 0b00100101 */
	"BINARY",            /* "&"   38 0x26 0b00100110 */
	"STR_UTF8",          /* "'"   39 0x27 0b00100111 */
	"REFN",              /* "("   40 0x28 0b00101000 */
	"REFP",              /* ")"   41 0x29 0b00101001 */
	"HASH",              /* "*"   42 0x2a 0b00101010 */
	"ARRAY",             /* "+"   43 0x2b 0b00101011 */
	"OBJECT",            /* ","   44 0x2c 0b00101100 */
	"OBJECTV",           /* "-"   45 0x2d 0b00101101 */
	"ALIAS",             /* "."   46 0x2e 0b00101110 */
	"COPY",              /* "/"   47 0x2f 0b00101111 */
	"WEAKEN",            /* "0"   48 0x30 0b00110000 */
	"REGEXP",            /* "1"   49 0x31 0b00110001 */
	"OBJECT_FREEZE",     /* "2"   50 0x32 0b00110010 */
	"OBJECTV_FREEZE",    /* "3"   51 0x33 0b00110011 */
	"RESERVED_0",        /* "4"   52 0x34 0b00110100 */
	"RESERVED_1",        /* "5"   53 0x35 0b00110101 */
	"RESERVED_2",        /* "6"   54 0x36 0b00110110 */
	"RESERVED_3",        /* "7"   55 0x37 0b00110111 */
	"RESERVED_4",        /* "8"   56 0x38 0b00111000 */
	"CANONICAL_UNDEF",   /* "9"   57 0x39 0b00111001 */
	"FALSE",             /* ":"   58 0x3a 0b00111010 */
	"TRUE",              /* ";"   59 0x3b 0b00111011 */
	"MANY",              /* "<"   60 0x3c 0b00111100 */
	"PACKET_START",      /* "="   61 0x3d 0b00111101 */
	"EXTEND",            /* ">"   62 0x3e 0b00111110 */
	"PAD",               /* "?"   63 0x3f 0b00111111 */
	"ARRAYREF_0",        /* "\@"  64 0x40 0b01000000 */
	"ARRAYREF_1",        /* "A"   65 0x41 0b01000001 */
	"ARRAYREF_2",        /* "B"   66 0x42 0b01000010 */
	"ARRAYREF_3",        /* "C"   67 0x43 0b01000011 */
	"ARRAYREF_4",        /* "D"   68 0x44 0b01000100 */
	"ARRAYREF_5",        /* "E"   69 0x45 0b01000101 */
	"ARRAYREF_6",        /* "F"   70 0x46 0b01000110 */
	"ARRAYREF_7",        /* "G"   71 0x47 0b01000111 */
	"ARRAYREF_8",        /* "H"   72 0x48 0b01001000 */
	"ARRAYREF_9",        /* "I"   73 0x49 0b01001001 */
	"ARRAYREF_10",       /* "J"   74 0x4a 0b01001010 */
	"ARRAYREF_11",       /* "K"   75 0x4b 0b01001011 */
	"ARRAYREF_12",       /* "L"   76 0x4c 0b01001100 */
	"ARRAYREF_13",       /* "M"   77 0x4d 0b01001101 */
	"ARRAYREF_14",       /* "N"   78 0x4e 0b01001110 */
	"ARRAYREF_15",       /* "O"   79 0x4f 0b01001111 */
	"HASHREF_0",         /* "P"   80 0x50 0b01010000 */
	"HASHREF_1",         /* "Q"   81 0x51 0b01010001 */
	"HASHREF_2",         /* "R"   82 0x52 0b01010010 */
	"HASHREF_3",         /* "S"   83 0x53 0b01010011 */
	"HASHREF_4",         /* "T"   84 0x54 0b01010100 */
	"HASHREF_5",         /* "U"   85 0x55 0b01010101 */
	"HASHREF_6",         /* "V"   86 0x56 0b01010110 */
	"HASHREF_7",         /* "W"   87 0x57 0b01010111 */
	"HASHREF_8",         /* "X"   88 0x58 0b01011000 */
	"HASHREF_9",         /* "Y"   89 0x59 0b01011001 */
	"HASHREF_10",        /* "Z"   90 0x5a 0b01011010 */
	"HASHREF_11",        /* "["   91 0x5b 0b01011011 */
	"HASHREF_12",        /* "\\"  92 0x5c 0b01011100 */
	"HASHREF_13",        /* "]"   93 0x5d 0b01011101 */
	"HASHREF_14",        /* "^"   94 0x5e 0b01011110 */
	"HASHREF_15",        /* "_"   95 0x5f 0b01011111 */
	"SHORT_BINARY_0",    /* "`"   96 0x60 0b01100000 */
	"SHORT_BINARY_1",    /* "a"   97 0x61 0b01100001 */
	"SHORT_BINARY_2",    /* "b"   98 0x62 0b01100010 */
	"SHORT_BINARY_3",    /* "c"   99 0x63 0b01100011 */
	"SHORT_BINARY_4",    /* "d"  100 0x64 0b01100100 */
	"SHORT_BINARY_5",    /* "e"  101 0x65 0b01100101 */
	"SHORT_BINARY_6",    /* "f"  102 0x66 0b01100110 */
	"SHORT_BINARY_7",    /* "g"  103 0x67 0b01100111 */
	"SHORT_BINARY_8",    /* "h"  104 0x68 0b01101000 */
	"SHORT_BINARY_9",    /* "i"  105 0x69 0b01101001 */
	"SHORT_BINARY_10",   /* "j"  106 0x6a 0b01101010 */
	"SHORT_BINARY_11",   /* "k"  107 0x6b 0b01101011 */
	"SHORT_BINARY_12",   /* "l"  108 0x6c 0b01101100 */
	"SHORT_BINARY_13",   /* "m"  109 0x6d 0b01101101 */
	"SHORT_BINARY_14",   /* "n"  110 0x6e 0b01101110 */
	"SHORT_BINARY_15",   /* "o"  111 0x6f 0b01101111 */
	"SHORT_BINARY_16",   /* "p"  112 0x70 0b01110000 */
	"SHORT_BINARY_17",   /* "q"  113 0x71 0b01110001 */
	"SHORT_BINARY_18",   /* "r"  114 0x72 0b01110010 */
	"SHORT_BINARY_19",   /* "s"  115 0x73 0b01110011 */
	"SHORT_BINARY_20",   /* "t"  116 0x74 0b01110100 */
	"SHORT_BINARY_21",   /* "u"  117 0x75 0b01110101 */
	"SHORT_BINARY_22",   /* "v"  118 0x76 0b01110110 */
	"SHORT_BINARY_23",   /* "w"  119 0x77 0b01110111 */
	"SHORT_BINARY_24",   /* "x"  120 0x78 0b01111000 */
	"SHORT_BINARY_25",   /* "y"  121 0x79 0b01111001 */
	"SHORT_BINARY_26",   /* "z"  122 0x7a 0b01111010 */
	"SHORT_BINARY_27",   /* "{"  123 0x7b 0b01111011 */
	"SHORT_BINARY_28",   /* "|"  124 0x7c 0b01111100 */
	"SHORT_BINARY_29",   /* "}"  125 0x7d 0b01111101 */
	"SHORT_BINARY_30",   /* "~"  126 0x7e 0b01111110 */
	"SHORT_BINARY_31"    /*      127 0x7f 0b01111111 */
};

#define SRL_HDR_POS_0                  0
#define SRL_HDR_POS_1                  1
#define SRL_HDR_POS_2                  2
#define SRL_HDR_POS_3                  3
#define SRL_HDR_POS_4                  4
#define SRL_HDR_POS_5                  5
#define SRL_HDR_POS_6                  6
#define SRL_HDR_POS_7                  7
#define SRL_HDR_POS_8                  8
#define SRL_HDR_POS_9                  9
#define SRL_HDR_POS_10                10
#define SRL_HDR_POS_11                11
#define SRL_HDR_POS_12                12
#define SRL_HDR_POS_13                13
#define SRL_HDR_POS_14                14
#define SRL_HDR_POS_15                15
#define SRL_HDR_NEG_16                16
#define SRL_HDR_NEG_15                17
#define SRL_HDR_NEG_14                18
#define SRL_HDR_NEG_13                19
#define SRL_HDR_NEG_12                20
#define SRL_HDR_NEG_11                21
#define SRL_HDR_NEG_10                22
#define SRL_HDR_NEG_9                 23
#define SRL_HDR_NEG_8                 24
#define SRL_HDR_NEG_7                 25
#define SRL_HDR_NEG_6                 26
#define SRL_HDR_NEG_5                 27
#define SRL_HDR_NEG_4                 28
#define SRL_HDR_NEG_3                 29
#define SRL_HDR_NEG_2                 30
#define SRL_HDR_NEG_1                 31
#define SRL_HDR_RESERVED_0            52
#define SRL_HDR_RESERVED_1            53
#define SRL_HDR_RESERVED_2            54
#define SRL_HDR_RESERVED_3            55
#define SRL_HDR_RESERVED_4            56
#define SRL_HDR_ARRAYREF_0            64
#define SRL_HDR_ARRAYREF_1            65
#define SRL_HDR_ARRAYREF_2            66
#define SRL_HDR_ARRAYREF_3            67
#define SRL_HDR_ARRAYREF_4            68
#define SRL_HDR_ARRAYREF_5            69
#define SRL_HDR_ARRAYREF_6            70
#define SRL_HDR_ARRAYREF_7            71
#define SRL_HDR_ARRAYREF_8            72
#define SRL_HDR_ARRAYREF_9            73
#define SRL_HDR_ARRAYREF_10           74
#define SRL_HDR_ARRAYREF_11           75
#define SRL_HDR_ARRAYREF_12           76
#define SRL_HDR_ARRAYREF_13           77
#define SRL_HDR_ARRAYREF_14           78
#define SRL_HDR_ARRAYREF_15           79
#define SRL_HDR_HASHREF_0             80
#define SRL_HDR_HASHREF_1             81
#define SRL_HDR_HASHREF_2             82
#define SRL_HDR_HASHREF_3             83
#define SRL_HDR_HASHREF_4             84
#define SRL_HDR_HASHREF_5             85
#define SRL_HDR_HASHREF_6             86
#define SRL_HDR_HASHREF_7             87
#define SRL_HDR_HASHREF_8             88
#define SRL_HDR_HASHREF_9             89
#define SRL_HDR_HASHREF_10            90
#define SRL_HDR_HASHREF_11            91
#define SRL_HDR_HASHREF_12            92
#define SRL_HDR_HASHREF_13            93
#define SRL_HDR_HASHREF_14            94
#define SRL_HDR_HASHREF_15            95
#define SRL_HDR_SHORT_BINARY_0        96
#define SRL_HDR_SHORT_BINARY_1        97
#define SRL_HDR_SHORT_BINARY_2        98
#define SRL_HDR_SHORT_BINARY_3        99
#define SRL_HDR_SHORT_BINARY_4       100
#define SRL_HDR_SHORT_BINARY_5       101
#define SRL_HDR_SHORT_BINARY_6       102
#define SRL_HDR_SHORT_BINARY_7       103
#define SRL_HDR_SHORT_BINARY_8       104
#define SRL_HDR_SHORT_BINARY_9       105
#define SRL_HDR_SHORT_BINARY_10      106
#define SRL_HDR_SHORT_BINARY_11      107
#define SRL_HDR_SHORT_BINARY_12      108
#define SRL_HDR_SHORT_BINARY_13      109
#define SRL_HDR_SHORT_BINARY_14      110
#define SRL_HDR_SHORT_BINARY_15      111
#define SRL_HDR_SHORT_BINARY_16      112
#define SRL_HDR_SHORT_BINARY_17      113
#define SRL_HDR_SHORT_BINARY_18      114
#define SRL_HDR_SHORT_BINARY_19      115
#define SRL_HDR_SHORT_BINARY_20      116
#define SRL_HDR_SHORT_BINARY_21      117
#define SRL_HDR_SHORT_BINARY_22      118
#define SRL_HDR_SHORT_BINARY_23      119
#define SRL_HDR_SHORT_BINARY_24      120
#define SRL_HDR_SHORT_BINARY_25      121
#define SRL_HDR_SHORT_BINARY_26      122
#define SRL_HDR_SHORT_BINARY_27      123
#define SRL_HDR_SHORT_BINARY_28      124
#define SRL_HDR_SHORT_BINARY_29      125
#define SRL_HDR_SHORT_BINARY_30      126
#define SRL_HDR_SHORT_BINARY_31      127

#define CASE_SRL_HDR_ARRAYREF    \
   case SRL_HDR_ARRAYREF_0:    \
   case SRL_HDR_ARRAYREF_1:    \
   case SRL_HDR_ARRAYREF_2:    \
   case SRL_HDR_ARRAYREF_3:    \
   case SRL_HDR_ARRAYREF_4:    \
   case SRL_HDR_ARRAYREF_5:    \
   case SRL_HDR_ARRAYREF_6:    \
   case SRL_HDR_ARRAYREF_7:    \
   case SRL_HDR_ARRAYREF_8:    \
   case SRL_HDR_ARRAYREF_9:    \
   case SRL_HDR_ARRAYREF_10:    \
   case SRL_HDR_ARRAYREF_11:    \
   case SRL_HDR_ARRAYREF_12:    \
   case SRL_HDR_ARRAYREF_13:    \
   case SRL_HDR_ARRAYREF_14:    \
   case SRL_HDR_ARRAYREF_15


#define CASE_SRL_HDR_HASHREF    \
   case SRL_HDR_HASHREF_0:    \
   case SRL_HDR_HASHREF_1:    \
   case SRL_HDR_HASHREF_2:    \
   case SRL_HDR_HASHREF_3:    \
   case SRL_HDR_HASHREF_4:    \
   case SRL_HDR_HASHREF_5:    \
   case SRL_HDR_HASHREF_6:    \
   case SRL_HDR_HASHREF_7:    \
   case SRL_HDR_HASHREF_8:    \
   case SRL_HDR_HASHREF_9:    \
   case SRL_HDR_HASHREF_10:    \
   case SRL_HDR_HASHREF_11:    \
   case SRL_HDR_HASHREF_12:    \
   case SRL_HDR_HASHREF_13:    \
   case SRL_HDR_HASHREF_14:    \
   case SRL_HDR_HASHREF_15


#define CASE_SRL_HDR_NEG    \
   case SRL_HDR_NEG_16:    \
   case SRL_HDR_NEG_15:    \
   case SRL_HDR_NEG_14:    \
   case SRL_HDR_NEG_13:    \
   case SRL_HDR_NEG_12:    \
   case SRL_HDR_NEG_11:    \
   case SRL_HDR_NEG_10:    \
   case SRL_HDR_NEG_9:    \
   case SRL_HDR_NEG_8:    \
   case SRL_HDR_NEG_7:    \
   case SRL_HDR_NEG_6:    \
   case SRL_HDR_NEG_5:    \
   case SRL_HDR_NEG_4:    \
   case SRL_HDR_NEG_3:    \
   case SRL_HDR_NEG_2:    \
   case SRL_HDR_NEG_1


#define CASE_SRL_HDR_POS    \
   case SRL_HDR_POS_0:    \
   case SRL_HDR_POS_1:    \
   case SRL_HDR_POS_2:    \
   case SRL_HDR_POS_3:    \
   case SRL_HDR_POS_4:    \
   case SRL_HDR_POS_5:    \
   case SRL_HDR_POS_6:    \
   case SRL_HDR_POS_7:    \
   case SRL_HDR_POS_8:    \
   case SRL_HDR_POS_9:    \
   case SRL_HDR_POS_10:    \
   case SRL_HDR_POS_11:    \
   case SRL_HDR_POS_12:    \
   case SRL_HDR_POS_13:    \
   case SRL_HDR_POS_14:    \
   case SRL_HDR_POS_15


#define CASE_SRL_HDR_RESERVED    \
   case SRL_HDR_RESERVED_0:    \
   case SRL_HDR_RESERVED_1:    \
   case SRL_HDR_RESERVED_2:    \
   case SRL_HDR_RESERVED_3:    \
   case SRL_HDR_RESERVED_4


#define CASE_SRL_HDR_SHORT_BINARY    \
   case SRL_HDR_SHORT_BINARY_0:    \
   case SRL_HDR_SHORT_BINARY_1:    \
   case SRL_HDR_SHORT_BINARY_2:    \
   case SRL_HDR_SHORT_BINARY_3:    \
   case SRL_HDR_SHORT_BINARY_4:    \
   case SRL_HDR_SHORT_BINARY_5:    \
   case SRL_HDR_SHORT_BINARY_6:    \
   case SRL_HDR_SHORT_BINARY_7:    \
   case SRL_HDR_SHORT_BINARY_8:    \
   case SRL_HDR_SHORT_BINARY_9:    \
   case SRL_HDR_SHORT_BINARY_10:    \
   case SRL_HDR_SHORT_BINARY_11:    \
   case SRL_HDR_SHORT_BINARY_12:    \
   case SRL_HDR_SHORT_BINARY_13:    \
   case SRL_HDR_SHORT_BINARY_14:    \
   case SRL_HDR_SHORT_BINARY_15:    \
   case SRL_HDR_SHORT_BINARY_16:    \
   case SRL_HDR_SHORT_BINARY_17:    \
   case SRL_HDR_SHORT_BINARY_18:    \
   case SRL_HDR_SHORT_BINARY_19:    \
   case SRL_HDR_SHORT_BINARY_20:    \
   case SRL_HDR_SHORT_BINARY_21:    \
   case SRL_HDR_SHORT_BINARY_22:    \
   case SRL_HDR_SHORT_BINARY_23:    \
   case SRL_HDR_SHORT_BINARY_24:    \
   case SRL_HDR_SHORT_BINARY_25:    \
   case SRL_HDR_SHORT_BINARY_26:    \
   case SRL_HDR_SHORT_BINARY_27:    \
   case SRL_HDR_SHORT_BINARY_28:    \
   case SRL_HDR_SHORT_BINARY_29:    \
   case SRL_HDR_SHORT_BINARY_30:    \
   case SRL_HDR_SHORT_BINARY_31



/*
* NOTE the above section is auto-updated by author_tools/update_from_header.pl

=for autoupdater stop

*/
#endif
