#ifndef SRL_DECODER_H_
#define SRL_DECODER_H_

#include "EXTERN.h"
#include "perl.h"
#include "assert.h"

typedef struct PTABLE * ptable_ptr;
typedef struct {
    unsigned char *buf_start;           /* ptr to "physical" start of input buffer */
    unsigned char *buf_end;             /* ptr to end of input buffer */
    unsigned char *pos;                 /* ptr to current position within input buffer */
    unsigned char *save_pos;

    U32 flags;                          /* flag-like options: See F_* defines in srl_decoder.c */
    unsigned int depth;                 /* current Perl-ref recursion depth */
    ptable_ptr ref_seenhash;            /* ptr table for avoiding circular refs */
    ptable_ptr ref_stashes;             /* ptr table for tracking stashes we will bless into - key: ofs, value: stash */
    ptable_ptr ref_bless_av;            /* ptr table for tracking which objects need to be bless - key: ofs, value: mortal AV (of refs)  */
    AV* weakref_av;
} srl_decoder_t;

/* constructor; don't need destructor, this sets up a callback */
srl_decoder_t *srl_build_decoder_struct(pTHX_ HV *opt);

/* main routine */
SV *srl_decode_into(pTHX_ srl_decoder_t *dec, SV *src, SV *into);

/* Explicit destructor */
void srl_destroy_decoder(pTHX_ srl_decoder_t *dec);

/* destructor hook - called automagically */
void srl_decoder_destructor_hook(pTHX_ void *p);

#define BUF_POS(dec) ((dec)->pos)
#define BUF_SPACE(dec) ((dec)->buf_end - (dec)->pos)
#define BUF_POS_OFS(dec) ((dec)->pos - (dec)->buf_start)
#define BUF_SIZE(dec) ((dec)->buf_end - (dec)->buf_start)
#define BUF_NOT_DONE(dec) ((dec)->pos < (dec)->buf_end)
#define BUF_DONE(dec) ((dec)->pos >= (dec)->buf_end)


#define MYCROAK(fmt, args...) croak("Sereal: Error in %s line %u: " fmt, __FILE__, __LINE__ , ## args)
#define ERROR(msg) MYCROAK("%s", msg)
#define ERRORf1(fmt,var) MYCROAK(fmt, (var))
#define ERRORf2(fmt,var1,var2) MYCROAK(fmt, (var1),(var2))
#define ERROR_UNIMPLEMENTED(dec,tag,str) \
    MYCROAK("Tag %u %s is unimplemented at ofs: %d", tag,str, BUF_POS_OFS(dec)); 
#define ERROR_UNTERMINATED(dec, tag,str) MYCROAK("Tag SRL_HDR_%s %s was not terminated properly at ofs %lu with %lu to go", tag_name[tag & 127], str,dec->pos - dec->buf_start,dec->buf_end - dec->pos)
#define ERROR_BAD_COPY(dec, tag) MYCROAK("While processing tag SRL_HDR_%s encountered a bad COPY tag", tag_name[tag & 127])
#define ERROR_UNEXPECTED(dec, tag, msg) MYCROAK("Unexpected tag %s while expecting %s", tag_name[(tag || *(dec)->pos) & 127], msg)
#define ERROR_PANIC(dec, msg) MYCROAK("Panic: %s", msg);

/* If set, the decoder struct needs to be cleared instead of freed at
 * the end of a deserialization operation */
#define SRL_F_REUSE_DECODER 1UL
/* If set, then the decoder destructor was already pushed to the
 * callback stack */
#define SRL_F_DECODER_DESTRUCTOR_OK 2UL
/* Non-persistent flag! */
#define SRL_F_DECODER_NEEDS_FINALIZE 4UL

#define SRL_DEC_HAVE_OPTION(dec, flag_num) ((dec)->flags & flag_num)
#define SRL_DEC_SET_OPTION(dec, flag_num) ((dec)->flags |= flag_num)
#define SRL_DEC_UNSET_OPTION(dec, flag_num) ((dec)->flags &= ~flag_num)
#define SRL_DEC_VOLATILE_FLAGS (SRL_F_DECODER_NEEDS_FINALIZE)
#define SRL_DEC_RESET_VOLATILE_FLAGS(dec) ((dec)->flags &= ~SRL_DEC_VOLATILE_FLAGS)

/*
perl -MData::Dumper -lne'BEGIN{$Data::Dumper::Sortkeys=1}if(/^#define\s+SRL_HDR_(\S+)\s+\(\(char\)(\d+)\)/i) { $sym{$1}=$2; $val{$2}= $1; } sub f { my $pfx= shift; for my $i ($sym{$pfx . "_LOW"} .. $sym{$pfx . "_HIGH"}) { next if $val{$i}; $sym{$pfx."_".$i}=$i; $val{$i}= $pfx . "_". $i; }} END{foreach my $pfx (keys %sym) { if ($pfx=~/^(.*)_LOW/) { f($1) }} print "static const char * const tag_name[] = {\n".join(",\n",map { sprintf qq(\t/).qq(* %3d *).qq(/ "%s"),$_,$val{$_} } 0..127)."\n};\n"}' srl_protocol.h 
*/
static const char * const tag_name[] = {
        /*   0 */ "POS_LOW",
        /*   1 */ "POS_1",
        /*   2 */ "POS_2",
        /*   3 */ "POS_3",
        /*   4 */ "POS_4",
        /*   5 */ "POS_5",
        /*   6 */ "POS_6",
        /*   7 */ "POS_7",
        /*   8 */ "POS_8",
        /*   9 */ "POS_9",
        /*  10 */ "POS_10",
        /*  11 */ "POS_11",
        /*  12 */ "POS_12",
        /*  13 */ "POS_13",
        /*  14 */ "POS_14",
        /*  15 */ "POS_HIGH",
        /*  16 */ "NEG_LOW",
        /*  17 */ "NEG_17",
        /*  18 */ "NEG_18",
        /*  19 */ "NEG_19",
        /*  20 */ "NEG_20",
        /*  21 */ "NEG_21",
        /*  22 */ "NEG_22",
        /*  23 */ "NEG_23",
        /*  24 */ "NEG_24",
        /*  25 */ "NEG_25",
        /*  26 */ "NEG_26",
        /*  27 */ "NEG_27",
        /*  28 */ "NEG_28",
        /*  29 */ "NEG_29",
        /*  30 */ "NEG_30",
        /*  31 */ "NEG_HIGH",
        /*  32 */ "VARINT",
        /*  33 */ "ZIGZAG",
        /*  34 */ "FLOAT",
        /*  35 */ "DOUBLE",
        /*  36 */ "LONG_DOUBLE",
        /*  37 */ "UNDEF",
        /*  38 */ "STRING",
        /*  39 */ "STRING_UTF8",
        /*  40 */ "REFP",
        /*  41 */ "REFN",
        /*  42 */ "HASH",
        /*  43 */ "ARRAY",
        /*  44 */ "BLESS",
        /*  45 */ "BLESSV",
        /*  46 */ "ALIAS",
        /*  47 */ "COPY",
        /*  48 */ "EXTEND",
        /*  49 */ "LIST",
        /*  50 */ "WEAKEN",
        /*  51 */ "REGEXP",
        /*  52 */ "PAD",
        /*  53 */ "RESERVED_LOW",
        /*  54 */ "RESERVED_54",
        /*  55 */ "RESERVED_55",
        /*  56 */ "RESERVED_56",
        /*  57 */ "RESERVED_57",
        /*  58 */ "RESERVED_58",
        /*  59 */ "RESERVED_59",
        /*  60 */ "RESERVED_60",
        /*  61 */ "RESERVED_61",
        /*  62 */ "RESERVED_62",
        /*  63 */ "RESERVED_HIGH",
        /*  64 */ "ASCII_LOW",
        /*  65 */ "ASCII_65",
        /*  66 */ "ASCII_66",
        /*  67 */ "ASCII_67",
        /*  68 */ "ASCII_68",
        /*  69 */ "ASCII_69",
        /*  70 */ "ASCII_70",
        /*  71 */ "ASCII_71",
        /*  72 */ "ASCII_72",
        /*  73 */ "ASCII_73",
        /*  74 */ "ASCII_74",
        /*  75 */ "ASCII_75",
        /*  76 */ "ASCII_76",
        /*  77 */ "ASCII_77",
        /*  78 */ "ASCII_78",
        /*  79 */ "ASCII_79",
        /*  80 */ "ASCII_80",
        /*  81 */ "ASCII_81",
        /*  82 */ "ASCII_82",
        /*  83 */ "ASCII_83",
        /*  84 */ "ASCII_84",
        /*  85 */ "ASCII_85",
        /*  86 */ "ASCII_86",
        /*  87 */ "ASCII_87",
        /*  88 */ "ASCII_88",
        /*  89 */ "ASCII_89",
        /*  90 */ "ASCII_90",
        /*  91 */ "ASCII_91",
        /*  92 */ "ASCII_92",
        /*  93 */ "ASCII_93",
        /*  94 */ "ASCII_94",
        /*  95 */ "ASCII_95",
        /*  96 */ "ASCII_96",
        /*  97 */ "ASCII_97",
        /*  98 */ "ASCII_98",
        /*  99 */ "ASCII_99",
        /* 100 */ "ASCII_100",
        /* 101 */ "ASCII_101",
        /* 102 */ "ASCII_102",
        /* 103 */ "ASCII_103",
        /* 104 */ "ASCII_104",
        /* 105 */ "ASCII_105",
        /* 106 */ "ASCII_106",
        /* 107 */ "ASCII_107",
        /* 108 */ "ASCII_108",
        /* 109 */ "ASCII_109",
        /* 110 */ "ASCII_110",
        /* 111 */ "ASCII_111",
        /* 112 */ "ASCII_112",
        /* 113 */ "ASCII_113",
        /* 114 */ "ASCII_114",
        /* 115 */ "ASCII_115",
        /* 116 */ "ASCII_116",
        /* 117 */ "ASCII_117",
        /* 118 */ "ASCII_118",
        /* 119 */ "ASCII_119",
        /* 120 */ "ASCII_120",
        /* 121 */ "ASCII_121",
        /* 122 */ "ASCII_122",
        /* 123 */ "ASCII_123",
        /* 124 */ "ASCII_124",
        /* 125 */ "ASCII_125",
        /* 126 */ "ASCII_126",
        /* 127 */ "ASCII_HIGH"
};
#endif
