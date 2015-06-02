VALUE sereal_to_rb_object(sereal_t *s);
static VALUE s_default_reader(sereal_t *s, u8 tag);
static VALUE s_read_zigzag(sereal_t *s, u8 tag);
static VALUE s_read_varint(sereal_t *s, u8 tag);
static VALUE s_read_float(sereal_t *s, u8 tag);
static VALUE s_read_double(sereal_t *s, u8 tag);
static VALUE s_read_long_double(sereal_t *s, u8 tag);
static VALUE s_read_small_positive_int(sereal_t *s, u8 tag);
static VALUE s_read_small_negative_int(sereal_t *s, u8 tag);
static VALUE s_read_array(sereal_t *s,u8 tag);
static VALUE s_read_arrayref(sereal_t *s, u8 tag);
static VALUE s_read_hash(sereal_t *s, u8 tag);
static VALUE s_read_hashref(sereal_t *s, u8 tag);
static VALUE s_read_rb_string_bang(sereal_t *s,u8 t);
static VALUE s_read_next_rb_string_bang(sereal_t *s);
static VALUE s_read_regexp(sereal_t *s, u8 tag);
static VALUE s_read_nil(sereal_t *s, u8 tag);
static VALUE s_read_true(sereal_t *s, u8 tag);
static VALUE s_read_false(sereal_t *s, u8 tag);
static VALUE s_read_pad(sereal_t *s, u8 tag);
static VALUE s_read_extend(sereal_t *s, u8 tag);
static VALUE s_read_ref(sereal_t *s, u8 tag);
static VALUE s_read_copy(sereal_t *s, u8 tag);
static VALUE s_read_object_freeze(sereal_t *s, u8 tag);
static VALUE s_read_perl_object(sereal_t *s, u8 tag);

static VALUE (*READERS[256])(sereal_t *, u8) = {
        s_read_small_positive_int,                      // 0    SRL_HDR_POS_LOW
        s_read_small_positive_int,                      // 1 
        s_read_small_positive_int,                      // 2
        s_read_small_positive_int,                      // 3
        s_read_small_positive_int,                      // 4
        s_read_small_positive_int,                      // 5
        s_read_small_positive_int,                      // 6
        s_read_small_positive_int,                      // 7
        s_read_small_positive_int,                      // 8
        s_read_small_positive_int,                      // 9
        s_read_small_positive_int,                      // 10
        s_read_small_positive_int,                      // 11
        s_read_small_positive_int,                      // 12
        s_read_small_positive_int,                      // 13
        s_read_small_positive_int,                      // 14
        s_read_small_positive_int,                      // 15   SRL_HDR_POS_HIGH
        s_read_small_negative_int,                      // 16   SRL_HDR_NEG_LOW
        s_read_small_negative_int,                      // 17
        s_read_small_negative_int,                      // 18
        s_read_small_negative_int,                      // 19
        s_read_small_negative_int,                      // 20
        s_read_small_negative_int,                      // 21
        s_read_small_negative_int,                      // 22
        s_read_small_negative_int,                      // 23
        s_read_small_negative_int,                      // 24
        s_read_small_negative_int,                      // 25
        s_read_small_negative_int,                      // 26
        s_read_small_negative_int,                      // 27
        s_read_small_negative_int,                      // 28
        s_read_small_negative_int,                      // 29
        s_read_small_negative_int,                      // 30
        s_read_small_negative_int,                      // 31   SRL_HDR_NEG_HIGH
        s_read_varint,                                  // 32   SRL_HDR_VARINT
        s_read_zigzag,                                  // 33   SRL_HDR_ZIGZAG
        s_read_float,                                   // 34   SRL_HDR_FLOAT
        s_read_double,                                  // 35   SRL_HDR_DOUBLE
        s_read_long_double,                             // 36   SRL_HDR_LONG_DOUBLE
        s_read_nil,                                     // 37   SRL_HDR_UNDEF
        s_read_rb_string_bang,                          // 38   SRL_HDR_BINARY
        s_read_rb_string_bang,                          // 39   SRL_HDR_STR_UTF8
        s_read_pad,                                     // 40   SRL_HDR_REFN
        s_read_ref,                                     // 41   SRL_HDR_REFP
        s_read_hash,                                    // 42   SRL_HDR_HASH
        s_read_array,                                   // 43   SRL_HDR_ARRAY
        s_read_perl_object,                             // 44   SRL_HDR_OBJECT
        s_read_perl_object,                             // 45   SRL_HDR_OBJECTV
        s_read_ref,                                     // 46   SRL_HDR_ALIAS
        s_read_copy,                                    // 47   SRL_HDR_COPY
        s_default_reader, /* XXX */                     // 48   SRL_HDR_WEAKEN
        s_read_regexp,                                  // 49   SRL_HDR_REGEXP
        s_read_object_freeze,                           // 50   OBJECT_FREEZE
        s_read_object_freeze,                           // 51   OBJECTV_FREEZE
        s_default_reader, /* XXX */                     // 52
        s_default_reader, /* XXX */                     // 53
        s_default_reader, /* XXX */                     // 54
        s_default_reader, /* XXX */                     // 55
        s_default_reader, /* XXX */                     // 56
        s_default_reader, /* XXX */                     // 57   SRL_HDR_RESERVED_HIGH
        s_read_false,                                   // 58   SRL_HDR_FALSE
        s_read_true,                                    // 59   SRL_HDR_TRUE
        s_default_reader, /* XXX */                     // 60   SRL_HDR_MANY
        s_default_reader, /* XXX */                     // 61   SRL_HDR_PACKET_START
        s_read_extend,                                  // 62   SRL_HDR_EXTEND
        s_read_pad,                                     // 63   SRL_HDR_PAD
        s_read_arrayref,                                // 64   SRL_HDR_ARRAYREF_LOW
        s_read_arrayref,                                // 65
        s_read_arrayref,                                // 66
        s_read_arrayref,                                // 67
        s_read_arrayref,                                // 68
        s_read_arrayref,                                // 69
        s_read_arrayref,                                // 70
        s_read_arrayref,                                // 71
        s_read_arrayref,                                // 72
        s_read_arrayref,                                // 73
        s_read_arrayref,                                // 74
        s_read_arrayref,                                // 75
        s_read_arrayref,                                // 76
        s_read_arrayref,                                // 77
        s_read_arrayref,                                // 78
        s_read_arrayref,                                // 79   SRL_HDR_ARRAYREF_HIGH
        s_read_hashref,                                 // 80   SRL_HDR_HASHREF_LOW
        s_read_hashref,                                 // 81
        s_read_hashref,                                 // 82
        s_read_hashref,                                 // 83
        s_read_hashref,                                 // 84
        s_read_hashref,                                 // 85
        s_read_hashref,                                 // 86
        s_read_hashref,                                 // 87
        s_read_hashref,                                 // 88
        s_read_hashref,                                 // 89
        s_read_hashref,                                 // 90
        s_read_hashref,                                 // 91
        s_read_hashref,                                 // 92
        s_read_hashref,                                 // 93
        s_read_hashref,                                 // 94
        s_read_hashref,                                 // 95   SRL_HDR_HASHREF_HIGH
        s_read_rb_string_bang,                          // 96   SRL_HDR_SHORT_BINARY_LOW
        s_read_rb_string_bang,                          // 97
        s_read_rb_string_bang,                          // 98
        s_read_rb_string_bang,                          // 99
        s_read_rb_string_bang,                          // 100
        s_read_rb_string_bang,                          // 101
        s_read_rb_string_bang,                          // 102
        s_read_rb_string_bang,                          // 103
        s_read_rb_string_bang,                          // 104
        s_read_rb_string_bang,                          // 105
        s_read_rb_string_bang,                          // 106
        s_read_rb_string_bang,                          // 107
        s_read_rb_string_bang,                          // 108
        s_read_rb_string_bang,                          // 109
        s_read_rb_string_bang,                          // 110
        s_read_rb_string_bang,                          // 111
        s_read_rb_string_bang,                          // 112
        s_read_rb_string_bang,                          // 113
        s_read_rb_string_bang,                          // 114
        s_read_rb_string_bang,                          // 115
        s_read_rb_string_bang,                          // 116
        s_read_rb_string_bang,                          // 117
        s_read_rb_string_bang,                          // 118
        s_read_rb_string_bang,                          // 119
        s_read_rb_string_bang,                          // 120
        s_read_rb_string_bang,                          // 121
        s_read_rb_string_bang,                          // 122
        s_read_rb_string_bang,                          // 123
        s_read_rb_string_bang,                          // 124
        s_read_rb_string_bang,                          // 125
        s_read_rb_string_bang,                          // 126
        s_read_rb_string_bang,                          // 127
        s_default_reader, /* XXX */                     // 128  SRL_HDR_TRACK_FLAG
        s_default_reader, /* XXX */                     // 129
        s_default_reader, /* XXX */                     // 130
        s_default_reader, /* XXX */                     // 131
        s_default_reader, /* XXX */                     // 132
        s_default_reader, /* XXX */                     // 133
        s_default_reader, /* XXX */                     // 134
        s_default_reader, /* XXX */                     // 135
        s_default_reader, /* XXX */                     // 136
        s_default_reader, /* XXX */                     // 137
        s_default_reader, /* XXX */                     // 138
        s_default_reader, /* XXX */                     // 139
        s_default_reader, /* XXX */                     // 140
        s_default_reader, /* XXX */                     // 141
        s_default_reader, /* XXX */                     // 142
        s_default_reader, /* XXX */                     // 143
        s_default_reader, /* XXX */                     // 144
        s_default_reader, /* XXX */                     // 145
        s_default_reader, /* XXX */                     // 146
        s_default_reader, /* XXX */                     // 147
        s_default_reader, /* XXX */                     // 148
        s_default_reader, /* XXX */                     // 149
        s_default_reader, /* XXX */                     // 150
        s_default_reader, /* XXX */                     // 151
        s_default_reader, /* XXX */                     // 152
        s_default_reader, /* XXX */                     // 153
        s_default_reader, /* XXX */                     // 154
        s_default_reader, /* XXX */                     // 155
        s_default_reader, /* XXX */                     // 156
        s_default_reader, /* XXX */                     // 157
        s_default_reader, /* XXX */                     // 158
        s_default_reader, /* XXX */                     // 159
        s_default_reader, /* XXX */                     // 160
        s_default_reader, /* XXX */                     // 161
        s_default_reader, /* XXX */                     // 162
        s_default_reader, /* XXX */                     // 163
        s_default_reader, /* XXX */                     // 164
        s_default_reader, /* XXX */                     // 165
        s_default_reader, /* XXX */                     // 166
        s_default_reader, /* XXX */                     // 167
        s_default_reader, /* XXX */                     // 168
        s_default_reader, /* XXX */                     // 169
        s_default_reader, /* XXX */                     // 170
        s_default_reader, /* XXX */                     // 171
        s_default_reader, /* XXX */                     // 172
        s_default_reader, /* XXX */                     // 173
        s_default_reader, /* XXX */                     // 174
        s_default_reader, /* XXX */                     // 175
        s_default_reader, /* XXX */                     // 176
        s_default_reader, /* XXX */                     // 177
        s_default_reader, /* XXX */                     // 178
        s_default_reader, /* XXX */                     // 179
        s_default_reader, /* XXX */                     // 180
        s_default_reader, /* XXX */                     // 181
        s_default_reader, /* XXX */                     // 182
        s_default_reader, /* XXX */                     // 183
        s_default_reader, /* XXX */                     // 184
        s_default_reader, /* XXX */                     // 185
        s_default_reader, /* XXX */                     // 186
        s_default_reader, /* XXX */                     // 187
        s_default_reader, /* XXX */                     // 188
        s_default_reader, /* XXX */                     // 189
        s_default_reader, /* XXX */                     // 190
        s_default_reader, /* XXX */                     // 191
        s_default_reader, /* XXX */                     // 192
        s_default_reader, /* XXX */                     // 193
        s_default_reader, /* XXX */                     // 194
        s_default_reader, /* XXX */                     // 195
        s_default_reader, /* XXX */                     // 196
        s_default_reader, /* XXX */                     // 197
        s_default_reader, /* XXX */                     // 198
        s_default_reader, /* XXX */                     // 199
        s_default_reader, /* XXX */                     // 200
        s_default_reader, /* XXX */                     // 201
        s_default_reader, /* XXX */                     // 202
        s_default_reader, /* XXX */                     // 203
        s_default_reader, /* XXX */                     // 204
        s_default_reader, /* XXX */                     // 205
        s_default_reader, /* XXX */                     // 206
        s_default_reader, /* XXX */                     // 207
        s_default_reader, /* XXX */                     // 208
        s_default_reader, /* XXX */                     // 209
        s_default_reader, /* XXX */                     // 210
        s_default_reader, /* XXX */                     // 211
        s_default_reader, /* XXX */                     // 212
        s_default_reader, /* XXX */                     // 213
        s_default_reader, /* XXX */                     // 214
        s_default_reader, /* XXX */                     // 215
        s_default_reader, /* XXX */                     // 216
        s_default_reader, /* XXX */                     // 217
        s_default_reader, /* XXX */                     // 218
        s_default_reader, /* XXX */                     // 219
        s_default_reader, /* XXX */                     // 220
        s_default_reader, /* XXX */                     // 221
        s_default_reader, /* XXX */                     // 222
        s_default_reader, /* XXX */                     // 223
        s_default_reader, /* XXX */                     // 224
        s_default_reader, /* XXX */                     // 225
        s_default_reader, /* XXX */                     // 226
        s_default_reader, /* XXX */                     // 227
        s_default_reader, /* XXX */                     // 228
        s_default_reader, /* XXX */                     // 229
        s_default_reader, /* XXX */                     // 230
        s_default_reader, /* XXX */                     // 231
        s_default_reader, /* XXX */                     // 232
        s_default_reader, /* XXX */                     // 233
        s_default_reader, /* XXX */                     // 234
        s_default_reader, /* XXX */                     // 235
        s_default_reader, /* XXX */                     // 236
        s_default_reader, /* XXX */                     // 237
        s_default_reader, /* XXX */                     // 238
        s_default_reader, /* XXX */                     // 239
        s_default_reader, /* XXX */                     // 240
        s_default_reader, /* XXX */                     // 241
        s_default_reader, /* XXX */                     // 242
        s_default_reader, /* XXX */                     // 243
        s_default_reader, /* XXX */                     // 244
        s_default_reader, /* XXX */                     // 245
        s_default_reader, /* XXX */                     // 246
        s_default_reader, /* XXX */                     // 247
        s_default_reader, /* XXX */                     // 248
        s_default_reader, /* XXX */                     // 249
        s_default_reader, /* XXX */                     // 250
        s_default_reader, /* XXX */                     // 251
        s_default_reader, /* XXX */                     // 252
        s_default_reader, /* XXX */                     // 253
        s_default_reader, /* XXX */                     // 254
        s_default_reader  /* XXX */                     // 255
};
