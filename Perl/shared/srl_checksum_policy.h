#ifndef SRL_CHECKSUM_POLICY_H_
#define SRL_CHECKSUM_POLICY_H_

#define SRL_CHECKSUM_POLICY_VERIFY_IF_AVAILABLE    0
#define SRL_CHECKSUM_POLICY_DIE                    1
#define SRL_CHECKSUM_POLICY_IGNORE                 2
#define SRL_CHECKSUM_POLICY_IGNORE_WARN            3
#define SRL_CHECKSUM_POLICY_REQUIRE                4
#define SRL_CHECKSUM_POLICY_REQUIRE_V2             5
#define SRL_CHECKSUM_POLICY_REQUIRE_V3             6

#define SRL_CHECKSUM_MODE_UNSET                   -1
#define SRL_CHECKSUM_MODE_NONE                     0
#define SRL_CHECKSUM_MODE_V1                       1
#define SRL_CHECKSUM_MODE_V2                       2
#define SRL_CHECKSUM_MODE_V3                       3

SRL_STATIC_INLINE U8
srl_checksum_required_mode(U8 policy)
{
    return policy == SRL_CHECKSUM_POLICY_REQUIRE ? 1
         : policy == SRL_CHECKSUM_POLICY_REQUIRE_V2 ? 2
         : policy == SRL_CHECKSUM_POLICY_REQUIRE_V3 ? 3 : 0;
}

SRL_STATIC_INLINE int
srl_checksum_policy_is_strict(U8 policy)
{
    return policy == SRL_CHECKSUM_POLICY_DIE || srl_checksum_required_mode(policy);
}

SRL_STATIC_INLINE const char *
srl_checksum_policy_name(U8 policy)
{
    switch (policy) {
    case SRL_CHECKSUM_POLICY_DIE: return "die";
    case SRL_CHECKSUM_POLICY_REQUIRE: return "require_key";
    case SRL_CHECKSUM_POLICY_REQUIRE_V2: return "require_v2";
    case SRL_CHECKSUM_POLICY_REQUIRE_V3: return "require_v3";
    case SRL_CHECKSUM_POLICY_IGNORE: return "ignore";
    case SRL_CHECKSUM_POLICY_IGNORE_WARN: return "ignore-warn";
    default: return "verify-if-available";
    }
}

SRL_STATIC_INLINE U8
srl_parse_checksum_policy(pTHX_ const char *policy, STRLEN len, const char *source)
{
    if (len == sizeof("verify-if-available") - 1 && memEQ(policy, "verify-if-available", len))
        return SRL_CHECKSUM_POLICY_VERIFY_IF_AVAILABLE;
    if (len == sizeof("die") - 1 && memEQ(policy, "die", len))
        return SRL_CHECKSUM_POLICY_DIE;
    if ((len == sizeof("require_key") - 1 && memEQ(policy, "require_key", len))
            || (len == sizeof("require") - 1 && memEQ(policy, "require", len))
            || (len == sizeof("require_v1") - 1 && memEQ(policy, "require_v1", len)))
        return SRL_CHECKSUM_POLICY_REQUIRE;
    if (len == sizeof("require_v2") - 1 && memEQ(policy, "require_v2", len))
        return SRL_CHECKSUM_POLICY_REQUIRE_V2;
    if (len == sizeof("require_v3") - 1 && memEQ(policy, "require_v3", len))
        return SRL_CHECKSUM_POLICY_REQUIRE_V3;
    if (len == sizeof("ignore") - 1 && memEQ(policy, "ignore", len))
        return SRL_CHECKSUM_POLICY_IGNORE;
    if (len == sizeof("ignore-warn") - 1 && memEQ(policy, "ignore-warn", len))
        return SRL_CHECKSUM_POLICY_IGNORE_WARN;
    croak("Invalid %s; expected 'verify-if-available', 'die', 'require_key', 'require_v1', 'require_v2', 'require_v3', 'ignore', or 'ignore-warn'", source);
    return SRL_CHECKSUM_POLICY_VERIFY_IF_AVAILABLE; /* not reached */
}

SRL_STATIC_INLINE U8
srl_get_checksum_policy(pTHX_ SV *value, const char *module_policy_var)
{
    STRLEN len;
    const char *policy;

    if (value && SvGMAGICAL(value))
        SvGETMAGIC(value);
    if (value && SvOK(value)) {
        policy = SvPVbyte(value, len);
        return srl_parse_checksum_policy(aTHX_ policy, len, "checksum_policy");
    }

    value = get_sv(module_policy_var, 0);
    if (value && SvGMAGICAL(value))
        SvGETMAGIC(value);
    if (value && SvOK(value)) {
        policy = SvPVbyte(value, len);
        return srl_parse_checksum_policy(aTHX_ policy, len, module_policy_var);
    }

    value = get_sv("Sereal::ChecksumPolicy", 0);
    if (value && SvGMAGICAL(value))
        SvGETMAGIC(value);
    if (value && SvOK(value)) {
        policy = SvPVbyte(value, len);
        return srl_parse_checksum_policy(aTHX_ policy, len, "$Sereal::ChecksumPolicy");
    }

    return SRL_CHECKSUM_POLICY_VERIFY_IF_AVAILABLE;
}

/* -1 means unspecified. Configured values use 0 for none and 1..3 for
 * wire checksum types 0..2. Accept words as the readable API form. */
SRL_STATIC_INLINE int
srl_checksum_mode_from_sv(pTHX_ SV *value, const char *option_name)
{
    STRLEN len;
    const char *mode;
    if (value && SvGMAGICAL(value))
        SvGETMAGIC(value);
    if (!value || !SvOK(value))
        croak("The '%s' option must be 'none', 'v1', 'v2', or 'v3'", option_name);
    mode = SvPVbyte(value, len);
    if (memEQs(mode, len, "none") || memEQs(mode, len, "0")) return 0;
    if (memEQs(mode, len, "v1") || memEQs(mode, len, "1")) return 1;
    if (memEQs(mode, len, "v2") || memEQs(mode, len, "2")) return 2;
    if (memEQs(mode, len, "v3") || memEQs(mode, len, "3")) return 3;
    croak("The '%s' option must be 0/'none', 1/'v1', 2/'v2', or 3/'v3'", option_name);
    return -1;
}

SRL_STATIC_INLINE int
srl_get_default_checksum_mode(pTHX_ const char *module_mode_var, int default_mode)
{
    SV *value;

    value = get_sv(module_mode_var, 0);
    if (value && SvGMAGICAL(value))
        SvGETMAGIC(value);
    if (value && SvOK(value)) {
        return srl_checksum_mode_from_sv(aTHX_ value, module_mode_var);
    }

    value = get_sv("Sereal::ChecksumMode", 0);
    if (value && SvGMAGICAL(value))
        SvGETMAGIC(value);
    if (value && SvOK(value)) {
        return srl_checksum_mode_from_sv(aTHX_ value, "$Sereal::ChecksumMode");
    }
    return default_mode;
}

SRL_STATIC_INLINE SV *
srl_get_default_checksum_key(pTHX_ const char *module_key_var)
{
    SV *value = get_sv(module_key_var, 0);
    if (value && SvGMAGICAL(value))
        SvGETMAGIC(value);
    if (value && SvOK(value))
        return value;
    return get_sv("Sereal::DefaultKey", 0);
}

SRL_STATIC_INLINE int
srl_checksum_keys_equal(pTHX_ SV *left, SV *right)
{
    STRLEN left_len, right_len;
    const char *left_bytes, *right_bytes;
    if (left == right)
        return 1;
    if (!left || !right || !SvOK(left) || !SvOK(right))
        return (!left || !SvOK(left)) && (!right || !SvOK(right));
    left_bytes = SvPVbyte(left, left_len);
    right_bytes = SvPVbyte(right, right_len);
    return left_len == right_len && memEQ(left_bytes, right_bytes, left_len);
}

/* Generic options and versioned aliases assign the same two values. These
 * setters resolve constructor duplicates before package defaults are read. */
SRL_STATIC_INLINE void
srl_set_checksum_key(pTHX_ SV **key, SV *value)
{
    if (*key) {
        if (!srl_checksum_keys_equal(aTHX_ *key, value))
            croak("Different keys were supplied for checksum_key: conflicting checksum key options");
        warn("Attempt to set checksum_key more than once");
    }
    *key = value;
}

SRL_STATIC_INLINE const char *
srl_checksum_mode_name(int mode)
{
    return mode == SRL_CHECKSUM_MODE_NONE ? "none"
         : mode == SRL_CHECKSUM_MODE_V1 ? "v1"
         : mode == SRL_CHECKSUM_MODE_V2 ? "v2" : "v3";
}

SRL_STATIC_INLINE void
srl_set_checksum_mode(pTHX_ int *mode, int value)
{
    if (*mode != SRL_CHECKSUM_MODE_UNSET) {
        int selected = *mode > value ? *mode : value;
        warn("Attempt to set checksum_mode more than once (%s and %s); using %s",
             srl_checksum_mode_name(*mode), srl_checksum_mode_name(value),
             srl_checksum_mode_name(selected));
        *mode = selected;
    }
    else {
        *mode = value;
    }
}

SRL_STATIC_INLINE void
srl_validate_checksum_key(pTHX_ SV *value, const char *option_name, U8 policy)
{
    if (value && SvOK(value)
            && srl_checksum_policy_is_strict(policy)) {
        STRLEN len;
        (void)SvPVbyte(value, len);
        if (len != 16)
            croak("The '%s' key must be exactly 16 bytes with checksum_policy '%s'",
                  option_name, srl_checksum_policy_name(policy));
    }
}

SRL_STATIC_INLINE void
srl_validate_checksum_required_key(pTHX_ SV *key, U8 policy)
{
    if (srl_checksum_required_mode(policy) && (!key || !SvOK(key)))
        croak("checksum_policy '%s' requires a checksum key", srl_checksum_policy_name(policy));
}

SRL_STATIC_INLINE void
srl_validate_checksum_required_mode(pTHX_ int mode, U8 policy)
{
    U8 minimum = srl_checksum_required_mode(policy);
    if (minimum && mode < minimum)
        croak("checksum_policy '%s' requires checksum mode v%u or higher",
              srl_checksum_policy_name(policy), (unsigned int)minimum);
}

#endif
