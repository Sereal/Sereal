#ifndef SRL_CHECKSUM_POLICY_H_
#define SRL_CHECKSUM_POLICY_H_

#define SRL_CHECKSUM_POLICY_VERIFY_IF_AVAILABLE    0
#define SRL_CHECKSUM_POLICY_DIE                    1
#define SRL_CHECKSUM_POLICY_IGNORE                 2
#define SRL_CHECKSUM_POLICY_IGNORE_WARN            3
#define SRL_CHECKSUM_POLICY_REQUIRE                4

SRL_STATIC_INLINE U8
srl_parse_checksum_policy(pTHX_ const char *policy, STRLEN len, const char *source)
{
    if (len == sizeof("verify-if-available") - 1 && memEQ(policy, "verify-if-available", len))
        return SRL_CHECKSUM_POLICY_VERIFY_IF_AVAILABLE;
    if (len == sizeof("die") - 1 && memEQ(policy, "die", len))
        return SRL_CHECKSUM_POLICY_DIE;
    if (len == sizeof("require") - 1 && memEQ(policy, "require", len))
        return SRL_CHECKSUM_POLICY_REQUIRE;
    if (len == sizeof("ignore") - 1 && memEQ(policy, "ignore", len))
        return SRL_CHECKSUM_POLICY_IGNORE;
    if (len == sizeof("ignore-warn") - 1 && memEQ(policy, "ignore-warn", len))
        return SRL_CHECKSUM_POLICY_IGNORE_WARN;
    croak("Invalid %s; expected 'verify-if-available', 'die', 'require', 'ignore', or 'ignore-warn'", source);
    return SRL_CHECKSUM_POLICY_VERIFY_IF_AVAILABLE; /* not reached */
}

SRL_STATIC_INLINE U8
srl_get_checksum_policy(pTHX_ SV *value)
{
    if (value && SvOK(value)) {
        STRLEN len;
        const char *policy = SvPVbyte(value, len);
        return srl_parse_checksum_policy(aTHX_ policy, len, "checksum_policy");
    } else {
        const char *policy = PerlEnv_getenv("SEREAL_CHECKSUM_POLICY");
        if (policy && *policy)
            return srl_parse_checksum_policy(aTHX_ policy, strlen(policy), "SEREAL_CHECKSUM_POLICY");
    }
    return SRL_CHECKSUM_POLICY_VERIFY_IF_AVAILABLE;
}

/* Validate before allocating constructor state; never include key bytes in
 * diagnostics. Permissive policies retain the established normalization. */
SRL_STATIC_INLINE void
srl_validate_checksum_key(pTHX_ SV *value, U8 version, U8 policy)
{
    if (value && SvOK(value)
            && (policy == SRL_CHECKSUM_POLICY_DIE || policy == SRL_CHECKSUM_POLICY_REQUIRE)) {
        STRLEN len;
        (void)SvPVbyte(value, len);
        if (len != 16)
            croak("The '%s' key must be exactly 16 bytes with checksum_policy 'die' or 'require'",
                  version == 0 ? "checksum_v0_key" : "checksum_v1_key");
    }
}

#endif
