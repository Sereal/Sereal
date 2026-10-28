/* libFuzzer + ASan harness for Sereal::Decoder (protocol v1..v6, incl. the
 * v6 keyed-checksum envelope). Embeds a Perl interpreter, loads the
 * ASan+coverage-instrumented Sereal::Decoder, and decodes each fuzz input
 * via $dec->decode($bytes) wrapped in eval so malformed input (croak) is
 * recovered exactly as in real use. ASan catches OOB reads/writes in the
 * decoder's buffer handling; Perl here is built usemymalloc=n, so the
 * document PV lives in a system-malloc allocation ASan can instrument. */
#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

static PerlInterpreter *my_perl;

/* Minimal bootstrap: DynaLoader so `use Sereal::Decoder` can load the XS .so */
EXTERN_C void boot_DynaLoader(pTHX_ CV *cv);
static void xs_init(pTHX) {
    static const char file[] = __FILE__;
    newXS("DynaLoader::boot_DynaLoader", boot_DynaLoader, file);
}

static const char *BOOT =
    "our $DEC = Sereal::Decoder->new({"
    "   checksum_key => ('k' x 16),"   /* enables checksum verification path */
    "   max_recursion_depth => 1000,"  /* bound stack use under fuzzing */
    "});"
    "sub _fuzz_decode {"
    "   my $out; eval { $out = $DEC->decode($_[0]); 1 } or return 0; return 1;"
    "}"
    "1;";

int LLVMFuzzerInitialize(int *argc, char ***argv) {
    static int inited = 0;
    if (inited) return 0;
    inited = 1;
    int fake_argc = 2;
    char *fake_argv[] = { "fuzz_decode", "-e0", NULL };
    char **fa = fake_argv;
    PERL_SYS_INIT3(&fake_argc, &fa, NULL);
    my_perl = perl_alloc();
    perl_construct(my_perl);
    PL_exit_flags |= PERL_EXIT_DESTRUCT_END;
    PL_perl_destruct_level = 0;
    if (perl_parse(my_perl, xs_init, fake_argc, fa, NULL)) {
        fprintf(stderr, "perl_parse failed\n"); _exit(1);
    }
    perl_run(my_perl);
    {
        dTHX;
        eval_pv("$SIG{__WARN__}=sub{};", TRUE);
        eval_pv("use Sereal::Decoder;", TRUE);
        if (SvTRUE(ERRSV)) { fprintf(stderr, "use failed: %s\n", SvPV_nolen(ERRSV)); _exit(1); }
        eval_pv(BOOT, TRUE);
        if (SvTRUE(ERRSV)) { fprintf(stderr, "boot failed: %s\n", SvPV_nolen(ERRSV)); _exit(1); }
    }
    return 0;
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
    dTHX;
    dSP;
    ENTER;
    SAVETMPS;
    PUSHMARK(SP);
    XPUSHs(sv_2mortal(newSVpvn((const char *)data, size)));
    PUTBACK;
    call_pv("main::_fuzz_decode", G_EVAL | G_DISCARD | G_SCALAR);
    SPAGAIN;
    if (SvTRUE(ERRSV)) {
        /* croak escaped the inner eval (shouldn't normally) -- swallow it */
        sv_setpvs(ERRSV, "");
    }
    PUTBACK;
    FREETMPS;
    LEAVE;
    return 0;
}
