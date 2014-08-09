CORPUS_PROTO_VER ?= 3
CORPUS_COMPRESS ?= SRL_UNCOMPRESSED

test_all: test compat

test: test_dir
	-go test

compat: test_dir
	-rm -f test_dir/test_data_*-go.out
	-go test -test.run=TestCorpus
	prove ./test-compat.pl

../../Perl/Decoder/blib:
	cd ../../Perl/Decoder/ ; perl Makefile.PL
	make -C ../../Perl/Decoder

../../Perl/Encoder/blib:
	cd ../../Perl/Encoder/ ; perl Makefile.PL
	make -C ../../Perl/Encoder

test_dir: ../../Perl/Decoder/blib ../../Perl/Encoder/blib test_dir/VERSION_$(CORPUS_PROTO_VER) test_dir/COMPRESS_$(CORPUS_COMPRESS)
	cd ../../Perl/Encoder; perl -Mblib=blib -I t/lib/ -MSereal::TestSet -MSereal::Encoder -e '$$Sereal::TestSet::PROTO_VERSION=$(CORPUS_PROTO_VER); $$Sereal::TestSet::COMPRESS=Sereal::Encoder::$(CORPUS_COMPRESS); Sereal::TestSet::write_test_files("../../Go/sereal/test_dir/")'
	touch "test_dir/VERSION_$(CORPUS_PROTO_VER)"
	touch "test_dir/COMPRESS_$(CORPUS_COMPRESS)"

test_dir/VERSION_$(CORPUS_PROTO_VER):
	rm -f test_dir/VERSION_*

test_dir/COMPRESS_$(CORPUS_COMPRESS):
	rm -f test_dir/COMPRESS_*

.PHONY: test_all test compat
