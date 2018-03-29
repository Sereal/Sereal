CORPUS_PROTO_VER ?= 4
CORPUS_COMPRESS ?= SRL_UNCOMPRESSED

TAGS ?=

test_all: test compat

test: test_dir
	-go test $(TAGS)

check: test

compat: test_dir
	-rm -f test_dir/test_data_*-go.out
	-rm -f test_freeze/*-go.out
	go test $(TAGS) -test.run=TestCorpus
	go test $(TAGS) -test.run=TestPrepareFreezeRoundtrip
	prove ./test-compat.pl
	env RUN_FREEZE=1 go test $(TAGS) -test.run=TestFreezeRoundtrip

../../Perl/Decoder/blib:
	cd ../../Perl/Decoder/ ; perl Makefile.PL
	make -C ../../Perl/Decoder

../../Perl/Encoder/blib:
	cd ../../Perl/Encoder/ ; perl Makefile.PL
	make -C ../../Perl/Encoder

test_dir: ../../Perl/Decoder/blib ../../Perl/Encoder/blib test_dir/VERSION_$(CORPUS_PROTO_VER) test_dir/COMPRESS_$(CORPUS_COMPRESS) test_files/VERSION_$(CORPUS_PROTO_VER) test_files
	cd ../../Perl/Encoder; perl -Mblib=blib -I t/lib/ -I ../shared/t/lib -MSereal::TestSet -MSereal::Encoder -e '$$Sereal::TestSet::PROTO_VERSION=$(CORPUS_PROTO_VER); $$Sereal::TestSet::COMPRESS=Sereal::Encoder::$(CORPUS_COMPRESS); Sereal::TestSet::write_test_files("../../Go/sereal/test_dir/", $(CORPUS_PROTO_VER))'
	touch "test_dir/VERSION_$(CORPUS_PROTO_VER)"
	touch "test_dir/COMPRESS_$(CORPUS_COMPRESS)"

test_files:
	mkdir -p test_dir
	mkdir -p test_freeze
	touch "test_dir/VERSION_$(CORPUS_PROTO_VER)"
	touch "test_freeze/VERSION_$(CORPUS_PROTO_VER)"
	perl -Mblib=../../Perl/Encoder/blib -MSereal::Encoder cmd/gen/test-decode-struct.pl test_dir

test_files/VERSION_$(CORPUS_PROTO_VER):
	rm -f test_freeze/VERSION_*

test_dir/VERSION_$(CORPUS_PROTO_VER):
	rm -f test_dir/VERSION_*

test_dir/COMPRESS_$(CORPUS_COMPRESS):
	rm -f test_dir/COMPRESS_*

.PHONY: test_all test compat
