test: test_dir
	-go test

compat: test_dir
	-rm -f test_dir/test_data_*-go.out
	-go test -test.run=TestCorpus
	perl ./test-compat.pl

../../Perl/Decoder/blib:
	cd ../../Perl/Decoder/ ; perl Makefile.PL
	make -C ../../Perl/Decoder

../../Perl/Encoder/blib:
	cd ../../Perl/Encoder/ ; perl Makefile.PL
	make -C ../../Perl/Encoder

test_dir: ../../Perl/Decoder/blib/ ../../Perl/Encoder/blib
	perl -Mblib=../../Perl/Decoder/blib/ -Mblib=../../Perl/Encoder/blib -I../../Perl/shared/t/lib/ -MSereal::TestSet -MSereal::Encoder -e'Sereal::TestSet::write_test_files("test_dir")'
