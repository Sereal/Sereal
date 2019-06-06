Sereal implementation in Java
=============================

Java8 required.

Hack
----

Run the basic Java test suite

    $ mvn test

or

    $ make test

Run the full compatibility test suite (builds and uses the Perl Sereal
implementation)

    $ make compat CORPUS_PROTO_VER=[1|2|3|4] CORPUS_COMPRESS=[SRL_UNCOMPRESSED|SRL_SNAPPY|SRL_ZLIB|SRL_ZSTD]

Run the benchmarks:

    $ mvn -P benchmarks clean package
    # use java -jar benchmarks/target/benchmarks.jar -h for detailed usage
    $ java -jar benchmarks/target/benchmarks.jar
