Sereal implementation in Java
=============================

Java1.6 required.

Hack
----

Run the basic Java test suite

    $ mvn test

or

    $ make test

Run the full compatibility test suite (builds and uses the Perl Sereal
implementation)

    $ make compat CORPUS_PROTO_VER=[1|2] CORPUS_COMPRESS=[SRL_UNCOMPRESSED|SRL_SNAPPY]

Run the benchmarks:

    $ cd benchmarks
    $ mvn clean package
    # use java -jar target/benchmarks.jar -h for detailed usage
    $ java -jar target/benchmarks.jar
