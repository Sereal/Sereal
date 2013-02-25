package sereal

/*

   TODO:
    - rename pack.go -> encoder.go, unpack.go -> decoder.go
    - rearrange github directories so we can 'go get' it:
    - export fewer constants
    - better test infrastructure: table-driven tests that use Perl's test cases
    - create tracked references during pack (circular structures)
    - how do we handle hashes?
       : only string keys are supported -- do we call value.String() or panic()?
    - figure out how to nicely pack 'structs'
    - how to deal with interface{} everywhere for deserialization
    - add decoder and encoder objects + options (snappy threshold, snappy on/off, string vs. byte array handling..)
    - make slice usage more idiomatic
    - "header size" -> "optional header size"
    - roundtrip test: perl obj -> perl-sereal stream -> go1 obj -> go-sereal stream -> go2 obj, DeepEqual(go1, go2)
    - godoc ALL THE THINGS
    - we should treat input buffer as read-only when unpacking
*/
