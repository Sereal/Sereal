package sereal

/*

   TODO:
    - rearrange github directories so we can 'go get' it
    - snappy support
    - better test infrastructure: table-driven tests that use Perl's test cases
    - replace `header' with actual header construction/parsing
    - create tracked references during pack (circular structures)
    - create string table
    - how do we handle hashes?
       : only string keys are supported -- do we call value.String() or panic()?
    - figure out how to pack 'structs'
    - floating-point support to pack

*/

