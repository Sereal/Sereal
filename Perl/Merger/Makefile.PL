use 5.008005;
use ExtUtils::MakeMaker;
WriteMakefile(
    NAME           => 'Sereal::Merger',
    VERSION_FROM   => 'lib/Sereal/Merger.pm',
    PREREQ_PM      => { 'Test::More' => 0,
                        'Test::Exception' => 0 },
    ABSTRACT_FROM  => 'lib/Sereal/Merger.pm',
    AUTHOR         => [ 'Damien Krotkine', 'Ivan Kruglov' ],
    DEFINE         => '',
    INC            => '-I.',
    OBJECT         => '$(O_FILES)',
    PREREQ_PM         => {
        'XSLoader' => 0,
    },

);
