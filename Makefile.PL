use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'WebService::GoogleMaps',
    AUTHOR              => 'Karl Lohner <karllohner+googlemaps@thespill.com>',
    VERSION_FROM        => 'lib\WebService\GoogleMaps.pm',
    ABSTRACT_FROM       => 'lib\WebService\GoogleMaps.pm',
    PL_FILES            => {},
    PREREQ_PM => {
        'Test::More' => 0,
        'GD' => '2.17',
        'LWP::UserAgent' => 0,
        'HTTP::Request' => 0,
        'POSIX' => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'WebService-GoogleMaps-*' },
);
