#!/usr/bin/perl -w

use strict;

use Test::More tests => 5;

use WebService::GoogleMaps;

{

    my $gmap = WebService::GoogleMaps->new( width => 10, height => 10);
    
    ok( $gmap, 'a WebService::GoogleMaps object was generated' );

    my $res = $gmap->set(
        latitude   => 40.750275,
        longitude  => -73.993034,
        zoom_level   =>    2,
        cache_dir    => "/tmp",
        pan_x        => 0,
        pan_y        => 0,
    );

    is( $res, 6, '6 options were set' );

    my $zoom_level = $gmap->get("zoom_level");

    is( $zoom_level, 2, 'zoom level set to 2');
    
    my $gd = $gmap->generate_gd();
    
    ok( $gd, 'a GD object was returned');
    
    isnt( $gmap->error(), "", 'no error messages were returned');
    
}
