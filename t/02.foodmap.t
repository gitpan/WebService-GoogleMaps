#!/usr/bin/perl -w

use strict;

use Test::More tests => 6;

use WebService::GoogleMaps;

{

    my $gmap = WebService::GoogleMaps->new( 640, 480 );
    
    ok( $gmap, 'a WebService::GoogleMaps object was generated' );

    my $res = $gmap->set(
        cache_dir    => "/tmp",
    );

    ok( $res, 'cache dir was set' );

    my @markerlist=(
        [40.725464,  -74.002289, "H!Hiroko's Place"],
        [40.748199,  -74.031575, "=!The Bagel Smashery"],
        [40.8161293, -73.979786, "+!Taiyaki"]
    );

    $res = $gmap->center_viewport_on_coordinates(@markerlist);

    ok( $res, 'viewport centered on coordinates');
    
    $res = $gmap->generate_gd();
    
    ok( $res, 'a GD object was generated');
    
    $res = $gmap->insert_latlon_marker_gd(@markerlist);

    ok( $res, 'markers were plotted on the map');

    isnt( $gmap->error(), "", 'no error messages were returned');
    
}
