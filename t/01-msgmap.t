#!/usr/bin/perl -w

use strict;

use Test::More tests => 5;

use WebService::GoogleMaps;

{

	my $gmap = WebService::GoogleMaps->new( width => 10, height => 10);
	
    ok( $gmap, 'a WebService::GoogleMaps object was generated' );

	my $res = $gmap->set(
		latitude	 =>	40.43305,
		longitude	 =>	-84.49757,
		zoom_level   =>	13,
		cache_dir    => "/tmp",
		pan_x        => 0,
		pan_y        => 0,
	);

	is( $res, 6, '6 options were set' );

	my $zoom_level = $gmap->get("zoom_level");

	is( $zoom_level, 13, 'zoom level set to 13');
	
	my $gd = $gmap->generate_gd();
	
	ok( $gd, 'a GD object was returned');
	
	is( $gmap->{error}, "", 'no error messages were returned');
	
}
