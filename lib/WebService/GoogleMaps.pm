package WebService::GoogleMaps;

# GoogleMaps (GMaps)
# a perl interface to google maps
# Copyright (c) 2005 - Karl Lohner    <karllohner+googlemaps@gmail.com>

our $VERSION = '0.06';

use warnings;
use strict;
use GD;
use LWP::UserAgent;
use HTTP::Request;
# use HTTP::Cookies;
use POSIX qw/floor ceil/;

use constant GOOGLEMAPS_TILE_URL => 'http://mt.google.com/mt?v=%s&x=%d&y=%d&zoom=%d';
use constant GOOGLEMAPS_TILE_RES_X => 128;
use constant GOOGLEMAPS_TILE_RES_Y => 128;
use constant GOOGLEMAPS_TILE_WATER_URL => 'http://www.google.com/mapfiles/water.gif';
use constant GOOGLEMAPS_TILE_TRANSPARENT_URL => 'http://www.google.com/mapfiles/transparent.gif';

sub new {
    my $class = shift;
    my %opts;
    if (scalar @_ == 2 && $_[0] =~ /^\d+$/ && $_[1] =~ /^\d+$/) {
        $opts{width}=$_[0];
        $opts{height}=$_[1];
    }else{
        %opts = @_;
    }
    my $self = bless(\%opts,$class);

    $self->{debug} && print STDERR "In new()\n";

    $self->{debug} && print STDERR "Setting user-agent to [$opts{agent}]\n";
    if (!$self->{ua}) {
        $self->{ua} = LWP::UserAgent->new();
        $self->{agent} ||= "Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.7.5) Gecko/20041106 Firefox/1.0";  # fake as FireFox 1.0
        $self->{ua}->agent($self->{agent});
    }

    # $self->{debug} && print STDERR "Setting up cookie jar (HTTP\:\:Cookies)\n";
    # $self->{ua}->cookie_jar(HTTP::Cookies->new(%{$self->{cookies}}));
    
    $self->{gm_version} ||= ".1";

    $self->{debug} && print STDERR "new() complete\n";
    
    return $self;
}

sub get_tile_http {
    my ($self, $x, $y, $z) = @_;

    $self->{debug} && print STDERR "In get_tile_http($x, $y, $z), making request...\n";
    
    my $req = HTTP::Request->new(GET => sprintf(GOOGLEMAPS_TILE_URL, $self->{gm_version}, $x, $y, $z));
    my $res = $self->{ua}->request($req);
    
    $self->{debug} && print STDERR "Got request\n";
    
    if ($res->is_success) {
        $self->{debug} && print STDERR "Request success\n";
        return $res->content;
    } else {
        $self->{debug} && print STDERR "Request failure error[".$res->status_line."]\n";
        $self->error($res->status_line);
    }
    
    return undef;
}

sub get_tile_cache {
    my $self = shift;
    my ($x, $y, $z) = @_;

    my $gifdata;
    if (defined($self->{cache_dir}) && -d $self->{cache_dir}) {
        # my $tile_cache_filename = sprintf("%s%d/%d_%d_%d.gif", $self->{cache_dir}, $z, $z, $x, $y);
        my $tile_cache_filename = sprintf("%s/gm_%s_%02d_%d_%d.gif", $self->{cache_dir}, $self->{gm_version}, $z, $x, $y, );
        if (-e($tile_cache_filename)) {
            if (open (GM_CACHE, "<", $tile_cache_filename)) {
                binmode GM_CACHE;
                $gifdata = do { local $/; <GM_CACHE> };
                close(GM_CACHE);
            }
        }
    }

    return $gifdata;
}    

sub put_tile_cache {
    my ($self, $gifdata, $x, $y, $z) = @_;

    if (defined($self->{cache_dir}) && -d $self->{cache_dir}) {
        # my $tile_cache_filename = sprintf("%s%d/%d_%d_%d.gif", $self->{cache_dir}, $z, $z, $x, $y);
        my $tile_cache_filename = sprintf("%s/gm_%s_%02d_%d_%d.gif", $self->{cache_dir}, $self->{gm_version}, $z, $x, $y, );
        # !!! create directory structure if necessary
        if (open (GM_CACHE, ">", $tile_cache_filename)) {
            binmode GM_CACHE;
            print GM_CACHE $gifdata;
            close(GM_CACHE);
        }
    }
    
    return $gifdata;
}    

sub get_tile {
    my $self = shift;
    my ($x, $y, $z) = @_;
    my $gifdata = $self->get_tile_cache($x, $y, $z);
    if (!defined($gifdata)) {
        $gifdata = $self->get_tile_http($x, $y, $z);
        if (defined($gifdata)) {
            $self->put_tile_cache($gifdata, $x, $y, $z);
        }
    }
    return $gifdata;
}

sub set {
    my $self = shift;
    my %opts = @_;
    my $opt_count = 0;
    foreach (keys %opts) {
        $self->{$_}=$opts{$_};
        $opt_count++;
    }
    return $opt_count;
}

sub get {
    my $self = shift;
    if (exists $self->{$_[0]}) {
        return $self->{$_[0]};
    }
    return undef;
}

sub error {
    my $self = shift;
    if( @_ ) {
        $self->{ _ERROR } = shift;
    }
    return $self->{ _ERROR };
}



sub latlonzoom_to_gmap_xy_float {
    my $self = shift;
    # figure out which tile is at a given latitude and longitude for a specific zoom_level
    return [ (($_[1] + 98.35) * (0x20000 >> $_[2])  * 0.77162458338772) / GOOGLEMAPS_TILE_RES_X,
             ((39.5 - $_[0])  * (0x20000 >> $_[2]))                     / GOOGLEMAPS_TILE_RES_Y ];
}

sub generate_gd {
    my $self = shift;
    
    if (scalar @_ != 3) {
        my %opts = @_;
        foreach (keys %opts) {
            $self->{$_}=$opts{$_};
        }
    } 
    else {
        $self->{latitude}   = $_[0];
        $self->{longitude}  = $_[1];
        $self->{zoom_level} = $_[2];
    }
    my $latitude   = defined($_[0]) ? $_[0] : $self->{latitude};
    my $longitude  = defined($_[1]) ? $_[1] : $self->{longitude};
    my $zoom_level = defined($_[2]) ? $_[2] : $self->{zoom_level};
    
    my $gd = GD::Image->new($self->{width}, $self->{height}, 1);
    
    $self->{debug} && print STDERR "In generate( $latitude, $longitude, $zoom_level)\n";

    # figure out which tile is at current latlong
    my $gmap_center_tile_xy_float = $self->latlonzoom_to_gmap_xy_float($latitude, $longitude, $zoom_level);
    my $gmap_center_tile_xy       = [ POSIX::floor($gmap_center_tile_xy_float->[0]), POSIX::floor($gmap_center_tile_xy_float->[1]) ];
    
    # figure out where this tile should go in our gd image
    my $gd_pan_offset = [ (defined($self->{pan_x}) ? $self->{pan_x} : 0), (defined($self->{pan_y}) ? $self->{pan_y} : 0) ];

    my $gd_center_tile_xy = [ int( ( ($self->{width}  / 2) - $gd_pan_offset->[0] ) - 
                                   ( ($gmap_center_tile_xy_float->[0] - $gmap_center_tile_xy->[0]) * GOOGLEMAPS_TILE_RES_X ) 
                                 ),
                              int( ( ($self->{height} / 2) - $gd_pan_offset->[1] ) - 
                                   ( ($gmap_center_tile_xy_float->[1] - $gmap_center_tile_xy->[1]) * GOOGLEMAPS_TILE_RES_Y )
                                 ) ];

    # set up a hash to track some info about our current gd generation
    my $tile_hash = {};
    # We need to limit our tile requests to within a specific viewport of offset ranges.
    # As we process, we may need to further reduce this viewport if there isn't data on the server to get some tiles.
    $tile_hash->{viewport}{0}{-1} = 0 - POSIX::ceil(($gd_center_tile_xy->[0] / GOOGLEMAPS_TILE_RES_X));
    $tile_hash->{viewport}{0}{1}  =     POSIX::ceil((($self->{width}  - $gd_center_tile_xy->[0]) / GOOGLEMAPS_TILE_RES_X)) - 1;
    $tile_hash->{viewport}{1}{-1} = 0 - POSIX::ceil(($gd_center_tile_xy->[1] / GOOGLEMAPS_TILE_RES_Y));
    $tile_hash->{viewport}{1}{1}  =     POSIX::ceil((($self->{height} - $gd_center_tile_xy->[1]) / GOOGLEMAPS_TILE_RES_Y)) - 1;
    
    # We need to set up a queue of tile offsets to fetch relative to our desired latitude/longitude.
    # This is also influenced by our panning
    # We also need to track tile offset coordinates that get added to our tile fetch queue so we don't redundantly request tiles
    # We'll need to track which adjacent tile requested that the tile get added to the queue, so we know which Google map boundary we hit if we hit one.
    # Start with adding the tile at the center of the viewport to the queue.  If this tile doesn't exist, we'll go no further.
    my $tile_xy_offset = [ int(($tile_hash->{viewport}{0}{-1}+$tile_hash->{viewport}{0}{1})/2),
                           int(($tile_hash->{viewport}{1}{-1}+$tile_hash->{viewport}{1}{1})/2) ];
    push (@{$tile_hash->{tile_queue}}, [$tile_xy_offset->[0], $tile_xy_offset->[1]]);

    # Set the vector of the tile referring this tile to the queue.  In this initial case, use [0,0] because it referred itself.
    $tile_hash->{queued}{$tile_xy_offset->[0]}{$tile_xy_offset->[1]} = [0, 0];

    while ($tile_xy_offset = shift(@{$tile_hash->{tile_queue}})) {
        # skip if it's outside our viewport
        next if ($tile_xy_offset->[0]) < $tile_hash->{viewport}{0}{-1};
        next if ($tile_xy_offset->[0]) > $tile_hash->{viewport}{0}{1};
        next if ($tile_xy_offset->[1]) < $tile_hash->{viewport}{1}{-1};
        next if ($tile_xy_offset->[1]) > $tile_hash->{viewport}{1}{1};
        # get the tile
        my $gifdata = $self->get_tile($gmap_center_tile_xy->[0] + $tile_xy_offset->[0], $gmap_center_tile_xy->[1] + $tile_xy_offset->[1], $zoom_level);
        if ($gifdata) {
            # place it into our gd viewport
            my $mt = GD::Image->newFromGifData($gifdata);
            $gd->copy($mt,$gd_center_tile_xy->[0] + ($tile_xy_offset->[0] * GOOGLEMAPS_TILE_RES_X), $gd_center_tile_xy->[1] + ($tile_xy_offset->[1] * GOOGLEMAPS_TILE_RES_Y), 0, 0, GOOGLEMAPS_TILE_RES_X, GOOGLEMAPS_TILE_RES_Y);
            # queue the top, bottom, left, right tiles if they're within our viewport and within available range
            foreach my $vector ([0,-1],[0,1],[-1,0],[1,0]) {
                # make sure it's not already queued
                next if ( exists ( $tile_hash->{queued}{$tile_xy_offset->[0] + $vector->[0]} ) 
                       && exists ( $tile_hash->{queued}{$tile_xy_offset->[0] + $vector->[0]}{$tile_xy_offset->[1] + $vector->[1]} ) );
                push (@{$tile_hash->{tile_queue}}, [$tile_xy_offset->[0]+$vector->[0], $tile_xy_offset->[1]+$vector->[1]]);
                $tile_hash->{queued}{$tile_xy_offset->[0] + $vector->[0]}{$tile_xy_offset->[1] + $vector->[1]} = [ $vector->[0], $vector->[1] ];
            }
        }else{
            # tile not available.
            my $from_vector = $tile_hash->{queued}{$tile_xy_offset->[0]}{$tile_xy_offset->[1]};
            # abort here if this happened on the first tile requested
            if (!$from_vector->[0] && !$from_vector->[1]) {
                $self->error("tile at center of image is not available");
                last;
            }
            # reduce our allowed viewport if we've hit a image source boundary
            foreach my $axis ("0","1") {
                if ($from_vector->[$axis]) {
                    # hit a boundary along an axis
                    $self->{debug} && print STDERR "hit a boundary\n";
                    $self->{debug} && print STDERR "was:    viewport [$axis,$from_vector->[$axis]] = $tile_hash->{viewport}{$axis}{$from_vector->[$axis]}\n";
                    $tile_hash->{viewport}{$axis}{$from_vector->[$axis]}=$tile_xy_offset->[$axis]-$from_vector->[$axis];
                    $self->{debug} && print STDERR "now:    viewport [$axis,$from_vector->[$axis]] = $tile_hash->{viewport}{$axis}{$from_vector->[$axis]}\n";
                }
            }
        }
    }

    # Add copyright notice
    my $black = $gd->colorAllocate(0,0,0);
    $gd->string(gdTinyFont,5,$self->{height}-10,"(c) 2005 Google",$black);
    $gd->string(gdTinyFont,$self->{width}-200,$self->{height}-10,"Map data (c) 2005 NAVTEQ(tm), TeleAtlas",$black);

    return $gd;
}

sub generate_html {
    my $self = shift;
    my $latitude   = defined($_[0]) ? $_[0] : $self->{latitude};
    my $longitude  = defined($_[1]) ? $_[1] : $self->{longitude};
    my $zoom_level = defined($_[2]) ? $_[2] : $self->{zoom_level};
    
    return "<div>Planned for next version...</div>";
}


1; # End of WebService::GoogleMaps


__END__

=head1 NAME

WebService::GoogleMaps - Automated interface to Google Maps

=head1 SYNOPSIS

    use WebService::GoogleMaps;

    # Set up a new object with a viewport of 640 x 480 pixels
    my $gmap = WebService::GoogleMaps->new( 640, 480 );

    # Specify a location to view
    $gmap->set(
        latitude   => 40.750275,
        longitude  => -73.993034,
        zoom_level => 4,          # valid values are 0..14, lower value is more zoomed
        cache_dir  => "/tmp",     # optional, but recommended!  Helps speed up future requests
        pan_x      => 0,          # move viewport to the east  (+) or west  (-) a number of pixels
        pan_y      => 0,          # move viewport to the south (+) or north (-) a number of pixels
    );

    # create a GD object containing our bitmapped map object
    my $gd = $gmap->generate_gd();

    # or simply
    # my $gd = $gmap->generate_gd(40.750275, -73.993034, 4);  # latitude, longitude, zoom_level

    my $error = $gmap->error();
    $error && print "Error: $error\n";
    
    open (FH, ">", "mymap.png");
    binmode FH;
    print FH $gd->png;
    close(FH);

=head1 DESCRIPTION

WebService::GoogleMaps provides an automated interface to Google Maps
L<http://maps.google.com/>, which provides free street maps of locations
in the USA and Canada.  This module allows you to specify an image size,
latitude, longitude, and zoom level and returns a GD object containing a
street level map.

=head1 METHODS

=head2 new()

The constructor. You can pass it an image width and height in pixels, or 
nothing to let it default to 640 x 480 pixels.  Or instead, pass it a
list of initial options and values.

    # set up our object to create a 320 x 240 pixel image
    my $gmap = WebService::GoogleMaps->new( 800, 600 );

    # use the default 640 x 480 image size
    my $gmap = WebService::GoogleMaps->new();

    # specify several options and values
    my $gmap = WebService::GoogleMaps->new(
        width      => 800,
        height     => 600,
        latitude   => 40.750275,
        longitude  => -73.993034,
        zoom_level => 3,
        agent      => "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)",  # optional, default is a Firefox agent.
        cache_dir  => "/tmp",     # optional, but helps speed up future requests
        pan_x      => 100,        # move viewport 100 pixels east
        pan_y      => 100,        # move viewport 100 pixels south
    );

=head2 set()

Use this method to add or change any of the map options after you have created
the object.

    # Create our object, using the default 640 x 480 image size
    my $gmap = WebService::GoogleMaps->new();
    
    # Set width, height, latitude, longitude, zoom level and cache directory when requesting map data
    $gmap->set(
        width      => 800,
        height     => 600,
        latitude   => 40.750275,
        longitude  => -73.993034,
        zoom_level => 9,
        cache_dir  => "/tmp",
    );

=head2 get()

Use this method to retrieve the value of any of the current options

    # Find out what the current zoom level is set to
    my $zoom_level = $gmap->get("zoom_level");

=head2 error()

Use this to retrieve any errors generated by this module.

=head2 generate_gd()

This performs the retrieval of the image from Google Maps.  If the images
are not found in the cache, (assuming you have specified a directory to
use for the cache), this may take a little extra time to complete.

It will return a GD object, which you may then use as you please.  It is
useful to save it to a file.

If there is a problem creating the image, an error message will be set.
You may retrieve this error message at $gmap->error()

    # create a GD object containing our bitmapped map object
    my $gd = $gmap->generate_gd();

    if ( $gmap->error() ) {
        print "Error: ".$gmap->error()."\n";
    }
    else {
        open (FH, ">", "mymap.png");
        binmode FH;
        print FH $gd->png;
        close(FH);
    }


=head2 generate_html()

Not available in this version.  The plan is to have this return HTML code that
will reference images on the Google Maps server to render the map image.

=head1 TODO

=over 4

=item * Improve caching method.  Maybe use subdirectories to separate cached images into geographic areas?

=item * Put a water tile in places ourside the available data ranges?

=item * Develop the generate_html method

=back

=head1 AUTHOR

Karl Lohner, E<lt>karllohner+googlemaps@gmail.comE<gt>

=head1 BUGS

Please report any bugs or feature requests to E<lt>karllohner+googlemaps@gmail.comE<gt>.

Really, I would like to know how you are using this module and what you would like to see
to make it better.

=head1 ACKNOWLEDGEMENTS

Thanks to Joel Webber and his blog article at http://jgwebber.blogspot.com/2005/02/mapping-google.html
as well as to the many people who added comments there, particularly the anonymous comments that detailed
the algorithm used to map latitude/longitude to specific tiles.

=head1 COPYRIGHT AND LICENSE

Copyright 2005 by Karl Lohner, All Rights Reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

