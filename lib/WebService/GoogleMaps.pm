package WebService::GoogleMaps;

# GoogleMaps (GMaps)
# a perl interface to google maps
# Copyright (c) 2005 - Karl Lohner    <karllohner+googlemaps@gmail.com>

our $VERSION = '0.07';

use warnings;
use strict;
use GD;
use LWP::UserAgent;
use HTTP::Request;
# use HTTP::Cookies;
# use Data::Dump qw/pp/;
use POSIX qw/floor ceil/;

use constant GOOGLEMAPS_TILE_URL   => 'http://mt.google.com/mt?v=%s&x=%d&y=%d&zoom=%d';
use constant GOOGLEMAPS_TILE_RES_X => 128;
use constant GOOGLEMAPS_TILE_RES_Y => 128;
use constant GOOGLEMAPS_ZOOM_MIN   => 0;
use constant GOOGLEMAPS_ZOOM_MAX   => 14;

use constant GOOGLEMAPS_MAPFILES_URL => 'http://www.google.com/mapfiles/';

use constant GOOGLEMAPS_TILE_WATER_GIF       => 'water.gif';
use constant GOOGLEMAPS_TILE_TRANSPARENT_GIF => 'transparent.gif';

use constant GOOGLEMAPS_MARKER_PNG           => 'marker%s.png';  # %s is /[A-J]?/
use constant GOOGLEMAPS_MARKER_SHADOW_PNG    => 'shadow50.png';
use constant GOOGLEMAPS_MARKER_DD_START_PNG  => 'dd-start.png';
use constant GOOGLEMAPS_MARKER_DD_END_PNG    => 'dd-end.png';
use constant GOOGLEMAPS_MARKER_CENTER_OFFSET => [10, 34];  # pinpoint in image relative to top left corner
use constant GOOGLEMAPS_MARKER_SIZE          => [37, 34];

sub new {
    my $class = shift;
    my %opts;
    if (scalar @_ == 2 && $_[0] =~ /^\d+$/ && $_[1] =~ /^\d+$/) {
        $opts{width}  =$_[0];
        $opts{height} =$_[1];
    }else{
        %opts = @_;
    }
    my $self = bless(\%opts,$class);
    # $self->{debug} && print STDERR "In new()\n";
    # $self->{debug} && print STDERR "Setting user-agent to [$opts{agent}]\n";
    if (!$self->{ua}) {
        $self->{ua} = LWP::UserAgent->new();
        $self->{agent} ||= "Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.7.5) Gecko/20041106 Firefox/1.0";  # fake as FireFox 1.0
        $self->{ua}->agent($self->{agent});
    }

    # $self->{debug} && print STDERR "Setting up cookie jar (HTTP\:\:Cookies)\n";
    # $self->{ua}->cookie_jar(HTTP::Cookies->new(%{$self->{cookies}}));
    
    $self->{gm_version} ||= ".1";
    $self->{width}  ||= 640;
    $self->{height} ||= 480;

    # $self->{debug} && print STDERR "new() complete\n";
    
    return $self;
}

sub error {
    my $self = shift;
    if( @_ ) {
        $self->{ _ERROR } = shift;
    }
    return $self->{ _ERROR };
}

sub http_request {
#    my($self, @args) = @_;
#    LWP::Debug::trace('simple_request()');
#    my(@timing_tries) = ( $self->timing() =~ m<(\d+(?:\.\d+)*)>g );
#    my $determination = $self->codes_to_determinate();
#    LWP::Debug::debug("My retrial code policy is ["
#      . join(' ', sort keys %$determination) . "].");
#    LWP::Debug::debug("My retrial timing policy is [@timing_tries].");
#  
#    my $resp;
#    my $before_c = $self->before_determined_callback;
#    my $after_c  = $self->after_determined_callback;
#    foreach my $pause_if_unsuccessful (@timing_tries, undef) {
#        LWP::Debug::debug("Trying simple_request with args: ["
#          . join(',', map $_||"''", @args) . "]");
#        
#        $before_c and $before_c->(
#           $self, \@timing_tries, $pause_if_unsuccessful, $determination, \@args);
#        $resp = $self->SUPER::simple_request(@args);
#        $after_c and $after_c->(
#            $self, \@timing_tries, $pause_if_unsuccessful, $determination, \@args, $resp);
#    
#        my $code = $resp->code;
#        my $message = $resp->message;
#        $message =~ s/\s+$//s;
#        unless( $determination->{$code} ) { # normal case: all is well (or 404, etc)
#            LWP::Debug::debug("It returned a code ($code $message) blocking a retry");
#            return $resp;
#        }
#        if(defined $pause_if_unsuccessful) { # it's undef only on the last
#            LWP::Debug::debug("It returned a code ($code $message) that'll make me retry, after $pause_if_unsuccessful seconds.");
#            sleep $pause_if_unsuccessful if $pause_if_unsuccessful;
#        } else {
#            LWP::Debug::debug("I give up.  I'm returning this \"$code $message\" response.");
#        }
#    }
#    return $resp;
}


sub get_tile_http {
    my ($self, $x, $y, $z) = @_;

    # $self->{debug} && print STDERR "In get_tile_http($x, $y, $z), making request...\n";
    
    my $req = HTTP::Request->new(GET => sprintf(GOOGLEMAPS_TILE_URL, $self->{gm_version}, $x, $y, $z));
    my $res = $self->{ua}->request($req);
    
    # $self->{debug} && print STDERR "Got request\n";
    
    if ($res->is_success) {
        # $self->{debug} && print STDERR "Request success\n";
        return $res->content;
    } else {
        # $self->{debug} && print STDERR "Request failure error[".$res->status_line."]\n";
        $self->error($res->status_line);
        # TODO: have this retry a couple times on error.
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

sub get_mapfile_data_http {
    my ($self, $mapfile_name) = @_;

    # $self->{debug} && print STDERR "In get_mapfile_data_http($mapfile_name), making request...\n";
    
    my $req = HTTP::Request->new(GET => GOOGLEMAPS_MAPFILES_URL.$mapfile_name);
    my $res = $self->{ua}->request($req);
    
    # $self->{debug} && print STDERR "Got request\n";
    
    if ($res->is_success) {
        # $self->{debug} && print STDERR "Request success\n";
        return $res->content;
    } else {
        # $self->{debug} && print STDERR "Request failure error[".$res->status_line."]\n";
        $self->error($res->status_line);
        # TODO: have this retry a couple times on error.
    }
    
    return undef;
}

sub get_mapfile_data_cache {
    my $self = shift;
    my $mapfile_name = $_[0];

    my $mapfile_data;
    if (defined($self->{cache_dir}) && -d $self->{cache_dir}) {
        my $mapfile_data_cache_filename = sprintf("%s/mapfile_%s", $self->{cache_dir}, $mapfile_name );
        if (-e($mapfile_data_cache_filename)) {
            if (open (GM_CACHE, "<", $mapfile_data_cache_filename)) {
                binmode GM_CACHE;
                $mapfile_data = do { local $/; <GM_CACHE> };
                close(GM_CACHE);
            }
        }
    }

    return $mapfile_data;
}    

sub put_mapfile_data_cache {
    my ($self, $mapfile_data, $mapfile_name) = @_;

    if (defined($self->{cache_dir}) && -d $self->{cache_dir}) {
        my $mapfile_data_cache_filename = sprintf("%s/mapfile_%s", $self->{cache_dir}, $mapfile_name );
        if (open (GM_CACHE, ">", $mapfile_data_cache_filename)) {
            binmode GM_CACHE;
            print GM_CACHE $mapfile_data;
            close(GM_CACHE);
        }
    }
    
    return $mapfile_data;
}    

sub get_mapfile_data {
    my $self = shift;
    my ($mapfile_name) = $_[0];
    my $mapfile_data = $self->get_mapfile_data_cache($mapfile_name);
    if (!defined($mapfile_data)) {
        $mapfile_data = $self->get_mapfile_data_http($mapfile_name);
        if (defined($mapfile_data)) {
            $self->put_mapfile_data_cache($mapfile_data, $mapfile_name);
        }
    }
    return $mapfile_data;
}

sub set {
    my $self = shift;
    my $opt_count = 0;
    if (scalar @_ != 3) {
        my %opts = @_;
        foreach (keys %opts) {
            $self->{$_}=$opts{$_};
            $opt_count++;
        }
    } 
    else {
        $self->set_viewport_latlonzoom(@_);
        $opt_count = 3;
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

sub set_viewport_latlonzoom {
    my $self = shift;
    if (defined($_[0]) && defined($_[1]) && defined($_[2])) {
        ($self->{latitude}, $self->{longitude}, $self->{zoom_level}) = ($_[0], $_[1], $_[2]);
        return [$_[0], $_[1], $_[2]];
    }else{
        return undef;
    }
}

sub get_viewport_latlonzoom {
    my $self = shift;
    if (defined($self->{latitude}) && defined($self->{longitude}) && defined($self->{zoom_level})) {
        return [$self->{latitude}, $self->{longitude}, $self->{zoom_level}];
    }else{
        return undef;
    }
}

sub set_viewport_pan_offset {
    my $self = shift;
    if (defined($_[0]) && defined($_[1])) {
        ($self->{pan_x}, $self->{pan_y}) = ($_[0], $_[1]);
        return [$_[0], $_[1]];
    }else{
        return undef;
    }
}

sub get_viewport_pan_offset {
    my $self = shift;
    return [ (defined($self->{pan_x}) ? $self->{pan_x} : 0), (defined($self->{pan_y}) ? $self->{pan_y} : 0) ];
}

sub latlonzoom_to_gmap_tile_xy_float {
    my $self = shift;
    # figure out which tile is at a given latitude and longitude for a specific zoom_level
    return [ (($_[1] + 98.35) * (0x20000 >> $_[2])  * 0.77162458338772) / GOOGLEMAPS_TILE_RES_X,
             ((39.5 - $_[0])  * (0x20000 >> $_[2]))                     / GOOGLEMAPS_TILE_RES_Y ];
}

sub latlonzoom_to_viewport_xy {
    my $self = shift;
    my $viewport_latlonzoom = $self->get_viewport_latlonzoom();
    if ($viewport_latlonzoom) {
        my $gmap_tile_xy_float = $self->latlonzoom_to_gmap_tile_xy_float(@_);
        # Returned array will also be adjusted for any specified panning.
        my $viewport_pan_offset = $self->get_viewport_pan_offset();
        my $gmap_latlon_tile_xy_float = $self->latlonzoom_to_gmap_tile_xy_float(@{$viewport_latlonzoom});
        return [
            int( ($self->{width}  / 2) - 
                 ( ($gmap_latlon_tile_xy_float->[0] - $gmap_tile_xy_float->[0]) * GOOGLEMAPS_TILE_RES_X )
               ) - $viewport_pan_offset->[0] ,
            int( ($self->{height}  / 2) - 
                 ( ($gmap_latlon_tile_xy_float->[1] - $gmap_tile_xy_float->[1]) * GOOGLEMAPS_TILE_RES_Y )
               ) - $viewport_pan_offset->[1]
        ];
    }else{
        $self->error("latitude, longitude, and zoom_level for viewport must be specified first");
        return undef;
    }
}

sub log2 {
    my $n = shift;
    return 0 if !$n;
    return log($n)/log(2);
}

sub markerlist_to_latlonzoom {
    # This is useful to help determine the center and zoom level for the map
    # based on a list of points you wish to make sure are contained inside
    # the viewport.
    my $self = shift;
    return undef unless scalar(@_);
    my $latlonzoom = [];
    my ($maxlat, $maxlon, $minlat, $minlon);
    foreach my $marker (@_) {
        my ($lat, $lon) = ($marker->[0], $marker->[1]);
        if (!defined($maxlat) || ($lat > $maxlat)) {
            $maxlat = $lat;
        }
        if (!defined($minlat) || ($lat < $minlat)) {
            $minlat = $lat;
        }
        if (!defined($maxlon) || ($lon > $maxlon)) {
            $maxlon = $lon;
        }
        if (!defined($minlon) || ($lon < $minlon)) {
            $minlon = $lon;
        }
    }
    $latlonzoom->[0] = ($minlat + $maxlat)/2;
    $latlonzoom->[1] = ($minlon + $maxlon)/2;
    $latlonzoom->[2] = GOOGLEMAPS_ZOOM_MIN;
    my $zoom_level = POSIX::ceil(log2(POSIX::ceil(($maxlon - $minlon) * 0x20000 * 0.77162458338772) / $self->{width}));
    $latlonzoom->[2] = ($zoom_level > $latlonzoom->[2])         ? $zoom_level         : $latlonzoom->[2];
    $zoom_level = POSIX::ceil(log2(POSIX::ceil(($maxlat - $minlat) * 0x20000) / $self->{height}));
    $latlonzoom->[2] = ($zoom_level > $latlonzoom->[2])         ? $zoom_level         : $latlonzoom->[2];
    if ($latlonzoom->[2] > GOOGLEMAPS_ZOOM_MAX) {
        $latlonzoom->[2] = GOOGLEMAPS_ZOOM_MAX;
        $self->error("cannot zoom out far enough to show all markers in viewport");
    }
    return $latlonzoom;
}

sub center_viewport_on_coordinates {
    my $self = shift;
    return $self->set(@{$self->markerlist_to_latlonzoom(@_)});
}


sub generate_gd {
    my $self = shift;
    $self->set(@_);

    my $viewport_latlonzoom = $self->get_viewport_latlonzoom();
    if (!$viewport_latlonzoom) {
        $self->error("latitude, longitude, and zoom_level must be specified");
        return undef;
    }

    # figure out which tile is at current latlong
    my $gmap_latlon_tile_xy_float = $self->latlonzoom_to_gmap_tile_xy_float(@{$viewport_latlonzoom});
    my $gmap_latlon_tile_xy       = [ POSIX::floor($gmap_latlon_tile_xy_float->[0]), POSIX::floor($gmap_latlon_tile_xy_float->[1]) ];

    # figure out where this tile should go in our gd image, adjusted for panning
    my $viewport_pan_offset = $self->get_viewport_pan_offset();
    my $viewport_latlon_tile_xy = [ int( ($self->{width}  / 2) - 
                                       ( ($gmap_latlon_tile_xy_float->[0] - $gmap_latlon_tile_xy->[0]) * GOOGLEMAPS_TILE_RES_X ) 
                                    ) - $viewport_pan_offset->[0] ,
                                    int( ($self->{height} / 2) - 
                                       ( ($gmap_latlon_tile_xy_float->[1] - $gmap_latlon_tile_xy->[1]) * GOOGLEMAPS_TILE_RES_Y )
                                    ) - $viewport_pan_offset->[1]
                                  ];
    # set up a hash to track info about our current gd generation
    my $tile_hash = {};
    # We need to limit our tile requests to within a specific viewport of offset ranges.
    # Set up a mapping of Google Maps tiles to x,y coordinates in our viewport
    my $viewport_id = "VIEWID";
    # $tile_hash->{VIEWPORT}{$viewport_id}{BOUNDARY};
    $tile_hash->{VIEWPORT}{$viewport_id}{VER}  = $self->{gm_version};
    $tile_hash->{VIEWPORT}{$viewport_id}{ZOOM} = $viewport_latlonzoom->[2];
    $tile_hash->{VIEWPORT}{$viewport_id}{BOUNDARY} = {
        0 => {
            -1 => $gmap_latlon_tile_xy->[0] - POSIX::ceil(($viewport_latlon_tile_xy->[0] / GOOGLEMAPS_TILE_RES_X)),
            1  => $gmap_latlon_tile_xy->[0] + POSIX::floor((($self->{width}  - $viewport_latlon_tile_xy->[0]) / GOOGLEMAPS_TILE_RES_X)),
        },
        1 => {
            -1 => $gmap_latlon_tile_xy->[1] - POSIX::ceil(($viewport_latlon_tile_xy->[1] / GOOGLEMAPS_TILE_RES_Y)),
            1  => $gmap_latlon_tile_xy->[1] + POSIX::floor((($self->{height} - $viewport_latlon_tile_xy->[1]) / GOOGLEMAPS_TILE_RES_Y)),
        }
    };

    # Create a queue of tiles to fetch.  Queue will be ordered so that the tile at the center of the viewport is first
    # and subsequent tiles are added as they radiate out from that tile.  Identify tile in this queue by coordinate of
    # tile and coordinate of tile that referred it to the queue.  We need referrer to help us know what direction we
    # were coming from if we hit unavailable data from Google Maps (and thus can assume this is water and there's more water beyond)
    my @temp_queue = ( [
        POSIX::floor(($tile_hash->{VIEWPORT}{$viewport_id}{BOUNDARY}{0}{-1} + $tile_hash->{VIEWPORT}{$viewport_id}{BOUNDARY}{0}{1})/2),
        POSIX::floor(($tile_hash->{VIEWPORT}{$viewport_id}{BOUNDARY}{1}{-1} + $tile_hash->{VIEWPORT}{$viewport_id}{BOUNDARY}{1}{1})/2),
        POSIX::floor(($tile_hash->{VIEWPORT}{$viewport_id}{BOUNDARY}{0}{-1} + $tile_hash->{VIEWPORT}{$viewport_id}{BOUNDARY}{0}{1})/2),
        POSIX::floor(($tile_hash->{VIEWPORT}{$viewport_id}{BOUNDARY}{1}{-1} + $tile_hash->{VIEWPORT}{$viewport_id}{BOUNDARY}{1}{1})/2),
    ] );
    while (my $tile_xy = shift(@temp_queue)) {
        # skip if it's outside our viewport
        next if ($tile_xy->[0]) < $tile_hash->{VIEWPORT}{$viewport_id}{BOUNDARY}{0}{-1};
        next if ($tile_xy->[0]) > $tile_hash->{VIEWPORT}{$viewport_id}{BOUNDARY}{0}{1};
        next if ($tile_xy->[1]) < $tile_hash->{VIEWPORT}{$viewport_id}{BOUNDARY}{1}{-1};
        next if ($tile_xy->[1]) > $tile_hash->{VIEWPORT}{$viewport_id}{BOUNDARY}{1}{1};
        # or if it's already been queued
        next if $tile_hash->{VIEWPORT}{$viewport_id}{TILE_MAP}{$tile_xy->[0]}{$tile_xy->[1]}{QUEUED_BY};
        # queue it up for our viewport
        push (@{$tile_hash->{VIEWPORT}{$viewport_id}{TILE_QUEUE}}, $tile_xy);
        $tile_hash->{VIEWPORT}{$viewport_id}{TILE_MAP}{$tile_xy->[0]}{$tile_xy->[1]}{QUEUED_BY} = [$tile_xy->[2],$tile_xy->[3]];
        $tile_hash->{VIEWPORT}{$viewport_id}{TILE_MAP}{$tile_xy->[0]}{$tile_xy->[1]}{XY} = [
            $viewport_latlon_tile_xy->[0] + (($tile_xy->[0] - $gmap_latlon_tile_xy->[0]) * GOOGLEMAPS_TILE_RES_X),
            $viewport_latlon_tile_xy->[1] + (($tile_xy->[1] - $gmap_latlon_tile_xy->[1]) * GOOGLEMAPS_TILE_RES_Y)
        ];
        # See if we should add any of this tile's neighbors to the queue.
        foreach my $vector ([0,-1],[0,1],[-1,0],[1,0]) {
            # make sure it's not already queued
            next if ( exists($tile_hash->{VIEWPORT}{$viewport_id}{TILE_MAP}{$tile_xy->[0]+$vector->[0]})
                  &&  exists($tile_hash->{VIEWPORT}{$viewport_id}{TILE_MAP}{$tile_xy->[0]+$vector->[0]}{$tile_xy->[1]+$vector->[1]})
                  && !exists($tile_hash->{VIEWPORT}{$viewport_id}{TILE_MAP}{$tile_xy->[0]+$vector->[0]}{$tile_xy->[1]+$vector->[1]}{QUEUED_BY}) );
            # Add it to our temp queue for further processing.
            push (@temp_queue, [ $tile_xy->[0]+$vector->[0], $tile_xy->[1]+$vector->[1], $tile_xy->[0], $tile_xy->[1] ]);
        }
    }

    # Process the queue to load data into our tile buffer
    foreach my $tile_xy (@{$tile_hash->{VIEWPORT}{$viewport_id}{TILE_QUEUE}}) {
        # Tile is water if referring tile was water
        my $queued_by_xy = $tile_hash->{VIEWPORT}{$viewport_id}{TILE_MAP}{$tile_xy->[0]}{$tile_xy->[1]}{QUEUED_BY};
        my $queued_by_code = $tile_hash->{GM_TILES}{$tile_hash->{VIEWPORT}{$viewport_id}{VER}}{$tile_hash->{VIEWPORT}{$viewport_id}{ZOOM}}{$queued_by_xy->[0]}{$queued_by_xy->[1]}{CODE};
        if ($queued_by_code && $queued_by_code =~ /^(WATER)$/) {
            $tile_hash->{GM_TILES}{$tile_hash->{VIEWPORT}{$viewport_id}{VER}}{$tile_hash->{VIEWPORT}{$viewport_id}{ZOOM}}{$tile_xy->[0]}{$tile_xy->[1]}{CODE}=$queued_by_code;
            next;
        }
        # next if we already know what this tile contains
        next if $tile_hash->{GM_TILES}{$tile_hash->{VIEWPORT}{$viewport_id}{VER}}{$tile_hash->{VIEWPORT}{$viewport_id}{ZOOM}}{$tile_xy->[0]}{$tile_xy->[1]}{CODE};
        # get the tile
        my $gifdata = $self->get_tile($tile_xy->[0], $tile_xy->[1], $tile_hash->{VIEWPORT}{$viewport_id}{ZOOM});
        if ($gifdata) {
            $tile_hash->{GM_TILES}{$tile_hash->{VIEWPORT}{$viewport_id}{VER}}{$tile_hash->{VIEWPORT}{$viewport_id}{ZOOM}}{$tile_xy->[0]}{$tile_xy->[1]}{CODE}=1;
            $tile_hash->{GM_TILES}{$tile_hash->{VIEWPORT}{$viewport_id}{VER}}{$tile_hash->{VIEWPORT}{$viewport_id}{ZOOM}}{$tile_xy->[0]}{$tile_xy->[1]}{DATA}=$gifdata;
        }else{
            $self->{debug} && print STDERR "hit water!\n";
            $tile_hash->{GM_TILES}{$tile_hash->{VIEWPORT}{$viewport_id}{VER}}{$tile_hash->{VIEWPORT}{$viewport_id}{ZOOM}}{$tile_xy->[0]}{$tile_xy->[1]}{CODE}="WATER";
            # "draw" a horizontal or vertical water boundary
            if ($tile_xy->[0] == $queued_by_xy->[0]) {
                # draw a horizontal boundary
                foreach my $x ($tile_hash->{VIEWPORT}{$viewport_id}{BOUNDARY}{0}{-1} .. $tile_hash->{VIEWPORT}{$viewport_id}{BOUNDARY}{0}{1}) {
                    $tile_hash->{GM_TILES}{$tile_hash->{VIEWPORT}{$viewport_id}{VER}}{$tile_hash->{VIEWPORT}{$viewport_id}{ZOOM}}{$x}{$tile_xy->[1]}{CODE}="WATER";
                }
            }else{
                # draw a vertical boundary
                foreach my $y ($tile_hash->{VIEWPORT}{$viewport_id}{BOUNDARY}{1}{-1} .. $tile_hash->{VIEWPORT}{$viewport_id}{BOUNDARY}{1}{1}) {
                    $tile_hash->{GM_TILES}{$tile_hash->{VIEWPORT}{$viewport_id}{VER}}{$tile_hash->{VIEWPORT}{$viewport_id}{ZOOM}}{$tile_xy->[0]}{$y}{CODE}="WATER";
                }
            }
        }
    }

    # Assemble our viewport image
    $self->{gd} = GD::Image->new($self->{width}, $self->{height}, 1);  # new truecolor image
    foreach my $tile_xy (@{$tile_hash->{VIEWPORT}{$viewport_id}{TILE_QUEUE}}) {
        my $viewport_xy = $tile_hash->{VIEWPORT}{$viewport_id}{TILE_MAP}{$tile_xy->[0]}{$tile_xy->[1]}{XY};
        my $tile_data = $tile_hash->{GM_TILES}{$tile_hash->{VIEWPORT}{$viewport_id}{VER}}{$tile_hash->{VIEWPORT}{$viewport_id}{ZOOM}}{$tile_xy->[0]}{$tile_xy->[1]}{DATA};
        if ($tile_data) {
            my $mt = GD::Image->newFromGifData($tile_data);
            $self->{gd}->copy($mt, $viewport_xy->[0], $viewport_xy->[1], 0, 0, GOOGLEMAPS_TILE_RES_X, GOOGLEMAPS_TILE_RES_Y);
        }else{
            my $tile_code = $tile_hash->{GM_TILES}{$tile_hash->{VIEWPORT}{$viewport_id}{VER}}{$tile_hash->{VIEWPORT}{$viewport_id}{ZOOM}}{$tile_xy->[0]}{$tile_xy->[1]}{CODE};
            my $mapfile_gif;
            if ($tile_code eq "WATER") {
                $mapfile_gif = GOOGLEMAPS_TILE_WATER_GIF;
                $tile_hash->{GM_TILES}{MAPFILES}{$tile_code}{DATA} ||= $self->get_mapfile_data($mapfile_gif);
            }else{
                # some other mapfile?
                next;
            }
            my $mt = GD::Image->newFromGifData($tile_hash->{GM_TILES}{MAPFILES}{$tile_code}{DATA});
            $self->{gd}->copy($mt, $viewport_xy->[0], $viewport_xy->[1], 0, 0, GOOGLEMAPS_TILE_RES_X, GOOGLEMAPS_TILE_RES_Y);
        }
    }
    
    # Add copyright notice
    $self->insert_google_maps_copyright_gd();
    
    return $self->{gd};
}

sub insert_google_maps_copyright_gd {
    my $self = shift;
    if ($self->{gd}) {
        my $white = $self->{gd}->colorAllocateAlpha(255,255,255,20);
        $self->{gd}->filledRectangle(0,$self->{height}-13,$self->{width},$self->{height},$white);
        my $black = $self->{gd}->colorAllocate(0,0,0);
        $self->{gd}->line(0,$self->{height}-14,$self->{width},$self->{height}-14,$black);
        $self->{gd}->string(gdSmallFont,5,$self->{height}-14,"(c) 2005 Google",$black);
        $self->{gd}->string(gdSmallFont,$self->{width}-240,$self->{height}-14,"Map data (c) 2005 NAVTEQ(tm), TeleAtlas",$black);
        return $self->{gd};
    }else{
        $self->error("cannot add copyright until gd map is generated");
        return undef;
    }
}

sub insert_latlon_marker_gd {
    my $self = shift;
    my $viewport_latlonzoom = $self->get_viewport_latlonzoom();
    if (!$viewport_latlonzoom) {
        $self->error("cannot insert marker unless viewport latitude, longitude, and zoom level are set");
        return undef;
    }
    if (!scalar (@_)) {
        return $self->insert_xy_marker_gd([$viewport_latlonzoom->[0], $viewport_latlonzoom->[1], ""]);
    }else{
        my @marker_list;
        if (!ref($_[0])) {
            return $self->insert_xy_marker_gd([$viewport_latlonzoom->[0], $viewport_latlonzoom->[1], ""]);
        }else{
            foreach my $marker (@_) {
                if (!scalar(@{$marker})) {
                    push (@marker_list, [@{$self->latlonzoom_to_viewport_xy(@{$viewport_latlonzoom})}, $marker->[2]]);
                }else{
                    push (@marker_list, [@{$self->latlonzoom_to_viewport_xy($marker->[0], $marker->[1], $viewport_latlonzoom->[2])}, $marker->[2]]);
                }
            }
        }
        return $self->insert_xy_marker_gd(@marker_list);
    }
}

sub insert_xy_marker_gd {
    my $self = shift;
    if ($self->{gd}) {
        my @marker_list;
        if (!ref($_[0])) {
            push (@marker_list, [@_]);
        }else{
            @marker_list = @_;
        }
        # draw all shadows first, then markers
        my $marker_data = $self->get_mapfile_data(GOOGLEMAPS_MARKER_SHADOW_PNG);
        if ($marker_data) {
            foreach my $marker (@marker_list) {
                my ($cx, $cy) = @{$marker};
                my $marker_gd = GD::Image->newFromPngData($marker_data, 1);
                $marker_gd->alphaBlending(0);
                $marker_gd->saveAlpha(1);
                $self->{gd}->copy($marker_gd, $cx - GOOGLEMAPS_MARKER_CENTER_OFFSET->[0], $cy - GOOGLEMAPS_MARKER_CENTER_OFFSET->[1], 0, 0, $marker_gd->getBounds());
            }
        }
        foreach my $marker (@marker_list) {
            my ($cx, $cy, $label) = @{$marker};
            my $marker_gif;
            my $symbol;
            if ($label =~ /^([A-J\.\+\=])(\!(.*?)$|$)/) {  # valid are: A B C D E F G H I J . + =
                $symbol = $1;
                $label = $3;
                if ($symbol =~ /[A-J]/) {
                    $marker_gif = sprintf(GOOGLEMAPS_MARKER_PNG, $symbol);
                }elsif ($symbol =~ /\./) {
                    $marker_gif = sprintf(GOOGLEMAPS_MARKER_PNG, "");
                }elsif ($symbol =~ /\+/) {
                    $marker_gif = GOOGLEMAPS_MARKER_DD_START_PNG;
                }elsif ($symbol =~ /\=/) {
                    $marker_gif = GOOGLEMAPS_MARKER_DD_END_PNG;
                }
            }else{
                $symbol = ".";
                $label ||= "";
                $marker_gif = sprintf(GOOGLEMAPS_MARKER_PNG, "");
            }
            if ($label) {
                my $white = $self->{gd}->colorAllocateAlpha(255,255,255,40);
                $self->{gd}->filledRectangle($cx + 10 - 2, $cy - 31, $cx + length($label)*6 + 13, $cy - 31 + 13 ,$white);
                my $black = $self->{gd}->colorAllocate(0,0,0);
                $self->{gd}->string(gdSmallFont,$cx + 10 + 2, $cy - 31 , $label, $black);
            }
            $marker_data = $self->get_mapfile_data($marker_gif);
            if ($marker_data) {
                my $marker_gd = GD::Image->newFromPngData($marker_data, 1);
                $marker_gd->alphaBlending(0);
                $marker_gd->saveAlpha(1);
                $self->{gd}->copy($marker_gd, $cx - GOOGLEMAPS_MARKER_CENTER_OFFSET->[0], $cy - GOOGLEMAPS_MARKER_CENTER_OFFSET->[1], 0, 0, $marker_gd->getBounds());
            }
        }
    }else{
        $self->error("cannot insert marker unless gd map has been generated");
    }
    return $self->{gd};
}    


sub generate_html {
    my $self = shift;
    $self->set(@_);

    my $viewport_latlonzoom = $self->get_viewport_latlonzoom();
    if (!$viewport_latlonzoom) {
        $self->error("latitude, longitude, and zoom_level must be specified");
        return undef;
    }
    
    return "<div>Not available yet...</div>";
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
    $gmap->generate_gd();

    # or simply
    # $gmap->generate_gd(40.750275, -73.993034, 4);  # latitude, longitude, zoom_level

    my $error = $gmap->error();
    $error && print "Error: $error\n";
    
    open (FH, ">", "mymap.png");
    binmode FH;
    print FH $gmap->{gd}->png;
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


=head2 center_viewport_on_coordinates()

Pass it a list of array references, each array must contain at
least two elements, a latitude and longitude of a place on the
map.

The method will set the center of the viewport to the middle of
all the passed coordinates, and set a zoom level that will keep 
all the coordinates inside the viewport.


    my @markerlist=(
        [40.725464,  -74.002289, "H!Hiroko's Place"],
        [40.748199,  -74.031575, "=!The Bagel Smashery"],
        [40.8161293, -73.979786, "+!Taiyaki"]
    );
    
    $gmap->center_viewport_on_coordinates(@markerlist);


=head2 generate_gd()

This performs the retrieval of the image from Google Maps.  If the images
are not found in the cache, (assuming you have specified a directory to
use for the cache), this may take a little extra time to complete.

It will return a GD object, which you may then use as you please.  It is
useful to save it to a file.

If there is a problem creating the image, an error message will be set.
You may retrieve this error message at $gmap->error()


=head2 insert_latlon_marker_gd()

After you use generate_gd() to draw the map, you may add a number of
labeled points to the map.

Pass this method a list of array references.  Each array reference must
contain at least two elements, the latitude and longitude of a coordinate
that you want to be marked.  Optionally, each array reference may have
a third element, a label for the point.

The syntax for this point may be:

=over 4

=item * A single letter A-J for a red pin with that letter

=item * A single period character (.) for a red pin with a black dot

=item * An plus sign (+) for a green pin with a black triangle

=item * An equals sign (=) for a red pin with a black square

=item * A string of text, for a labeled red pin with a black dot

=item * A string of text in the format "x!Label", where "x" is one of 
the characters that specifies the pin type, "!" is simply an exclamation 
mark, and "Label" is any text you want to appear beside the pin.

=back

    use WebService::GoogleMaps;
    
    my $gmap = WebService::GoogleMaps->new(640, 480);
    
    $gmap->set(
        cache_dir  => "/tmp",     # optional, but helps speed up future requests
    );
    
    my @markerlist=(
        [40.725464,  -74.002289, "H!Hiroko's Place"],
        [40.748199,  -74.031575, "=!The Bagel Smashery"],
        [40.8161293, -73.979786, "+!Taiyaki"]
    );
    
    $gmap->center_viewport_on_coordinates(@markerlist);
    
    $gmap->generate_gd();
    
    $gmap->insert_latlon_marker_gd(@markerlist);
    
    $gmap->{error} && die "Error: $gmap->{error}\n";
    
    open (FH, ">", "goodeats.png");
    binmode FH;
    print FH $gmap->{gd}->png;
    close(FH);



=head2 generate_html()

Not available in this version.  The plan is to have this return HTML code that
will reference images on the Google Maps server to render the map image.


=head1 TODO

=over 4

=item * Improve caching method.  Maybe use subdirectories to separate cached images into geographic areas?

=item * Develop the generate_html method

=item * Method to draw paths on the map?

=back


=head1 AUTHOR

Karl Lohner, E<lt>karllohner+googlemaps@gmail.comE<gt>


=head1 BUGS

I am sure there is a few in here...

Please report any bugs or feature requests to E<lt>karllohner+googlemaps@gmail.comE<gt>.

Really, I would like to know how you are using this module and what you would like to see
to make it better.


=head1 REFERENCES

=over 4

=item * Google Maps Hacking: http://69.90.152.144/collab/GoogleMapsHacking

=item * Google Maps Live for Firefox: http://ted.mielczarek.org/code/mozilla/gmaplive/

=item * Mapping Google: http://jgwebber.blogspot.com/2005/02/mapping-google.html

=item * Google Maps Hacking and Bookmarklets: http://libgmail.sourceforge.net/googlemaps.html

=back

=head1 ACKNOWLEDGEMENTS

Thanks to Joel Webber and his blog article at http://jgwebber.blogspot.com/2005/02/mapping-google.html
as well as to the many people who added comments there, particularly the anonymous comments that detailed
the algorithm used to map latitude/longitude to specific tiles.


=head1 COPYRIGHT AND LICENSE

Copyright 2005 by Karl Lohner, All Rights Reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
