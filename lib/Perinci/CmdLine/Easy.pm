package Perinci::CmdLine::Easy;

use 5.010;
use strict;
use warnings;
use Perinci::CmdLine;
use Scalar::Util qw(reftype);

require Exporter;
our @ISA       = qw(Exporter);
our @EXPORT_OK = qw(run_cmdline_app);

# VERSION

our %SPEC;

$SPEC{run_cmdline_app} = {
    v       => 1.1,
    summary => "A simple interface to run a subroutine as command-line app",
    args    => {
        sub => {
            req => 1,
            summary => "Coderef or subroutine name",
        },
        summary => {
            schema => "str*",
        },
        description => {
            schema => "str*",
        },
        argv => {
            schema  => ["array*" => {of=>"str*", default=>[]}],
            summary => "List of arguments",
            description => <<'_',

Each argument is NAME, NAME* (marking required argument), or NAME+ (marking
greedy argument, where the rest of command-line arguments will be fed into this
array).

_
        },
    },
    result_naked => 1,
    "_perinci.sub.wrapper.validate_args" => 0,
};
sub run_cmdline_app {
    my %args = @_; # VALIDATE_ARGS

    my $meta = {
        v            => 1.1,
        summary      => $args{summary},
        description  => $args{description},
        result_naked => 1,
        args_as      => "array",
        args         => {},
    };

    my $i = 0;
    for my $arg (@{ $args{argv} // []}) {
        my $req    = $arg =~ s/\*$//;
        my $greedy = $arg =~ s/\+$//;

        $meta->{args}{$arg} = {
            pos     => $i,
            req     => $req,
            greedy  => $greedy,
            summary => "Argument #$i",
            schema  => "str*",
        };
        $i++;
    }

    my @caller = caller(1);

    no strict 'refs';
    my $sub = $args{sub};
    my $url;
    if (!$sub) {
        die "Please supply sub\n";
    } elsif (reftype($sub) eq 'CODE') {
        my $name = "$sub";
        $name =~ s/[^A-Za-z0-9]+//g;
        $main::SPEC{$name} = $meta;
        *{ "main::$name" } = $sub;
        $url = "/main/$name";
    } else {
        my ($pkg, $local) = $sub =~ /\A(.+::)?(.+)\z/;
        $pkg = $caller[0] . '::' unless $pkg;
        ${ $pkg . "SPEC" }{$local} = $meta;
        $url = $pkg;
        $url =~ s!::!/!g;
        $url = "/$url";
    }

    Perinci::CmdLine->new(url => $url)->run;
}

1;
# ABSTRACT: A simple interface to run a subroutine as command-line app

=head1 SYNOPSIS

In your command-line script (e.g. named list-cpan-dists):

 use JSON qw(decode_json);
 use LWP::Simple;
 use Perinci::CmdLine::Easy qw(run_cmdline_app);
 run_cmdline_app(
     summary => "List CPAN distributions that belong to an author",
     sub     => sub {
         my $cpanid = shift or die "Please supply CPAN ID\n";
         my $res = get "http://api.metacpan.org/v0/release/_search?q=author:".
             uc($cpanid)."%20AND%20status:latest&fields=name&size=5000"
             or die "Can't query MetaCPAN";
         $res = $json->decode($res);
         die "MetaCPAN timed out\n" if $res->{timed_out};
         my @dists;
         for my $hit (@{ $res->{hits}{hits} }) {
             my $dist = $hit->{fields}{name};
             $dist =~ s/-\d.+//;
             push @dists, $dist;
         }
         \@dists;
     },
     argv    => [qw/cpanid*/],
 );

To run this program:

 % list-cpan-dists --help ;# display help message
 % LANG=id_ID list-cpan-dists --help ;# display help message in Indonesian
 % list-cpan-dists SHARYANTO

To do bash tab completion:

 % complete -C list-cpan-dists list-cpan-dists
 % list-cpan-dists <tab> ;# completes to --help, --version, --cpanid, etc
 % list-cpan-dists --c<tab> ;# completes to --cpanid


=head1 DESCRIPTION

Perinci::CmdLine::Easy provides an easier alternative to L<Perinci::CmdLine>.
You do not need to know any L<Rinci> or L<Riap> concepts, or provide your own
metadata. Just supply the subroutine, summary, list of arguments, and you're
good to go. Of course, if you need more customization, there's Perinci::CmdLine.

What you'll get:

=over 4

=item * Command-line options parsing

=item * Help message (supports translation)

=item * Tab completion for bash

=item * Formatting of output (supports complex data structure)

=item * Logging

=back


=head1 SEE ALSO

L<Perinci::CmdLine>

=cut
