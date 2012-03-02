package Perinci::CmdLine;

use 5.010;
use strict;
use warnings;
use Log::Any '$log';
use Moo;

# VERSION

has program_name => (is => 'rw', default=>sub {local $_=$0; s!.+/!!; $_});
has url => (is => 'rw');
has summary => (is => 'rw');
has subcommands => (is => 'rw');
has exit => (is => 'rw', default=>sub{1});
has log_any_app => (is => 'rw', default=>sub{1});
has custom_completer => (is => 'rw');
has custom_arg_completer => (is => 'rw');
has dash_to_underscore => (is => 'rw', default=>sub{1});
has undo => (is=>'rw', default=>sub{0});
has format => (is => 'rw', default=>sub{'text'});

sub BUILD {
    require Perinci::Access;
    my ($self, $args) = @_;
    $self->{_pa} = Perinci::Access->new;
}

sub format_result {
    require Data::Format::Pretty;
    *format_pretty = \&Data::Format::Pretty::format_pretty;

    my ($self) = @_;
    my $format = $self->format;

    if ($format eq 'yaml') {
        $self->{_fres} = format_pretty($self->{_res}, {module=>'YAML'});
        return;
    }
    if ($format eq 'json') {
        $self->{_fres} = format_pretty($self->{_res}, {module=>'JSON'});
        return;
    }
    if ($format eq 'php') {
        $self->{_fres} = format_pretty($self->{_res}, {module=>'PHP'});
        return;
    }
    if ($format =~ /^(text|pretty|nopretty)$/) {
        if (!defined($self->{_res}[2])) {
            $self->{_fres} = $self->{_res}[0] == 200 ? "" :
                "ERROR $self->{_res}[0]: $self->{_res}[1]\n";
            return;
        }
        my $r = $self->{_res}[0] == 200 ? $self->{_res}[2] : $self->{_res};
        if ($format eq 'text') {
            $self->{_fres} = format_pretty($r, {module=>'Console'});
            return;
        }
        if ($format eq 'pretty') {
            $self->{_fres} = format_pretty($r, {module=>'Text'});
            return;
        }
        if ($format eq 'nopretty') {
            $self->{_fres} = format_pretty($r, {module=>'SimpleText'});
            return;
        }
    }

    die "BUG: Unknown output format `$format`";
}

sub display_result {
    my ($self) = @_;
    print $self->{_fres};
}

sub get_subcommand {
    my ($self, $name) = @_;

    my $scs = $self->subcommands;
    return undef unless $scs;

    if (ref($scs) eq 'CODE') {
        return $scs->($self, name=>$name);
    } else {
        return $scs->{$name};
    }
}

sub list_subcommands {
    my ($self) = @_;
    state $cached;
    return $cached if $cached;

    my $scs = $self->subcommands;
    my $res;
    if ($scs) {
        if (ref($scs) eq 'CODE') {
            $scs = $scs->($self);
            die "ERROR: Subcommands code didn't return a hashref\n"
                unless ref($scs) eq 'HASH';
        }
        $res = $scs;
    } else {
        $res = {};
    }
    $cached = $res;
}

sub run_list {
    my ($self) = @_;

    my $subcommands = $self->list_subcommands;

    # XXX get summary from Riap if not exist

    my %percat_subc; # (cat1 => {subcmd1=>..., ...}, ...)
    while (my ($scn, $sc) = each %$subcommands) {
        my $cat = "";
        if ($sc->{tags}) {
            for (@{$sc->{tags}}) {
                next unless /^category:(.+)/;
                $cat = $1;
                last;
            }
        }
        $percat_subc{$cat}       //= {};
        $percat_subc{$cat}{$scn}   = $sc;
    }
    my $has_many_cats = scalar(keys %percat_subc) > 1;

    my $i = 0;
    for my $cat (sort keys %percat_subc) {
        print "\n" if $i++;
        if ($has_many_cats) {
            print "List of", ucfirst($cat) || "main",
                " subcommands:\n";
        } else {
            print "List of subcommands:\n";
        }
        my $subc = $percat_subc{$cat};
        for my $scn (sort keys %$subc) {
            my $sc = $subc->{$scn};
            say "  $scn", ($sc->{summary} ? " - $sc->{summary}" : "");
        }
    }

    0;
}

sub run_version {
    my ($self) = @_;

    # get from pkg_version property

    # XXX url does not necessarily a package url, we should URI->new and then
    # cut one path
    my $pkg_url = $self->url or
        die "ERROR: 'url' not set, required for 'version'";

    my $res = $self->{_pa}->request(meta => $pkg_url);
    my $version;
    if ($res->[0] == 200) {
        $version = $res->[2]{pkg_version} // "?";
    } else {
        $log->warnf("Can't request 'meta' action on %s: %d - %s",
                    $pkg_url, $res->[0], $res->[1]);
        $version = '?';
    }

    say $self->program_name, " version ", $version;

    0;
}

sub run_completion {
    # Perinci::BashComplete already required by run()

    my ($self) = @_;

    my $sc = $self->{_subcommand};
    my $words = $self->{_comp_parse_res}{words};
    my $cword = $self->{_comp_parse_res}{cword};
    my $word  = $words->[$cword] // "";

    # determine whether we should complete function arg names/values or just
    # top-level opts + subcommands name
    my $do_arg;
    {
        if (!$self->subcommands) {
            $log->trace("do_arg because single command");
            $do_arg++; last;
        }

        my $scn = $sc->{name};

        # whether user typed 'blah blah ^' or 'blah blah^'
        my $space_typed = !defined($word);

        # e.g: spanel delete-account ^
        if ($self->subcommands && $cword > 0 && $space_typed) {
            $log->trace("do_arg because last word typed (+space) is ".
                            "subcommand name");
            $do_arg++; last;
        }

        # e.g: spanel delete-account --yaml --acc^
        if ($cword > 0 && !$space_typed && $word ne $scn) {
            $log->trace("do_arg because subcommand name has been typed ".
                            "in past words");
            $do_arg++; last;
        }

        $log->tracef("not do_arg, cword=%d, words=%s, scn=%s, space_typed=%s",
                     $cword, $words, $scn, $space_typed);
    }

    my @top_opts; # contain --help, -h, --yaml, etc.
    for my $o (keys %{$self->{_getopts_common}}) {
        $o =~ s/^--//;
        my @o = split /\|/, $o;
        for (@o) { push @top_opts, length > 1 ? "--$_" : "-$_" }
    }

    my $res;
    if ($do_arg) {
        $log->trace("Completing subcommand argument names & values ...");

        # remove subcommand name and general options from words so it doesn't
        # interfere with matching function args
        my $i = 0;
        while ($i < @$words) {
            if ($words->[$i] ~~ @top_opts ||
                    (defined($self->{_scn_in_argv}) &&
                         $words->[$i] eq $self->{_scn_in_argv})) {
                splice @$words, $i, 1;
                $cword-- unless $cword <= $i;
                next;
            } else {
                $i++;
            }
        }
        $log->tracef("cleaned words=%s, cword=%d", $words, $cword);

        $res = Perinci::BashComplete::bash_complete_riap_func_arg(
            url=>$sc->{url}, words=>$words, cword=>$cword,
            custom_completer=>$self->custom_completer,
            custom_arg_completer => $self->custom_arg_completer
        );

    } else {
        $log->trace("Completing top-level options + subcommand name ...");
        my @ary;
        push @ary, @top_opts;
        my $scs = $self->list_subcommands;
        push @ary, keys %$scs;
        $res = Perinci::BashComplete::complete_array(
            word=>$word, array=>\@ary);
    }

    # display completion result for bash
    print map {Perinci::BashComplete::_add_slashes($_), "\n"} @$res;
    0;
}

sub run_help {
    my ($self) = @_;

    my $prog = $self->program_name;

    # XXX custom help subroutine

    my $sc = $self->{_subcommand};
    if ($sc && $sc->{name}) {
        my $res = $self->{_pa}->request(meta => $sc->{url});
        if ($res->[0] == 200) {
            # XXX meta to pod
            require YAML::Syck;
            print "Temporary help message for subcommand $sc->{name}:\n";
            print YAML::Syck::Dump($res->[2]);
            return 0;
        }
    }

    say <<_;
Usage:
  To get general help:
    $prog --help (or -h)
  To list subcommands:
    $prog --list (or -l)
  To show version:
    $prog --version (or -v)
  To get help on a subcommand:
    $prog SUBCOMMAND --help
  To run a subcommand:
    $prog SUBCOMMAND [COMMON OPTIONS] [SUBCOMMAND ARGS ...]

Common options:
  --yaml, -y      Format result as YAML
  --json, -j      Format result as JSON
  --pretty, -p    Format result as pretty formatted text
  --nopretty, -P  Format result as simple formatted text
  --text         (Default) Select --pretty or --nopretty depends on if run piped
_
    0;
}

sub run_subcommand {
    my ($self) = @_;

    # call function
    $self->{_res} = $self->{_pa}->request(
        call => $self->{_subcommand}{url},
        {args=>$self->{_args}});
    $log->tracef("res=%s", $self->{_res});

    # currently only for checking _cmdline.suppress_output_on_success property
    my $res = $self->{_pa}->request(meta => $self->{_subcommand}{url});
    if ($res->[2] == 200) { $self->{_meta} = $res->[2] };

    # format & display result
    $self->format_result();
    $self->display_result();

    $self->{_res}[0] == 200 ? 0 : $self->{_res}[0] - 300;
}

sub _parse_common_opts {
    require Getopt::Long;

    my ($self, $phase) = @_;
    $log->tracef("-> _parse_common_opts(%d)", $phase);

    $Perinci::Sub::GetArgs::Argv::_pa_skip_check_required_args = 0;

    my $old_go_opts = Getopt::Long::Configure(
        "pass_through", "no_ignore_case", ($phase == 1 ? "no_":"")."permute");
    my %getopts = (
        "list|l"     => sub {
            $Perinci::Sub::GetArgs::Argv::_pa_skip_check_required_args = 1
                if $phase == 1;
            unshift @{$self->{_actions}}, 'list';
        },
        "version|v"  => sub {
            unshift @{$self->{_actions}}, 'version';
            die "ERROR: 'url' not set, required for --version\n"
                unless $self->url;
            $Perinci::Sub::GetArgs::Argv::_pa_skip_check_required_args = 1
                if $phase == 1;
        },
        "help|h|?"   => sub {
            $Perinci::Sub::GetArgs::Argv::_pa_skip_check_required_args = 1
                if $phase == 1;
            unshift @{$self->{_actions}}, 'help';
        },

        "text"       => sub { $self->format('text')     },
        "yaml|y"     => sub { $self->format('yaml')     },
        "json|j"     => sub { $self->format('json')     },
        "pretty|p"   => sub { $self->format('pretty')   },
        "nopretty|P" => sub { $self->format('nopretty') },
    );

    # convenience for Log::Any::App-using apps
    if ($self->log_any_app) {
        for (qw/quiet verbose debug trace log_level/) {
            $getopts{$_} = sub {};
        }
    }

    # UNFINISHED. check whether we should add undo related command-line
    # arguments

    #{
    #    last unless $spec || $args{undo};
    #    require Sub::Spec::Object;
    #    my $ssspec = Sub::Spec::Object::ssspec($spec);
    #    last unless $ssspec->feature('undo');
    #
    #    $opts{undo_action}    = 'do';
    #    $getopts{undo_data}   = sub { $opts{undo_data} = shift };
    #    $getopts{undo}        = sub { $opts{undo_action} = 'undo' };
    #    $getopts{redo}        = sub { $opts{undo_action} = 'redo' };
    #    $getopts{list_undos}  = sub { $opts{undo_action} = 'list_undos' };
    #    $getopts{clear_undos} = sub { $opts{undo_action} = 'clear_undos' };
    #}

    # store for other methods, e.g. run_completion()
    $self->{_getopts_common} = \%getopts;

    $log->tracef("GetOptions spec for parsing common options: %s", \%getopts);
    Getopt::Long::GetOptions(%getopts);
    $log->tracef("result of GetOptions for common options: remaining argv=%s, ".
                     "actions=%s", \@ARGV, $self->{_actions});
    Getopt::Long::Configure($old_go_opts);

    $log->tracef("<- _parse_common_opts()");
}

sub _parse_subcommand_opts {
    require Perinci::Sub::GetArgs::Argv;

    my ($self) = @_;
    my $sc = $self->{_subcommand};
    return unless $self->{_subcommand};
    $log->tracef("-> _parse_subcommand_opts()");

    my $res = $self->{_pa}->request(meta=>$sc->{url});
    unless ($res->[0] == 200) {
        $log->warnf("Can't get metadata from %s: %d - %s", $sc->{url},
                    $res->[0], $res->[1]);
        $self->{_args} = {};
        $log->tracef("<- _parse_subcommand_opts() (bailed)");
        return;
    }
    my $meta = $res->[2];

    # parse argv
    my %ga_args = (argv=>\@ARGV, meta=>$meta,
                   extra_getopts => $self->{_getopts_common});
    $res = Perinci::Sub::GetArgs::Argv::get_args_from_argv(%ga_args);
    die "ERROR: $sc->{name}: Failed parsing arguments: $res->[0] - $res->[1]\n"
        unless $res->[0] == 200;
    $self->{_args} = $res->[2];
    $log->tracef("result of GetArgs for subcommand: remaining argv=%s, args=%s".
                     ", actions=%s", \@ARGV, $self->{_args}, $self->{_actions});

    $log->tracef("<- _parse_subcommand_opts()");
}

sub _set_subcommand {
    my ($self) = @_;

    if ($self->subcommands) {
        if (@ARGV) {
            my $scn = shift @ARGV;
            $self->{_scn_in_argv} = $scn;
            $scn =~ s/-/_/g if $self->dash_to_underscore;
            my $sc = $self->get_subcommand($scn);
            unless ($sc) {
                if ($ENV{COMP_LINE}) {
                    require Object::BlankStr;
                    die Object::BlankStr->new;
                } else {
                    die "ERROR: Unknown subcommand '$scn', use '".
                        $self->program_name.
                            " -l' to list available subcommands\n";
                }
            }
            $self->{_subcommand} = $sc;
            $self->{_subcommand}{name} = $scn;
            push @{$self->{_actions}}, 'subcommand';
        }
    } else {
        $self->{_subcommand} = {url=>$self->url, summary=>$self->summary};
        $self->{_subcommand}{name} = '';
        push @{$self->{_actions}}, 'subcommand';
    }
    unshift @{$self->{_actions}}, 'completion' if $ENV{COMP_LINE};
    push @{$self->{_actions}}, 'help' if !@{$self->{_actions}};
    $log->tracef("subcommand=%s", $self->{_actions}, $self->{_subcommand});
}

sub run {
    my ($self) = @_;

    #
    # load Log::Any::App
    #

    unless ($ENV{COMP_LINE}) {
        if ($self->log_any_app) {
            require Log::Any::App;
            Log::Any::App::init();
        }
    }

    $log->trace("-> CmdLine's run()");

    #
    # workaround: detect (1) if we're being invoked for bash completion, get
    # @ARGV from parsing COMP_LINE/COMP_POINT instead, since @ARGV given by bash
    # is messed up / different
    #

    if ($ENV{COMP_LINE}) {
        require Perinci::BashComplete;
        my $res = Perinci::BashComplete::_parse_request();
        @ARGV = @{ $res->{words} };
        $self->{_comp_parse_res} = $res; # store for run_completion()
    }

    #
    # parse common options from @ARGV, 1st phase (no_permute). This is to allow
    # function having arguments like --list or --help the chance to catch them.
    #

    $self->{_actions} = []; # first action will be tried first, then 2nd, ...
    $self->_parse_common_opts(1);

    #
    # find out which subcommand to run, store it in $self->{_subcommand}
    #

    $self->_set_subcommand;

    #
    # parse subcommand options, this is to give change to function arguments
    # like --help to be parsed into $self->{_args}
    #

    $self->_parse_subcommand_opts unless $ENV{COMP_LINE};

    # parse common options once again, to sweep unparsed common options after
    # subcommand argument

    $self->_parse_common_opts(2);

    #
    # finally invoke the appropriate run_*() action method(s)
    #

    my $exit_code;
    while (@{$self->{_actions}}) {
        my $action = shift @{$self->{_actions}};
        my $meth = "run_$action";
        $log->tracef("-> %s()", $meth);
        $exit_code = $self->$meth;
        $log->tracef("<- %s(), return=%s", $meth, $exit_code);
        last if defined $exit_code;
    }

    $log->tracef("<- CmdLine's run(), exit code=%d", $exit_code);
    if ($self->exit) { exit $exit_code } else { return $exit_code }
}

1;
# ABSTRACT: Rinci/Riap-based command-line application framework

=head1 SYNOPSIS

In your command-line script:

 #!/usr/bin/perl
 use Perinci::CmdLine;
 Perinci::CmdLine->new(url => 'Your::Module', ...)->run;

See also the L<peri-run> script which provides a command-line interface for
Perinci::CmdLine.


=head1 DESCRIPTION

Perinci::CmdLine is a command-line application framework. It access functions
using Riap protocol (L<Perinci::Access>) so you get transparent remote access.
It utilizes L<Rinci> metadata in the code so the amount of plumbing that you
have to do is quite minimal.

What you'll get:

=over 4

=item * Command-line parsing (currently using Getopt::Long, with some tweaks)

=item * Help message (utilizing information from metadata)

=item * Tab completion for bash (including completion from remote code)

=back

This module uses L<Log::Any> and L<Log::Any::App> for logging.

This module uses L<Moo> for OO.


=head1 ATTRIBUTES

=head2 program_name => STR (default from $0)

=head2 url => STR

Required if you only want to run one function. URL should point to a function
entity.

Alternatively you can provide multiple functions from which the user can select
using the first argument (see B<subcommands>).

=head2 summary => STR

If unset, will be retrieved from function metadata when needed.

=head2 subcommands => {NAME => {ARGUMENT=>...}, ...} | CODEREF

Should be a hash of subcommand specifications or a coderef.

Each subcommand specification is also a hash(ref) and should contain these keys:
C<url>. It can also contain these keys: C<summary> (will be retrieved from
function metadata if unset), C<tags> (for categorizing subcommands).

Subcommands can also be a coderef, for dynamic list of subcommands. The coderef
will be called as a method with hash arguments. It can be called in two cases.
First, if called without argument C<name> (usually when doing --list) it must
return a hashref of subcommand specifications. If called with argument C<name>
it must return subcommand specification for subcommand with the requested name
only.

=head2 exit => BOOL (default 1)

If set to 0, instead of exiting with exit(), run() will return the exit code
instead.

=head2 custom_completer => CODEREF

Will be passed to L<Perinci::BashComplete>'s C<bash_complete_riap_func_arg>. See
its documentation for more details.

=head2 custom_arg_completer => CODEREF | {ARGNAME=>CODEREF, ...}

Will be passed to L<Perinci::BashComplete>. See its documentation for more
details.

=head2 dash_to_underscore => BOOL (optional, default 1)

If set to 1, subcommand like a-b-c will be converted to a_b_c. This is for
convenience when typing in command line.

=head2 undo => BOOL (optional, default 0)

UNFINISHED. If set to 1, --undo and --undo-dir will be added to command-line
options. --undo is used to perform undo: -undo and -undo_data will be passed to
subroutine, an error will be thrown if subroutine does not have C<undo>
features. --undo-dir is used to set location of undo data (default C<~/.undo>;
undo directory will be created if not exists; each subroutine will have its own
subdir here).


=head1 METHODS

=head2 new(%opts) => OBJ

Create an instance.

=head2 run() -> INT

The main routine. Its job is to parse command-line options in @ARGV and
determine which action method (e.g. run_subcommand(), run_help(), etc) to run.
Action method should return an integer containing exit code. If action method
returns undef, the next action candidate method will be tried.

After that, exit() will be called with the exit code from the action method (or,
if C<exit> attribute is set to false, routine will return with exit code
instead).


=head1 FAQ

=head2 How does Perinci::CmdLine compare with other CLI-app frameworks?

Perinci::CmdLine is part of a more general metadata and wrapping framework
(Perinci::* modules family). Aside from a command-line application, your
metadata is also usable for other purposes, like providing access over HTTP/TCP,
documentation. Sub::Spec::CmdLine is not OO. Configuration file support is
missing (coming soon, most probably based on L<Config::Ini::OnDrugs>). Also
lacking is more documentation and more plugins.

=head2 Why is nonscalar arguments parsed as YAML instead of JSON/etc?

I think YAML is nicer in command-line because quotes are optional in a few
places:

 $ cmd --array '[a, b, c]' --hash '{foo: bar}'

versus:

 $ cmd --array '["a","b","c"]' --hash '{"foo":"bar"}'

Though YAML requires spaces in some places where JSON does not. A flag to parse
as JSON can be added upon request.


=head1 SEE ALSO

L<Perinci>, L<Rinci>, L<Riap>.

Other CPAN modules to write command-line applications: L<App::Cmd>, L<App::Rad>,
L<MooseX::Getopt>.

=cut
