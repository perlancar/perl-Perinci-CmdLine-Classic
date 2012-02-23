package Perinci::CmdLine;

use 5.010;
use strict;
use warnings;
use Log::Any '$log';

use Perinci::Access;
use Moo;

# VERSION

has program_name => (is => 'rw', default=>sub {local $_=$0; s!.+/!!; $_});
has url => (is => 'rw');
has summary => (is => 'rw');
has subcommands => (is => 'rw');
has exit => (is => 'rw', default=>sub{1});
has custom_completer => (is => 'rw');
has custom_arg_completer => (is => 'rw');
has dash_to_underscore => (is => 'rw', default=>sub{1});
#has undo => (is=>'rw', default=>sub{0});
has format => (is => 'rw', default=>sub{'text'});

sub BUILD {
    my ($self, $args) = @_;
    $self->{_pa} = Perinci::Access->new;
}

sub format_output {
    require Data::Format::Pretty;

    my ($self) = @_;
    my $format = $self->format;

    if ($format eq 'yaml') {
        $self->{_fres} = return format_pretty($self->{_res}, {module=>'YAML'});
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

sub display_output {
    my ($self) = @_;
    print $self->{_fres};
}

sub run_list {
    my ($self) = @_;

    my $subcommands = $self->subcommands;

    return unless $subcommands;

    if (ref($subcommands) eq 'CODE') {
        $subcommands = $subcommands->($self);
        die "Error: subcommands code didn't return a hashref\n"
            unless ref($subcommands) eq 'HASH';
    }

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
            print "List of ", ucfirst($cat) || "main",
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
    my $pkg_url = $self->url;

    my $res = $self->{_pa}->request(meta => $pkg_url);
    die "Can't request 'meta' action on $pkg_url: $res->[0] - $res->[1]\n"
        unless $res->[0] == 200;

    my $version = $res->[2]{pkg_version} // "?";

    say $self->program_name, " version ", $version;

    0;
}

sub _run_completion {
    my %args = @_;

    my @general_opts;
    for my $o (keys %{$args{getopts}}) {
        $o =~ s/^--//;
        my @o = split /\|/, $o;
        for (@o) { push @general_opts, length > 1 ? "--$_" : "-$_" }
    }

    my $spec  = $args{spec};
    my $subc  = $args{subcommand};
    my $subcn = $args{subcommand_name};
    my $words = $args{words};
    my $cword = $args{cword};

    # whether we should complete arg names/values or general opts + subcommands
    # name
    my $do_arg;
    {
        # we can't do arg unless we already get the spec
        if (!$spec) {
            $log->trace("not do_arg because there is no spec");
            last;
        }

        # single-sub directly complete arg names/values
        if (!$subc) {
            $log->trace("do_arg because single sub");
            $do_arg++; last;
        }

        # multiple-sub, just typing "CMD subc ^" (space already typed)
        if ($cword > 0 && $args{space_typed} && $subcn) {
            $log->trace("do_arg because last word typed (+space) is ".
                            "subcommand name");
            $do_arg++; last;
        }

        # multiple-sub, already typing subc in past words
        if ($cword > 0 && !$args{space_typed} && $words->[$cword] ne $subcn) {
            $log->trace("do_arg because subcommand name has been typed ".
                            "in past words");
            $do_arg++; last;
        }

        $log->tracef("not do_arg, cword=%d, words=%s, subcommand_name=%s, ".
                         "space_typed=%s",
                     $cword, $words, $subcn, $args{space_typed});
    }
    if ($do_arg) {
        $log->trace("Complete subcommand argument names & values");

        # remove subcommand name and general options from words so it doesn't
        # interfere with matching spec args
        my $i = 0;
        while ($i < @$words) {
            if ($words->[$i] ~~ @general_opts || $words->[$i] eq $subcn) {
                splice @$words, $i, 1;
                $cword-- unless $cword <= $i;
                next;
            } else {
                $i++;
            }
        }
        $log->tracef("cleaned words=%s, cword=%d", $words, $cword);

        return Perinci::BashComplete::bash_complete_riap_func_arg(
            meta => $spec,
                words            => $words,
                cword            => $cword,
                arg_sub          => $args{arg_sub},
                args_sub         => $args{args_sub},
                custom_completer =>
                    ($subc ? $subc->{custom_completer} :
                         undef) // $args{parent_args}{custom_completer}
        );
    } else {
        $log->trace("Complete general options & names of subcommands");
        my $subcommands = $args{parent_args}{subcommands};
        if (ref($subcommands) eq 'CODE') {
            $subcommands = $subcommands->(parent_args=>$args{parent_args});
            die "Error: subcommands code didn't return hashref (2)\n"
                unless ref($subcommands) eq 'HASH';
        }
        return Sub::Spec::BashComplete::_complete_array(
            $args{word},
            [@general_opts, keys(%$subcommands)]
        );
    }
}

sub run_help {
    my ($help, $spec, $cmd, $summary, $argv) = @_;

    my $out = "";

    #$out .= $cmd . ($summary ? " - $summary" : "") . "\n\n";

    if ($help) {
        if (ref($help) eq 'CODE') {
            $out .= $help->(
                spec=>$spec, cmd=>$cmd,
                argv=>$argv,
            );
        } else {
            $out .= $help;
        }
    } elsif ($spec) {
        $out .= spec_to_usage(spec=>$spec, command_name=>$cmd)->[2];
    } else {
            $out .= <<_;
Usage:
  To get general help:
    $cmd --help (or -h)
  To list subcommands:
    $cmd --list (or -l)
  To show version:
    $cmd --version (or -v)
  To get help on a subcommand:
    $cmd --help SUBCOMMAND
  To run a subcommand:
    $cmd SUBCOMMAND [ARGS ...]

_
    }
    $out;
}

sub run {
    $log->trace("-> CmdLine's run()");
    require Getopt::Long;

    my %args = @_;
    my $exit = $args{exit} // 1;

    # detect (1) if we're being invoked for bash completion, get ARGV from
    # COMP_LINE instead since ARGV given by bash is messed up / different
    my ($comp_words, $comp_cword, $comp_word);
    if ($ENV{COMP_LINE}) {
        eval { require Sub::Spec::BashComplete };
        my $eval_err = $@;
        if ($eval_err) {
            die "Can't load Sub::Spec::BashComplete: $eval_err\n";
        }

        my $res = Sub::Spec::BashComplete::_parse_request();
        $comp_words = $res->{words};
        $comp_cword = $res->{cword};
        $comp_word  = $comp_words->[$comp_cword] // "";

        @ARGV = @$comp_words;
    }

    my %opts = (format => undef, action => 'run');
    my $old_go_opts = Getopt::Long::Configure(
        "pass_through", "no_ignore_case", "no_permute");
    my %getopts = (
        "list"       => sub {
            $Sub::Spec::GetArgs::Argv::_pa_skip_check_required_args++;
            $opts{action} = 'list'     },
        "version"    => sub {
            $Sub::Spec::GetArgs::Argv::_pa_skip_check_required_args++;
            $opts{action} = 'version'  },
        "help"       => sub {
            $Sub::Spec::GetArgs::Argv::_pa_skip_check_required_args++;
            $opts{action} = 'help'     },

        "text"       => sub { $opts{format} = 'text'     },
        "yaml"       => sub { $opts{format} = 'yaml'     },
        "json"       => sub { $opts{format} = 'json'     },
        "pretty"     => sub { $opts{format} = 'pretty'   },
        "nopretty"   => sub { $opts{format} = 'nopretty' },
    );
    # aliases. we don't use "version|v" etc so the key can be compared with spec
    # arg in get_args_from_argv()
    $getopts{l} = $getopts{list};
    $getopts{v} = $getopts{version};
    $getopts{h} = $getopts{help};
    $getopts{'please_help_me|?'} = $getopts{help}; # Go::L doesn't accept '?'

    # convenience for Log::Any::App-using apps
    if (is_loaded('Log::Any::App')) {
        for (qw/quiet verbose debug trace log_level/) {
            $getopts{$_} = sub {};
        }
    }

    Getopt::Long::GetOptions(%getopts);
    Getopt::Long::Configure($old_go_opts);

    my $cmd = $args{cmd};
    if (!$cmd) {
        $cmd = $0;
        $cmd =~ s!.+/!!;
    }
    my $subcommands = $args{subcommands};
    my $module;
    my $sub;

    # finding out which module/sub to use
    my $subc;
    my $subc_name;
    my $load;
    if ($subcommands && @ARGV) {
        $subc_name = shift @ARGV;
        $subc_name =~ s/-/_/g if $args{dash_to_underscore};
        $subc      = ref($subcommands) eq 'CODE' ?
            $subcommands->(name=>$subc_name, args=>\%args) :
            $subcommands->{$subc_name};
        # it's ok if user type incomplete subcommand name under completion
        unless ($ENV{COMP_LINE}) {
            $subc or die "Unknown subcommand `$subc_name`, please ".
                "use $cmd -l to list available subcommands\n";
        }
        $module        = $subc->{module}        // $args{module};
        $sub           = $subc->{sub}           // $subc_name;
        $load          = $subc->{load}          // $args{load} // 1;
    } else {
        $module        = $args{module};
        $sub           = $args{sub};
        $load          = $args{load}            // 1;
    }

    # require module and get spec
    my $spec;
    if ($subc && $subc->{spec}) {
        $spec = ref($subc->{spec}) eq 'CODE' ?
            $subc->{spec}->(module=>$module, sub=>$sub) :
                $subc->{spec};
    } elsif ($args{spec}) {
        $spec = ref($args{spec}) eq 'CODE' ?
            $args{spec}->(module=>$module, sub=>$sub) :
                $args{spec};
    } elsif ($module) {
        {
            my $modulep = $args{module};
            $modulep =~ s!::!/!g; $modulep .= ".pm";
            if ($load) {
                eval { require $modulep };
                if ($@) {
                    die $@ unless $ENV{COMP_LINE};
                    last;
                }
            }

            if ($sub) {
                no strict 'refs';
                my $subs = \%{$module."::SPEC"};
                $spec = $subs->{$sub};
                die "Can't find spec for sub $module\::$sub\n"
                    unless $spec || $ENV{COMP_LINE};
            }
        }
    }

    # check whether we should add undo related command-line arguments
    {
        last unless $spec || $args{undo};
        require Sub::Spec::Object;
        my $ssspec = Sub::Spec::Object::ssspec($spec);
        last unless $ssspec->feature('undo');

        $opts{undo_action}    = 'do';
        $getopts{undo_data}   = sub { $opts{undo_data} = shift };
        $getopts{undo}        = sub { $opts{undo_action} = 'undo' };
        $getopts{redo}        = sub { $opts{undo_action} = 'redo' };
        $getopts{list_undos}  = sub { $opts{undo_action} = 'list_undos' };
        $getopts{clear_undos} = sub { $opts{undo_action} = 'clear_undos' };
    }

    # now that we have spec, detect (2) if we're being invoked for bash
    # completion and do completion, and exit.
    if ($ENV{COMP_LINE}) {
        # user has typed 'CMD subc ^' instead of just 'CMD subc^', in the latter
        # case we still need to complete subcommands name.
        my $space_typed = !defined($comp_words->[$comp_cword]);

        my $complete_arg;
        my $complete_args;
        if ($subc) {
            $complete_arg    = $subc->{complete_arg};
            $complete_args   = $subc->{complete_args};
        }
        $complete_arg      //= $args{complete_arg};
        $complete_args     //= $args{complete_args};
        my @res = _run_completion(
            space_typed     => $space_typed,
            parent_args     => \%args,
            spec            => $spec,
            getopts         => \%getopts,
            words           => $comp_words,
            cword           => $comp_cword,
            word            => $comp_word ,
            arg_sub         => $complete_arg,
            args_sub        => $complete_args,
            subcommand      => $subc,
            subcommand_name => $subc_name,
        );
        $log->tracef("completion result: %s", \@res);
        print map {Sub::Spec::BashComplete::_add_slashes($_), "\n"} @res;
        #print map {"$_\n"} @res;
        if ($exit) { exit 0 } else { return 0 }
    }

    # parse argv
    my $args;
    if ($spec && $opts{action} eq 'run') {
        my %ga_args = (argv=>\@ARGV, spec=>$spec);
        $ga_args{strict} = 0
            if $subc->{allow_unknown_args} // $args{allow_unknown_args};

        # this allows us to catch --help, --version, etc specified after
        # subcommand name (if it doesn't collide with any spec arg). for
        # convenience, e.g.: allowing 'cmd subcmd --help' in addition to 'cmd
        # --help subcmd'.
        $ga_args{extra_getopts} = \%getopts;
        $args = get_args_from_argv(%ga_args);
    }

    # handle --list
    if ($opts{action} eq 'list') {
        _run_list($subcommands, \%args);
        if ($exit) { exit 0 } else { return 0 }
    }

    # handle --version
    if ($opts{action} eq 'version') {
        _run_version($module, $cmd, $args{summary});
        if ($exit) { exit 0 } else { return 0 }
    }

    # handle --help
    if ($opts{action} eq 'help') {
        if ($spec) {
            print _run_help(
                $subc->{help}, $spec,
                ($subc_name ? "$cmd $subc_name" : $cmd),
                ($subc ? $subc->{summary} : $args{summary}),
                 \@ARGV);
        if ($exit) { exit 0 } else { return 0 }
        } else {
            print _run_help(
                $args{help}, undef, $cmd,
                $subc ? $subc->{summary} : $args{summary},
                \@ARGV, undef);
        }
        if ($exit) { exit 0 } else { return 0 }
    }

    die "Please specify a subcommand, ".
        "use $cmd -l to list available subcommands\n"
            unless $spec;

    # finally, run!
    my $res;
    if ($subc && $subc->{run}) {
        # use run routine instead if supplied
        $res = $subc->{run}->(
            subcommand_name => $subc_name,
            module   => $module,
            sub      => $sub,
            spec     => $spec,
            sub_args => $args,
        );
    } else {
        # run sub
        {
            require Sub::Spec::Runner;
            my $runner = Sub::Spec::Runner->new;
            $runner->load_modules($load);
            $runner->undo($opts{undo});
            $runner->undo_data_dir($opts{undo_dir}) if $opts{undo_dir};
            eval { $runner->add("$module\::$sub", $args) };
            my $eval_err = $@;
            if ($eval_err) {
                chomp($eval_err);
                $res = [412, $eval_err];
                last;
            }
            $res = $runner->run(use_last_res=>1);
        }
    }

    my $exit_code = $res->[0] == 200 ? 0 : $res->[0] - 300;

    # output
    $log->tracef("res=%s", $res);
    $log->tracef("opts=%s", \%opts);
    print format_result($res, $opts{format})
        unless $spec->{cmdline_suppress_output} && !$exit_code;

    $log->trace("<- CmdLine's run()");
    if ($exit) { exit $exit_code } else { return $exit_code }
}

1;
# ABSTRACT: Access Perl subs via command line

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

Should be a hash of subcommands or a coderef which should return a hash of
subcommands.

Each subcommand specification is also a hash and should contain these keys:
C<url>, C<summary>. It can also contain these keys: C<tags> (for categorizing
subcommands).

If subcommands is a coderef, it will be called as a method. The code is expected
to return subcommands hashref.

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

=head2 run(%args) -> INT

The main routine. It does roughly the following:

=over 4

=item * Parse command-line options in @ARGV (using Sub::Spec::GetArgs::Argv)

Determine which subcommand is accessed and parse function arguments from
command-line options. Also handle common options like --help, --yaml.

=item * Call function

=item * Format the return value from function and display it

=item * Exit with appropriate exit code

0 if 200, or CODE-300.

(If C<exit> attribute is set to false, will return with exit code instead of
directly calling exit().)

=back

run() can also perform completion for bash. To get bash completion for your
B<perlprog>, just type this in bash:

 % complete -C /path/to/perlprog perlprog

You can add that line in bash startup file (~/.bashrc, /etc/bash.bashrc, etc).


=head1 FAQ

=head2 How does Perinci::CmdLine compare with other CLI-app frameworks?

Perinci::CmdLine is part of a more general metadata and wrapping framework
(Perinci::* modules family). Aside from a command-line application, your
metadata is also usable for other purposes, like providing access over HTTP/TCP,
documentation. Sub::Spec::CmdLine is not OO. Configuration file support is
missing (coming soon).

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
