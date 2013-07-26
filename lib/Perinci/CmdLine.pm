package Perinci::CmdLine;

use 5.010001;
use strict;
use warnings;
use Log::Any '$log';

use Data::Dump::OneLine qw(dump1);
use Moo;
use experimental 'smartmatch'; # must be after Moo
use Perinci::Object;
use Perinci::ToUtil;
use Scalar::Util qw(reftype blessed);

# VERSION

with 'Perinci::To::Text::AddDocLinesRole';
with 'SHARYANTO::Role::Doc::Section';
with 'SHARYANTO::Role::I18N';
with 'SHARYANTO::Role::I18NRinci';

has program_name => (
    is => 'rw',
    lazy => 1,
    default => sub {
        my $pn = $ENV{PERINCI_CMDLINE_PROGRAM_NAME};
        if (!defined($pn)) {
            $pn = $0; $pn =~ s!.+/!!;
        }
        $pn;
    }
);
has url => (is => 'rw');
has summary => (is => 'rw');
has subcommands => (is => 'rw');
has default_subcommand => (is => 'rw');
has exit => (is => 'rw', default=>sub{1});
has log_any_app => (is => 'rw', default=>sub{$ENV{LOG} // 1});
has custom_completer => (is => 'rw');
has custom_arg_completer => (is => 'rw');
has dash_to_underscore => (is => 'rw', default=>sub{1});
has pass_cmdline_object => (is => 'rw', default=>sub{0});
has undo => (is=>'rw', default=>sub{0});
has undo_dir => (
    is => 'rw',
    lazy => 1,
    default => sub {
        my $self = shift;
        my $dir = $ENV{HOME} . "/." . $self->program_name;
        mkdir $dir unless -d $dir;
        $dir .= "/.undo";
        mkdir $dir unless -d $dir;
        $dir;
    }
);
has format => (is => 'rw', default=>sub{'text'});
# bool, is format set via cmdline opt?
has format_set => (is => 'rw');
has format_options => (is => 'rw');
# bool, is format_options set via cmdline opt?
has format_options_set => (is => 'rw');
has pa_args => (is => 'rw');
has _pa => (
    is => 'rw',
    lazy => 1,
    default => sub {
        my $self = shift;

        require Perinci::Access;
        my %args = %{$self->pa_args // {}};
        if ($self->undo) {
            require Perinci::Access::InProcess;
            my $pai = Perinci::Access::InProcess->new(
                use_tx => 1,
                custom_tx_manager => sub {
                    my $pa = shift;
                    require Perinci::Tx::Manager;
                    state $txm = Perinci::Tx::Manager->new(
                        data_dir => $self->undo_dir,
                        pa => $pa,
                    );
                    $txm;
                },
                extra_wrapper_args => {
                    # turn off arg validation generation to reduce startup cost
                    validate_args => $ENV{COMP_LINE} ? 0:undef,
                },
            );
            $args{handlers} = {
                pl   => $pai,
                riap => $pai,
            };
        }
        $log->tracef("Creating Perinci::Access object with args: %s", \%args);
        Perinci::Access->new(%args);
    }
);
has common_opts => (
    is      => 'rw',
    lazy    => 1,
    default => sub {
        my ($self) = @_;

        my %opts;

        # 'action=subcommand' can be used to override --help (or --list,
        # --version) if one of function arguments happens to be 'help', 'list',
        # or 'version'. currently this is undocumented.
        $opts{action} = {
            getopt  => "action=s",
            handler => sub {
                if ($_[1] eq 'subcommand') {
                    $self->{_force_subcommand} = 1;
                }
            },
        };

        $opts{version} = {
            getopt  => "version|v",
            usage   => "--version (or -v)",
            summary => "Show version",
            handler => sub {
                die "ERROR: 'url' not set, required for --version\n"
                    unless $self->url;
                unshift @{$self->{_actions}}, 'version';
                $self->{_check_required_args} = 0;
            },
        };

        $opts{help} = {
            getopt  => "help|h|?",
            usage   => "--help (or -h, -?)",
            summary => "Display this help message",
            handler => sub {
                unshift @{$self->{_actions}}, 'help';
                $self->{_check_required_args} = 0;
            },
            order   => 0, # high
        };

        $opts{format} = {
            getopt  => "format=s",
            summary => "Choose output format, e.g. json, text",
            handler => sub {
                $self->format_set(1);
                $self->format($_[1]);
            },
        };

        $opts{format_options} = {
            getopt  => "format-options=s",
            summary => "Pass options to formatter",
            handler => sub {
                $self->format_options_set(1);
                $self->format_options(__json_decode($_[1]));
            },
        };

        if ($self->subcommands) {
            $opts{list} = {
                getopt  => "list|l",
                usage   => "--list (or -l)",
                summary => "List available subcommands",
                handler => sub {
                    unshift @{$self->{_actions}}, 'list';
                    $self->{_check_required_args} = 0;
                },
            };
        }

        if (defined $self->default_subcommand) {
            # 'cmd=SUBCOMMAND_NAME' can be used to select other subcommands when
            # default_subcommand is in effect.
            $opts{cmd} = {
                getopt  => "cmd=s",
                handler => sub {
                    $self->{_selected_subcommand} = $_[1];
                },
            };
        }

        # convenience for Log::Any::App-using apps
        if ($self->log_any_app) {
            # since the cmdline opts is consumed, Log::Any::App doesn't see
            # this. we currently work around this via setting env.
            for my $o (qw/quiet verbose debug trace/) {
                $opts{$o} = {
                    getopt  => $o,
                    summary => "Set log level to $o",
                    handler => sub {
                        $ENV{uc $o} = 1;
                    },
                };
            }
            $opts{log_level} = {
                getopt  => "log-level=s",
                summary => "Set log level",
                handler => sub {
                    $ENV{LOG_LEVEL} = $_[1];
                },
            };
        }

        if ($self->undo) {
            $opts{history} = {
                category => 'Undo options',
                getopt  => 'history',
                summary => 'List actions history',
                handler => sub {
                    unshift @{$self->{_actions}}, 'history';
                    $self->{_check_required_args} = 0;
                },
            };
            $opts{clear_history} = {
                category => 'Undo options',
                getopt  => "clear-history",
                summary => 'Clear actions history',
                handler => sub {
                    unshift @{$self->{_actions}}, 'clear_history';
                    $self->{_check_required_args} = 0;
                },
            };
            $opts{undo} = {
                category => 'Undo options',
                getopt  => 'undo',
                summary => 'Undo previous action',
                handler => sub {
                    unshift @{$self->{_actions}}, 'undo';
                    #$self->{_tx_id} = $_[1];
                    $self->{_check_required_args} = 0;
                },
            };
            $opts{redo} = {
                category => 'Undo options',
                getopt  => 'redo',
                summary => 'Redo previous undone action',
                handler => sub {
                    unshift @{$self->{_actions}}, 'redo';
                    #$self->{_tx_id} = $_[1];
                    $self->{_check_required_args} = 0;
                },
            };
        }

        \%opts;
    },
);

sub __json_decode {
    require JSON;
    state $json = do { JSON->new->allow_nonref };
    $json->decode(shift);
}

sub BUILD {
    my ($self, $args) = @_;
    #$self->{indent} = $args->{indent} // "    ";
}

sub format_result {
    require Perinci::Result::Format;

    my $self = shift;
    my $res  = $self->{_res};
    return unless $res;

    my $resmeta = $res->[3] // {};
    unless ($resmeta->{"cmdline.display_result"} // 1) {
        $res->[2] = undef;
        return;
    }

    my $format = $self->format_set ?
        $self->format :
            $self->{_meta}{"x.perinci.cmdline.default_format"} // $self->format;
    die "ERROR: Unknown output format '$format', please choose one of: ".
        join(", ", sort keys(%Perinci::Result::Format::Formats))."\n"
            unless $Perinci::Result::Format::Formats{$format};
    if ($self->format_options_set) {
        $res->[3]{result_format_options} = $self->format_options;
        $resmeta = $res->[3];
    }

    if ($resmeta->{is_stream}) {
        $log->tracef("Result is a stream");
    } else {
        $log->tracef("Formatting output with %s", $format);
        $self->{_fres} = Perinci::Result::Format::format(
            $self->{_res}, $format);
    }
}

# format array item as row
sub format_row {
    require Data::Format::Pretty::Console;
    state $dfpc = Data::Format::Pretty::Console->new({interactive=>0});

    my ($self, $row) = @_;
    my $ref = ref($row);
    # we catch common cases to be faster (avoid dfpc's structure identification)
    if (!$ref) {
        # simple scalar
        return ($row // "") . "\n";
    } elsif ($ref eq 'ARRAY' && !(grep {ref($_)} @$row)) {
        # an array of scalars
        return join("\t", map { $dfpc->_format_cell($_) } @$row) . "\n";
    } else {
        # otherwise, just feed it to dfpc
        return $dfpc->_format($row);
    }
}

sub display_result {
    require File::Which;

    my $self = shift;

    my $res  = $self->{_res};
    return unless $res;

    my $resmeta = $res->[3] // {};

    # XXX allow programs to opt out from this
    binmode(STDOUT, ":utf8");

    my $handle;
    {
        if ($resmeta->{"cmdline.page_result"}) {
            my $pager = $resmeta->{"cmdline.pager"} //
                $ENV{PAGER};
            unless (defined $pager) {
                $pager = "less -FRSX" if File::Which::which("less");
            }
            unless (defined $pager) {
                $pager = "more" if File::Which::which("more");
            }
            unless (defined $pager) {
                die "Can't determine PAGER";
            }
            last unless $pager; # ENV{PAGER} can be set 0/'' to disable paging
            $log->tracef("Paging output using %s", $pager);
            open $handle, "| $pager";
        }
    }
    $handle //= \*STDOUT;

    if ($resmeta->{is_stream}) {
        die "Can't format stream as " . $self->format .
            ", please use --format text\n" unless $self->format =~ /^text/;
        my $r = $res->[2];
        if (ref($r) eq 'GLOB') {
            while (!eof($r)) {
                print $handle ~~<$r>;
            }
        } elsif (blessed($r) && $r->can('getline') && $r->can('eof')) {
            # IO::Handle-like object
            while (!$r->eof) {
                print $r->getline;
            }
        } elsif (ref($r) eq 'ARRAY') {
            # tied array
            while (~~(@$r) > 0) {
                print $self->format_row(shift(@$r));
            }
        } else {
            die "Invalid stream in result (not a glob/IO::Handle-like object/".
                "(tied) array)\n";
        }
    } else {
        print $handle $self->{_fres} // "";
    }
}

sub get_subcommand {
    my ($self, $name) = @_;

    my $scs = $self->subcommands;
    return undef unless $scs;

    if (reftype($scs) eq 'CODE') {
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
        if (reftype($scs) eq 'CODE') {
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

    if (!$self->subcommands) {
        say $self->loc("There are no subcommands") . ".";
        return 0;
    }

    my $subcommands = $self->list_subcommands;

    # XXX get summary from Riap if not exist, but this results in multiple Riap
    # requests.

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
            say $self->loc("List of available [_1] subcommands",
                           ucfirst($cat) || "main") . ":";
        } else {
            say $self->loc("List of available subcommands") . ":";
        }
        my $subc = $percat_subc{$cat};
        for my $scn (sort keys %$subc) {
            my $sc = $subc->{$scn};
            my $summary = $self->langprop($sc, "summary");
            say "  $scn", $summary ? " - $summary" : "";
        }
    }

    0;
}

sub run_version {
    my ($self) = @_;

    my $url = $self->{_subcommand} && $self->{_subcommand}{url} ?
        $self->{_subcommand}{url} : $self->url;
    my $res = $self->_pa->request(meta => $url);
    my $ver;
    if ($res->[0] == 200) {
        $ver = $res->[2]{entity_version} // "?";
    } else {
        $log->warnf("Can't request 'meta' action on %s: %d - %s",
                    $url, $res->[0], $res->[1]);
        $ver = '?';
    }

    say $self->loc("[_1] version [_2]", $self->program_name, $ver);
    {
        no strict 'refs';
        say "  ", $self->loc("[_1] version [_2]", "Perinci::CmdLine",
                             $Perinci::CmdLine::VERSION || "dev");
    }

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

        my $scn = $sc->{name} // "";

        # whether user typed 'blah blah ^' or 'blah blah^'
        my $space_typed = !defined($word);

        # e.g: spanel delete-account ^
        if ($self->subcommands && $cword > 0 && $space_typed) {
            $log->trace("do_arg because last word typed (+space) is ".
                            "subcommand name");
            $do_arg++; last;
        }

        # e.g: spanel delete-account --format=yaml --acc^
        if ($cword > 0 && !$space_typed && $word ne $scn) {
            $log->trace("do_arg because subcommand name has been typed ".
                            "in past words");
            $do_arg++; last;
        }

        $log->tracef("not do_arg, cword=%d, words=%s, scn=%s, space_typed=%s",
                     $cword, $words, $scn, $space_typed);
    }

    my @top_opts; # contain --help, -h, etc.
    for my $o (keys %{{@{ $self->{_go_specs_common} }}}) {
        $o =~ s/^--//;
        $o =~ s/=.+$//;
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

        # convert @getopts' ('help|h|?' => ..., ...) to ['--help', '-h', '-?',
        # ...]. XXX this should be moved to another module to remove
        # duplication, as Perinci::Sub::GetArgs::Argv also does something
        # similar.
        my $common_opts = [];
        for my $k (keys %{{@{ $self->{_go_specs_common} }}}) {
            $k =~ s/^--?//;
            $k =~ s/^([\w?-]+(?:\|[\w?-]+)*)(?:\W.*)?/$1/;
            for (split /\|/, $k) {
                push @$common_opts, (length == 1 ? "-$_" : "--$_");
            }
        }

        $res = Perinci::BashComplete::bash_complete_riap_func_arg(
            url=>$sc->{url}, words=>$words, cword=>$cword,
            common_opts => $common_opts,
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

sub before_generate_doc {
    my ($self) = @_;

    my $sc = $self->{_subcommand};
    my $url = $sc ? $sc->{url} : $self->url;
    if ($url) {
        my $res = $self->_pa->request(info => $url);
        die "ERROR: Can't info '$url': $res->[0] - $res->[1]\n"
            unless $res->[0] == 200;
        $self->{_info} = $res->[2];
        $res = $self->_pa->request(meta => $url);
        die "ERROR: Can't meta '$url': $res->[0] - $res->[1]\n"
            unless $res->[0] == 200;
        $self->{_meta} = $res->[2];
        $self->_add_common_opts_after_meta;
    }
}

# some common opts can be added only after we get the function metadata
sub _add_common_opts_after_meta {
    my $self = shift;

    if (risub($self->{_meta})->can_dry_run) {
        $self->common_opts->{dry_run} = {
            getopt  => 'dry-run',
            summary => "Run in simulation mode (also via DRY_RUN=1)",
            handler => sub {
                $self->{_dry_run} = 1;
                $ENV{VERBOSE} = 1;
            },
        };
    }

    # update the cached getopt specs
    my @go_opts = $self->_gen_go_specs_from_common_opts;
    $self->{_go_specs_common} = \@go_opts;
}

sub doc_parse_summary {
    my ($self) = @_;

    my $sc = $self->{_subcommand};

    $self->doc_parse->{name} = $self->program_name .
        ($sc && length($sc->{name}) ? " $sc->{name}" : "");

    if ($self->{_meta}) {
        $self->doc_parse->{summary} =
            $self->langprop($self->{_meta}, "summary");
    }
}

sub doc_gen_summary {
    my ($self) = @_;

    my $name_summary = join(
        "",
        $self->doc_parse->{name} // "",
        ($self->doc_parse->{name} && $self->doc_parse->{summary} ? ' - ' : ''),
        $self->doc_parse->{summary} // ""
    );

    $self->add_doc_lines($name_summary, "");
}

sub doc_parse_usage {}

sub doc_gen_usage {
    my ($self) = @_;

    my $co = $self->common_opts;
    my @con = sort {
        ($co->{$a}{order}//1) <=> ($co->{$b}{order}//1) || $a cmp $b
    } keys %$co;


    $self->{_common_opts} = \@con; # save for doc_gen_options
    $self->add_doc_lines($self->loc("Usage").":");
    my $pn = $self->program_name;
    for my $con (@con) {
        my $cov = $co->{$con};
        next unless $cov->{usage};
        $self->add_doc_lines("    $pn ".$self->locopt($cov->{usage}));
    }
    if ($self->subcommands) {
        if (defined $self->default_subcommand) {
            $self->add_doc_lines("    $pn ".$self->loc("--cmd=OTHER_SUBCOMMAND (options)"));
        } else {
            $self->add_doc_lines("    $pn ".$self->loc("SUBCOMMAND (options)"));
        }
    } else {
        $self->add_doc_lines("    $pn ".$self->loc("(options)"));
    }
    $self->add_doc_lines("");
}

sub doc_parse_options {}

sub doc_gen_options {
    require SHARYANTO::Getopt::Long::Util;

    my ($self) = @_;
    my $info = $self->{_info};
    my $meta = $self->{_meta};
    my $args_p = $meta->{args};
    my $sc = $self->subcommands;

    # stored gathered options by category, e.g. $catopts{"Common options"} (an
    # array containing options)
    my %catopts;

    my $t_opts = $self->loc("Options");
    my $t_copts = $self->loc("Common options");

    # gather common opts
    my $co = $self->common_opts;
    my @con = sort {
        ($co->{$a}{order}//1) <=> ($co->{$b}{order}//1) || $a cmp $b
    } keys %$co;
    for my $con (@con) {
        my $cov = $co->{$con};
        my $cat = $cov->{category} ? $self->locopt($cov->{category}) :
            ($sc ? $t_copts : $t_opts);
        my $go = $cov->{getopt};
        push @{ $catopts{$cat} }, {
            getopt=>SHARYANTO::Getopt::Long::Util::gospec2human($cov->{getopt}),
            summary=> $cov->{summary} ? $self->locopt($cov->{summary}) : "",
        };
    }

    # gather function opts (XXX: categorize according to tags)
    if ($info && $info->{type} eq 'function' && $args_p && %$args_p) {
        for my $an (sort {
            ($args_p->{$a}{pos} // 99) <=> ($args_p->{$b}{pos} // 99) ||
                $a cmp $b
            } keys %$args_p) {
            my $a = $args_p->{$an};
            my $s = $a->{schema} || [any=>{}];
            my $ane = $an; $ane =~ s/_/-/g; $ane =~ s/\W/-/g;
            $ane = "no$ane" if $s->[0] eq 'bool' && $s->[1]{default};
            for my $al0 (keys %{ $a->{cmdline_aliases} // {}}) {
                my $al = $al0; $al =~ s/_/-/g;
                $al = length($al) > 1 ? "--$al" : "-$al";
                $ane .= ", $al";
            }
            my $def = defined($s->[1]{default}) && $s->[0] ne 'bool' ?
                " (default: ".dump1($s->[1]{default}).")" : "";
            my $src = $a->{cmdline_src} // "";
            my $in;
            if ($s->[1]{in} && @{ $s->[1]{in} }) {
                $in = dump1($s->[1]{in});
            }
            push @{ $catopts{$t_opts} }, {
                getopt => "--$ane",
                getopt_note => sprintf(
                    "[%s]%s",
                    Perinci::ToUtil::sah2human_short($s),
                    join(
                        "",
                        (defined($a->{pos}) ? " (" .
                             $self->loc("or as argument #[_1]",
                                        ($a->{pos}+1).($a->{greedy} ? "+":"")).")":""),
                        ($src eq 'stdin' ?
                             " (" . $self->loc("or from stdin") . ")" : ""),
                        ($src eq 'stdin_or_files' ?
                             " (" . $self->loc("or from stdin/files") . ")" : ""),
                        $def
                    )),
                summary => $self->langprop($a, "summary"),
                description => $self->langprop($a, "description"),
                in => $in,
            };
        }
    }

    # output gathered options
    for my $cat (sort keys %catopts) {
        $self->add_doc_lines("$cat:\n", "");
        for my $o (@{ $catopts{$cat} }) {
            $self->inc_indent(1);
            $self->add_doc_lines($o->{getopt} . ($o->{getopt_note} ? " $o->{getopt_note}" : ""));
            if ($o->{in} || $o->{summary} || $o->{description}) {
                $self->inc_indent(2);
                $self->add_doc_lines(
                    "",
                    ucfirst($self->loc("value in")). ": $o->{in}")
                    if $o->{in};
                $self->add_doc_lines($o->{summary} . ".") if $o->{summary};
                $self->add_doc_lines("", $o->{description}) if $o->{description};
                $self->dec_indent(2);
                $self->add_doc_lines("");
            } else {
                $self->add_doc_lines("");
            }
            $self->dec_indent(1);
        }
    }

    #$self->add_doc_lines("");
}

sub doc_parse_description {
}

sub doc_gen_description {
}

sub doc_parse_examples {
}

sub doc_gen_examples {
}

sub doc_parse_links {
}

sub doc_gen_links {
}

my ($ph1, $ph2); # patch handles
sub _setup_progress_output {
    my $self = shift;

    if ($ENV{PROGRESS} // (-t STDOUT)) {
        require Progress::Any::Output;
        Progress::Any::Output->set("TermProgressBarColor");
        my $out = $Progress::Any::outputs{''}[0];
        # we need to patch the logger adapters so it won't interfere with
        # progress meter's output
        require Monkey::Patch::Action;
        $ph1 = Monkey::Patch::Action::patch_package(
            'Log::Log4perl::Appender::Screen', 'log',
            'replace', sub {
                my ($self, %params) = @_;

                my $msg = $params{message};
                $msg =~ s/\n//g;

                # clean currently displayed progress bar first
                if ($out->{lastlen}) {
                    print
                        "\b" x $out->{lastlen},
                            " " x $out->{lastlen},
                                "\b" x $out->{lastlen};
                    undef $out->{lastlen};
                }

                say $msg;
            },
        ) if defined &{"Log::Log4perl::Appender::Screen::log"};
        $ph2 = Monkey::Patch::Action::patch_package(
            'Log::Log4perl::Appender::ScreenColoredLevels', 'log',
            'replace', sub {
                my ($self, %params) = @_;
                # BEGIN copy-paste'ish from ScreenColoredLevels.pm
                my $msg = $params{message};
                $msg =~ s/\n//g;
                if (my $color=$self->{color}->{$params{log4p_level}}) {
                    $msg = Term::ANSIColor::colored($msg, $color);
                }
                # END copy-paste'ish

                # clean currently displayed progress bar first
                if ($out->{lastlen}) {
                    print
                        "\b" x $out->{lastlen},
                            " " x $out->{lastlen},
                                "\b" x $out->{lastlen};
                    undef $out->{lastlen};
                }

                # XXX duplicated code above, perhaps move this to
                # TermProgressBarColor's clean_bar() or something

                say $msg;
            }
        ) if defined &{"Log::Log4perl::Appender::ScreenColoredLevels::log"};
    }
}

sub run_help {
    my ($self) = @_;

    $self->{doc_sections} //= [
        'summary',
        'usage',
        'options',
        'description',
        'examples',
        'links',
    ];
    print $self->generate_doc();
    0;
}

sub run_subcommand {
    require File::Which;

    my ($self) = @_;
    my $sc = $self->{_subcommand};

    my %fargs = %{$self->{_args}};
    $fargs{-cmdline} = $self if $sc->{pass_cmdline_object} //
        $self->pass_cmdline_object;

    my $tx_id;

    my $using_tx = !$self->{_dry_run} && $self->undo && ($sc->{undo} // 1);

    if ($using_tx) {
        require UUID::Random;
        $tx_id = UUID::Random::generate();
        $tx_id =~ s/-.+//; # 32bit suffices for small number of txs
        my $summary = join(" ", @{ $self->{_orig_argv} });
        my $res = $self->_pa->request(
            begin_tx => "/", {tx_id=>$tx_id, summary=>$summary});
        if ($res->[0] != 200) {
            $self->{_res} = [$res->[0],
                             "Can't start transaction '$tx_id': $res->[1]"];
            return 1;
        }
    }

    # setup output progress indicator
    state $setup_progress;
    if ($self->{_meta}{features}{progress}) {
        unless ($setup_progress) {
            $self->_setup_progress_output;
            $setup_progress++;
        }
    }

    # call function
    $self->{_res} = $self->_pa->request(
        call => $self->{_subcommand}{url},
        {args=>\%fargs, tx_id=>$tx_id, dry_run=>$self->{_dry_run}});
    $log->tracef("call res=%s", $self->{_res});

    # commit transaction (if using tx)
    if ($using_tx && $self->{_res}[0] =~ /\A(?:200|304)\z/) {
        my $res = $self->_pa->request(commit_tx => "/", {tx_id=>$tx_id});
        if ($res->[0] != 200) {
            $self->{_res} = [$res->[0],
                             "Can't commit transaction '$tx_id': $res->[1]"];
            return 1;
        }
    }

    my $resmeta = $self->{_res}[3] // {};
    if (defined $resmeta->{"cmdline.exit_code"}) {
        return $resmeta->{"cmdline.exit_code"};
    } else {
        return $self->{_res}[0] =~ /\A(?:200|304)\z/ ?
            0 : $self->{_res}[0] - 300;
    }
}

sub run_history {
    my $self = shift;
    my $res = $self->_pa->request(list_txs => "/", {detail=>1});
    $log->tracef("list_txs res=%s", $res);
    return 1 unless $res->[0] == 200;
    $res->[2] = [sort {($b->{tx_commit_time}//0) <=> ($a->{tx_commit_time}//0)}
                     @{$res->[2]}];
    my @txs;
    for my $tx (@{$res->[2]}) {
        next unless $tx->{tx_status} =~ /[CUX]/;
        push @txs, {
            id          => $tx->{tx_id},
            start_time  => $tx->{tx_start_time},
            commit_time => $tx->{tx_commit_time},
            status      => $tx->{tx_status} eq 'X' ? 'error' :
                $tx->{tx_status} eq 'U' ? 'undone' : '',
            summary     => $tx->{tx_summary},
        };
    }
    $self->{_res} = [200, "OK", \@txs];
    0;
}

sub run_clear_history {
    my $self = shift;
    $self->{_res} = $self->_pa->request(discard_all_txs => "/");
    $self->{_res}[0] == 200 ? 0 : 1;
}

sub run_undo {
    my $self = shift;
    $self->{_res} = $self->_pa->request(undo => "/");
    $self->{_res}[0] == 200 ? 0 : 1;
}

sub run_redo {
    my $self = shift;
    $self->{_res} = $self->_pa->request(redo => "/");
    $self->{_res}[0] == 200 ? 0 : 1;
}

sub _gen_go_specs_from_common_opts {
    my $self = shift;

    my @go_opts;
    my $co = $self->common_opts;
    for my $con (sort {
        ($co->{$a}{order}//1) <=> ($co->{$b}{order}//1) || $a cmp $b
    } keys %$co) {
        my $cov = $co->{$con};
        die "Invalid common option '$con': empty getopt"
            unless $cov->{getopt};
        push @go_opts, $cov->{getopt} => $cov->{handler};
    }

    @go_opts;
}

sub parse_common_opts {
    require Getopt::Long;

    $log->tracef("-> parse_common_opts()");
    my ($self) = @_;

    my @orig_ARGV = @ARGV;
    $self->{_orig_argv} = \@orig_ARGV;

    my @go_opts = $self->_gen_go_specs_from_common_opts;
    $self->{_go_specs_common} = \@go_opts;
    my $old_go_opts = Getopt::Long::Configure(
        "pass_through", "no_ignore_case", "no_getopt_compat");
    Getopt::Long::GetOptions(@go_opts);
    $log->tracef("result of GetOptions for common options: remaining argv=%s, ".
                     "actions=%s", \@ARGV, $self->{_actions});
    Getopt::Long::Configure($old_go_opts);

    if ($self->{_force_subcommand}) {
        @ARGV = @orig_ARGV;
    }

    $log->tracef("<- parse_common_opts()");
}

sub parse_subcommand_opts {
    require Perinci::Sub::GetArgs::Argv;

    my ($self) = @_;
    my $sc = $self->{_subcommand};
    return unless $sc && $sc->{url};
    $log->tracef("-> parse_subcommand_opts()");

    my $res = $self->_pa->request(meta=>$sc->{url});
    unless ($res->[0] == 200) {
        $log->warnf("Can't get metadata from %s: %d - %s", $sc->{url},
                    $res->[0], $res->[1]);
        $self->{_args} = {};
        $log->tracef("<- parse_subcommand_opts() (bailed)");
        return;
    }
    my $meta = $res->[2];
    $self->{_meta} = $meta;
    $self->_add_common_opts_after_meta;

    # also set dry-run on environment
    do { $self->{_dry_run} = 1; $ENV{VERBOSE} = 1 } if $ENV{DRY_RUN};

    # parse argv
    $Perinci::Sub::GetArgs::Argv::_pa_skip_check_required_args = 1
        if $self->{_pa_skip_check_required_args};
    my $src_seen;
    my %ga_args = (
        argv                => \@ARGV,
        meta                => $meta,
        check_required_args => $self->{_check_required_args} // 1,
        allow_extra_elems   => 1,
        per_arg_json        => 1,
        per_arg_yaml        => 1,
        on_missing_required_args => sub {
            my %a = @_;
            my ($a, $aa, $as) = ($a{arg}, $a{args}, $a{spec});
            my $src = $as->{cmdline_src};
            if ($src) {
                die "ERROR: Invalid 'cmdline_src' value for argument '$a': ".
                    "$src\n" unless $src =~ /\A(stdin|stdin_or_files)\z/;
                die "ERROR: Sorry, argument '$a' is set cmdline_src=$src, ".
                    "but type is not 'str', only str is supported for now\n"
                        unless $as->{schema}[0] eq 'str';
                die "ERROR: Only one argument can be specified cmdline_src"
                    if $src_seen++;
                if ($src eq 'stdin') {
                    $log->trace("Getting argument '$a' value from stdin ...");
                    local $/;
                    $aa->{$a} = <STDIN>;
                } elsif ($src eq 'stdin_or_files') {
                    $log->trace("Getting argument '$a' value from ".
                                    "stdin_or_files ...");
                    local $/;
                    $aa->{$a} = <>;
                }
            }
        },
    );
    if ($self->{_force_subcommand}) {
        $ga_args{extra_getopts_before} = $self->{_go_specs_common};
    } else {
        $ga_args{extra_getopts_after}  = $self->{_go_specs_common};
    }
    $res = Perinci::Sub::GetArgs::Argv::get_args_from_argv(%ga_args);

    # We load Log::Any::App rather late here, to be able to customize level via
    # --debug, --dry-run, etc.
    unless ($ENV{COMP_LINE}) {
        $self->_load_log_any_app if
            $self->{_subcommand}{log_any_app} // $self->log_any_app;
    }

    die "ERROR: Failed parsing arguments: $res->[0] - $res->[1]\n"
        unless $res->[0] == 200;
    $self->{_args} = $res->[2];
    $log->tracef("result of GetArgs for subcommand: remaining argv=%s, args=%s".
                     ", actions=%s", \@ARGV, $self->{_args}, $self->{_actions});

    $log->tracef("<- _parse_subcommand_opts()");
}

# set $self->{_subcommand} for convenience, it can be taken from subcommands(),
# or, in the case of app with a single command, {name=>'', url=>$self->url()}.
sub _set_subcommand {
    my ($self) = @_;

    if ($self->subcommands) {
        my $scn;
        if (defined $self->{_selected_subcommand}) {
            $scn = $self->{_selected_subcommand};
        } elsif (defined $self->default_subcommand) {
            $scn = $self->default_subcommand;
        } elsif (@ARGV) {
            $scn = shift @ARGV;
            $self->{_scn_in_argv} = $scn;
            $scn =~ s/-/_/g if $self->dash_to_underscore;
        } else {
            goto L1;
        }
        my $sc = $self->get_subcommand($scn);
        unless ($sc) {
            if ($ENV{COMP_LINE}) {
                goto L1;
            } else {
                die "ERROR: Unknown subcommand '$scn', use '".
                    $self->program_name.
                        " -l' to list available subcommands\n";
            }
        }
        $self->{_subcommand} = $sc;
        $self->{_subcommand}{name} = $scn;
        if ($self->{_force_subcommand}) {
            unshift @{$self->{_actions}}, 'subcommand';
        } else {
            push @{$self->{_actions}}, 'subcommand';
        }
    } else {
        $self->{_subcommand} = {url=>$self->url, summary=>$self->summary};
        $self->{_subcommand}{name} = '';
        if ($self->{_force_subcommand}) {
            unshift @{$self->{_actions}}, 'subcommand';
        } else {
            push @{$self->{_actions}}, 'subcommand';
        }
    }
  L1:
    unshift @{$self->{_actions}}, 'completion' if $ENV{COMP_LINE};
    push @{$self->{_actions}}, 'help' if !@{$self->{_actions}};

    # unlogged, too early
    $log->tracef("actions=%s, subcommand=%s",
                 $self->{_actions}, $self->{_subcommand});
}

sub _load_log_any_app {
    my ($self) = @_;
    # Log::Any::App::init can already avoid being run twice, but we need to
    # check anyway to avoid logging starting message below twice.
    return if $self->{_log_any_app_loaded}++;
    require Log::Any::App;
    Log::Any::App::init();

    # we log this after we initialize Log::Any::App, since Log::Any::App might
    # not be loaded at all. yes, this means that this log message is printer
    # rather late and might not be the first message to be logged (see log
    # messages in run()) if user already loads Log::Any::App by herself.
    $self->{_original_argv} =
        $log->debugf("Program %s started with arguments: %s",
                     $0, $self->{_orig_argv});
}

sub run {
    my ($self) = @_;

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

    $self->{_actions} = []; # first action will be tried first, then 2nd, ...

    #
    # parse common opts first so we can catch --help, --list, etc.
    #

    $self->parse_common_opts;

    #
    # find out which subcommand to run, store it in $self->{_subcommand}
    #

    $self->_set_subcommand();

    #
    # parse subcommand options, this is to give change to function arguments
    # like --help to be parsed into $self->{_args}
    #

    $self->parse_subcommand_opts unless $ENV{COMP_LINE};

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
    $self->format_result;
    $self->display_result;

    $log->tracef("<- CmdLine's run(), exit code=%s", $exit_code);
    if ($self->exit) {
        $log->debugf("Program ending with exit code %d", $exit_code);
        exit $exit_code;
    } else {
        return $exit_code;
    }
}

1;
# ABSTRACT: Rinci/Riap-based command-line application framework

=for Pod::Coverage ^(BUILD|run_.+|doc_.+|before_.+|after_.+|format_result|format_row|display_result|get_subcommand|list_subcommands|parse_common_opts|parse_subcommand_opts|format_set|format_options|format_options_set)$

=head1 SYNOPSIS

In your command-line script:

 #!/usr/bin/perl
 use 5.010;
 use Log::Any '$log';
 use Perinci::CmdLine;

 our %SPEC;
 $SPEC{foo} = {
     v => 1.1,
     summary => 'Does foo to your computer',
     args => {
         bar => {
             summary=>'Barrr',
             req=>1,
             schema=>['str*', {in=>[qw/aa bb cc/]}],
         },
         baz => {
             summary=>'Bazzz',
             schema=>'str',
         },
     },
 };
 sub foo {
     my %args = @_;
     $log->debugf("Arguments are %s", \%args);
     [200, "OK", $args{bar} . ($args{baz} ? "and $args{baz}" : "")];
 }

 Perinci::CmdLine->new(url => '/main/foo')->run;

To run this program:

 % foo --help ;# display help message
 % LANG=id_ID foo --help ;# display help message in Indonesian
 % foo --version ;# display version
 % foo --bar aa ;# run function and display the result
 % foo --bar aa --debug ;# turn on debug output
 % foo --baz x  ;# fail because required argument 'bar' not specified

To do bash tab completion:

 % complete -C foo foo ;# can be put in ~/.bashrc
 % foo <tab> ;# completes to --help, --version, --bar, --baz and others
 % foo --b<tab> ;# completes to --bar and --baz
 % foo --bar <tab> ;# completes to aa, bb, cc

See also the L<peri-run> script which provides a command-line interface for
Perinci::CmdLine.


=head1 DESCRIPTION

Perinci::CmdLine is a command-line application framework. It parses command-line
options and dispatches to one of your specified Perl functions, passing the
command-line options and arguments to the function. It accesses functions via
L<Riap> protocol (using the L<Perinci::Access> Riap client library) so you can
access remote functions transparently. It utilizes L<Rinci> metadata in the code
so the amount of plumbing that you have to do is quite minimal. Basically most
of the time you just need to write your "business logic" in your function (along
with some metadata), and with a couple or several lines of script you have
created a command-line interface with the following features:

=over 4

=item * Command-line options parsing

Non-scalar arguments (array, hash, other nested) can also be passed as JSON or
YAML. For example, if the C<tags> argument is defined as 'array', then all of
below are equivalent:

 % mycmd --tags-yaml '[foo, bar, baz]'
 % mycmd --tags-yaml '["foo","bar","baz"]'
 % mycmd --tags foo --tags bar --tags baz

=item * Help message (utilizing information from metadata, supports translation)

 % mycmd --help
 % mycmd -h
 % mycmd -?

=item * Tab completion for bash (including completion from remote code)

 % complete -C mycmd mycmd
 % mycmd --he<tab> ; # --help
 % mycmd s<tab>    ; # sub1, sub2, sub3 (if those are the specified subcommands)
 % mycmd sub1 -<tab> ; # list the options available for sub1 subcommand

Support for other shell might be added in the future upon request.

=item * Undo/redo/history

If the function supports transaction (see L<Rinci::Transaction>,
L<Riap::Transaction>) the framework will setup transaction and provide command
to do undo (--undo) and redo (--redo) as well as seeing the undo/transaction
list (--history) and clearing the list (--clear-history).

=item * Version (--version, -v)

=item * List available subcommands (--list, -l)

=item * Configurable output format (--format, --format-options)

By default C<yaml>, C<json>, C<text>, C<text-simple>, C<text-pretty> are
recognized.

=back

Note that the each of the above command-line options (C<--help>, C<--version>,
etc) can be renamed or disabled.

This module uses L<Log::Any> and L<Log::Any::App> for logging. This module uses
L<Moo> for OO.


=head1 DISPATCHING

Below is the description of how the framework determines what action and which
function to call. (Currently lots of internal attributes are accessed directly,
this might be rectified in the future.)

B<Actions>. The C<_actions> attribute is an array which contains the list of
potential actions to choose, in order. It will then be filled out according to
the command-line options specified. For example, if C<--help> is specified,
C<help> action is shifted to the beginning of C<_actions>. Likewise for
C<--list>, etc. Finally, the C<subcommand> action (which means an action to call
our function) is added to this list. After we are finished filling out the
C<_actions> array, the first action is chosen by running a method called C<<
run_<ACTION> >>. For example if the chosen action is C<help>, C<run_help()> is
called. These C<run_*> methods must execute the action, display the output, and
return an exit code. Program will end with this exit code. A C<run_*> method can
choose to decline handling the action by returning undef, in which case the next
action will be tried, and so on until a defined exit code is returned.

B<The subcommand action and determining which subcommand (function) to call>.
The C<subcommand> action (implemented by C<run_subcommand()>) is the one that
actually does the real job, calling the function and displaying its result. The
C<_subcommand> attribute stores information on the subcommand to run, including
its Riap URL. If there are subcommands, e.g.:

 my $cmd = Perinci::CmdLine->new(
     subcommands => {
         sub1 => {
             url => '/MyApp/func1',
         },
         sub2 => {
             url => '/MyApp/func2',
         },
     },
 );

then which subcommand to run is determined by the command-line argument, e.g.:

 % myapp sub1 ...

then C<_subcommand> attribute will contain C<< {url=>'/MyApp/func1'} >>. When no
subcommand is specified on the command line, C<run_subcommand()> will decline
handling the action and returning undef, and the next action e.g. C<help> will
be executed. But if C<default_subcommand> attribute is set, C<run_subcommand()>
will run the default subcommand instead.

When there are no subcommands, e.g.:

 my $cmd = Perinci::CmdLine->new(url => '/MyApp/func');

C<_subcommand> will simply contain C<< {url=>'/MyApp/func'} >>.

C<run_subcommand()> will call the function specified in the C<url> in the
C<_subcommand> using C<Perinci::Access>. (Actually, C<run_help()> or
C<run_completion()> can be called instead, depending on which action to run.)


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
C<url>. It can also contain these keys: C<summary> (str, will be retrieved from
function metadata if unset), C<tags> (array of str, for categorizing
subcommands), C<log_any_app> (bool, whether to load Log::Any::App, default is
true, for subcommands that need fast startup you can try turning this off for
said subcommands), C<undo> (bool, can be set to 0 to disable transaction for
this subcommand; this is only relevant when C<undo> attribute is set to true),
C<pass_cmdline_object> (bool, to override C<pass_cmdline_object> attribute on a
per-subcommand basis).

Subcommands can also be a coderef, for dynamic list of subcommands. The coderef
will be called as a method with hash arguments. It can be called in two cases.
First, if called without argument C<name> (usually when doing --list) it must
return a hashref of subcommand specifications. If called with argument C<name>
it must return subcommand specification for subcommand with the requested name
only.

=head2 default_subcommand => NAME

If set, subcommand will always be set to this instead of from the first
argument. To use other subcommands, you will have to use --cmd option.

=head2 common_opts => HASH

A hash of common options, which are command-line options that are not associated
with any subcommand. Each option is itself a specification hash containing these
keys:

=over

=item * category (str)

Optional, for grouping options in help/usage message, defaults to C<Common
options>.

=item * getopt (str)

Required, for Getopt::Long specification.

=item * handler (code)

Required, for Getopt::Long specification.

=item * usage (str)

Optional, displayed in usage line in help/usage text.

=item * summary (str)

Optional, displayed in description of the option in help/usage text.

=item * order (int)

Optional, for ordering. Lower number means higher precedence, defaults to 1.

=back

A partial example from the default set by the framework:

 {
     help => {
         category    => 'Common options',
         getopt      => 'help|h|?',
         usage       => '%1 --help (or -h, -?)',
         handler     => sub { ... },
         order       => 0,
     },
     format => {
         category    => 'Common options',
         getopt      => 'format=s',
         summary     => 'Choose output format, e.g. json, text',
         handler     => sub { ... },
     },
     undo => {
         category => 'Undo options',
         getopt   => 'undo',
         ...
     },
     ...
 }

The default contains: help (getopt C<help|h|?>), version (getopt C<version|v>),
action (getopt C<action>), format (getopt C<format=s>), format_options (getopt
C<format-options=s>). If there are more than one subcommands, this will also be
added: list (getopt C<list|l>). If dry-run is supported by function, there will
also be: dry_run (getopt C<dry-run>). If undo is turned on, there will also be:
undo (getopt C<undo>), redo (getopt C<redo>), history (getopt C<history>),
clear_history (getopt C<clear-history>).

Sometimes you do not want some options, e.g. to remove C<format> and
C<format_options>:

 delete $cmd->common_opts->{format};
 delete $cmd->common_opts->{format_options};
 $cmd->run;

Sometimes you want to rename some command-line options, e.g. to change version
to use capital C<-V> instead of C<-v>:

 $cmd->common_opts->{version}{getopt} = 'version|V';

Sometimes you want to add subcommands as common options instead. For example:

 $cmd->common_opts->{halt} = {
     category    => 'Server options',
     getopt      => 'halt',
     summary     => 'Halt the server',
     handler     => sub {
         $cmd->{_selected_subcommand} = 'shutdown';
     },
 };

This will make:

 % cmd --halt

equivalent to executing the 'shutdown' subcommand:

 % cmd shutdown

=head2 exit => BOOL (default 1)

If set to 0, instead of exiting with exit(), run() will return the exit code
instead.

=head2 log_any_app => BOOL

Whether to load L<Log::Any::App>. Default is yes, or to look at LOG environment
variable. For faster startup, you might want to disable this or just use LOG=0
when running your scripts.

=head2 custom_completer => CODEREF

Will be passed to L<Perinci::BashComplete>'s C<bash_complete_riap_func_arg>. See
its documentation for more details.

=head2 custom_arg_completer => CODEREF | {ARGNAME=>CODEREF, ...}

Will be passed to L<Perinci::BashComplete>. See its documentation for more
details.

=head2 dash_to_underscore => BOOL (optional, default 1)

If set to 1, subcommand like C<a-b-c> will be converted to C<a_b_c>. This is for
convenience when typing in command line.

=head2 pass_cmdline_object => BOOL (optional, default 0)

Whether to pass special argument C<-cmdline> containing the Perinci::CmdLine
object to function. This can be overriden using the C<pass_cmdline_object> on a
per-subcommand basis.

Passing the cmdline object can be useful, e.g. to call run_help(), etc.

=head2 pa_args => HASH

Arguments to pass to L<Perinci::Access>. This is useful for passing e.g. HTTP
basic authentication to Riap client (L<Perinci::Access::HTTP::Client>):

 pa_args => {handler_args => {user=>$USER, password=>$PASS}}

=head2 undo => BOOL (optional, default 0)

Whether to enable undo/redo functionality. Some things to note if you intend to
use undo:

=over 4

=item * These common command-line options will be recognized

C<--undo>, C<--redo>, C<--history>, C<--clear-history>.

=item * Transactions will be used

C<< use_tx=>1 >> will be passed to L<Perinci::Access>, which will cause it to
initialize the transaction manager. Riap requests begin_tx and commit_tx will
enclose the call request to function.

=item * Called function will need to support transaction and undo

Function which does not meet qualifications will refuse to be called.

Exception is when subcommand is specified with C<< undo=>0 >>, where transaction
will not be used for that subcommand. For an example of disabling transaction
for some subcommands, see C<bin/u-trash> in the distribution.

=back

=head2 undo_dir => STR (optional, default ~/.<program_name>/.undo)

Where to put undo data. This is actually the transaction manager's data dir.


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


=head1 COMMAND-LINE OPTION/ARGUMENT PARSING

This section describes how Perinci::CmdLine parses command-line
options/arguments into function arguments. Command-line option parsing is
implemented by L<Perinci::Sub::GetArgs::Argv>.

For boolean function arguments, use C<--arg> to set C<arg> to true (1), and
C<--noarg> to set C<arg> to false (0). A flag argument (C<< [bool => {is=>1}]
>>) only recognizes C<--arg> and not C<--noarg>. For single letter arguments,
only C<-X> is recognized, not C<--X> nor C<--noX>.

For string and number function arguments, use C<--arg VALUE> or C<--arg=VALUE>
(or C<-X VALUE> for single letter arguments) to set argument value. Other scalar
arguments use the same way, except that some parsing will be done (e.g. for date
type, --arg 1343920342 or --arg '2012-07-31' can be used to set a date value,
which will be a DateTime object.) (Note that date parsing will be done by
L<Data::Sah> and currently not implemented yet.)

For arguments with type array of scalar, a series of C<--arg VALUE> is accepted,
a la L<Getopt::Long>:

 --tags tag1 --tags tag2 ; # will result in tags => ['tag1', 'tag2']

For other non-scalar arguments, also use C<--arg VALUE> or C<--arg=VALUE>, but
VALUE will be attempted to be parsed using JSON, and then YAML. This is
convenient for common cases:

 --aoa  '[[1],[2],[3]]'  # parsed as JSON
 --hash '{a: 1, b: 2}'   # parsed as YAML

For explicit JSON parsing, all arguments can also be set via --ARG-json. This
can be used to input undefined value in scalars, or setting array value without
using repetitive C<--arg VALUE>:

 --str-json 'null'    # set undef value
 --ary-json '[1,2,3]' # set array value without doing --ary 1 --ary 2 --ary 3
 --ary-json '[]'      # set empty array value

Likewise for explicit YAML parsing:

 --str-yaml '~'       # set undef value
 --ary-yaml '[a, b]'  # set array value without doing --ary a --ary b
 --ary-yaml '[]'      # set empty array value


=head1 BASH COMPLETION

To do bash completion, first create your script, e.g. C<myscript>, that uses
Perinci::CmdLine:

 #!/usr/bin/perl
 use Perinci::CmdLine;
 Perinci::CmdLine->new(...)->run;

then execute this in C<bash> (or put it in bash startup files like
C</etc/bash.bashrc> or C<~/.bashrc> for future sessions):

 % complete -C myscript myscript; # myscript must be in PATH


=head1 PROGRESS INDICATOR

For functions that express that they do progress updating (by setting their
C<progress> feature to true), Perinci::CmdLine will setup an output, currently
either L<Progress::Any::Output::TermProgressBar> if program runs interactively,
or L<Progress::Any::Output::LogAny> if program doesn't run interactively.


=head1 METADATA PROPERTY ATTRIBUTE

This module observes the following Rinci metadata property attributes:

=head2 x.perinci.cmdline.default_format => STR

Set default output format (if user does not specify via --format command-line
option).


=head1 RESULT METADATA

This module interprets the following result metadata keys:

=head2 is_stream => BOOL

XXX should perhaps be defined as standard in L<Rinci::function>.

If set to 1, signify that result is a stream. Result must be a glob, or an
object that responds to getline() and eof() (like a Perl L<IO::Handle> object),
or an array/tied array. Format must currently be C<text> (streaming YAML might
be supported in the future). Items of result will be displayed to output as soon
as it is retrieved, and unlike non-streams, it can be infinite.

An example function:

 $SPEC{cat_file} = { ... };
 sub cat_file {
     my %args = @_;
     open my($fh), "<", $args{path} or return [500, "Can't open file: $!"];
     [200, "OK", $fh, {is_stream=>1}];
 }

another example:

 use Tie::Simple;
 $SPEC{uc_file} = { ... };
 sub uc_file {
     my %args = @_;
     open my($fh), "<", $args{path} or return [500, "Can't open file: $!"];
     my @ary;
     tie @ary, "Tie::Simple", undef,
         SHIFT     => sub { eof($fh) ? undef : uc(~~<$fh> // "") },
         FETCHSIZE => sub { eof($fh) ? 0 : 1 };
     [200, "OK", \@ary, {is_stream=>1}];
 }

See also L<Data::Unixish> and L<App::dux> which deals with streams.

=head2 cmdline.display_result => BOOL

If you don't want to display function output (for example, function output is a
detailed data structure which might not be important for end users), you can set
C<cmdline.display_result> result metadata to false. Example:

 $SPEC{foo} = { ... };
 sub foo {
     ...
     [200, "OK", $data, {"cmdline.display_result"=>0}];
 }

=head2 cmdline.page_result => BOOL

If you want to filter the result through pager (currently defaults to
C<$ENV{PAGER}> or C<less -FRSX>), you can set C<cmdline.page_result> in result
metadata to true.

For example:

 $SPEC{doc} = { ... };
 sub doc {
     ...
     [200, "OK", $doc, {"cmdline.page_result"=>1}];
 }

=head2 cmdline.pager => STR

Instruct Perinci::CmdLine to use specified pager instead of C<$ENV{PAGER}> or
the default C<less> or C<more>.

=head2 cmdline.exit_code => INT

Instruct Perinci::CmdLine to use this exit code, instead of using (function
status - 300).


=head1 ENVIRONMENT

=over

=item * PERINCI_CMDLINE_PROGRAM_NAME => STR

Can be used to set CLI program name.

=item * PROGRESS => BOOL

Explicitly turn the progress bar on/off.

=item * PAGER => STR

Like in other programs, can be set to select the pager program (when
C<cmdline.page_result> result metadata is active). Can also be set to C<''> or
C<0> to explicitly disable paging even though C<cmd.page_result> result metadata
is active.

=back


=head1 FAQ

=head2 How does Perinci::CmdLine compare with other CLI-app frameworks?

The main difference is that Perinci::CmdLine accesses your code through L<Riap>
protocol, not directly. This means that aside from local Perl code,
Perinci::CmdLine can also provide CLI for code in remote hosts/languages. For a
very rough demo, download and run this PHP Riap::TCP server
https://github.com/sharyanto/php-Phinci/blob/master/demo/phi-tcpserve-terbilang.php
on your system. After that, try running:

 % peri-run riap+tcp://localhost:9090/terbilang --help
 % peri-run riap+tcp://localhost:9090/terbilang 1234

Everything from help message, calling, argument checking, tab completion works
for remote code as well as local Perl code.

=head2 How to add support for new output format (e.g. XML, HTML)?

See L<Perinci::Result::Format>.

=head2 My function has argument named 'format', but it is blocked by common option --format!

To add/remove/rename common options, see the documentation on C<common_opts>
attribute. In this case, you want:

 delete $cmd->common_opts->{format};
 #delete $cmd->common_opts->{format_options}; # you might also want this

or perhaps rename it:

 $cmd->common_opts->{output_format} = $cmd->common_opts->{format};
 delete $cmd->common_opts->{format};

=head2 How to accept input from STDIN (or files)?

If you specify 'cmdline_src' to 'stdin' to a 'str' argument, the argument's
value will be retrieved from standard input if not specified. Example:

 use Perinci::CmdLine;
 $SPEC{cmd} = {
     v => 1.1,
     args => {
         arg => {
             schema => 'str*',
             cmdline_src => 'stdin',
         },
     },
 };
 sub cmd {
     my %args = @_;
     [200, "OK", "arg is '$args{arg}'"];
 }
 Perinci::CmdLine->new(url=>'/main/cmd')->run;

When run from command line:

 % cat file.txt
 This is content of file.txt
 % cat file.txt | cmd
 arg is 'This is content of file.txt'

=head2 But I don't want it slurped into a single scalar, I want streaming!

See L<App::dux> for an example on how to accomplish that. Basically in App::dux,
you feed an array tied with L<Tie::Diamond> as a function argument. Thus you can
get lines from file/STDIN iteratively with each().

=head2 My application is OO?

This framework is currently non-OO and function-centric. There are already
several OO-based command-line frameworks on CPAN.


=head1 SEE ALSO

L<Perinci>, L<Rinci>, L<Riap>.

Other CPAN modules to write command-line applications: L<App::Cmd>, L<App::Rad>,
L<MooseX::Getopt>.

=cut
