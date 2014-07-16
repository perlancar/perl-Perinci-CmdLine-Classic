package Perinci::CmdLine;

# DATE
# VERSION

use 5.010001;
#use strict; # enabled by Moo
#use warnings; # enabled by Moo
use Log::Any '$log';

use Data::Dump::OneLine qw(dump1);
use Moo;
use experimental 'smartmatch'; # must be after Moo
use Locale::TextDomain::UTF8 'Perinci-CmdLine';
use Perinci::Object;
use Perinci::ToUtil;
use Scalar::Util qw(reftype blessed);

our $REQ_VERSION = 0; # version requested by user

extends 'Perinci::CmdLine::Base';

with 'SHARYANTO::Role::ColorTheme' unless $ENV{COMP_LINE};
#with 'SHARYANTO::Role::TermAttrs' unless $ENV{COMP_LINE}; already loaded by ColorTheme

has log_any_app => (is => 'rw', default=>sub{1});
has custom_completer => (is => 'rw');
has custom_arg_completer => (is => 'rw');
has undo => (is=>'rw', default=>sub{0});
has undo_dir => (
    is => 'rw',
    lazy => 1,
    default => sub {
        require File::HomeDir;

        my $self = shift;
        my $dir = File::HomeDir->my_home . "/." . $self->program_name;
        mkdir $dir unless -d $dir;
        $dir .= "/.undo";
        mkdir $dir unless -d $dir;
        $dir;
    }
);
has pa_args => (is => 'rw');
has _pa => (
    is => 'rw',
    lazy => 1,
    default => sub {
        my $self = shift;

        require Perinci::Access;
        require Perinci::Access::Perl;
        require Perinci::Access::Schemeless;
        my %args = %{$self->pa_args // {}};
        my %opts;
        if ($self->undo) {
            $opts{use_tx} = 1;
            $opts{custom_tx_manager} = sub {
                my $pa = shift;
                require Perinci::Tx::Manager;
                state $txm = Perinci::Tx::Manager->new(
                    data_dir => $self->undo_dir,
                    pa => $pa,
                );
                $txm;
            };
        }
        $args{handlers} = {
            pl => Perinci::Access::Perl->new(%opts),
            '' => Perinci::Access::Schemeless->new(%opts),
        };
        #$log->tracef("Creating Perinci::Access object with args: %s", \%args);
        Perinci::Access->new(%args);
    }
);

sub VERSION {
    my ($pkg, $req) = @_;
    $REQ_VERSION = $req;
}

sub __json_decode {
    require JSON;
    state $json = do { JSON->new->allow_nonref };
    $json->decode(shift);
}

sub err {
    my ($self, $msg) = @_;
    my $msg = shift; $msg .= "\n" unless $msg =~ /\n\z/;
     $self->_color('error_label', "ERROR: ") . $msg;
}

sub BUILD {
    require Perinci::Result::Format;

    my ($self, $args) = @_;

    if (!$self->{actions}) {
        $self->{actions} = {
            version => {
                default_log => 0,
                use_utf8 => 1,
            },
            help => {
                default_log => 0,
                use_utf8 => 1,
            },
            subcommands => {
                default_log => 0,
                use_utf8 => 1,
            },
            call => {},

            history => {},
            clear_history => {},
            redo => {},
            undo => {},
        };
    }

    if (!$self->{common_opts}) {
        $opts{version} = {
            getopt  => "version|v",
            usage   => N__("--version (or -v)"),
            summary => N__("Show version"),
            show_in_options => sub { $ENV{VERBOSE} },
            handler => sub {
                die "'url' not set, required for --version"
                    unless $self->url;
                $self->select_subcommand('version');
            },
        };

        $opts{help} = {
            getopt  => "help|h|?",
            usage   => N__("--help (or -h, -?) [--verbose]"),
            summary => N__("Display this help message"),
            show_in_options => sub { $ENV{VERBOSE} },
            handler => sub {
                $self->select_subcommand('help');
            },
            order   => 0, # high
        };

        $opts{format} = {
            getopt  => "format=s",
            summary => N__("Choose output format, e.g. json, text"),
            handler => sub {
                $self->selected_format($_[1]);
            },
        };

        if ($REQ_VERSION >= 1.04) {
            $opts{json} = {
                getopt  => "json",
                summary => N__("Equivalent to --format=json-pretty"),
                handler => sub {
                    $self->selected_format('json-pretty');
                },
            };
            $opts{yaml} = {
                getopt  => "yaml",
                summary => N__("Equivalent to --format=yaml"),
                handler => sub {
                    $self->selected_format('yaml');
                },
            };
            $opts{perl} = {
                getopt  => "perl",
                summary => N__("Equivalent to --format=perl"),
                handler => sub {
                    $self->selected_format('perl');
                },
            };
        }

        $opts{format_options} = {
            getopt  => "format-options=s",
            summary => N__("Pass options to formatter"),
            handler => sub {
                $self->format_options(__json_decode($_[1]));
            },
        };

        if ($self->subcommands) {
            $opts{subcommands} = {
                getopt  => "subcommands",
                usage   => N__("--subcommands"),
                show_in_usage => sub {
                    !$self->{selected_subcommand_name};
                },
                show_in_options => sub {
                    $ENV{VERBOSE} && !$self->{selected_subcommand_name};
                },
                show_usage_in_help => sub {
                    my $self = shift;
                },
                summary => N__("List available subcommands"),
                show_in_help => 0,
                handler => sub {
                    $self->select_subcommand('subcommands');
                },
            };
        }

        if (defined $self->default_subcommand) {
            # 'cmd=SUBCOMMAND_NAME' can be used to select other subcommands when
            # default_subcommand is in effect.
            $opts{cmd} = {
                getopt  => "cmd=s",
                handler => sub {
                    $self->select_subcommand($_[1]);
                },
            };
        }

        # convenience for Log::Any::App-using apps
        if ($self->log_any_app) {
            # since the cmdline opts is consumed, Log::Any::App doesn't see
            # this. we currently work around this via setting env.

            $opts{quiet} = {
                getopt  => "quiet",
                summary => N__("Set log level to quiet"),
                handler => sub {
                    $ENV{QUIET} = 1;
                },
            };
            $opts{verbose} = {
                getopt  => "verbose",
                summary => N__("Set log level to verbose"),
                handler => sub {
                    $ENV{VERBOSE} = 1;
                },
            };
            $opts{debug} = {
                getopt  => "debug",
                summary => N__("Set log level to debug"),
                handler => sub {
                    $ENV{DEBUG} = 1;
                },
            };
            $opts{trace} = {
                getopt  => "trace",
                summary => N__("Set log level to trace"),
                handler => sub {
                    $ENV{TRACE} = 1;
                },
            };

            $opts{log_level} = {
                getopt  => "log-level=s",
                summary => N__("Set log level"),
                handler => sub {
                    $ENV{LOG_LEVEL} = $_[1];
                },
            };
        }

        if ($self->undo) {
            $opts{history} = {
                category => 'Undo options',
                getopt  => 'history',
                summary => N__('List actions history'),
                handler => sub {
                    $self->r->{action} = 'history';
                },
            };
            $opts{clear_history} = {
                category => 'Undo options',
                getopt  => "clear-history",
                summary => N__('Clear actions history'),
                handler => sub {
                    $self->r->{action} = 'clear_history';
                },
            };
            $opts{undo} = {
                category => 'Undo options',
                getopt  => 'undo',
                summary => N__('Undo previous action'),
                handler => sub {
                    $self->r->{action} = 'undo';
                },
            };
            $opts{redo} = {
                category => 'Undo options',
                getopt  => 'redo',
                summary => N__('Redo previous undone action'),
                handler => sub {
                    $self->r->{action} = 'redo';
                },
            };
        }
        $self->{common_opts} = \%opts;
    }

    unless ($ENV{COMP_LINE}) {
        my $ct = $self->{color_theme} // $ENV{PERINCI_CMDLINE_COLOR_THEME};
        if (!$ct) {
            if ($self->use_color) {
                my $bg = $self->detect_terminal->{default_bgcolor} // '';
                $ct = 'Default::default' .
                    ($bg eq 'ffffff' ? '_whitebg' : '');
            } else {
                $ct = 'Default::no_color';
            }
        }
        $self->color_theme($ct);
    }

    # available formats
    $self->{formats} //= [keys %Perinci::Result::Format::Formats];
}

sub hook_format_result {
    require Perinci::Result::Format;

    my ($self, $r) = @_;

    my $res    = $r->{res};
    my $format = $r->{format};
    my $meta   = $r->{meta};

    # default
    $r->{fres} = '';

    my $resmeta = $res->[3] // {};
    unless ($resmeta->{"cmdline.display_result"} // 1) {
        $res->[2] = undef;
        return;
    }

    $format //= $meta->{"x.perinci.cmdline.default_format"};
    $format //= 'text';

    unless ($format ~~ @{ $self->formats }) {
        warn "Unknown output format '$format'";
        $format = 'text';
    }

    $resmeta->{result_format_options} = $self->format_options
        if $self->format_options;

    if ($resmeta->{is_stream}) {
        $log->tracef("Result is a stream");
        return undef;
    } else {
        $log->tracef("Formatting output with %s", $format);
        $r->{fres} = Perinci::Result::Format::format($res, $format);
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

    my ($self, $r) = @_;

    my $res  = $r->{res};
    my $fres = $r->{fres};
    my $resmeta = $res->[3] // {};

    # determine whether to binmode(STDOUT,":utf8")
    my $utf8 = $ENV{UTF8};
    if (!defined($utf8)) {
        my $am = $self->action_metadata->{$action};
        $utf8 //= $am->{use_utf8};
    }
    if (!defined($utf8) && $self->{_subcommand}) {
        $utf8 //= $self->{_subcommand}{use_utf8};
    }
    $utf8 //= $self->use_utf8;
    if ($utf8) {
        binmode(STDOUT, ":utf8");
    }

    # determine filehandle to output to (normally STDOUT, but we can also send
    # to a pager
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
                $self->_err("Can't determine PAGER");
            }
            last unless $pager; # ENV{PAGER} can be set 0/'' to disable paging
            $log->tracef("Paging output using %s", $pager);
            open $handle, "| $pager";
        }
    }
    $handle //= \*STDOUT;

    if ($resmeta->{is_stream}) {
        $self->_err("Can't format stream as " . $self->format .
                        ", please use --format text")
            unless $self->format =~ /^text/;
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
            $self->_err("Invalid stream in result (not a glob/IO::Handle-like ".
                            "object/(tied) array)\n");
        }
    } else {
        print $handle $fres;
    }
}

sub run_subcommands {
    my ($self) = @_;

    if (!$self->subcommands) {
        say __("There are no subcommands") . ".";
        return 0;
    }

    my $subcommands = $self->list_subcommands;

    # XXX get summary from Riap if not exist, but this results in multiple Riap
    # requests.

    my %percat_subc; # (cat1 => {subcmd1=>..., ...}, ...)
    while (my ($scn, $sc) = each %$subcommands) {
        my $cat = "";
        for my $tag (@{$sc->{tags} // []}) {
            my $tn = ref($tag) ? $tag->{name} : $tag;
            next unless $tn =~ /^category:(.+)/;
            $cat = $1;
            last;
        }
        $percat_subc{$cat}       //= {};
        $percat_subc{$cat}{$scn}   = $sc;
    }
    my $has_many_cats = scalar(keys %percat_subc) > 1;

    my $i = 0;
    for my $cat (sort keys %percat_subc) {
        if ($has_many_cats) {
            $self->_help_add_heading(
                __x("{category} subcommands",
                    category => ucfirst($cat || __("main"))));
        }
        my $subc = $percat_subc{$cat};
        for my $scn (sort keys %$subc) {
            my $sc = $subc->{$scn};
            my $summary = rimeta($sc)->langprop("summary");
            $self->_help_add_row(
                [$self->_color('program_name', $scn), $summary],
                {column_widths=>[-17, -40], indent=>1});
        }
    }
    $self->_help_draw_curtbl;

    0;
}

sub run_version {
    my ($self) = @_;

    my $url = $self->{_subcommand} && $self->{_subcommand}{url} ?
        $self->{_subcommand}{url} : $self->url;
    my $res = $self->_pa->request(meta => $url);
    my ($ver, $date);
    if ($res->[0] == 200) {
        $ver = $res->[2]{entity_v} // "?";
        $date = $res->[2]{entity_date};
    } else {
        $log->warnf("Can't request 'meta' action on %s: %d - %s",
                    $url, $res->[0], $res->[1]);
        $ver = '?';
        $date = undef;
    }

    say __x(
        "{program} version {version}",
        program => $self->_color('program_name',
                                 $self->_program_and_subcommand_name),
        version => $self->_color('emphasis', $ver)) .
            ($date ? " ($date)" : "");
    {
        no strict 'refs';
        say "  ", __x(
            "{program} version {version}",
            program => $self->_color('emphasis', "Perinci::CmdLine"),
            version => $self->_color('emphasis',
                                     $Perinci::CmdLine::VERSION || "dev"))
            . ($Perinci::CmdLine::DATE ? " ($Perinci::CmdLine::DATE)" : "");
    }

    0;
}

sub run_completion {
    require Complete::Bash;
    require Complete::Util;
    require Perinci::Sub::Complete;

    my ($self) = @_;

    my $sc = $self->{_subcommand};
    my $word  = $ARGV[$self->{_comp_cword}] // "";

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
                     $self->{_comp_cword}, \@ARGV, $scn, $space_typed);
    }

    my $res;
    if ($do_arg) {
        $log->trace("Completing subcommand argument names & values ...");

        my $rres = $self->_pa->request(meta => $sc->{url});
        if ($rres->[0] != 200) {
            $log->debugf("Can't get meta for completion: %s", $rres);
            $res = [];
            goto DISPLAY_RES;
        }
        my $meta = $rres->[2];
        $self->_add_common_opts_after_meta;

        $res = Perinci::Sub::Complete::complete_cli_arg(
            meta=>$meta, words=>\@ARGV, cword=>$self->{_comp_cword},
            common_opts => $common_opts,
            riap_server_url => $sc->{url},
            riap_uri        => undef,
            riap_client     => $self->_pa,
            custom_completer     => $self->custom_completer,
            custom_arg_completer => $self->custom_arg_completer,
        );

    } else {
        $log->trace("Completing top-level options + subcommand name ...");
        my @ary;
        push @ary, @top_opts;
        my $scs = $self->list_subcommands;
        push @ary, keys %$scs;
        $res = {
            completion => Complete::Util::complete_array_elem(
                word=>$word, array=>\@ary,
            ),
            type=>'option',
        };
    }

  DISPLAY_RES:
    my $compres = Complete::Bash::format_completion($res);
    if ($ENV{PERINCI_CMDLINE_SERVER}) {
        # a flag that causes completion result to be stored in the object
        # instead of printed. this is because we want to return result in a
        # function
        $self->{_compres} = $compres;
    } else {
        print $compres;
    }
    0;
}

sub get_meta {
    my ($self, $meta) = @_;

    my $res = $self->_pa->request(meta => $self->selected_subcommand->{url});
    die $res unless $res->[0] == 200;
    my $meta = $res->[2];

    if (risub($meta)->can_dry_run) {
        $self->common_opts->{dry_run} = {
            getopt  => 'dry-run',
            summary => N__("Run in simulation mode (also via DRY_RUN=1)"),
            handler => sub {
                $self->dry_run(1);
                $ENV{VERBOSE} = 1;
            },
        };
    }

    $meta;
}

sub _help_draw_curtbl {
    my $self = shift;

    if ($self->{_help_curtbl}) {
        print $self->{_help_curtbl}->draw;
        undef $self->{_help_curtbl};
    }
}

# ansitables are used to draw formatted help. they are 100% wide, with no
# borders (except space), but you can customize the number of columns (which
# will be divided equally)
sub _help_add_table {
    require Text::ANSITable;

    my ($self, %args) = @_;
    my $columns = $args{columns} // 1;

    $self->_help_draw_curtbl;
    my $t = Text::ANSITable->new;
    $t->border_style('Default::spacei_ascii');
    $t->cell_pad(0);
    if ($args{column_widths}) {
        for (0..$columns-1) {
            $t->set_column_style($_, width => $args{column_widths}[$_]);
        }
    } else {
        my $tw = $self->term_width;
        my $cw = int($tw/$columns)-1;
        $t->cell_width($cw);
    }
    $t->show_header(0);
    $t->column_wrap(0); # we'll do our own wrapping, before indent
    $t->columns([0..$columns-1]);

    $self->{_help_curtbl} = $t;
}

sub _help_add_row {
    my ($self, $row, $args) = @_;
    $args //= {};
    my $wrap    = $args->{wrap}   // 0;
    my $indent  = $args->{indent} // 0;
    my $columns = @$row;

    # start a new table if necessary
    $self->_help_add_table(
        columns=>$columns, column_widths=>$args->{column_widths})
        if !$self->{_help_curtbl} ||
            $columns != @{ $self->{_help_curtbl}{columns} };

    my $t = $self->{_help_curtbl};
    my $rownum = @{ $t->{rows} };

    $t->add_row($row);

    for (0..@{$t->{columns}}-1) {
        my %styles = (formats=>[]);
        push @{ $styles{formats} },
            [wrap=>{ansi=>1, mb=>1, width=>$t->{cell_width}-$indent*2}]
                if $wrap;
        push @{ $styles{formats} }, [lins=>{text=>"  " x $indent}]
            if $indent && $_ == 0;
        $t->set_cell_style($rownum, $_, \%styles);
    }
}

sub _help_add_heading {
    my ($self, $heading) = @_;
    $self->_help_add_row([$self->_color('heading', $heading)]);
}

sub _color {
    my ($self, $color_name, $text) = @_;
    my $color_code = $color_name ?
        $self->get_theme_color_as_ansi($color_name) : "";
    my $reset_code = $color_code ? "\e[0m" : "";
    "$color_code$text$reset_code";
}

sub help_section_summary {
    my ($self, %opts) = @_;

    my $summary = rimeta($self->{_help_meta})->langprop("summary");
    return unless $summary;

    my $name = $self->_program_and_subcommand_name;
    my $ct = join(
        "",
        $self->_color('program_name', $name),
        ($name && $summary ? ' - ' : ''),
        $summary // "",
    );
    $self->_help_add_row([$ct], {wrap=>1});
}

sub _usage_args {
    my $self = shift;

    my $m = $self->{_help_meta};
    return "" unless $m;
    my $aa = $m->{args};
    return "" unless $aa;

    # arguments with pos defined
    my @a = sort { $aa->{$a}{pos} <=> $aa->{$b}{pos} }
        grep { defined($aa->{$_}{pos}) } keys %$aa;
    my $res = "";
    for (@a) {
        $res .= " ";
        my $label = lc($_);
        $res .= $aa->{$_}{req} ? "<$label>" : "[$label]";
        $res .= " ..." if $aa->{$_}{greedy};
        last if $aa->{$_}{greedy};
    }
    $res;
}

sub help_section_usage {
    my ($self, %opts) = @_;

    my $co = $self->common_opts;
    my @con = grep {
        my $cov = $co->{$_};
        my $show = $cov->{show_in_usage} // 1;
        for ($show) { if (ref($_) eq 'CODE') { $_ = $_->($self) } }
        $show;
    } sort {
        ($co->{$a}{order}//1) <=> ($co->{$b}{order}//1) || $a cmp $b
    } keys %$co;

    my $pn = $self->_color('program_name', $self->_program_and_subcommand_name);
    my $ct = "";
    for my $con (@con) {
        my $cov = $co->{$con};
        next unless $cov->{usage};
        $ct .= ($ct ? "\n" : "") . $pn . " " . __($cov->{usage});
    }
    if ($self->subcommands && !$self->{_subcommand}) {
        if (defined $self->default_subcommand) {
            $ct .= ($ct ? "\n" : "") . $pn .
                " " . __("--cmd=<other-subcommand> [options]");
        } else {
            $ct .= ($ct ? "\n" : "") . $pn .
                " " . __("<subcommand> [options]");
        }
    } else {
            $ct .= ($ct ? "\n" : "") . $pn .
                " " . __("[options]"). $self->_usage_args;
    }
    $self->_help_add_heading(__("Usage"));
    $self->_help_add_row([$ct], {indent=>1});
}

sub help_section_options {
    require Getopt::Long::Util;

    my ($self, %opts) = @_;
    my $verbose = $opts{verbose};
    my $info = $self->{_help_info};
    my $meta = $self->{_help_meta};
    my $args_p = $meta->{args};
    my $sc = $self->subcommands;

    # stored gathered options by category, e.g. $catopts{"Common options"} (an
    # array containing options)
    my %catopts;

    my $t_opts = __("Options");
    my $t_copts = __("Common options");

    # gather common opts
    my $co = $self->common_opts;
    my @con = grep {
        my $cov = $co->{$_};
        my $show = $cov->{show_in_options} // 1;
        for ($show) { if (ref($_) eq 'CODE') { $_ = $_->($self) } }
        $show;
    } sort {
        ($co->{$a}{order}//1) <=> ($co->{$b}{order}//1) || $a cmp $b
    } keys %$co;
    for my $con (@con) {
        my $cov = $co->{$con};
        my $cat = $cov->{category} ? __($cov->{category}) :
            ($sc ? $t_copts : $t_opts);
        my $go = $cov->{getopt};
        push @{ $catopts{$cat} }, {
            getopt=>Getopt::Long::Util::humanize_getopt_long_opt_spec($cov->{getopt}),
            summary=> $cov->{summary} ? __($cov->{summary}) : "",
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
            my $got = Perinci::ToUtil::sah2human_short($s);
            my $ane = $an; $ane =~ s/_/-/g; $ane =~ s/\W/-/g;
            my $summary = rimeta($a)->langprop("summary");

            my $suf = "";
            if ($s->[0] eq 'bool') {
                $got = undef;
                if ($s->[1]{default}) {
                    $ane = "no$ane";
                    my $negsummary = rimeta($a)->langprop(
                        "x.perinci.cmdline.negative_summary");
                    $summary = $negsummary if $negsummary;
                } elsif (defined $s->[1]{default}) {
                    #$ane = $ane;
                } else {
                    $ane = "[no]$ane";
                }
            } elsif ($s->[0] eq 'float' || $s->[0] eq 'num') {
                $ane .= "=f";
            } elsif ($s->[0] eq 'int') {
                $ane .= "=i";
            } elsif ($s->[0] eq 'hash' || $s->[0] eq 'array') {
                $suf = "-json";
                $ane = "$ane-json=val";
            } else {
                $ane .= "=s";
            }

            # add aliases which does not have code
            for my $al0 (keys %{ $a->{cmdline_aliases} // {}}) {
                my $alspec = $a->{cmdline_aliases}{$al0};
                next if $alspec->{code};
                my $al = $al0; $al =~ s/_/-/g;
                if (length($al) == 1) {
                    $al = "-$al";
                    $ane .= ", $al";
                } else {
                    $al = "--$al";
                    $ane .= ", $al$suf";
                }
            }

            my $def = defined($s->[1]{default}) && $s->[0] ne 'bool' ?
                " (default: ".dump1($s->[1]{default}).")" : "";
            my $src = $a->{cmdline_src} // "";
            my $in;
            if ($s->[1]{in} && @{ $s->[1]{in} }) {
                $in = dump1($s->[1]{in});
            }

            my $cat;
            for my $tag (@{ $a->{tags} // []}) {
                my $tn = ref($tag) ? $tag->{name} : $tag;
                next unless $tn =~ /^category:(.+)/;
                $cat = $1;
                last;
            }
            if ($cat) {
                $cat = __x("{category} options", category=>ucfirst($cat));
            } else {
                $cat = $t_opts;
            }

            push @{ $catopts{$cat} }, {
                getopt => "--$ane",
                getopt_type => $got,
                getopt_note =>join(
                    "",
                    ($a->{req} ? " (" . __("required") . ")" : ""),
                    (defined($a->{pos}) ? " (" .
                         __x("or as argument #{index}",
                            index => ($a->{pos}+1).($a->{greedy} ? "+":"")).")":""),
                    ($src eq 'stdin' ?
                         " (" . __("from stdin") . ")" : ""),
                    ($src eq 'stdin_or_files' ?
                         " (" . __("from stdin/files") . ")" : ""),
                    $def
                ),
                req => $a->{req},
                summary => $summary,
                description => rimeta($a)->langprop("description"),
                in => $in,
            };

            # add aliases which have code as separate options
            for my $al0 (keys %{ $a->{cmdline_aliases} // {}}) {
                my $alspec = $a->{cmdline_aliases}{$al0};
                next unless $alspec->{code};
                push @{ $catopts{$cat} }, {
                    getopt => length($al0) > 1 ? "--$al0" : "-$al0",
                    getopt_type => $got,
                    getopt_note => undef,
                    #req => $a->{req},
                    summary => rimeta($alspec)->langprop("summary"),
                    description => rimeta($alspec)->langprop("description"),
                    #in => $in,
                };
            }

        }
    }

    # output gathered options
    for my $cat (sort keys %catopts) {
        $self->_help_add_heading($cat);
        my @opts = sort {
            my $va = $a->{getopt};
            my $vb = $b->{getopt};
            for ($va, $vb) { s/^--(\[no\])?// }
            $va cmp $vb;
        } @{$catopts{$cat}};
        if ($verbose) {
            for my $o (@opts) {
                my $ct = $self->_color('option_name', $o->{getopt}) .
                    ($o->{getopt_type} ? " [$o->{getopt_type}]" : "").
                        ($o->{getopt_note} ? $o->{getopt_note} : "");
                $self->_help_add_row([$ct], {indent=>1});
                if ($o->{in} || $o->{summary} || $o->{description}) {
                    my $ct = "";
                    $ct .= ($ct ? "\n\n":"").ucfirst(__("value in")).
                        ": $o->{in}" if $o->{in};
                    $ct .= ($ct ? "\n\n":"")."$o->{summary}." if $o->{summary};
                    $ct .= ($ct ? "\n\n":"").$o->{description}
                        if $o->{description};
                    $self->_help_add_row([$ct], {indent=>2, wrap=>1});
                }
            }
        } else {
            # for compactness, display in columns
            my $tw = $self->term_width;
            my $columns = int($tw/40); $columns = 1 if $columns < 1;
            while (1) {
                my @row;
                for (1..$columns) {
                    last unless @opts;
                    my $o = shift @opts;
                    push @row, $self->_color('option_name', $o->{getopt}) .
                        #($o->{getopt_type} ? " [$o->{getopt_type}]" : "") .
                            ($o->{getopt_note} ? $o->{getopt_note} : "");
                }
                last unless @row;
                for (@row+1 .. $columns) { push @row, "" }
                $self->_help_add_row(\@row, {indent=>1});
            }
        }
    }
}

sub help_section_subcommands {
    my ($self, %opts) = @_;

    my $scs = $self->subcommands;
    return unless $scs && !$self->{_subcommand};

    my @scs = sort keys %$scs;
    my @shown_scs;
    for my $scn (@scs) {
        my $sc = $scs->{$scn};
        next unless $sc->{show_in_help} // 1;
        $sc->{name} = $scn;
        push @shown_scs, $sc;
    }

    # for help_section_hints
    my $some_not_shown = @scs > @shown_scs;
    $self->{_some_subcommands_not_shown_in_help} = 1 if $some_not_shown;

    $self->_help_add_heading(
        $some_not_shown ? __("Popular subcommands") : __("Subcommands"));

    # in compact mode, we try to not exceed one screen, so show long mode only
    # if there are a few subcommands.
    my $long_mode = $opts{verbose} || @shown_scs < 12;
    if ($long_mode) {
        for (@shown_scs) {
            my $summary = rimeta($_)->langprop("summary");
            $self->_help_add_row(
                [$self->_color('program_name', $_->{name}), $summary],
                {column_widths=>[-17, -40], indent=>1});
        }
    } else {
        # for compactness, display in columns
        my $tw = $self->term_width;
        my $columns = int($tw/25); $columns = 1 if $columns < 1;
            while (1) {
                my @row;
                for (1..$columns) {
                    last unless @shown_scs;
                    my $sc = shift @shown_scs;
                    push @row, $sc->{name};
                }
                last unless @row;
                for (@row+1 .. $columns) { push @row, "" }
                $self->_help_add_row(\@row, {indent=>1});
            }

    }
}

sub help_section_hints {
    my ($self, %opts) = @_;
    my @hints;
    unless ($opts{verbose}) {
        push @hints, N__("For more complete help, use '--help --verbose'");
    }
    if ($self->{_some_subcommands_not_shown_in_help}) {
        push @hints,
            N__("To see all available subcommands, use '--subcommands'");
    }
    return unless @hints;

    $self->_help_add_row(
        [join(" ", map { __($_)."." } @hints)], {wrap=>1});
}

sub help_section_description {
    my ($self, %opts) = @_;

    my $desc = rimeta($self->{_help_meta})->langprop("description") //
        $self->description;
    return unless $desc;

    $self->_help_add_heading(__("Description"));
    $self->_help_add_row([$desc], {wrap=>1, indent=>1});
}

sub help_section_examples {
    my ($self, %opts) = @_;

    my $verbose = $opts{verbose};
    my $meta = $self->{_help_meta};
    my $egs = $meta->{examples};
    return unless $egs && @$egs;

    $self->_help_add_heading(__("Examples"));
    my $pn = $self->_color('program_name', $self->_program_and_subcommand_name);
    for my $eg (@$egs) {
        my $argv;
        my $ct;
        if (defined($eg->{src})) {
            # we only show shell command examples
            if ($eg->{src_plang} =~ /^(sh|bash)$/) {
                $ct = $eg->{src};
            } else {
                next;
            }
        } else {
            require String::ShellQuote;
            if ($eg->{argv}) {
                $argv = $eg->{argv};
            } else {
                require Perinci::Sub::ConvertArgs::Argv;
                my $res = Perinci::Sub::ConvertArgs::Argv::convert_args_to_argv(
                    args => $eg->{args}, meta => $meta);
                $self->_err("Can't convert args to argv: $res->[0] - $res->[1]")
                    unless $res->[0] == 200;
                $argv = $res->[2];
            }
            $ct = $pn;
            for my $arg (@$argv) {
                $arg = String::ShellQuote::shell_quote($arg);
                if ($arg =~ /^-/) {
                    $ct .= " ".$self->_color('option_name', $arg);
                } else {
                    $ct .= " $arg";
                }
            }
        }
        $self->_help_add_row([$ct], {indent=>1});
        if ($verbose) {
            $ct = "";
            my $summary = rimeta($eg)->langprop('summary');
            if ($summary) { $ct .= "$summary." }
            my $desc = rimeta($eg)->langprop('description');
            if ($desc) { $ct .= "\n\n$desc" }
            $self->_help_add_row([$ct], {indent=>2}) if $ct;
        }
    }
}

sub help_section_result {
    my ($self, %opts) = @_;

    my $meta   = $self->{_help_meta};
    my $rmeta  = $meta->{result};
    my $rmetao = rimeta($rmeta);
    my $text;

    my $summary = $rmetao->langprop('summary') // '';
    my $desc    = $rmetao->langprop('description') // '';
    $text = $summary . ($summary ? "\n\n" : "") . $desc;

    # collect handler
    my %handler_args;
    my %handler_metas;
    for my $k0 (keys %$rmeta) {
        my $v = $rmeta->{$k0};

        my $k = $k0; $k =~ s/\..+//;
        next if $k =~ /\A_/;

        # check builtin result spec key
        next if $k =~ /\A(
                           summary|description|tags|default_lang|
                           schema|
                           x
                       )\z/x;

        # try a property module first
        require "Perinci/Sub/Property/result/$k.pm";
        my $meth = "help_hookmeta_result__$k";
        unless ($self->can($meth)) {
            die "No help handler for property result/$k0 ($meth)";
        }
        my $hm = $self->$meth;
        my $ha = {
            prio=>$hm->{prio}, value=>$v->{$k0}, property=>$k0,
            meth=>"help_hook_result__$k",
        };
        $handler_args{$k} = $ha;
        $handler_metas{$k} = $hm;
    }

    # call all the handlers in order
    for my $k (sort {$handler_args{$a}{prio} <=> $handler_args{$b}{prio}}
                   keys %handler_args) {
        my $ha = $handler_args{$k};
        my $meth = $ha->{meth};
        my $t = $self->$meth(meta => $meta, %$ha);
        $text .= $t if $t;
    }

    return unless length $text;

    $self->_help_add_heading(__("Result"));
    $self->_help_add_row([$text], {wrap=>1, indent=>1});
}

sub help_section_links {
    # not yet
}

sub run_help {
    my ($self) = @_;

    my $verbose = $ENV{VERBOSE} // 0;
    my %opts = (verbose=>$verbose);

    # get function metadata first
    my $sc = $self->{_subcommand};
    my $url = $sc ? $sc->{url} : $self->url;
    if ($url) {
        my $res = $self->_pa->request(info => $url);
        $self->_err("Can't info '$url': $res->[0] - $res->[1]")
            unless $res->[0] == 200;
        $self->{_help_info} = $res->[2];
        $res = $self->_pa->request(meta => $url);
        $self->_err("Can't meta '$url': $res->[0] - $res->[1]")
            unless $res->[0] == 200;
        $self->{_help_meta} = $res->[2];
    }

    # ux: since --verbose will potentially show lots of paragraph text, let's
    # default to 80 and not wider width, unless user specifically requests
    # column width via COLUMNS.
    if ($verbose && !defined($ENV{COLUMNS}) && $self->term_width > 80) {
        $self->term_width(80);
    }

    # determine which help sections should we generate
    my @hsects;
    if ($verbose) {
        @hsects = (
            'summary',
            'usage',
            'subcommands',
            'examples',
            'description',
            'options',
            'result',
            'links',
            'hints',
        );
    } else {
        @hsects = (
            'summary',
            'usage',
            'subcommands',
            'examples',
            'options',
            'hints',
        );
    }

    for my $s (@hsects) {
        my $meth = "help_section_$s";
        $log->tracef("=> $meth(%s)", \%opts);
        $self->$meth(%opts);
    }
    $self->_help_draw_curtbl;
    0;
}

my ($ph1, $ph2); # patch handles
my $setup_progress;
sub _setup_progress_output {
    my $self = shift;

    if ($ENV{PROGRESS} // (-t STDOUT)) {
        require Progress::Any::Output;
        Progress::Any::Output->set("TermProgressBarColor");
        my $out = $Progress::Any::outputs{''}[0];
        $setup_progress = 1;
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

                # force output update so progress bar is displayed again
                # immediately
                $Progress::Any::output_data{"$out"}{force_update} = 1;

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

                # force output update so progress bar is displayed again
                # immediately
                $Progress::Any::output_data{"$out"}{force_update} = 1;

                # XXX duplicated code above, perhaps move this to
                # TermProgressBarColor's clean_bar() or something

                say $msg;
            }
        ) if defined &{"Log::Log4perl::Appender::ScreenColoredLevels::log"};
    }
}

sub _unsetup_progress_output {
    my $self = shift;

    return unless $setup_progress;
    my $out = $Progress::Any::outputs{''}[0];
    $out->cleanup if $out->can("cleanup");
    undef $ph1;
    undef $ph2;
    $setup_progress = 0;
}

sub run_call {
    my ($self) = @_;
    my $sc = $self->{_subcommand};
    my %fargs = %{$self->{_args} // {}};
    $fargs{-cmdline} = $self if $sc->{pass_cmdline_object} //
        $self->pass_cmdline_object;

    my $tx_id;

    my $dry_run = $self->{_dry_run};
    my $using_tx = !$dry_run && $self->undo && ($sc->{undo} // 1);

    # currently we don't attempt to insert tx_id or dry_run when using argv,
    # we'll just give up
    if ($self->{_send_argv} && ($dry_run || $using_tx)) {
        my $res = $self->{_getargs_result};
        $self->_err("Failed parsing arguments (2): $res->[0] - $res->[1]");
    }

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
    if ($self->{_meta}{features}{progress}) {
        $self->_setup_progress_output;
    }

    # call function
    if ($self->{_send_argv}) {
        $self->{_res} = $self->_pa->request(
            call => $self->{_subcommand}{url},
            {argv=>$self->{_orig_argv}}, # XXX tx_id, dry_run (see above)
        );
    } else {
        #$log->tracef("Calling function via _pa with arguments: %s", \%fargs);
        $self->{_res} = $self->_pa->request(
            call => $self->{_subcommand}{url},
            {args=>\%fargs, tx_id=>$tx_id, dry_run=>$dry_run});
    }
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
    return $res unless $res->[0] == 200;
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
    [200, "OK", \@txs];
}

sub run_clear_history {
    my $self = shift;
    $self->_pa->request(discard_all_txs => "/");
}

sub run_undo {
    my $self = shift;
    $self->_pa->request(undo => "/");
}

sub run_redo {
    my $self = shift;
    $self->_pa->request(redo => "/");
}

sub _load_log_any_app {
    my ($self) = @_;
    # Log::Any::App::init can already avoid being run twice, but we need to
    # check anyway to avoid logging starting message below twice.
    return if $self->{_log_any_app_loaded}++;
    require Log::Any::App;
    Log::Any::App::init();

    # we log this after we initialize Log::Any::App, since Log::Any::App might
    # not be loaded at all. yes, this means that this log message is printed
    # rather late and might not be the first message to be logged (see log
    # messages in run()) if user already loads Log::Any::App by herself.
    $log->debugf("Program %s started with arguments: %s",
                 $0, $self->{_orig_argv});
}

# this hook is called at the start of run(), can be used to initialize stuffs
sub hook_before_run {
    my ($self, $r) = @_;

    $log->tracef("Start of CLI run");

    # save, for showing in history, among others
    $r->{orig_argv} = [@ARGV];
}

sub hook_after_run {
    my ($self, $r) = @_;
    $self->_unsetup_progress_output;
    $log->tracef("End of CLI run, res status=%s, exit code=%s",
                 $r->{res}[0], $self->status2exitcode($r->{res}[0]));
}

sub hook_after_parse_argv {
    my ($self, $r) = @_;

    # We load Log::Any::App rather late here, so user can customize level via
    # --debug, --dry-run, etc.
    unless ($ENV{COMP_LINE}) {
        my $do_log = $r->{subcommand_data}{log_any_app}
            if $r->{subcommand_data};
        $do_log //= $ENV{LOG};
        $do_log //= $self->{action_metadata}{$r->{action}}{default_log}
            if $self->{action};
        $do_log //= $self->log_any_app;
        $self->_load_log_any_app if $do_log;
    }

    # we'll try giving argv to server side, but this currently means we skip
    # processing cmdline_src.
    if ($r->{parse_argv_res}[0] == 502) {
        #$log->debugf("Failed parsing arguments (status 502), will try to send ".
        #                 "argv to server");
        $r->{send_argv} = 1;
        return;
    }

    #$log->tracef("result of GetArgs for subcommand: remaining argv=%s, args=%s".
    #                 ", actions=%s", \@ARGV, $self->{_args}, $self->{_actions});

    my $action = $r->{action};
    my $meta   = $r->{meta};

    # handle cmdline_src
    if ($action eq 'call') {
        my $args_p = $meta->{args} // {};
        my $stdin_seen;
        for my $an (sort keys %$args_p) {
            #$log->tracef("TMP: handle cmdline_src for arg=%s", $an);
            my $as = $args_p->{$an};
            my $src = $as->{cmdline_src};
            if ($src) {
                $self->_err(
                    "Invalid 'cmdline_src' value for argument '$an': $src")
                    unless $src =~ /\A(stdin|file|stdin_or_files)\z/;
                $self->_err(
                    "Sorry, argument '$an' is set cmdline_src=$src, but type ".
                        "is not 'str' or 'array', only those are supported now")
                    unless $as->{schema}[0] =~ /\A(str|array)\z/;
                if ($src =~ /stdin/) {
                    $self->_err("Only one argument can be specified ".
                                    "cmdline_src stdin/stdin_or_files")
                        if $stdin_seen++;
                }
                my $is_ary = $as->{schema}[0] eq 'array';
                if ($src eq 'stdin' || $src eq 'file' &&
                        ($self->{_args}{$an}//"") eq '-') {
                    $self->_err("Argument $an must be set to '-' which means ".
                                    "from stdin")
                        if defined($self->{_args}{$an}) &&
                            $self->{_args}{$an} ne '-';
                    #$log->trace("Getting argument '$an' value from stdin ...");
                    $self->{_args}{$an} = $is_ary ? [<STDIN>] :
                        do { local $/; <STDIN> };
                } elsif ($src eq 'stdin_or_files') {
                    # push back argument value to @ARGV so <> can work to slurp
                    # all the specified files
                    local @ARGV = @ARGV;
                    unshift @ARGV, $self->{_args}{$an}
                        if defined $self->{_args}{$an};
                    #$log->tracef("Getting argument '$an' value from ".
                    #                 "stdin_or_files, \@ARGV=%s ...", \@ARGV);
                    $self->{_args}{$an} = $is_ary ? [<>] : do { local $/; <> };
                } elsif ($src eq 'file') {
                    unless (exists $self->{_args}{$an}) {
                        if ($as->{req}) {
                            $self->_err(
                                "Please specify filename for argument '$an'");
                        } else {
                            next;
                        }
                    }
                    $self->_err("Please specify filename for argument '$an'")
                        unless defined $self->{_args}{$an};
                    #$log->trace("Getting argument '$an' value from ".
                    #                "file ...");
                    my $fh;
                    unless (open $fh, "<", $self->{_args}{$an}) {
                        $self->_err("Can't open file '$self->{_args}{$an}' ".
                                        "for argument '$an': $!")
                    }
                    $self->{_args}{$an} = $is_ary ? [<$fh>] :
                        do { local $/; <$fh> };
                }
            }
        }
    }
    #$log->tracef("args after cmdline_src is processed: %s", $self->{_args});

    #$log->tracef("<- _parse_subcommand_opts()");
}

1;
# ABSTRACT: Rinci/Riap-based command-line application framework

=for Pod::Coverage ^(.+)$

=head1 SYNOPSIS

See L<Perinci::CmdLine::Manual::Examples>.


=head1 DESCRIPTION

See L<Perinci::CmdLine::Manual>.


=head1 REQUEST KEYS

Extra stuffs put by this module to the C<$r> hash/stash.

=over

=item * format_options => hash

=back


=head1 SEE ALSO

L<Perinci>, L<Rinci>, L<Riap>.

L<Perinci::CmdLine::Base>.

L<Perinci::CmdLine::Lite>.

Other CPAN modules to write command-line applications: L<App::Cmd>, L<App::Rad>,
L<MooseX::Getopt>.

=cut
