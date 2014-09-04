package Perinci::CmdLine;

# DATE
# VERSION

use 5.010001;
#use strict; # enabled by Moo
#use warnings; # enabled by Moo
use Log::Any '$log';

use Moo;
use experimental 'smartmatch'; # must be after Moo
use Locale::TextDomain::UTF8 'Perinci-CmdLine';
use Perinci::Object;
use Scalar::Util qw(blessed);

our $REQ_VERSION = 0; # version requested by user

extends 'Perinci::CmdLine::Base';

with 'SHARYANTO::Role::ColorTheme' unless $ENV{COMP_LINE};
#with 'SHARYANTO::Role::TermAttrs' unless $ENV{COMP_LINE}; already loaded by ColorTheme
with 'Perinci::CmdLine::Role::Help' unless $ENV{COMP_LINE};

has log_any_app => (is => 'rw', default=>sub{1});
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
has riap_client => (
    is => 'rw',
    lazy => 1,
    default => sub {
        my $self = shift;

        require Perinci::Access;
        require Perinci::Access::Perl;
        require Perinci::Access::Schemeless;
        my %args = %{$self->riap_client_args // {}};
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
has action_metadata => (
    is => 'rw',
    default => sub {
        +{
            clear_history => {
            },
            help => {
                default_log => 0,
                use_utf8 => 1,
            },
            history => {
            },
            subcommands => {
                default_log => 0,
                use_utf8 => 1,
            },
            redo => {
            },
            call => {
            },
            undo => {
            },
            version => {
                default_log => 0,
                use_utf8 => 1,
            },
        },
    },
);

sub VERSION {
    my ($pkg, $req) = @_;
    $REQ_VERSION = $req;
}

sub BUILD {
    my ($self, $args) = @_;

    my $formats = [qw(
                         text text-simple text-pretty
                         json json-pretty yaml perl
                         ruby phpserialization)];

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

        my $copts = {};

        $copts->{version} = {
            getopt  => "version|v",
            usage   => N__("--version (or -v)"),
            summary => N__("Show version"),
            show_in_options => sub { $ENV{VERBOSE} },
            handler => sub {
                my ($go, $val, $r) = @_;
                die "'url' not set, required for --version"
                    unless $self->url;
                $r->{action} = 'version';
                $r->{skip_parse_subcommand_argv} = 1;
            },
        };

        $copts->{help} = {
            getopt  => "help|h|?",
            usage   => N__("--help (or -h, -?) [--verbose]"),
            summary => N__("Display this help message"),
            show_in_options => sub { $ENV{VERBOSE} },
            handler => sub {
                my ($go, $val, $r) = @_;
                $r->{action} = 'help';
                $r->{skip_parse_subcommand_argv} = 1;
            },
            order   => 0, # high
        };

        $copts->{format} = {
            getopt  => "format=s",
            summary => N__("Choose output format, e.g. json, text"),
            schema => ['str*' => in => $formats],
            handler => sub {
                my ($go, $val, $r) = @_;
                $r->{format} = $val;
            },
        };

        if ($REQ_VERSION >= 1.04) {
            $copts->{json} = {
                getopt  => "json",
                summary => N__("Equivalent to --format=json-pretty"),
                handler => sub {
                    my ($go, $val, $r) = @_;
                    $r->{format} = 'json-pretty';
                },
            };
            $copts->{yaml} = {
                getopt  => "yaml",
                summary => N__("Equivalent to --format=yaml"),
                handler => sub {
                    my ($go, $val, $r) = @_;
                    $r->{format} = 'yaml';
                },
            };
            $copts->{perl} = {
                getopt  => "perl",
                summary => N__("Equivalent to --format=perl"),
                handler => sub {
                    my ($go, $val, $r) = @_;
                    $r->{format} = 'perl';
                },
            };
        }

        $copts->{format_options} = {
            getopt  => "format-options=s",
            summary => N__("Pass options to formatter"),
            handler => sub {
                my ($go, $val, $r) = @_;
                $r->{format_options} = __json_decode($val);
            },
        };

        if ($self->subcommands) {
            $copts->{subcommands} = {
                getopt  => "subcommands",
                usage   => N__("--subcommands"),
                show_in_usage => sub {
                    my ($self, $r) = @_;
                    !$r->{subcommand_name};
                },
                show_in_options => sub {
                    my ($self, $r) = @_;
                    $ENV{VERBOSE} && !$r->{subcommand_name};
                },
                summary => N__("List available subcommands"),
                show_in_help => 0,
                handler => sub {
                    my ($go, $val, $r) = @_;
                    $r->{action} = 'subcommands';
                    $r->{skip_parse_subcommand_argv} = 1;
                },
            };
        }

        if (defined $self->default_subcommand) {
            # 'cmd=SUBCOMMAND_NAME' can be used to select other subcommands
            # when default_subcommand is in effect.
            $copts->{cmd} = {
                getopt  => "cmd=s",
                handler => sub {
                    my ($go, $val, $r) = @_;
                    $r->{subcommand_name} = $val;
                    $r->{subcommand_name_from} = '--cmd';
                },
            };
        }

        if ($self->read_config) {
            $copts->{config_path} = {
                getopt  => 'config-path=s@',
                summary => N__('Set path to configuration file'),
                handler => sub {
                    my ($go, $val, $r) = @_;
                    $r->{config_paths} //= [];
                    push @{ $r->{config_paths} }, $val;
                },
            };
            $copts->{no_config} = {
                getopt  => 'noconfig|no-config',
                summary => N__('Do not use any configuration file'),
                handler => sub {
                    my ($go, $val, $r) = @_;
                    $r->{read_config} = 0;
                },
            };
            $copts->{config_profile} = {
                getopt  => 'config-profile=s',
                summary => N__('Set configuration profile to use'),
                handler => sub {
                    my ($go, $val, $r) = @_;
                    $r->{config_profile} = $val;
                },
            };
        }

        # convenience for Log::Any::App-using apps
        if ($self->log_any_app) {
            # since the cmdline opts is consumed, Log::Any::App doesn't see
            # this. we currently work around this via setting env.

            $copts->{quiet} = {
                getopt  => "quiet",
                summary => N__("Set log level to quiet"),
                handler => sub {
                    my ($go, $val, $r) = @_;
                    $ENV{QUIET} = 1;
                },
            };
            $copts->{verbose} = {
                getopt  => "verbose",
                summary => N__("Set log level to verbose"),
                handler => sub {
                    my ($go, $val, $r) = @_;
                    $ENV{VERBOSE} = 1;
                    $r->{_help_verbose} = 1;
                },
            };
            $copts->{debug} = {
                getopt  => "debug",
                summary => N__("Set log level to debug"),
                handler => sub {
                    my ($go, $val, $r) = @_;
                    $ENV{DEBUG} = 1;
                },
            };
            $copts->{trace} = {
                getopt  => "trace",
                summary => N__("Set log level to trace"),
                handler => sub {
                    my ($go, $val, $r) = @_;
                    $ENV{TRACE} = 1;
                },
            };

            $copts->{log_level} = {
                getopt  => "log-level=s",
                summary => N__("Set log level"),
                handler => sub {
                    my ($go, $val, $r) = @_;
                    $ENV{LOG_LEVEL} = $val;
                },
            };
        }

        if ($self->undo) {
            $copts->{history} = {
                category => 'Undo options',
                getopt  => 'history',
                summary => N__('List actions history'),
                handler => sub {
                    my ($go, $val, $r) = @_;
                    $r->{action} = 'history';
                    $r->{skip_parse_subcommand_argv} = 1;
                },
            };
            $copts->{clear_history} = {
                category => 'Undo options',
                getopt  => "clear-history",
                summary => N__('Clear actions history'),
                handler => sub {
                    my ($go, $val, $r) = @_;
                    $r->{action} = 'clear_history';
                    $r->{skip_parse_subcommand_argv} = 1;
                },
            };
            $copts->{undo} = {
                category => 'Undo options',
                getopt  => 'undo',
                summary => N__('Undo previous action'),
                handler => sub {
                    my ($go, $val, $r) = @_;
                    $r->{action} = 'undo';
                    $r->{skip_parse_subcommand_argv} = 1;
                },
            };
            $copts->{redo} = {
                category => 'Undo options',
                getopt  => 'redo',
                summary => N__('Redo previous undone action'),
                handler => sub {
                    my ($go, $val, $r) = @_;
                    $r->{action} = 'redo';
                    $r->{skip_parse_subcommand_argv} = 1;
                },
            };
        }
        $self->{common_opts} = $copts;
    }

    $self->{formats} //= $formats;
    $self->{per_arg_json} //= 1;
    $self->{per_arg_yaml} //= 1;

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
}

sub __json_decode {
    require JSON;
    state $json = do { JSON->new->allow_nonref };
    $json->decode(shift);
}

sub __json_encode {
    require JSON;
    state $json = do { JSON->new->allow_nonref };
    $json->encode(shift);
}

sub _color {
    my ($self, $color_name, $text) = @_;
    my $color_code = $color_name ?
        $self->get_theme_color_as_ansi($color_name) : "";
    my $reset_code = $color_code ? "\e[0m" : "";
    "$color_code$text$reset_code";
}

#NOW UNUSED
#sub err {
#    my ($self, $msg) = @_;
#    $msg .= "\n" unless $msg =~ /\n\z/;
#    $self->_color('error_label', "ERROR: ") . $msg;
#}

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

sub hook_after_get_meta {
    my ($self, $r) = @_;

    if (risub($r->{meta})->can_dry_run) {
        $self->common_opts->{dry_run} = {
            getopt  => 'dry-run',
            summary => N__("Run in simulation mode (also via DRY_RUN=1)"),
            handler => sub {
                my ($go, $val, $r) = @_;
                $r->{dry_run} = 1;
                $ENV{VERBOSE} = 1;
            },
        };
    }
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

sub _load_log_any_app {
    my ($self, $r) = @_;
    # Log::Any::App::init can already avoid being run twice, but we need to
    # check anyway to avoid logging starting message below twice.
    return if $r->{_log_any_app_loaded}++;
    require Log::Any::App;
    Log::Any::App::init();

    # we log this after we initialize Log::Any::App, since Log::Any::App might
    # not be loaded at all. yes, this means that this log message is printed
    # rather late and might not be the first message to be logged (see log
    # messages in run()) if user already loads Log::Any::App by herself.
    $log->debugf("Program %s started with arguments: %s",
                 $0, $r->{orig_argv});
}

# this hook is called at the start of run(), can be used to initialize stuffs
sub hook_before_run {
    my ($self, $r) = @_;

    $log->tracef("Start of CLI run");

    # save, for showing in history, among others
    $r->{orig_argv} = [@ARGV];
}

sub hook_after_parse_argv {
    my ($self, $r) = @_;

    # We load Log::Any::App rather late here, so user can customize level via
    # --debug, --dry-run, etc.
    unless ($ENV{COMP_LINE}) {
        my $do_log = $r->{subcommand_data}{log_any_app}
            if $r->{subcommand_data};
        $do_log //= $ENV{LOG};
        $do_log //= $self->action_metadata->{$r->{action}}{default_log}
            if $self->{action};
        $do_log //= $self->log_any_app;
        $self->_load_log_any_app($r) if $do_log;
    }
}

sub hook_format_result {
    # save startup time under completion
    return if $ENV{COMP_LINE};

    my ($self, $r) = @_;

    my $res    = $r->{res};
    my $format = $r->{format} // 'text';
    my $meta   = $r->{meta};

    require Perinci::Result::Format;

    unless ($format ~~ @{ $self->formats }) {
        warn "Unknown output format '$format'";
        $format = 'text';
    }

    $res->[3]{result_format_options} = $r->{format_options}
        if $r->{format_options};

    if ($res->[3]{is_stream}) {
        $log->tracef("Result is a stream");
        return undef;
    } else {
        $log->tracef("Formatting output with %s", $format);
        $r->{fres} = Perinci::Result::Format::format($res, $format);
    }
}

sub hook_display_result {
    require File::Which;

    my ($self, $r) = @_;

    my $res  = $r->{res};
    my $fres = $r->{fres};
    my $resmeta = $res->[3] // {};

    if ($ENV{COMP_LINE} || $res->[3]{"cmdline.skip_format"}) {
        print $res->[2];
        return;
    }

    # determine whether to binmode(STDOUT,":utf8")
    my $utf8 = $ENV{UTF8};
    if (!defined($utf8)) {
        my $am = $self->action_metadata->{$r->{action}}
            if $r->{action};
        $utf8 //= $am->{use_utf8};
    }
    if (!defined($utf8) && $r->{subcommand_data}) {
        $utf8 //= $r->{subcommand_data}{use_utf8};
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
                die [500, "Can't determine PAGER"];
            }
            last unless $pager; # ENV{PAGER} can be set 0/'' to disable paging
            $log->tracef("Paging output using %s", $pager);
            open $handle, "| $pager";
        }
    }
    $handle //= \*STDOUT;

    if ($resmeta->{is_stream}) {
        die [500, "Can't format stream as " . $self->format .
                 ", please use --format text"]
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
            die [500, "Invalid stream in result (not a glob/IO::Handle-like ".
                     "object/(tied) array)\n"];
        }
    } else {
        print $handle $fres;
    }
}

sub hook_after_run {
    my ($self, $r) = @_;
    $self->_unsetup_progress_output;
    $log->tracef("End of CLI run, res status=%s, exit code=%s",
                 $r->{res}[0], $self->status2exitcode($r->{res}[0]));
}

sub run_subcommands {
    my ($self, $r) = @_;

    if (!$self->subcommands) {
        return [200, "OK", __("There are no subcommands") . ".",
                {"x.perinci.cmdline._skip_format"=>1}];
    }

    $r->{_help_buf} = '';

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
                $r,
                __x("{category} subcommands",
                    category => ucfirst($cat || __("main"))));
        }
        my $subc = $percat_subc{$cat};
        for my $scn (sort keys %$subc) {
            my $sc = $subc->{$scn};
            my $summary = rimeta($sc)->langprop("summary");
            $self->_help_add_row(
                $r,
                [$self->_color('program_name', $scn), $summary],
                {column_widths=>[-17, -40], indent=>1});
        }
    }
    $self->_help_draw_curtbl($r);

    [200, "OK", $r->{_help_buf},
     {"x.perinci.cmdline._skip_format"=>1}];
}

sub run_version {
    my ($self, $r) = @_;

    my $url = $r->{subcommand_data}{url} // $self->url;
    my $meta = $self->get_meta($r, $url);
    my $ver = $meta->{entity_v} // "?";
    my $date = $meta->{entity_date};

    [200, "OK", join(
        "",
        __x(
            "{program} version {version}",
            program => $self->_color('program_name',
                                     $self->get_program_and_subcommand_name),
            version => $self->_color('emphasis', $ver)),
        ($date ? " ($date)" : ""),
        "\n",
        "  ", __x(
            "{program} version {version}",
            program => $self->_color('emphasis', "Perinci::CmdLine"),
            version => $self->_color('emphasis',
                                     $Perinci::CmdLine::VERSION || "dev")),
        ($Perinci::CmdLine::DATE ? " ($Perinci::CmdLine::DATE)" : ""),
        "\n",
    ), {"x.perinci.cmdline._skip_format"=>1}];
}

sub run_call {
    my ($self, $r) = @_;

    my $scn = $r->{subcommand_name};
    my $scd = $r->{subcommand_data};

    my %fargs = %{$r->{args} // {}};

    my $tx_id;

    my $dry_run = $r->{dry_run};
    my $using_tx = !$dry_run && $self->undo && ($scd->{undo} // 1);

    # currently we don't attempt to insert tx_id or dry_run when using argv,
    # we'll just give up
    if ($r->{send_argv} && ($dry_run || $using_tx)) {
        return $r->{parse_argv_res};
    }

    if ($using_tx) {
        require UUID::Random;
        $tx_id = UUID::Random::generate();
        $tx_id =~ s/-.+//; # 32bit suffices for small number of txs
        my $summary = join(" ", @{ $r->{orig_argv} });
        my $tres = $self->riap_client->request(
            begin_tx => "/", {tx_id=>$tx_id, summary=>$summary});
        if ($tres->[0] != 200) {
            return [$tres->[0], "Can't start transaction '$tx_id': $tres->[1]"];
        }
    }

    # setup output progress indicator
    if ($r->{meta}{features}{progress}) {
        $self->_setup_progress_output;
    }

    # call function
    my $res;
    if ($r->{send_argv}) {
        $res = $self->riap_client->request(
            call => $scd->{url},
            {argv=>$r->{orig_argv}}, # XXX tx_id, dry_run (see above)
        );
    } else {
        #$log->tracef("Calling function via _pa with arguments: %s", \%fargs);
        $res = $self->riap_client->request(
            call => $scd->{url},
            {args=>\%fargs, tx_id=>$tx_id, dry_run=>$dry_run});
    }
    $log->tracef("call res=%s", $res);

    # commit transaction (if using tx)
    if ($using_tx && $res->[0] =~ /\A(?:200|304)\z/) {
        my $tres = $self->riap_client->request(commit_tx => "/", {tx_id=>$tx_id});
        if ($tres->[0] != 200) {
            return [$tres->[0],"Can't commit transaction '$tx_id': $tres->[1]"];
        }
    }

    $res;
}

sub run_history {
    my ($self, $r) = @_;
    my $res = $self->riap_client->request(list_txs => "/", {detail=>1});
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
    my ($self, $r) = @_;
    $self->riap_client->request(discard_all_txs => "/");
}

sub run_undo {
    my ($self, $r) = @_;
    $self->riap_client->request(undo => "/");
}

sub run_redo {
    my ($self, $r) = @_;
    $self->riap_client->request(redo => "/");
}

1;
# ABSTRACT: Rinci/Riap-based command-line application framework

=for Pod::Coverage ^(.+)$

=head1 SYNOPSIS

See L<Perinci::CmdLine::Manual::Examples>.


=head1 DESCRIPTION

See L<Perinci::CmdLine::Manual>.


=head1 REQUEST KEYS

See also L<Perinci::CmdLine::Base>. Extra stuffs put by this module to the C<$r>
hash/stash.

=over

=item * format_options => hash

=back


=head1 ATTRIBUTES

All the attributes of L<Perinci::CmdLine::Base>, plus:

=head2 log_any_app => BOOL (default: 1)

Whether to load L<Log::Any::App> (enable logging output) by default. See
L</"LOGGING"> for more details.

=head2 use_utf8 => BOOL

From L<SHARYANTO::Role::TermAttrs> (please see its docs for more details). There
are several other attributes added by the role.

=head2 undo => BOOL (optional, default 0)

Whether to enable undo/redo functionality. Some things to note if you intend to
use undo:

=over

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

All the methods of L<Perinci::CmdLine::Base>, plus:

=over

=back


=head1 RESULT METADATA

All those supported by L<Perinci::CmdLine::Base>, plus:

=head2 property: is_stream => BOOL

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

=head2 attribute: cmdline.page_result => BOOL

If you want to filter the result through pager (currently defaults to
C<$ENV{PAGER}> or C<less -FRSX>), you can set C<cmdline.page_result> in result
metadata to true.

For example:

 $SPEC{doc} = { ... };
 sub doc {
     ...
     [200, "OK", $doc, {"cmdline.page_result"=>1}];
 }

=head2 attribute: cmdline.pager => STR

Instruct Perinci::CmdLine to use specified pager instead of C<$ENV{PAGER}> or
the default C<less> or C<more>.


=head1 ENVIRONMENT

All the environment variables that L<Perinci::CmdLine::Base> supports, plus:

=head2 PERINCI_CMDLINE_COLOR_THEME => STR

Can be used to set C<color_theme>.

=head2 PROGRESS => BOOL

Explicitly turn the progress bar on/off.

=head2 PAGER => STR

Like in other programs, can be set to select the pager program (when
C<cmdline.page_result> result metadata is active). Can also be set to C<''> or
C<0> to explicitly disable paging even though C<cmd.page_result> result metadata
is active.

=head2 COLOR => INT

Please see L<SHARYANTO::Role::TermAttrs>.

=head2 UTF8 => BOOL

Please see L<SHARYANTO::Role::TermAttrs>.


=head1 SEE ALSO

L<Perinci>, L<Rinci>, L<Riap>.

L<Perinci::CmdLine::Base>.

L<Perinci::CmdLine::Lite>.

Other CPAN modules to write command-line applications: L<App::Cmd>, L<App::Rad>,
L<MooseX::Getopt>.

=cut
