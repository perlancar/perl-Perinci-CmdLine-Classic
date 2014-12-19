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

with 'Color::Theme::Role::ANSI' unless $ENV{COMP_LINE};
with 'Perinci::CmdLine::Role::Help' unless $ENV{COMP_LINE};
with 'Term::App::Role::Attrs' unless $ENV{COMP_LINE};

has log => (is => 'rw', default=>sub{1});
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
        my %args = (
            riap_version => $self->riap_version,
            %{$self->riap_client_args // {}},
        );
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
has default_prompt_template => (is=>'rw');

# OLD name for backward compat, will be removed later
sub log_any_app { goto \&log }

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

    if (!$self->{default_prompt_template}) {
        $self->{default_prompt_template} = N__("Enter %s:") . " ",
    }

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

    # translate summary & usage
    my $_t = sub {
        no warnings;
        my $co_name = shift;
        my $copt = $Perinci::CmdLine::Base::copts{$co_name};
        my %res;
        for (keys %$copt) {
            if ($_ eq 'summary' || $_ eq 'usage') {
                $res{$_} = N__($copt->{$_});
            } else {
                $res{$_} = $copt->{$_};
            }
        }
        %res;
    };

    if (!$self->{common_opts}) {
        my $copts = {};

        $copts->{version} = {
            $_t->('version'),
            show_in_options => sub { $ENV{VERBOSE} },
        };
        $copts->{help} = {
            $_t->('help'),
            show_in_options => sub { $ENV{VERBOSE} },
        };
        $copts->{format} = {
            $_t->('format'),
            schema => ['str*' => in => $formats],
        };
        # XXX support --naked-res
        if ($REQ_VERSION >= 1.04) {
            $copts->{json} = {
                $_t->('json'),
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
                $_t->('subcommands'),
                show_in_options => sub {
                    my ($self, $r) = @_;
                    $ENV{VERBOSE} && !$r->{subcommand_name};
                },
                show_in_help => 0,
            };
        }

        if (defined $self->default_subcommand) {
            $copts->{cmd} = { $_t->('cmd') };
        }

        if ($self->read_config) {
            $copts->{config_path}    = { $_t->('config_path') };
            $copts->{no_config}      = { $_t->('no_config') };
            $copts->{config_profile} = { $_t->('config_profile') };
        }

        if ($self->log) {
            $copts->{log_level} = { $_t->('log_level'), };
            $copts->{trace}     = { $_t->('trace'), };
            $copts->{debug}     = { $_t->('debug'), };
            $copts->{verbose}   = { $_t->('verbose'), };
            $copts->{quiet}     = { $_t->('quiet'), };
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
sub hook_format_row {
    state $dfpc = do {
        require Data::Format::Pretty::Console;
        Data::Format::Pretty::Console->new({interactive=>0});
    };

    my ($self, $r, $row) = @_;
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
        my $do_log = $r->{subcommand_data}{log} //
            $r->{subcommand_data}{log_any_app} # OLD compat, will be removed later
                if $r->{subcommand_data};
        $do_log //= $ENV{LOG};
        $do_log //= $self->action_metadata->{$r->{action}}{default_log}
            if $self->{action};
        $do_log //= $self->log;
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
    } elsif ($res->[3]{'x.hint.result_binary'} && $format =~ /text/) {
        $r->{fres} = $res->[2];
    } else {
        $log->tracef("Formatting output with %s", $format);
        $r->{fres} = Perinci::Result::Format::format($res, $format);
    }
}

sub hook_display_result {
    my ($self, $r) = @_;

    my $res  = $r->{res};
    my $fres = $r->{fres};
    my $resmeta = $res->[3] // {};

    my $handle = $r->{output_handle};

    # determine whether to binmode(STDOUT,":utf8")
    my $utf8;
    {
        last if defined($utf8 = $ENV{UTF8});
        if ($resmeta->{'x.hint.result_binary'}) {
            # XXX only when format is text?
            $utf8 = 0; last;
        }
        my $am = $self->action_metadata->{$r->{action}}
            if $r->{action};
        last if defined($utf8 //= $am->{use_utf8});

        if ($r->{subcommand_data}) {
            last if defined($utf8 //= $r->{subcommand_data}{use_utf8});
        }

        $utf8 //= $self->use_utf8
            if $self->can("use_utf8");
    }
    binmode(STDOUT, ":utf8") if $utf8;

    if ($ENV{COMP_LINE} || $res->[3]{"cmdline.skip_format"}) {
        print $handle $res->[2];
        return;
    }

    $self->display_result($r);
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

Screenshots:

=begin HTML

<p><img src="http://blogs.perl.org/users/perlancar/screenshot-pericmd-help.jpg" /><br />Autogenerated help message

<p><img src="http://blogs.perl.org/users/perlancar/screenshot-pericmd-help_verbose.jpg" /><br />Autogenerated help message (verbose mode)

<p><img src="http://blogs.perl.org/users/perlancar/progany-tpc.jpg" /><br />Progress bar

<p><img src="http://blogs.perl.org/users/perlancar/screenshot-pericmd-completion.jpg" /><br />Tab completion

<p><img src="http://blogs.perl.org/users/perlancar/screenshot-pericmd-format.jpg" /><br />Some output formats

<p><img src="http://blogs.perl.org/users/perlancar/screenshot-trashu.jpg" /><br />Undo/redo

=end HTML


=head1 REQUEST KEYS

See also L<Perinci::CmdLine::Base>. Extra stuffs put by this module to the C<$r>
hash/stash.

=over

=item * format_options => hash

=back


=head1 ATTRIBUTES

All the attributes of L<Perinci::CmdLine::Base>, plus:

=head2 log => BOOL (default: 1)

Whether to load L<Log::Any::App> (enable logging output) by default. See
L</"LOGGING"> for more details.

=head2 use_utf8 => BOOL

From L<Term::App::Role::Attrs> (please see its docs for more details). There are
several other attributes added by the role.

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

=head2 x.hint.result_binary => bool

If set to true, then when formatting to C<text> formats, this class won't print
any newline to keep the data being printed unmodified.



=head1 ENVIRONMENT

All the environment variables that L<Perinci::CmdLine::Base> supports, plus:

=head2 PERINCI_CMDLINE_COLOR_THEME => STR

Can be used to set C<color_theme>.

=head2 PROGRESS => BOOL

Explicitly turn the progress bar on/off.

=head2 COLOR => INT

Please see L<Term::App::Role::Attrs>.

=head2 UTF8 => BOOL

Please see L<Term::App::Role::Attrs>.


=head1 SEE ALSO

L<Perinci>, L<Rinci>, L<Riap>.

L<Perinci::CmdLine::Base>.

L<Perinci::CmdLine::Lite>.

Other CPAN modules to write command-line applications: L<App::Cmd>, L<App::Rad>,
L<MooseX::Getopt>.

=cut
