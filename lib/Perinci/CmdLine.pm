package Perinci::CmdLine;

use 5.010;
use strict;
use warnings;
use Data::Dump::OneLine qw(dump1);
use Log::Any '$log';
use Moo;
#use Perinci::Object;
use Perinci::ToUtil;

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
has exit => (is => 'rw', default=>sub{1});
has log_any_app => (is => 'rw', default=>sub{$ENV{LOG} // 1});
has custom_completer => (is => 'rw');
has custom_arg_completer => (is => 'rw');
has dash_to_underscore => (is => 'rw', default=>sub{1});
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
has _pa => (
    is => 'rw',
    lazy => 1,
    default => sub {
        my $self = shift;

        require Perinci::Access;
        my %args;
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
            );
            $args{handlers} = {
                pl   => $pai,
                riap => $pai,
            };
        }
        Perinci::Access->new(%args);
    }
);

sub BUILD {
    my ($self, $args) = @_;
    #$self->{indent} = $args->{indent} // "    ";
}

sub format_and_display_result {
    require Perinci::Result::Format;

    my $self = shift;
    return unless $self->{_res};

    my $resmeta = $self->{_res}->[3] // {};
    unless ($resmeta->{"cmdline.display_result"}//1) {
        $self->{_res}[2] = undef;
    }

    my $format = $self->format;
    die "ERROR: Unknown output format '$format', please choose one of: ".
        join(", ", sort keys(%Perinci::Result::Format::Formats))."\n"
            unless $Perinci::Result::Format::Formats{$format};
    $self->{_fres} = Perinci::Result::Format::format($self->{_res}, $format);

    # display result
    if ($resmeta->{"cmdline.page_result"}) {
        my $pager = $resmeta->{"cmdline.pager"} //
            $ENV{PAGER};
        unless (defined $pager) {
            $pager = "less -FRS" if File::Which::which("less");
        }
        unless (defined $pager) {
            $pager = "more" if File::Which::which("more");
        }
        unless (defined $pager) {
            die "Can't determine PAGER";
        }
        $log->tracef("Paging output using %s", $pager);
        open my($p), "| $pager";
        print $p $self->{_fres};
    } else {
        print $self->{_fres};
    }
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

    my $url = $self->{_subcommand} ? $self->{_subcommand}{url} : $self->url;
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
    for my $o (keys %{{@{ $self->{_getopts_common} }}}) {
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

        # convert @getopts' ('help|h|?' => ..., ...) to ['--help', '-h', '-?',
        # ...]. XXX this should be moved to another module to remove
        # duplication, as Perinci::Sub::GetArgs::Argv also does something
        # similar.
        my $common_opts = [];
        for my $k (keys %{{@{ $self->{_getopts_common} }}}) {
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
    }
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
    my $text;
    if ($self->subcommands) {
        $text = <<_;
Usage:

    [_1] --help (or -h, -?)
    [_1] --version (or -v)
    [_1] --list (or -l)
    [_1] SUBCOMMAND (common options) (options)
_
    } else {
        $text = <<_;
Usage:

    [_1] --help (or -h, -?)
    [_1] --version (or -v)
    [_1] (common options) (options)
_
    }

    $self->add_doc_lines($self->loc($text, $self->program_name), "");
}

sub doc_parse_common_options {}

sub doc_gen_common_options {
    my ($self) = @_;

    my $text = <<_;
Common options:

    --yaml, -y      Format result as YAML
    --json, -j      Format result as JSON
    --text-pretty   Format result as pretty formatted text
    --text-simple   Format result as simple formatted text
    --text         (Default) Use --text-pretty, or --text-simple when run piped
    --format=FMT    Choose output format
_
    $self->add_doc_lines($self->loc($text), "");

    $text = <<_;
Undo options:

    --undo <ID>     Undo previous action (use --history to get IDs)
    --redo <ID>     Redo previous undo action (use --history to get IDs)
    --history       List actions history
    --clear-history Clear actions history
_
    $self->add_doc_lines($self->loc($text), "") if $self->undo;
}

sub doc_parse_options {}

sub doc_gen_options {
    my ($self) = @_;
    my $info = $self->{_info};
    my $meta = $self->{_meta};
    my $args_p = $meta->{args};
    return if !$info || $info->{type} ne 'function' || !$args_p || !%$args_p;

    $self->add_doc_lines($self->loc("Options") . ":\n", "");

    # XXX categorize
    for my $an (sort {
        ($args_p->{$a}{pos} // 99) <=> ($args_p->{$b}{pos} // 99) ||
            $a cmp $b
    } keys %$args_p) {
        my $a = $args_p->{$an};
        my $s = $a->{schema} || [any=>{}];
        my $ane = $an; $ane =~ s/_/-/g; $ane =~ s/\W/-/g;
        $ane = "no$ane" if $s->[0] eq 'bool' && $s->[1]{default};
        my $def = defined($s->[1]{default}) ?
            " (default: ".dump1($s->[1]{default}).")" : "";
        my $text = sprintf(
            "  --%s [%s]%s%s\n",
            $ane,
            Perinci::ToUtil::sah2human_short($s),
            (defined($a->{pos}) ? " (" .
                 $self->loc("or as argument #[_1]",
                            ($a->{pos}+1).($a->{greedy} ? "+":"")) . ")" : ""),
            $def,
        );
        $self->add_doc_lines($text);
        my $in;
        if ($s->[1]{in} && @{ $s->[1]{in} }) {
            $in = dump1($s->[1]{in});
        }
        my $summary     = $self->langprop($a, "summary");
        my $description = $self->langprop($a, "description");
        if ($in || $summary || $description || $in) {
            $self->inc_indent(2);
            $self->add_doc_lines(
                "",
                ucfirst($self->loc("value in")). ": $in")
                if $in;
            $self->add_doc_lines("", $summary . ".") if $summary;
            $self->add_doc_lines("", $description) if $description;
            $self->dec_indent(2);
            $self->add_doc_lines("");
        }
    }

    my @spec;
    my $ff = $meta->{features} // {};
    if ($ff->{dry_run}) {
        push @spec, {opt=>"dry-run", type=>"bool",
                     summary=>$self->loc(
                         "Run in simulation mode ".
                             "(can also be set via environment DRY_RUN=1)")};
    }
    if (@spec) {
        $self->add_doc_lines($self->loc("Special options"). ":\n", "");
        for my $spec (@spec) {
            $self->add_doc_lines("  --$spec->{opt} [$spec->{type}]\n");
            $self->inc_indent(2);
            $self->add_doc_lines("", $spec->{summary}.".") if $spec->{summary};
            $self->dec_indent(2);
            $self->add_doc_lines("");
        }
        $self->add_doc_lines("");
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

sub run_help {
    require Text::Wrap;

    my ($self) = @_;
    my $sc = $self->{_subcommand};

    $self->{doc_sections} //= [
        'summary',
        'usage',
        'common_options',
        'options',
        'description',
        'examples',
        'links',
    ];
    $Text::Wrap::columns = 80;
    print $self->generate_doc();
    0;
}

sub run_subcommand {
    require File::Which;

    my ($self) = @_;
    my $tx_id;

    my $ff = $self->{_meta}{features} // {};
    my $dry_run = $ff->{dry_run} && $self->{_args}{-dry_run};
    my $begun;

    # begin transaction (if using undo)
    if ($self->undo && !$dry_run) {
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
        $begun++;
    }

    # call function
    $self->{_res} = $self->_pa->request(
        call => $self->{_subcommand}{url},
        {args=>$self->{_args}, tx_id=>$tx_id});
    $log->tracef("call res=%s", $self->{_res});

    # commit transaction (if using undo)
    if ($begun && $self->{_res}[0] =~ /\A(?:200|304)\z/) {
        my $res = $self->_pa->request(commit_tx => "/", {tx_id=>$tx_id});
        if ($res->[0] != 200) {
            $self->{_res} = [$res->[0],
                             "Can't commit transaction '$tx_id': $res->[1]"];
            return 1;
        }
    }

    $self->{_res}[0] =~ /\A(?:200|304)\z/ ? 0 : $self->{_res}[0] - 300;
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
            start_time  => scalar(localtime $tx->{tx_start_time}),
            commit_time => scalar(localtime $tx->{tx_commit_time}),
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

sub gen_common_opts {
    require Getopt::Long;

    my ($self) = @_;
    $log->tracef("-> gen_common_opts()");

    my @getopts = (
        "action=s"   => sub {
            # 'action=subcommand' can be used to override --help (or --list,
            # --version) if one of function arguments happens to be 'help',
            # 'list', or 'version'. currently udocumented.
            if ($_[1] eq 'subcommand') {
                $self->{_force_subcommand} = 1;
            }
        },
        "list|l"     => sub {
            unshift @{$self->{_actions}}, 'list';
            $self->{_check_required_args} = 0;
        },
        "version|v"  => sub {
            die "ERROR: 'url' not set, required for --version\n"
                unless $self->url;
            unshift @{$self->{_actions}}, 'version';
            $self->{_check_required_args} = 0;
        },
        "help|h|?"   => sub {
            unshift @{$self->{_actions}}, 'help';
            $self->{_check_required_args} = 0;
        },

        "yaml|y"      => sub { $self->format('yaml')        },
        "json|j"      => sub { $self->format('json')        },
        "text-pretty" => sub { $self->format('text-pretty') },
        "text-simple" => sub { $self->format('text-simple') },
        "text"        => sub { $self->format('text')        },
        "format=s"    => sub { $self->format($_[1])         },
    );

    # convenience for Log::Any::App-using apps
    if ($self->log_any_app) {
        for (qw/quiet verbose debug trace log-level/) {
            push @getopts, $_ => sub {};
        }
    }

    if ($self->undo) {
        push @getopts, "history" => sub {
            unshift @{$self->{_actions}}, 'history';
            $self->{_check_required_args} = 0;
        };
        push @getopts, "clear-history" => sub {
            unshift @{$self->{_actions}}, 'clear_history';
            $self->{_check_required_args} = 0;
        };
        push @getopts, "undo" => sub {
            unshift @{$self->{_actions}}, 'undo';
            #$self->{_tx_id} = $_[1];
            $self->{_check_required_args} = 0;
        };
        push @getopts, "redo" => sub {
            unshift @{$self->{_actions}}, 'redo';
            #$self->{_tx_id} = $_[1];
            $self->{_check_required_args} = 0;
        };
    }

    $log->tracef("GetOptions spec for parsing common options: %s", \@getopts);
    $log->tracef("<- gen_common_opts()");
    return \@getopts;
}

sub parse_common_opts {
    $log->tracef("-> parse_common_opts()");
    my ($self) = @_;

    my @orig_ARGV = @ARGV;
    $self->{_orig_argv} = \@orig_ARGV;

    my $old_go_opts = Getopt::Long::Configure(
        "pass_through", "no_ignore_case");
    Getopt::Long::GetOptions(@{$self->{_getopts_common}});
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
    return unless $self->{_subcommand};
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

    # parse --dry-run
    my $ff = $meta->{features} // {};
    my %merge_args;
    if ($ff->{dry_run}) {
        push @{$self->{_getopts_common}}, "dry-run" => sub {
            $merge_args{-dry_run} = 1;
        };
    }
    $merge_args{-dry_run} = 1 if $ENV{DRY_RUN};

    # parse argv
    $Perinci::Sub::GetArgs::Argv::_pa_skip_check_required_args = 1
        if $self->{_pa_skip_check_required_args};
    my %ga_args = (
        argv=>\@ARGV, meta=>$meta,
        check_required_args => $self->{_check_required_args} // 1,
    );
    if ($self->{_force_subcommand}) {
        $ga_args{extra_getopts_before} = $self->{_getopts_common};
    } else {
        $ga_args{extra_getopts_after}  = $self->{_getopts_common};
    }
    $res = Perinci::Sub::GetArgs::Argv::get_args_from_argv(%ga_args);
    die "ERROR: Failed parsing arguments: $res->[0] - $res->[1]\n"
        unless $res->[0] == 200;
    $self->{_args} = { %merge_args, %{ $res->[2] } };
    $log->tracef("result of GetArgs for subcommand: remaining argv=%s, args=%s".
                     ", actions=%s", \@ARGV, $self->{_args}, $self->{_actions});

    $log->tracef("<- _parse_subcommand_opts()");
}

# set $self->{_subcommand} for convenience, it can be taken from subcommands(),
# or, in the case of app with a single command, {name=>'', url=>$self->url()}.
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
            if ($self->{_force_subcommand}) {
                unshift @{$self->{_actions}}, 'subcommand';
            } else {
                push @{$self->{_actions}}, 'subcommand';
            }
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
    unshift @{$self->{_actions}}, 'completion' if $ENV{COMP_LINE};
    push @{$self->{_actions}}, 'help' if !@{$self->{_actions}};

    unless ($ENV{COMP_LINE}) {
        $self->_load_log_any_app if
            $self->{_subcommand}{log_any_app} // $self->log_any_app;
    }

    $log->tracef("actions=%s, subcommand=%s",
                 $self->{_actions}, $self->{_subcommand});
}

sub _load_log_any_app {
    my ($self) = @_;
    # Log::Any::App::init can already avoid being run twice
    #return unless $self->{_log_any_app_loaded}
    require Log::Any::App;
    Log::Any::App::init();
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

    my $getopts_common = $self->gen_common_opts();

    # store for other methods, e.g. run_subcommand() & run_completion()
    $self->{_getopts_common} = $getopts_common;

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
    $self->format_and_display_result;

    $log->tracef("<- CmdLine's run(), exit code=%s", $exit_code);
    if ($self->exit) { exit $exit_code } else { return $exit_code }
}

1;
# ABSTRACT: Rinci/Riap-based command-line application framework

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

Perinci::CmdLine is a command-line application framework. It accesses functions
using Riap protocol (L<Perinci::Access>) so you get transparent remote access.
It utilizes L<Rinci> metadata in the code so the amount of plumbing that you
have to do is quite minimal.

What you'll get:

=over 4

=item * Command-line options parsing

=item * Help message (utilizing information from metadata, supports translation)

=item * Tab completion for bash (including completion from remote code)

=item * Undo/redo/history

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
C<url>. It can also contain these keys: C<summary> (str, will be retrieved from
function metadata if unset), C<tags> (array of str, for categorizing
subcommands), C<log_any_app> (bool, whether to load Log::Any::App, default is
true, for subcommands that need fast startup you can try turning this off for
said subcommands).

Subcommands can also be a coderef, for dynamic list of subcommands. The coderef
will be called as a method with hash arguments. It can be called in two cases.
First, if called without argument C<name> (usually when doing --list) it must
return a hashref of subcommand specifications. If called with argument C<name>
it must return subcommand specification for subcommand with the requested name
only.

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

If set to 1, subcommand like a-b-c will be converted to a_b_c. This is for
convenience when typing in command line.

=head2 undo => BOOL (optional, default 0)

Whether to enable undo/redo functionality. Some things to note if you intend to
use undo:

=over 4

=item * These command-line options will be recognized

C<--undo>, C<--redo>, C<--history>, C<--clear-history>.

=item * Transactions will be used

use_tx=>1 will be passed to L<Perinci::Access>, which will cause it to
initialize the transaction manager. Riap requests begin_tx and commit_tx will
enclose the call request to function.

=item * Called function will need to support transaction and undo

Function which do not meet qualifications will refuse to be called.

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


=head1 BASH COMPLETION

To do bash completion, first create your script, e.g. C<myscript>, that uses
Perinci::CmdLine:

 #!/usr/bin/perl
 use Perinci::CmdLine;
 Perinci::CmdLine->new(...)->run;

then execute this in C<bash> (or put it in bash startup files like
C</etc/bash.bashrc> or C<~/.bashrc> for future sessions):

 % complete -C myscript myscript; # myscript must be in PATH


=head1 RESULT METADATA

This module interprets the following result metadata keys:

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
C<$ENV{PAGER}> or C<less -FRS>), you can set C<cmdline.page_result> in result
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


=head1 FAQ

=head2 How does Perinci::CmdLine compare with other CLI-app frameworks?

Perinci::CmdLine is part of a more general metadata and wrapping framework
(Perinci::* modules family). Aside from a command-line application, your
metadata is also usable for other purposes, like providing access over HTTP/TCP,
documentation, etc.

Configuration file support is missing (coming soon, most probably will be based
on L<Config::Ini::OnDrugs>).

Also lacking is more documentation and more plugins.

=head2 Why is nonscalar arguments parsed as YAML instead of JSON/etc?

I think YAML is nicer in command-line because quotes are optional in a few
places:

 $ cmd --array '[a, b, c]' --hash '{foo: bar}'

versus:

 $ cmd --array '["a","b","c"]' --hash '{"foo":"bar"}'

Though YAML requires spaces in some places where JSON does not. A flag to parse
as JSON can be added upon request.

=head2 How to add support for new output format (e.g. XML, HTML)?

See L<Perinci::Result::Format>.


=head1 SEE ALSO

L<Perinci>, L<Rinci>, L<Riap>.

Other CPAN modules to write command-line applications: L<App::Cmd>, L<App::Rad>,
L<MooseX::Getopt>.

=cut
