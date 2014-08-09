package Perinci::CmdLine::Role::Help;

# DATE
# VERSION

# split here just so it's more organized

use 5.010;
use Moo::Role;

use Locale::TextDomain::UTF8 'Perinci-CmdLine';
use Perinci::Object;
use Perinci::ToUtil;

sub _help_draw_curtbl {
    my ($self, $r) = @_;

    if ($r->{_help_curtbl}) {
        print $r->{_help_curtbl}->draw;
        undef $r->{_help_curtbl};
    }
}

# ansitables are used to draw formatted help. they are 100% wide, with no
# borders (except space), but you can customize the number of columns (which
# will be divided equally)
sub _help_add_table {
    require Text::ANSITable;

    my ($self, $r, %args) = @_;
    my $columns = $args{columns} // 1;

    $self->_help_draw_curtbl($r);
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

    $r->{_help_curtbl} = $t;
}

sub _help_add_row {
    my ($self, $r, $row, $args) = @_;
    $args //= {};
    my $wrap    = $args->{wrap}   // 0;
    my $indent  = $args->{indent} // 0;
    my $columns = @$row;

    # start a new table if necessary
    $self->_help_add_table(
        $r,
        columns=>$columns, column_widths=>$args->{column_widths})
        if !$r->{_help_curtbl} ||
            $columns != @{ $r->{_help_curtbl}{columns} };

    my $t = $r->{_help_curtbl};
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
    my ($self, $r, $heading) = @_;
    $self->_help_add_row($r, [$self->_color('heading', $heading)]);
}

sub _color {
    my ($self, $color_name, $text) = @_;
    my $color_code = $color_name ?
        $self->get_theme_color_as_ansi($color_name) : "";
    my $reset_code = $color_code ? "\e[0m" : "";
    "$color_code$text$reset_code";
}

sub help_section_summary {
    my ($self, $r) = @_;

    my $summary = rimeta($r->{_help_meta})->langprop("summary");
    return unless $summary;

    my $name = $self->get_program_and_subcommand_name;
    my $ct = join(
        "",
        $self->_color('program_name', $name),
        ($name && $summary ? ' - ' : ''),
        $summary // "",
    );
    $self->_help_add_row($r, [$ct], {wrap=>1});
}

sub _usage_args {
    my ($self, $r) = @_;

    my $m = $r->{_help_meta};
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
    my ($self, $r) = @_;

    my $co = $self->common_opts;
    my @con = grep {
        my $cov = $co->{$_};
        my $show = $cov->{show_in_usage} // 1;
        for ($show) { if (ref($_) eq 'CODE') { $_ = $_->($self, $r) } }
        $show;
    } sort {
        ($co->{$a}{order}//1) <=> ($co->{$b}{order}//1) || $a cmp $b
    } keys %$co;

    my $pn = $self->_color(
        'program_name', $self->get_program_and_subcommand_name);
    my $ct = "";
    for my $con (@con) {
        my $cov = $co->{$con};
        next unless $cov->{usage};
        $ct .= ($ct ? "\n" : "") . $pn . " " . __($cov->{usage});
    }
    if ($self->subcommands && !$r->{subcommand_data}) {
        if (defined $self->default_subcommand) {
            $ct .= ($ct ? "\n" : "") . $pn .
                " " . __("--cmd=<other-subcommand> [options]");
        } else {
            $ct .= ($ct ? "\n" : "") . $pn .
                " " . __("<subcommand> [options]");
        }
    } else {
            $ct .= ($ct ? "\n" : "") . $pn .
                " " . __("[options]"). $self->_usage_args($r);
    }
    $self->_help_add_heading($r, __("Usage"));
    $self->_help_add_row($r, [$ct], {indent=>1});
}

sub help_section_options {
    require Getopt::Long::Util;

    my ($self, $r) = @_;
    my $verbose = $r->{_help_verbose};
    my $info = $r->{_help_info};
    my $meta = $r->{_help_meta};
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
                " (default: ".Perinci::CmdLine::__json_decode($s->[1]{default}).")" : "";
            my $src = $a->{cmdline_src} // "";
            my $in;
            if ($s->[1]{in} && @{ $s->[1]{in} }) {
                $in = Perinci::CmdLine::__json_decode($s->[1]{in});
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
        $self->_help_add_heading($r, $cat);
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
                $self->_help_add_row($r, [$ct], {indent=>1});
                if ($o->{in} || $o->{summary} || $o->{description}) {
                    my $ct = "";
                    $ct .= ($ct ? "\n\n":"").ucfirst(__("value in")).
                        ": $o->{in}" if $o->{in};
                    $ct .= ($ct ? "\n\n":"")."$o->{summary}." if $o->{summary};
                    $ct .= ($ct ? "\n\n":"").$o->{description}
                        if $o->{description};
                    $self->_help_add_row($r, [$ct], {indent=>2, wrap=>1});
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
                $self->_help_add_row($r, \@row, {indent=>1});
            }
        }
    }
}

sub help_section_subcommands {
    my ($self, $r) = @_;

    my $verbose = $r->{_help_verbose};
    my $scs = $self->subcommands;
    return unless $scs && !$r->{subcommand_data};

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
    $r->{_help_hide_some_subcommands} = 1 if $some_not_shown;

    $self->_help_add_heading(
        $r, $some_not_shown ? __("Popular subcommands") : __("Subcommands"));

    # in compact mode, we try to not exceed one screen, so show long mode only
    # if there are a few subcommands.
    my $long_mode = $verbose || @shown_scs < 12;
    if ($long_mode) {
        for (@shown_scs) {
            my $summary = rimeta($_)->langprop("summary");
            $self->_help_add_row(
                $r,
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
                $self->_help_add_row($r, \@row, {indent=>1});
            }

    }
}

sub help_section_hints {
    my ($self, $r) = @_;

    my $verbose = $r->{_help_verbose};
    my @hints;
    unless ($verbose) {
        push @hints, N__("For more complete help, use '--help --verbose'");
    }
    if ($self->{_help_hide_some_subcommands}) {
        push @hints,
            N__("To see all available subcommands, use '--subcommands'");
    }
    return unless @hints;

    $self->_help_add_row(
        $r, [join(" ", map { __($_)."." } @hints)], {wrap=>1});
}

sub help_section_description {
    my ($self, $r) = @_;

    my $desc = rimeta($r->{_help_meta})->langprop("description") //
        $self->description;
    return unless $desc;

    $self->_help_add_heading($r, __("Description"));
    $self->_help_add_row($r, [$desc], {wrap=>1, indent=>1});
}

sub help_section_examples {
    my ($self, $r) = @_;

    my $verbose = $r->{_help_verbose};
    my $meta = $r->{_help_meta};
    my $egs = $meta->{examples};
    return unless $egs && @$egs;

    $self->_help_add_heading($r, __("Examples"));
    my $pn = $self->_color(
        'program_name', $self->get_program_and_subcommand_name);
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
        $self->_help_add_row($r, [$ct], {indent=>1});
        if ($verbose) {
            $ct = "";
            my $summary = rimeta($eg)->langprop('summary');
            if ($summary) { $ct .= "$summary." }
            my $desc = rimeta($eg)->langprop('description');
            if ($desc) { $ct .= "\n\n$desc" }
            $self->_help_add_row($r, [$ct], {indent=>2}) if $ct;
        }
    }
}

sub help_section_result {
    my ($self, $r) = @_;

    my $meta   = $r->{_help_meta};
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

    $self->_help_add_heading($r, __("Result"));
    $self->_help_add_row($r, [$text], {wrap=>1, indent=>1});
}

sub help_section_links {
    # not yet
}

sub run_help {
    my ($self, $r) = @_;

    my $verbose = $ENV{VERBOSE} // 0;
    local $r->{_help_verbose} = $verbose;

    # get function metadata first
    my $sc = $self->{_subcommand};
    my $url = $sc ? $sc->{url} : $self->url;
    if ($url) {
        my $res = $self->riap_client->request(info => $url);
        $self->_err("Can't info '$url': $res->[0] - $res->[1]")
            unless $res->[0] == 200;
        $r->{_help_info} = $res->[2];
        $res = $self->riap_client->request(meta => $url);
        $self->_err("Can't meta '$url': $res->[0] - $res->[1]")
            unless $res->[0] == 200;
        $r->{_help_meta} = $res->[2];
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
        #$log->tracef("=> $meth()");
        $self->$meth($r);
    }
    $self->_help_draw_curtbl($r);
    0;
}

1;
# ABSTRACT: Help-related routines

=for Pod::Coverage ^(.+)$

=head1 REQUEST KEYS

=over

=item * _help_*

Temporary. Various data stored during help generation that is passed between the
various C<_help_*> methods.

=back
