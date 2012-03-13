package Perinci::To::Usage;

use 5.010;
use Log::Any '$log';
use Moo;

extends 'Perinci::To::Text';

has cmdline => (is => 'rw'); # reference to Perinci::CmdLine object

sub BUILD {
    require Perinci::CmdLine;

    my ($self, $args) = @_;
    unless ($self->cmdline) {
        $self->cmdline(Perinci::CmdLine->new(url=>$self->url));
    }

    $self->{sections} = $args->{sections} // [
        'summary',
        'usage',
        'subcommands',
        'description',
        'links',
    ];

    $self->{function_sections} = $args->{function_sections} // [
        'summary',
        'usage',
        'description',
        'examples',
        'links',
    ];

    $self->{loc_class} //= "Perinci::CmdLine::I18N";
}

sub gen_summary {
    my ($self) = @_;

    my $name    = $self->cmdline->program_name;
    if (!$name || $name eq '-e') {
        $name = $self->_parse->{name} // "";
    }
    my $summary = $self->_parse->{summary} // "";

    my $name_summary = join(
        "",
        $name,
        $name && $summary ? " - " : "",
        $summary
    );

    $self->add_lines($name_summary, "");
}

sub gen_description {
    my ($self) = @_;

    return unless $self->_parse->{description};

    $self->add_lines($self->_parse->{description}, "");
}

sub parse_usage {}

sub gen_usage {}

sub _gen_function {

}

sub fparse_usage {}

sub fgen_usage {}

sub parse_subcommands {
    my ($self) = @_;
    $self->parse_functions;
}

sub gen_subcommands {
    my ($self) = @_;
    my $cmd = $self->cmdline;
    my $scs = $cmd->subcommands;
    my $pff = $self->_parse->{functions};

    return unless keys %$scs;

    $self->add_lines($self->loc("List of available subcommands") . ':', "");
    $self->inc_indent;

   # XXX categorize based on tags
    if ($scs) {
        for my $scn (keys %$scs) {
            my $sc  = $scs->{$scn};
            my $url = $sc->{url};
            my $pf  = $pff->{$url};
            my $summary;
            $summary = $self->_get_langprop($sc, "summary");
            if (!$summary && $pf->{schema}) {
                $summary = $self->_get_langprop($pf->{schema}, "summary");
            }
            $self->add_lines({wrap=>0}, $scn . ($summary ? " - $summary" : ""));
        }
    }

    $self->dec_indent;
}

1;
# ABSTRACT: Generate usage (--help) message from Rinci metadata

=head1 DESCRIPTION

=cut
