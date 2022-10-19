package ColorTheme::Perinci::CmdLine::Classic::Default;

use strict;
use warnings;
use parent 'ColorThemeBase::Static';

# AUTHORITY
# DATE
# DIST
# VERSION

our %THEME = (
    v => 2,
    summary => 'Default color theme for Perinci::CmdLine::Classic (for terminals with black background)',
    items => {
        heading       => 'ff9933',
        text          => undef,
        error_label   => 'cc0000',
        warning_label => 'cccc00',
        program_name  => {ansi_fg=>"\e[1m"}, # bold
        option_name   => 'cc6633',
        emphasis      => {ansi_fg=>"\e[1m"}, # bold
        #option_value  => undef,
        #argument      => undef,
    },
);

1;
# ABSTRACT:
