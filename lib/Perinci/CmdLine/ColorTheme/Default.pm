package Perinci::CmdLine::ColorTheme::Default;

use 5.010;
use strict;
use warnings;

# VERSION

our %color_themes = (

    no_color => {
        v => 1.1,
        summary => 'Special theme that means no color',
        colors => {
        },
        no_color => 1,
    },

    default => {
        v => 1.1,
        summary => 'Default (for terminals with black background)',
        colors => {
            heading       => 'ff9933',
            text          => undef,
            error_label   => 'cc0000',
            warning_label => 'cccc00',
            program_name  => {ansi_fg=>"\e[1m"}, # bold
            option_name   => 'cc6633',
            #option_value  => undef,
            #argument      => undef,
        },
    },

    default_whitebg => {
        v => 1.1,
        summary => 'Default (for terminals with white background)',
        colors => {
        },
    },

);

1;
# ABSTRACT: Default color themes
