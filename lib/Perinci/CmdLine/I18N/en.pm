package Perinci::CmdLine::I18N::en;
use parent qw(Perinci::CmdLine::I18N Perinci::To::Text::I18N::en);

use Locale::Maketext::Lexicon::Gettext;
our %Lexicon = %{ Locale::Maketext::Lexicon::Gettext->parse(<DATA>) };

# VERSION

#use Data::Dump; dd \%Lexicon;

1;
# ABSTRACT: English translation for Perinci::CmdLine
__DATA__

### action: list

msgid  "There are no subcommands"
msgstr "There are no subcommands"

msgid  "List of available %1 subcommands"
msgstr "List of available %1 subcommands"

### action: version

msgid  "%1 version %2"
msgstr "%1 version %2"

### action: help

msgid  "Usage"
msgstr "Usage"

msgid  "--help (or -h, -?) [--verbose]"
msgstr "--help (or -h, -?) [--verbose]"

msgid  "--version (or -v)"
msgstr "--version (or -v)"

msgid  "--list (or -l)"
msgstr "--list (or -l)"

# usage when there are no subcommands
msgid  "(options)"
msgstr "[options]"

# help when there are subcommands and no default subcommand
msgid  "SUBCOMMAND (options)"
msgstr "SUBCOMMAND [options]"

# help when there are subcommands and there is a default subcommand
msgid  "--cmd=OTHER_SUBCOMMAND (options)"
msgstr "--cmd=OTHER_SUBCOMMAND [options]"

msgid  "Options"
msgstr "Options"

msgid  "Common options"
msgstr "Common options"

msgid  "Undo options"
msgstr "Undo options"

msgid  "required"
msgstr "required"

# --format
msgid  "Choose output format, e.g. json, text"
msgstr "Choose output format, e.g. json, text"

# --format-options
msgid  "Pass options to formatter"
msgstr "Pass options to formatter"

# --undo
msgid  "Undo previous action"
msgstr "Undo previous action"

# --redo
msgid  "Redo previous undone action"
msgstr "Redo previous undone action"

# --history
msgid  "List actions history"
msgstr "List actions history"

# --clear-history
msgid  "Clear actions history"
msgstr "Clear actions history"

# --dry-run
msgid  "Run in simulation mode (also via DRY_RUN=1)"
msgstr "Run in simulation mode (also via DRY_RUN=1)"

# --version
msgid  "Show version"
msgstr "Show version"

# --help
msgid  "Display this help message"
msgstr "Display this help message"

# --list
msgid  "List available subcommands"
msgstr "List available subcommands"

# --cmd
msgid  "Select subcommand"
msgstr "Select subcommand"

# --action currently undocumented

# --quiet, --verbose, --debug, --trace, --log-level
msgid  "Set log level to quiet"
msgstr "Set log level to quiet (error and up)"

msgid  "Set log level to verbose"
msgstr "Set log level to verbose (info and up)"

msgid  "Set log level to debug"
msgstr "Set log level to debug and up"

msgid  "Set log level to trace"
msgstr "Set log level to trace and up"

msgid  "Set log level"
msgstr "Set log level"


msgid  "List of available subcommands"
msgstr "List of available subcommands"

msgid  "Subcommand"
msgstr "Subcommand"

msgid  "Subcommands"
msgstr "Subcommands"

msgid  "For general help, type '%1'"
msgstr "For general help, type '%1'"

msgid  "For help on a subcommand, type '%1'"
msgstr "For help on a subcommand, type '%1'"

msgid  "For more complete help, try '--help --verbose'"
msgstr "For more complete help, try '--help --verbose'"

# usage/function

msgid  "or as argument #%1"
msgstr "or as argument #%1"

msgid  "or from stdin"
msgstr "or from stdin"

msgid  "or from stdin/files"
msgstr "or from stdin/files"

# etc

msgid  "value in"
msgstr "value in"
