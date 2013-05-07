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

msgid  "%1 --help (or -h, -?)"
msgstr "%1 --help (or -h, -?)"

msgid  "%1 --version (or -v)"
msgstr "%1 --version (or -v)"

msgid  "%1 --list (or -l)"
msgstr "%1 --list (or -l)"

# usage when there are no subcommands
msgid  "%1 (options)"
msgstr "%1 [options]"

# help when there are subcommands and no default subcommand
msgid  "%1 (common options) SUBCOMMAND (options)"
msgstr "%1 [common options] SUBCOMMAND [options]"

# help when there are subcommands and there is a default subcommand
msgid  "%1 (common options) (options)"
msgstr "%1 [common options] [options]"

msgid  "Options"
msgstr "Options"

msgid  "Common options"
msgstr "Common options"

msgid  "Undo options"
msgstr "Undo options"

# --format
msgid  "Choose output format, e.g. json, text"
msgstr "Choose output format, e.g. json, text"

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

# usage/function

msgid  "or as argument #%1"
msgstr "or as argument #%1"

msgid  "or from stdin"
msgstr "or from stdin"

msgid  "or from stdin/files"
msgstr "or from stdin/files"

