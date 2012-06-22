package Perinci::CmdLine::I18N::en;
use parent qw(Perinci::CmdLine::I18N Perinci::To::Text::I18N::en);

use Locale::Maketext::Lexicon::Gettext;
our %Lexicon = %{ Locale::Maketext::Lexicon::Gettext->parse(<DATA>) };

# VERSION

#use Data::Dump; dd \%Lexicon;

1;
# ABSTRACT: English translation for Perinci::CmdLine
__DATA__

# list

msgid  "There are no subcommands"
msgstr "There are no subcommands"

msgid  "List of available %1 subcommands"
msgstr "List of available %1 subcommands"

# version

msgid  "%1 version %2"
msgstr "%1 version %2"

# usage

msgid  ""
"Usage:\n"
"\n"
"    %1 --help (or -h, -?)\n"
"    %1 --version (or -v)\n"
"    %1 (common options) (options)\n"
msgstr ""
"Usage:\n"
"\n"
"    %1 --help (or -h, -?)\n"
"    %1 --version (or -v)\n"
"    %1 [common options] [options]\n"

msgid  ""
"Usage:\n"
"\n"
"    %1 --help (or -h, -?)\n"
"    %1 --version (or -v)\n"
"    %1 --list (or -l)\n"
"    %1 SUBCOMMAND (common options) (options)\n"
msgstr ""
"Usage:\n"
"\n"
"    %1 --help (or -h, -?)\n"
"    %1 --version (or -v)\n"
"    %1 --list (or -l)\n"
"    %1 SUBCOMMAND [common options] [options]\n"

msgid  ""
"Common options:\n"
"\n"
"    --yaml, -y      Format result as YAML\n"
"    --json, -j      Format result as JSON\n"
"    --text-pretty   Format result as pretty formatted text\n"
"    --text-simple   Format result as simple formatted text\n"
"    --text         (Default) Use --text-pretty, or --text-simple when run piped\n"
"    --format=FMT    Choose output format\n"
msgstr ""
"Common options:\n"
"\n"
"    --yaml, -y      Format result as YAML\n"
"    --json, -j      Format result as JSON\n"
"    --text-pretty   Format result as pretty formatted text\n"
"    --text-simple   Format result as simple formatted text\n"
"    --text         (Default) Use --text-pretty, or --text-simple when run piped\n"
"    --format=FMT    Choose output format\n"

msgid  ""
"Undo options:\n"
"\n"
"    --undo <ID>     Undo previous action (use --list-history to get IDs)\n"
"    --redo <ID>     Redo previous undo action (use --list-history to get IDs)\n"
"    --list-history  List actions history\n"
"    --clear-history Clear actions history\n"
msgstr ""
"Undo options:\n"
"\n"
"    --undo <ID>     Undo previous action (use --list-history to get IDs)\n"
"    --redo <ID>     Redo previous undo action (use --list-history to get IDs)\n"
"    --list-history  List actions history\n"
"    --clear-history Clear actions history\n"

msgid  "Show version"
msgstr "Show version"

msgid  "Display this help message"
msgstr "Display this help message"

msgid  "List available subcommands"
msgstr "List available subcommands"

msgid  "List of available subcommands"
msgstr "List of available subcommands"

msgid  "Subcommand"
msgstr "Subcommand"

msgid  "Subcommands"
msgstr "Subcommands"

msgid  "Usage"
msgstr "Usage"

msgid  "Options"
msgstr "Options"

msgid  "For general help, type '%1'"
msgstr "For general help, type '%1'"

msgid  "For help on a subcommand, type '%1'"
msgstr "For help on a subcommand, type '%1'"

# usage/function

msgid  "or as argument #%1"
msgstr "or as argument #%1"

