package Perinci::CmdLine::I18N::id;
use parent qw(Perinci::CmdLine::I18N Perinci::To::Text::I18N::id);

use Locale::Maketext::Lexicon::Gettext;
our %Lexicon = %{ Locale::Maketext::Lexicon::Gettext->parse(<DATA>) };

# VERSION

#use Data::Dump; dd \%Lexicon;

1;
# ABSTRACT: Indonesian translation for Perinci::CmdLine
__DATA__

# version

msgid  "%1 version %2"
msgstr "%1 versi %2"

# usage

msgid  "Show version"
msgstr "Tampilkan versi"

msgid  "Display this help message"
msgstr "Tampilkan pesan bantuan ini"

msgid  "List available subcommands"
msgstr "Daftar subperintah yang ada"

msgid  "List of available subcommands"
msgstr "Daftar subperintah yang ada"

msgid  "Subcommand"
msgstr "Subperintah"

msgid  "Subcommands"
msgstr "Subperintah"

msgid  "Usage"
msgstr "Cara pakai"

msgid  "For general help, type '%1'"
msgstr "Untuk pesan bantuan umum, ketik '%1'"

msgid  "For help on a subcommand, type '%1'"
msgstr "Untuk pesan bantuan subperintah tertentu, ketik '%1'"

# usage/function

msgid  "or as argument #%1"
msgstr "atau sebagai argumen #%1"

