package Perinci::CmdLine::I18N::id;
use parent qw(Perinci::CmdLine::I18N Perinci::To::Text::I18N::id);

use Locale::Maketext::Lexicon::Gettext;
our %Lexicon = %{ Locale::Maketext::Lexicon::Gettext->parse(<DATA>) };

# VERSION

#use Data::Dump; dd \%Lexicon;

1;
# ABSTRACT: Indonesian translation for Perinci::CmdLine
__DATA__

# list

msgid  "There are no subcommands"
msgstr "Tidak ada subperintah"

msgid  "List of available %1 subcommands"
msgstr "Daftar subperintah kategori '%1'"

# version

msgid  "%1 version %2"
msgstr "%1 versi %2"

# usage

msgid ""
"Usage:\n"
"\n"
"    %1 --help (or -h, -?)\n"
"    %1 --version (or -v)\n"
"    %1 (common options) (options)\n"
msgstr ""
"Cara pakai:\n"
"\n"
"    %1 --help (atau -h, -?)\n"
"    %1 --version (atau -v)\n"
"    %1 [opsi umum] [opsi]\n"

msgid ""
"Usage:\n"
"\n"
"    %1 --help (or -h, -?)\n"
"    %1 --version (or -v)\n"
"    %1 --list (or -l)\n"
"    %1 SUBCOMMAND (common options) (options)\n"
msgstr ""
"Cara pakai:\n"
"\n"
"    %1 --help (atau -h, -?)\n"
"    %1 --version (atau -v)\n"
"    %1 --list (atau -l)\n"
"    %1 SUBPERINTAH [opsi umum] [opsi]\n"

msgid ""
"Common options:\n"
"\n"
"    --yaml, -y      Format result as YAML\n"
"    --json, -j      Format result as JSON\n"
"    --pretty, -p    Format result as pretty formatted text\n"
"    --nopretty, -P  Format result as simple formatted text\n"
"    --text         (Default) Select --pretty, or --nopretty when run piped\n"
msgstr ""
"Opsi umum:\n"
"\n"
"    --yaml, -y      Tampilkan hasil sebagai YAML\n"
"    --json, -j      Tampilkan hasil sebagai JSON\n"
"    --pretty, -p    Tampilkan hasil sebagai teks cantik/rapi (tabel, dll)\n"
"    --nopretty, -P  Tampilkan hasil sebagai teks sederhana\n"
"    --text         (Default) Pakai --pretty, atau --nopretty jika dipipa\n"

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

msgid  "Options"
msgstr "Opsi"

msgid  "For general help, type '%1'"
msgstr "Untuk pesan bantuan umum, ketik '%1'"

msgid  "For help on a subcommand, type '%1'"
msgstr "Untuk pesan bantuan subperintah tertentu, ketik '%1'"

# usage/function

msgid  "or as argument #%1"
msgstr "atau sebagai argumen #%1"

