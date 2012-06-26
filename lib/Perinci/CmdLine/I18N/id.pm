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
"    --format=FMT    Choose output format\n"
msgstr ""
"Opsi umum:\n"
"\n"
"    --format=FMT    Pilih format hasil\n"

msgid  ""
"Undo options:\n"
"\n"
"    --undo <ID>     Undo previous action (use --history to get IDs)\n"
"    --redo <ID>     Redo previous undo action (use --history to get IDs)\n"
"    --history       List actions history\n"
"    --clear-history Clear actions history\n"
msgstr ""
"Opsi undo:\n"
"\n"
"    --undo <ID>     Batalkan tindakan sebelumnya (ID dari --history)\n"
"    --redo <ID>     Ulangi tindakan sebelumnya (ID dari --history)\n"
"    --history       Daftar sejarah tindakan sebelumnya\n"
"    --clear-history Bersihkan daftar sejarah tindakan\n"

msgid  "Special options"
msgstr "Opsi khusus"

# --dry-run
msgid  "Run in simulation mode (can also be set via environment DRY_RUN=1)"
msgstr "Modus simulasi (juga bisa lewat variabel lingkungan DRY_RUN=1)"

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

