package Perinci::CmdLine::I18N::id;
use parent qw(Perinci::CmdLine::I18N Perinci::To::Text::I18N::id);

use Locale::Maketext::Lexicon::Gettext;
our %Lexicon = %{ Locale::Maketext::Lexicon::Gettext->parse(<DATA>) };

# VERSION

#use Data::Dump; dd \%Lexicon;

1;
# ABSTRACT: Indonesian translation for Perinci::CmdLine
__DATA__

### action: list

msgid  "There are no subcommands"
msgstr "Tidak ada subperintah"

msgid  "List of available %1 subcommands"
msgstr "Daftar subperintah kategori '%1'"

### action: version

msgid  "%1 version %2"
msgstr "%1 versi %2"

### action: help

msgid  "Usage"
msgstr "Cara pakai"

msgid  "%1 --help (or -h, -?)"
msgstr "%1 --help (atau -h, -?)"

msgid  "%1 --version (or -v)"
msgstr "%1 --version (atau -v)"

msgid  "%1 --list (or -l)"
msgstr "%1 --list (atau -l)"

# usage when there are no subcommands
msgid  "%1 (options)"
msgstr "%1 [opsi]"

# help when there are subcommands and no default subcommand
msgid  "%1 (common options) SUBCOMMAND (options)"
msgstr "%1 [opsi umum] SUBPERINTAH [opsi]"

# help when there are subcommands and there is a default subcommand
msgid  "%1 (common options) (options)"
msgstr "%1 [opsi umm] [opsi]"

msgid  "Options"
msgstr "Opsi"

msgid  "Common options"
msgstr "Opsi umum"

msgid  "Undo options"
msgstr "Opsi undo"

# --format
msgid  "Choose output format, e.g. json, text"
msgstr "Pilih format hasil, mis: json, text"

# --undo
msgid  "Undo previous action"
msgstr "Batalkan tindakan sebelumnya"

# --redo
msgid  "Redo previous undone action"
msgstr "Batalkan pembatalan sebelumnya"

# --history
msgid  "List actions history"
msgstr "Daftar sejarah tindakan sebelumnya"

# --clear-history
msgid  "Clear actions history"
msgstr "Bersihkan daftar sejarah tindakan"

# --dry-run
msgid  "Run in simulation mode (also via DRY_RUN=1)"
msgstr "Modus simulasi (bisa juga dengan DRY_RUN=1)"

# --version
msgid  "Show version"
msgstr "Tampilkan versi"

# --help
msgid  "Display this help message"
msgstr "Tampilkan pesan bantuan ini"

# --list
msgid  "List available subcommands"
msgstr "Daftar subperintah yang ada"

msgid  "List of available subcommands"
msgstr "Daftar subperintah yang ada"

msgid  "Subcommand"
msgstr "Subperintah"

msgid  "Subcommands"
msgstr "Subperintah"

msgid  "For general help, type '%1'"
msgstr "Untuk pesan bantuan umum, ketik '%1'"

msgid  "For help on a subcommand, type '%1'"
msgstr "Untuk pesan bantuan subperintah tertentu, ketik '%1'"

# usage/function

msgid  "or as argument #%1"
msgstr "atau sebagai argumen #%1"

msgid  "or from stdin"
msgstr "atau dari masukan standar"

msgid  "or from stdin/files"
msgstr "atau dari berkas/masukan standar"

