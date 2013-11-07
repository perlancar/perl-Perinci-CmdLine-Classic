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

msgid  "%1 subcommands"
msgstr "Subperintah kategori '%1'"

### action: version

msgid  "%1 version %2"
msgstr "%1 versi %2"

### action: help

msgid  "Usage"
msgstr "Cara pakai"

msgid  "--help (or -h, -?) (--verbose)"
msgstr "--help (atau -h, -?) [--verbose]"

msgid  "--version (or -v)"
msgstr "--version (atau -v)"

msgid  "--subcommand"
msgstr "--subcommand"

# usage when there are no subcommands
msgid  "(options)"
msgstr "[opsi]"

# help when there are subcommands and no default subcommand
msgid  "<subcommand> (options)"
msgstr "<subperintah> [opsi]"

# help when there are subcommands and there is a default subcommand
msgid  "--cmd=<other-subcommand> (options)"
msgstr "--cmd=<subperintah-lain> [opsi]"

msgid  "Options"
msgstr "Opsi"

msgid  "Common options"
msgstr "Opsi umum"

msgid  "Undo options"
msgstr "Opsi pembatalan"

msgid  "%1 options"
msgstr "Opsi kategori '%1'"

msgid  "required"
msgstr "wajib"

msgid  "Examples"
msgstr "Contoh"

msgid  "Links"
msgstr "Tautan"

# --format
msgid  "Choose output format, e.g. json, text"
msgstr "Pilih format hasil, mis: json, text"

# --format-options
msgid  "Pass options to formatter"
msgstr "Berikan opsi pada pemformat"

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

# --subcommand
msgid  "List available subcommands"
msgstr "Daftar subperintah yang ada"

# --cmd
msgid  "Select subcommand"
msgstr "Pilih subperintah"

# --action currently undocumented

# --quiet, --verbose, --debug, --trace, --log-level
msgid  "Set log level to quiet"
msgstr "Set level log ke diam (error ke atas)"

msgid  "Set log level to verbose"
msgstr "Set level log ke riuh (info ke atas)"

msgid  "Set log level to debug"
msgstr "Set level log ke debug ke atas"

msgid  "Set log level to trace"
msgstr "Set level log ke trace ke atas"

msgid  "Set log level"
msgstr "Set level log"


msgid  "Subcommand"
msgstr "Subperintah"

msgid  "Subcommands"
msgstr "Subperintah"

msgid  "Popular subcommands"
msgstr "Subperintah popular"

msgid  "For general help, use '%1'"
msgstr "Untuk pesan bantuan umum, ketik '%1'"

msgid  "For help on a subcommand, use '%1'"
msgstr "Untuk pesan bantuan subperintah tertentu, ketik '%1'"

msgid  "For more complete help, use '--help --verbose'"
msgstr "Untuk pesan bantuan lebih lengkap, gunakan '--help --verbose'"

msgid  "To see all available subcommands, use '--subcommands'"
msgstr "Untuk melihat semua subperintah yang ada, gunakan '--subcommands'"

# usage/function

msgid  "or as argument #%1"
msgstr "atau sebagai argumen #%1"

msgid  "from stdin"
msgstr "dari masukan standar"

msgid  "from file"
msgstr "dari berkas"

msgid  "from stdin/files"
msgstr "dari berkas/masukan standar"

# etc

msgid  "value in"
msgstr "nilai salah satu dari"

# error messages

msgid  "Argument %1 must be set to '-' which means from stdin"
msgstr "Argumen %1 harus diset '-' yang berarti dari masukan standar"
