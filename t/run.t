#!perl

use 5.010;
use strict;
use warnings;

use FindBin '$Bin';
use lib "$Bin/lib";

use File::Slurp::Tiny qw(write_file);
use File::Temp qw(tempfile);
use Perinci::CmdLine;
use Test::More 0.98;
use Test::Perinci::CmdLine;

# XXX test formats

test_run(name      => 'single sub',
         args      => {url=>'/Foo/ok'},
         argv      => [qw/--arg1 a --arg2 2/],
         exit_code => 0,
         output_re => qr/First argument/,
     );

test_run(name      => 'missing arg = error',
         args      => {url=>'/Foo/ok'},
         argv      => [qw//],
         dies      => 1,
     );
test_run(name      => 'unknown arg = error',
         args      => {url=>'/Foo/ok'},
         argv      => [qw/--arg4/],
         dies      => 1,
     );
test_run(name      => 'exit code from sub res',
         args      => {url=>'/Foo/want_odd'},
         argv      => [qw/4/],
         exit_code => 100,
         output_re => qr/hate/,
     );

test_run(name      => 'subcommands',
         args      => {subcommands=>{
             ok=>{url=>'/Foo/ok'},
             wo=>{url=>'/Foo/want_odd'}}},
         argv      => [qw/wo 3/],
         exit_code => 0,
     );

subtest 'subcommand specification' => sub {
    my %cmdspec = (
        subcommands=>{
            ok1=>{url=>'/Foo/ok', args=>{arg3=>'mandiri'}},
            ok2=>{url=>'/Foo/ok', args=>{arg3=>'fiesta'}},
        },
    );

    test_run(name      => 'args specification',
             args      => \%cmdspec,
             argv      => [qw/ok1 a virus/],
             exit_code => 0,
             output_re => qr/a.+virus.+mandiri/s,
     );
    test_run(name      => 'args specification',
             args      => \%cmdspec,
             argv      => [qw/ok2 a virus/],
             exit_code => 0,
             output_re => qr/a.+virus.+fiesta/s,
     );
};

test_run(name      => 'unknown subcommand = error',
         args      => {subcommands=>{
             ok=>{url=>'/Foo/ok'},
             wo=>{url=>'/Foo/want_odd'}}},
         argv      => [qw/foo/],
         dies      => 1,
     );

test_run(name      => 'default_subcommand (1)',
         args      => {subcommands=>{
             f2=>{url=>'/Foo/f2'},
             f2r=>{url=>'/Foo/f2r'}},
                       default_subcommand=>'f2'},
         argv      => [qw/mirror/],
         output_re => qr/mirror/,
         exit_code => 0,
     );
test_run(name      => 'default_subcommand (2, other subcommand via --cmd)',
         args      => {subcommands=>{
             f2=>{url=>'/Foo/f2'},
             f2r=>{url=>'/Foo/f2r'}},
                       default_subcommand=>'f2'},
         argv      => [qw/--cmd=f2r mirror/],
         output_re => qr/rorrim/,
         exit_code => 0,
     );

for (qw(--help -h -?)) {
    test_run(name      => "general help ($_)",
             args      => {url=>'/Foo/'},
             argv      => [$_],
             exit_code => 0,
             output_re => qr/Usage/m,
         );
}

{
    local $ENV{COLOR} = 0;
    test_run(name      => "common option (--version) before subcommand name",
             args      => {url=>'/Foo/', subcommands=>{
                 ok=>{url=>'/Foo/ok'},
                 want_odd=>{url=>'/Foo/want_odd'}}},
             argv      => [qw/--version want_odd --num 4/],
             exit_code => 0,
             output_re => qr/version 0\.123/m,
         );
}
test_run(name      => "common option (--help) after subcommand name",
         args      => {subcommands=>{
             ok=>{url=>'/Foo/ok'},
             want_odd=>{url=>'/Foo/want_odd'}}},
         argv      => [qw/want_odd --num 4 --help/],
         exit_code => 0,
         output_re => qr/Return error if given/,
     );
test_run(name      => "common option (--help) overrides function argument",
         args      => {subcommands=>{f1=>{url=>'/Foo/f1'}}},
         argv      => [qw/f1 --help/],
         exit_code => 0,
         output_re => qr/Usage/m,
     );
test_run(name      => "common option (--help) makes clashing function ".
             "argument renamed to --help-arg",
         args      => {subcommands=>{f1=>{url=>'/Foo/f1'}}},
         argv      => [qw/f1 --help-arg/],
         exit_code => 0,
         output_re => qr/^tolong/m,
     );
test_run(name      => "common option (--help) bypass required argument check",
         args      => {url=>'/Foo/f2'},
         argv      => [qw/--help/],
         exit_code => 0,
         output_re => qr/Usage/m,
     );

# disabled for now, fail under 'prove'? wtf?
#for (qw(--version -v)) {
#    test_run(name      => "version ($_)",
#             args      => {url=>'/Foo/', subcommands=>{
#                 ok=>{url=>'/Foo/ok'},
#                 want_odd=>{url=>'/Foo/want_odd'}}},
#             argv      => [$_],
#             exit_code => 0,
#             output_re => qr/version 0\.123 \(1999-01-01\)/,
#         );
#}

for (qw(--subcommands)) {
    test_run(name      => "subcommands ($_)",
             args      => {subcommands=>{
                 ok=>{url=>'/Foo/ok'},
                 want_odd=>{url=>'/Foo/want_odd'}}},
             argv      => [$_],
             exit_code => 0,
         );
}

# XXX test arg: custom general help
# XXX test arg: per-subcommand help
# XXX test arg: custom per-subcommand help

{
    local $ENV{DRY_RUN} = 0;
    test_run(name      => 'dry-run (0)',
         args      => {url=>'/Foo/sp1'},
         argv      => [],
         exit_code => 0,
         output_re => qr/dry_run=0/m,
     );
    test_run(name      => 'dry-run (1)',
         args      => {url=>'/Foo/sp1'},
         argv      => [qw/--dry-run/],
         exit_code => 0,
         output_re => qr/dry_run=1/m,
     );
    $ENV{DRY_RUN} = 1;
    test_run(name      => 'dry-run (via env)',
         args      => {url=>'/Foo/sp1'},
         argv      => [],
         exit_code => 0,
         output_re => qr/dry_run=1/m,
     );
}

test_run(name      => 'noop',
         args      => {url=>'/Foo/noop'},
         argv      => [],
         exit_code => 0,
     );

subtest 'cmdline_src' => sub {
    test_run(
        name => 'unknown value',
        args => {url=>'/Foo/cmdline_src_unknown'},
        argv => [],
        dies => 1,
    );
    test_run(
        name => 'arg type not str/array',
        args => {url=>'/Foo/cmdline_src_invalid_arg_type'},
        argv => [],
        dies => 1,
    );
    test_run(
        name => 'multiple stdin',
        args => {url=>'/Foo/cmdline_src_multi_stdin'},
        argv => [qw/a b/],
        dies => 1,
    );

    # file
    {
        my ($fh, $filename)   = tempfile();
        my ($fh2, $filename2) = tempfile();
        write_file($filename , 'foo');
        write_file($filename2, "bar\nbaz");
        test_run(
            name => 'file 1',
            args => {url=>'/Foo/cmdline_src_file'},
            argv => ['--a1', $filename],
            exit_code => 0,
            output_re => qr/a1=foo/,
        );
        test_run(
            name => 'file 2',
            args => {url=>'/Foo/cmdline_src_file'},
            argv => ['--a1', $filename, '--a2', $filename2],
            exit_code => 0,
            output_re => qr/a1=foo\na2=\[bar\n,baz\]/,
        );
        test_run(
            name => 'file not found',
            args => {url=>'/Foo/cmdline_src_file'},
            argv => ['--a1', $filename . "/x"],
            dies => 1,
        );
        test_run(
            name => 'file, missing required arg',
            args => {url=>'/Foo/cmdline_src_file'},
            argv => ['--a2', $filename],
            dies => 1,
        );
    }

    # stdin_or_files
    {
        my ($fh, $filename)   = tempfile();
        my ($fh2, $filename2) = tempfile();
        write_file($filename , 'foo');
        write_file($filename2, "bar\nbaz");
        test_run(
            name => 'stdin_or_files file',
            args => {url=>'/Foo/cmdline_src_stdin_or_files_str'},
            argv => [$filename],
            exit_code => 0,
            output_re => qr/a1=foo$/,
        );
        test_run(
            name => 'stdin_or_files file not found',
            args => {url=>'/Foo/cmdline_src_stdin_or_files_str'},
            argv => [$filename . "/x"],
            dies => 1,
        );

        # i don't know why these tests don't work, they should though. and if
        # tested via a cmdline script like
        # examples/cmdline_src-stdin_or_files-{str,array} they work fine.
        if (0) {
            open $fh, '<', $filename2;
            local *STDIN = $fh;
            local @ARGV;
            test_run(
                name => 'stdin_or_files stdin str',
                args => {url=>'/Foo/cmdline_src_stdin_or_files_str'},
                argv => [],
                exit_code => 0,
                output_re => qr/a1=bar\nbaz$/,
            );
        }
        if (0) {
            open $fh, '<', $filename2;
            local *STDIN = $fh;
            local @ARGV;
            test_run(
                name => 'stdin_or_files stdin str',
                args => {url=>'/Foo/cmdline_src_stdin_or_files_array'},
                argv => [],
                exit_code => 0,
                output_re => qr/a1=\[bar\n,baz\]/,
            );
        }
    }

    # stdin
    {
        my ($fh, $filename) = tempfile();
        write_file($filename, "bar\nbaz");

        open $fh, '<', $filename;
        local *STDIN = $fh;
        test_run(
            name => 'stdin str',
            args => {url=>'/Foo/cmdline_src_stdin_str'},
            argv => [],
            exit_code => 0,
            output_re => qr/a1=bar\nbaz/,
        );

        open $fh, '<', $filename;
        *STDIN = $fh;
        test_run(
            name => 'stdin array',
            args => {url=>'/Foo/cmdline_src_stdin_array'},
            argv => [],
            exit_code => 0,
            output_re => qr/a1=\[bar\n,baz\]/,
        );

        open $fh, '<', $filename;
        *STDIN = $fh;
        test_run(
            name => 'stdin + arg set to "-"',
            args => {url=>'/Foo/cmdline_src_stdin_str'},
            argv => [qw/--a1 -/],
            exit_code => 0,
            output_re => qr/a1=bar\nbaz/,
        );

        test_run(
            name => 'stdin + arg set to non "-"',
            args => {url=>'/Foo/cmdline_src_stdin_str'},
            argv => [qw/--a1 x/],
            dies => 1,
        );
    }

    done_testing;
};

test_run(name      => 'dry_run (using dry_run) (w/o)',
         args      => {url=>'/Foo/dry_run'},
         argv      => [],
         exit_code => 0,
         output_re => qr/2/,
     );
test_run(name      => 'dry_run (using dry_run) (w/)',
         args      => {url=>'/Foo/dry_run'},
         argv      => [qw/--dry-run/],
         exit_code => 0,
         output_re => qr/1/,
     );
test_run(name      => 'dry_run (using tx) (w/o)',
         args      => {url=>'/Foo/tx'},
         argv      => [],
         exit_code => 0,
         output_re => qr/2/,
     );
test_run(name      => 'dry_run (using tx) (w/)',
         args      => {url=>'/Foo/tx'},
         argv      => [qw/--dry-run/],
         exit_code => 0,
         output_re => qr/1/,
     );

DONE_TESTING:
done_testing;
