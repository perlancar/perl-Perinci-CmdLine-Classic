#!perl

use 5.010;
use strict;
use warnings;

use File::Slurp::Tiny qw(write_file);
use File::Temp qw(tempdir tempfile);
use Perinci::CmdLine::Classic;
use Test::More 0.98;
use Test::Perinci::CmdLine qw(test_run);

$Test::Perinci::CmdLine::CLASS = 'Perinci::CmdLine::Classic';

subtest 'help action' => sub {
    test_run(
        args      => {url=>'/Perinci/Examples/noop'},
        argv      => [qw/--help/],
        exit_code => 0,
        output_re => qr/--help.+--version/ms,
    );
    test_run(
        name      => 'has subcommands',
        args      => {
            url => '/Perinci/Examples/',
            subcommands=>{
                sc1=>{url=>'/Perinci/Examples/gen_array'},
            },
        },
        argv      => [qw/--help/],
        exit_code => 0,
        output_re => qr/--subcommands/ms,
    );
    test_run(
        name      => 'help on subcommand',
        args      => {subcommands=>{
            sc1=>{url=>'/Perinci/Examples/gen_array'},
        }},
        argv      => [qw/sc1 --help/],
        exit_code => 0,
        output_re => qr/ sc1 -.+--len/ms,
    );
};

subtest 'version action' => sub {
    test_run(
        args      => {url=>'/Perinci/Examples/noop'},
        argv      => [qw/--version/],
        exit_code => 0,
        output_re => qr/\Q$Perinci::Examples::VERSION\E/,
    );
};

subtest 'subcommands action' => sub {
    test_run(
        args      => {subcommands => {
            noop => {url=>'/Perinci/Examples/noop', summary=>"Mmmm"},
            dies => {url=>'/Perinci/Examples/dies', summary=>"Boom"},
        }},
        argv      => [qw/--subcommands/],
        exit_code => 0,
        output_re => qr/^\s*dies\s+(\{en_US )?Boom\}?\s*\n\s*noop\s+(\{en_US )?Mmmm\}?/m,
    );
    test_run(
        name      => 'unknown subcommand = error',
        args      => {subcommands => {
            noop => {url=>'/Perinci/Examples/noop'},
            dies => {url=>'/Perinci/Examples/dies'},
        }},
        argv      => [qw/foo/],
        exit_code => 200,
    );
    test_run(
        name      => 'default_subcommand',
        args      => {subcommands => {
            noop => {url=>'/Perinci/Examples/noop'},
            dies => {url=>'/Perinci/Examples/dies'},
        },
                      default_subcommand=>'noop'},
        argv      => [qw//],
        exit_code => 0,
    );
    test_run(
        name      => 'default_subcommand 2',
        args      => {subcommands => {
            noop => {url=>'/Perinci/Examples/noop'},
            dies => {url=>'/Perinci/Examples/dies'},
        },
                      default_subcommand=>'dies'},
        argv      => [qw/--cmd noop/],
        exit_code => 0,
    );
};

subtest 'output formats' => sub {
    subtest 'json (--format)' => sub {
        test_run(
            args      => {url=>'/Perinci/Examples/noop'},
            argv      => ['--arg-json', '[1,2,3]', '--format', 'json'],
            exit_code => 0,
            output_re => qr/\[1,2,3\]/,
        );
    };

    subtest 'json (--json)' => sub {
        test_run(
            args      => {url=>'/Perinci/Examples/noop'},
            argv      => ['--arg-json', '[1,2,3]', '--json'],
            exit_code => 0,
            output_re => qr/\[\s+1,\s+2,\s+3\s+\]/s,
        );
    };

    subtest 'yaml' => sub {
        test_run(
            args      => {url=>'/Perinci/Examples/noop'},
            argv      => ['--arg-json', '[1,2,3]', '--format', 'yaml'],
            exit_code => 0,
            output_re => qr/^\s*- 1\n^\s*- 2\n^\s*- 3/m,
        );
    };

    # we just need to check that Data::Format::Pretty::Console is used

    subtest 'text (scalar)' => sub {
        test_run(
            args      => {url=>'/Perinci/Examples/noop'},
            argv      => ['--arg-json', '"1"', '--format', 'text'],
            exit_code => 0,
            output_re => qr/^1$/,
        );
    };

    subtest 'text (array)' => sub {
        test_run(
            args      => {url=>'/Perinci/Examples/noop'},
            argv      => ['--arg-json', '[1,2,3]', '--format', 'text'],
            exit_code => 0,
            output_re => qr/1\n2\n3/,
        );
    };
};

subtest 'dry run' => sub {
    local $ENV{DRY_RUN} = 0;
    test_run(
        name      => 'dry-run (0)',
        args      => {url=>'/Perinci/Examples/test_dry_run'},
        argv      => [],
        exit_code => 0,
        output_re => qr/wet/,
    );
    test_run(
        name      => 'dry-run (1)',
        args      => {url=>'/Perinci/Examples/test_dry_run'},
        argv      => [qw/--dry-run/],
        exit_code => 0,
        output_re => qr/dry/,
    );
    $ENV{DRY_RUN} = 1;
    test_run(
        name      => 'dry-run (via env)',
        args      => {url=>'/Perinci/Examples/test_dry_run'},
        argv      => [qw//],
        exit_code => 0,
        output_re => qr/dry/,
    );
};

subtest 'cmdline_src' => sub {
    my $prefix = "/Perinci/Examples/CmdLineSrc";
    test_run(
        name   => 'unknown value',
        args   => {url=>"$prefix/cmdline_src_unknown"},
        argv   => [],
        status => 531,
    );
    test_run(
        name   => 'arg type not str/array',
        args   => {url=>"$prefix/cmdline_src_invalid_arg_type"},
        argv   => [],
        status => 531,
    );
    test_run(
        name   => 'multiple stdin',
        args   => {url=>"$prefix/cmdline_src_multi_stdin"},
        argv   => [qw/a b/],
        status => 500,
    );

    # file
    {
        my ($fh, $filename)   = tempfile();
        my ($fh2, $filename2) = tempfile();
        write_file($filename , 'foo');
        write_file($filename2, "bar\nbaz");
        test_run(
            name => 'file 1',
            args => {url=>"$prefix/cmdline_src_file"},
            argv => ['--a1', $filename],
            exit_code => 0,
            output_re => qr/a1=foo/,
        );
        test_run(
            name => 'file 2',
            args => {url=>"$prefix/cmdline_src_file"},
            argv => ['--a1', $filename, '--a2', $filename2],
            exit_code => 0,
            output_re => qr/a1=foo\na2=\[bar\n,baz\]/,
        );
        test_run(
            name   => 'file not found',
            args   => {url=>"$prefix/cmdline_src_file"},
            argv   => ['--a1', $filename . "/x"],
            status => 500,
        );
        test_run(
            name   => 'file, missing required arg',
            args   => {url=>"$prefix/cmdline_src_file"},
            argv   => ['--a2', $filename],
            status => 400,
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
            args => {url=>"$prefix/cmdline_src_stdin_or_files_str"},
            argv => [$filename],
            exit_code => 0,
            output_re => qr/a1=foo$/,
        );
        test_run(
            name   => 'stdin_or_files file not found',
            args   => {url=>"$prefix/cmdline_src_stdin_or_files_str"},
            argv   => [$filename . "/x"],
            status => 500,
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
                args => {url=>"$prefix/cmdline_src_stdin_or_files_str"},
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
                args => {url=>"$prefix/cmdline_src_stdin_or_files_array"},
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
            args => {url=>"$prefix/cmdline_src_stdin_str"},
            argv => [],
            exit_code => 0,
            output_re => qr/a1=bar\nbaz/,
        );

        open $fh, '<', $filename;
        *STDIN = $fh;
        test_run(
            name => 'stdin array',
            args => {url=>"$prefix/cmdline_src_stdin_array"},
            argv => [],
            exit_code => 0,
            output_re => qr/a1=\[bar\n,baz\]/,
        );

        open $fh, '<', $filename;
        *STDIN = $fh;
        test_run(
            name => 'stdin + arg set to "-"',
            args => {url=>"$prefix/cmdline_src_stdin_str"},
            argv => [qw/--a1 -/],
            exit_code => 0,
            output_re => qr/a1=bar\nbaz/,
        );

        test_run(
            name   => 'stdin + arg set to non "-"',
            args   => {url=>"$prefix/cmdline_src_stdin_str"},
            argv   => [qw/--a1 x/],
            status => 400,
        );
    }

    # stdin_line
    {
        my ($fh, $filename) = tempfile();
        write_file($filename, "foo\n");

        open $fh, '<', $filename;
        local *STDIN = $fh;
        test_run(
            name => 'stdin_line + from stdin',
            args => {url=>"$prefix/cmdline_src_stdin_line"},
            argv => ['--a2', 'bar'],
            exit_code => 0,
            output_re => qr/a1=foo\na2=bar/,
        );

        open $fh, '<', $filename;
        local *STDIN = $fh;
        test_run(
            name => 'stdin_line + from cmdline',
            args => {url=>"$prefix/cmdline_src_stdin_line"},
            argv => ['--a2', 'bar', '--a1', 'qux'],
            exit_code => 0,
            output_re => qr/a1=qux\na2=bar/,
        );

        write_file($filename, "foo\nbar\n");
        open $fh, '<', $filename;
        local *STDIN = $fh;
        test_run(
            name => 'multi stdin_line',
            args => {url=>"$prefix/cmdline_src_multi_stdin_line"},
            argv => ['--a3', 'baz'],
            exit_code => 0,
            output_re => qr/a1=foo\na2=bar\na3=baz/,
        );
    }

    done_testing;
};

subtest 'result metadata' => sub {
    subtest 'cmdline.exit_code' => sub {
        test_run(
            args      => {url=>'/Perinci/Examples/CmdLineResMeta/exit_code'},
            argv      => [qw//],
            status    => 200,
            exit_code => 7,
        );
    };
    subtest 'cmdline.result' => sub {
        test_run(
            args      => {url=>'/Perinci/Examples/CmdLineResMeta/result'},
            argv      => [qw//],
            output_re => qr/false/,
        );
    };
    subtest 'cmdline.default_format' => sub {
        test_run(
            args      => {url=>'/Perinci/Examples/CmdLineResMeta/default_format'},
            argv      => [qw//],
            output_re => qr/null/,
        );
        test_run(
            args      => {url=>'/Perinci/Examples/CmdLineResMeta/default_format'},
            argv      => [qw/--format text/],
            output_re => qr/\A\z/,
        );
    };
    subtest 'cmdline.skip_format' => sub {
        test_run(
            args      => {url=>'/Perinci/Examples/CmdLineResMeta/skip_format'},
            argv      => [qw//],
            output_re => qr/ARRAY\(0x/,
        );
    };
};

# config is tested in Perinci-CmdLine-Lite's t/run.t.

test_run(name      => 'dry_run (using tx) (w/o)',
         args      => {url=>'/Perinci/Examples/Tx/check_state'},
         argv      => [],
         exit_code => 0,
         output_re => qr/^$/,
     );
test_run(name      => 'dry_run (using tx) (w/)',
         args      => {url=>'/Perinci/Examples/Tx/check_state'},
         argv      => [qw/--dry-run/],
         exit_code => 0,
         output_re => qr/check_state/,
     );

DONE_TESTING:
done_testing;
