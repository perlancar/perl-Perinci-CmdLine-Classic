#!perl

use 5.010;
use strict;
use warnings;
use Log::Any '$log';
use Test::More 0.96;

use Capture::Tiny qw(capture);
use Perinci::CmdLine;

# XXX test formats

package Foo;
our $VERSION = "0.123";
our %SPEC;

$SPEC{':package'} = {v=>1.1};

$SPEC{noop} = {
    v => 1.1,
    summary => 'Always return noop',
};
sub noop {
    [304, "Nothing done"];
}

$SPEC{ok} = {
    v => 1.1,
    summary => 'Always return ok',
    args => {
        arg1 => {schema=>['str*' => {in=>[qw/a b c d/]}], pos=>0, req=>1},
        arg2 => {schema=>['str*'], pos=>1, req=>1},
        arg3 => {schema=>'str'},
    },
};
sub ok {
    my %args = @_;
    [200, "OK",
     {"First argument"=>$args{arg1}, "Second argument"=>$args{arg2}}];
}

$SPEC{want_odd} = {
    summary => 'Return error if given an even number',
    args => {
        num => ['int*' => {arg_pos=>0}],
    },
};
sub want_odd {
    my %args = @_;
    if ($args{num} % 2) {
        [200, "OK"];
    } else {
        [400, "You know I hate even numbers, right?"];
    }
}

$SPEC{f1} = {
    v => 1.1,
    summary => 'This function has arguments with names like "help", "list"',
    args => {
        help => {schema=>'bool'},
        list => {schema=>'bool'},
    },
};
sub f1 {
    my %args = @_;
    [200, "OK", $args{help} ? "tolong" : $args{list} ? "daftar" : "?"];
}

$SPEC{f2} = {
    v => 1.1,
    summary => 'This function has required positional argument',
    args => {
        a1 => {schema=>'str*', req=>1, pos=>0},
    },
};
sub f2 {
    my %args = @_;
    [200, "OK", $args{a1}];
}

$SPEC{sp1} = {
    v => 1.1,
    summary => 'This function supports dry run',
    args => {},
    features => {dry_run=>1},
};
sub sp1 {
    my %args = @_;
    [200, "OK", "dry_run=".($args{-dry_run} ? 1:0)];
}

package main;

subtest 'completion' => sub {
    test_complete(
        name        => 'arg name (single sub)',
        argv        => [],
        args        => {url=>'/Foo/ok'},
        comp_line   => 'CMD -',
        comp_point0 => '     ^',
        result      => [qw(
                           --action
                           --arg1 --arg2 --arg3 --debug --format --help
                           --log-level --quiet
                           --trace --verbose --version
                           -\? -h -v
                      )],
    );
    test_complete(
        name        => 'arg name (with subcommands)',
        argv        => [],
        args      => {subcommands=>{
            ok=>{url=>'/Foo/ok'},
            wo=>{url=>'/Foo/want_odd'}}},
        comp_line   => 'CMD -',
        comp_point0 => '     ^',
        result      => [qw(
                           --action
                           --arg1 --arg2 --arg3 --debug --format --help
                           --list --log-level --quiet
                           --trace --verbose --version
                           -\? -h -l -v
                      )],
    );
    test_complete(
        name        => 'arg value from arg spec "in" (single sub)',
        argv        => [],
        args        => {url=>'/Foo/ok'},
        comp_line   => 'CMD ',
        comp_point0 => '    ^',
        result      => [qw(a b c d)],
    );
    test_complete(
        name        => 'arg value from "custom_arg_completer" (single sub)',
        argv        => [],
        args        => {url=>'/Foo/ok',
                        custom_arg_completer=>sub {qw(e f g h)}},
        comp_line   => 'CMD arg1 ',
        comp_point0 => '         ^',
        result      => [qw(e f g h)],
    );
    test_complete(
        name        => 'arg value from "custom_arg_completer" (single sub) (2)',
        argv        => [],
        args        => {url=>'/Foo/ok',
                        custom_arg_completer=>{arg2=>sub{qw(e f g h)}}},
        comp_line   => 'CMD arg1 ',
        comp_point0 => '         ^',
        result      => [qw(e f g h)],
    );
    test_complete(
        name        => '--dry-run',
        argv        => [],
        args        => {url=>'/Foo/sp1'},
        comp_line   => 'CMD --dr',
        comp_point0 => '        ^',
        result      => [qw(--dry-run)],
    );
};

test_run(name      => 'single sub',
         args      => {url=>'/Foo/ok'},
         argv      => [qw/--arg1 1 --arg2 2/],
         exit_code => 0,
         output_re => qr/First argument/,
     );

test_run(name      => 'missing arg = error',
         args      => {url=>'/Foo/ok'},
         argv      => [qw/--arg3 3/],
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

test_run(name      => 'unknown subcommand = error',
         args      => {subcommands=>{
             ok=>{url=>'/Foo/ok'},
             wo=>{url=>'/Foo/want_odd'}}},
         argv      => [qw/foo/],
         dies      => 1,
     );

test_run(name      => 'arg: dash_to_underscore=0',
         args      => {module=>'Foo', dash_to_underscore=>0,
                       subcommands=>{ok=>{}, want_odd=>{}}},
         argv      => [qw/want-odd 3/],
         dies      => 1,
     );
test_run(name      => 'arg: dash_to_underscore=1 (default)',
         args      => {subcommands=>{
             ok=>{url=>'/Foo/ok'},
             want_odd=>{url=>'/Foo/want_odd'}}},
         argv      => [qw/want-odd 3/],
         exit_code => 0,
     );

for (qw(--help -h -?)) {
    test_run(name      => "general help ($_)",
             args      => {url=>'/Foo/'},
             argv      => [$_],
             exit_code => 0,
             output_re => qr/^Usage:/m,
         );
}

test_run(name      => "common option (--version) before subcommand name",
         args      => {url=>'/Foo/', subcommands=>{
             ok=>{url=>'/Foo/ok'},
             want_odd=>{url=>'/Foo/want_odd'}}},
         argv      => [qw/--version want_odd --num 4/],
         exit_code => 0,
         output_re => qr/version 0\.123/m,
     );
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
         output_re => qr/^Usage:/m,
     );
test_run(name      => "common option (--help) does not override ".
             "function argument when using --action=subcommand",
         args      => {subcommands=>{f1=>{url=>'/Foo/f1'}}},
         argv      => [qw/f1 --help --action=subcommand/],
         exit_code => 0,
         output_re => qr/^tolong/m,
     );
test_run(name      => "common option (--help) bypass required argument check",
         args      => {url=>'/Foo/f2'},
         argv      => [qw/--help/],
         exit_code => 0,
         output_re => qr/^Usage:/m,
     );

# disabled for now, fail under 'prove'? wtf?
#for (qw(--version -v)) {
#    test_run(name      => "version ($_)",
#             args      => {url=>'/Foo/', subcommands=>{
#                 ok=>{url=>'/Foo/ok'},
#                 want_odd=>{url=>'/Foo/want_odd'}}},
#             argv      => [$_],
#             exit_code => 0,
#             output_re => qr/version 0\.123/,
#         );
#}

for (qw(--list -l)) {
    test_run(name      => "list ($_)",
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

# XXX test arg: undo

DONE_TESTING:
done_testing();

sub test_run {
    my %args = @_;

    my $pc = Perinci::CmdLine->new(%{$args{args}}, exit=>0);

    local @ARGV = @{$args{argv}};
    my ($stdout, $stderr);
    my $exit_code;
    eval {
        if ($args{output_re}) {
            ($stdout, $stderr) = capture { $exit_code = $pc->run };
        } else {
            $exit_code = $pc->run;
        }
    };
    my $eval_err = $@;

    subtest $args{name} => sub {
        if ($args{dies}) {
            ok($eval_err || ref($eval_err), "dies");
        } else {
            ok(!$eval_err, "doesn't die") or diag("dies: $eval_err");
        }

        if ($args{exit_code}) {
            is($exit_code, $args{exit_code}, "exit code");
        }

        if ($args{output_re}) {
            like($stdout // "", $args{output_re}, "output_re")
                or diag("output is $stdout");
        }
    };
}

sub test_complete {
    my (%args) = @_;

    my $pc = Perinci::CmdLine->new(%{$args{args}}, exit=>0);

    local @ARGV = @{$args{argv}};
    local $ENV{COMP_LINE}  = $args{comp_line};
    local $ENV{COMP_POINT} = index($args{comp_point0}, "^");

    my ($stdout, $stderr);
    my $exit_code;
    ($stdout, $stderr) = capture {
        $exit_code = $pc->run;
    };

    subtest "completion: $args{name}" => sub {
        is($exit_code, 0, "exit code = 0");
        is($stdout // "", join("", map {"$_\n"} @{$args{result}}), "result");
    };
}

