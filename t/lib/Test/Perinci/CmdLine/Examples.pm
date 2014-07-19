package
    Test::Perinci::CmdLine::Examples;

our $VERSION = "0.123";
our $DATE = "1999-01-01";
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
     {
         "First argument"=>$args{arg1},
         "Second argument"=>$args{arg2},
         "Third argument"=>$args{arg3},
     }];
}

$SPEC{want_odd} = {
    v => 1.1,
    summary => 'Return error if given an even number',
    args => {
        num => { schema => 'int*', pos=>0 },
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
    summary => 'This function has arguments with names like "help", "subcommands"',
    args => {
        help => {schema=>'bool'},
        subcommands => {schema=>'bool'},
    },
};
sub f1 {
    my %args = @_;
    [200, "OK", $args{help} ? "tolong" : $args{subcommands} ? "daftar" : "?"];
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

$SPEC{f2r} = {
    v => 1.1,
    summary => 'This function has required positional argument',
    args => {
        a1 => {schema=>'str*', req=>1, pos=>0},
    },
};
sub f2r {
    my %args = @_;
    [200, "OK", scalar(reverse $args{a1})];
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

$SPEC{cmdline_src_unknown} = {
    v => 1.1,
    summary => 'This function has arg with unknown cmdline_src value',
    args => {
        a1 => {schema=>'str*', cmdline_src=>'foo'},
    },
};
sub cmdline_src_unknown {
    my %args = @_;
    [200, "OK", "a1=$args{a1}"];
}

$SPEC{cmdline_src_invalid_arg_type} = {
    v => 1.1,
    summary => 'This function has non-str/non-array arg with cmdline_src',
    args => {
        a1 => {schema=>'int*', cmdline_src=>'stdin'},
    },
};
sub cmdline_src_invalid_arg_type {
    my %args = @_;
    [200, "OK", "a1=$args{a1}"];
}

$SPEC{cmdline_src_stdin_str} = {
    v => 1.1,
    summary => 'This function has arg with cmdline_src=stdin',
    args => {
        a1 => {schema=>'str*', cmdline_src=>'stdin'},
    },
};
sub cmdline_src_stdin_str {
    my %args = @_;
    [200, "OK", "a1=$args{a1}"];
}

$SPEC{cmdline_src_stdin_array} = {
    v => 1.1,
    summary => 'This function has arg with cmdline_src=stdin',
    args => {
        a1 => {schema=>'array*', cmdline_src=>'stdin'},
    },
};
sub cmdline_src_stdin_array {
    my %args = @_;
    [200, "OK", "a1=[".join(",",@{ $args{a1} })."]"];
}

$SPEC{cmdline_src_file} = {
    v => 1.1,
    summary => 'This function has args with cmdline_src _file',
    args => {
        a1 => {schema=>'str*', req=>1, cmdline_src=>'file'},
        a2 => {schema=>'array*', cmdline_src=>'file'},
    },
};
sub cmdline_src_file {
    my %args = @_;
    [200, "OK", "a1=$args{a1}\na2=[".join(",", @{ $args{a2} // [] })."]"];
}

$SPEC{cmdline_src_stdin_or_files_str} = {
    v => 1.1,
    summary => 'This function has str arg with cmdline_src=stdin_or_files',
    args => {
        a1 => {schema=>'str*', cmdline_src=>'stdin_or_files'},
    },
};
sub cmdline_src_stdin_or_files_str {
    my %args = @_;
    [200, "OK", "a1=$args{a1}"];
}

$SPEC{cmdline_src_stdin_or_files_array} = {
    v => 1.1,
    summary => 'This function has array arg with cmdline_src=stdin_or_files',
    args => {
        a1 => {schema=>'array*', cmdline_src=>'stdin_or_files'},
    },
};
sub cmdline_src_stdin_or_files_array {
    my %args = @_;
    [200, "OK", "a1=[".join(",",@{ $args{a1} })."]"];
}

$SPEC{cmdline_src_multi_stdin} = {
    v => 1.1,
    summary => 'This function has multiple args with cmdline_src stdin/stdin_or_files',
    args => {
        a1 => {schema=>'str*', cmdline_src=>'stdin_or_files'},
        a2 => {schema=>'str*', cmdline_src=>'stdin'},
    },
};
sub cmdline_src_multi_stdin {
    my %args = @_;
    [200, "OK", "a1=$args{a1}\na2=$args{a2}"];
}

$SPEC{dry_run} = {
    v => 1.1,
    features => {dry_run=>1},
};
sub dry_run {
    my %args = @_;
    [200, "OK", $args{-dry_run} ? 1:2];
}

$SPEC{tx} = {
    v => 1.1,
    features => {tx=>{v=>2}, idempotent=>1},
};
sub tx {
    my %args = @_;
    [200, "OK", $args{-tx_action} eq 'check_state' ? 1:2];
}

1;
