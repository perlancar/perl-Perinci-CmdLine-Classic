package Sub::Extras;
# ABSTRACT: Limit subroutine execution

1;
__END__

=head1 SYNOPSIS

 use Sub::Extras;
 use MyModule qw(myfunc);

 # limit execution to 5 seconds
 my $res = func(arg1=>val, arg2=>val, ..., -timeout => 5);

 die "Function timed out"
     if $res->[0] == 504;

=head1 SEE ALSO

L<Util::Timeout>

=cut
