package Sub::Extras::Request;

1;
__END__
=head1 METHODS

=head2 new(%params)

Parameters:

=over 4

=item * source => SOURCE

SOURCE can be one of: XXX

=item * spec => SPEC

=back

=head2 $req->arg(NAME)

Access argument named NAME.

=head2 $req->input()

Input stream.

=head2 $req->output()

The output stream.

=head2 $req->errors()

The error stream.

=cut
