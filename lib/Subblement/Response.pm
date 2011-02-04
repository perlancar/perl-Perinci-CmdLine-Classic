package Sub::Extras::Response;

1;
__END__
=head1 METHODS

=head2 new(SOURCE) or NEW(%params)

Construct a response object from SOURCE. SOURCE can be one of:

=over 4

=item * an integer (200-555)

Representing a HTTP response status.

Under Sub::Extras strict mode, you cannot use this.

=item * an arrayref [STATUS*, MESSAGE, CONTENT*]

=item * an L<HTTP::Response> object

=back

%params:

=over 4

=item * status => INTEGER

Return status code.

=item * message => STRING

Return error message.

=item * content => CONTENT

=back

=head2 status

=head2 message

=head2 content

=head2 is_success

Return true if response is success (status is 2xx).

=head2 
