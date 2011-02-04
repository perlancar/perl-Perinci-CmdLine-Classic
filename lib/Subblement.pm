B0;115;0cpackage Sub::Subblement;
# ABSTRACT: Add various extra capabilities to your subroutines

use 5.010;
use strict;
use warnings;

use Data::Sah;
use Sub::Install;

sub md_target {}
sub md_desc {}
sub md_args {}
sub md_code {}

sub md_transaction {}
sub md_undo {}
sub md_dryrun {}
sub md_retry {}
sub md_timeout {}

1;
__END__

=head1 SYNOPSIS

 # in your module

 package MyModule;

 use strict;
 use warnings;

 our %SUBS;

 $SUBS{foo} = {
     name => 'pow',
     desc => 'Exponent a number',
     args => {
         base => [float => {desc=>"Base number", required=>1, arg_pos=>1}],
         exp  => [float => {desc=>"Exponent"   , required=>1, arg_pos=>2}],
     },
 };
 sub pow {
     my (%args) = @_;
     return [200, "OK", $arg{base} ** $arg{exp}];
 }


 # in your application/consumer module

 package MyApp;
 use Subblement;
 use MyModule qw(foo);

 my $res;

 $res = foo(base => 1);   # [400, "Missing argument: exp"]
 $res = foo(base => "a"); # [400, "Invalid argument: base must be a float"]

 $res = foo(base => 2, exp=>10); # [200, "OK", 1024]
 say $res->[2];

 ### using hypersubs in Perl ###
 use YourModule qw(cpanid2name);
 my $resp;

 # named args
 $resp = YourModule::pow(base=>2, exp=>8);
 say $resp->content; # 256

 # positional args
 my $resp = YourModule::pow(2, 8);
 say $resp->content; # 256

 $resp = cpanid2name('SHARYANTO');
 die $resp unless $resp->is_success;
 say $resp->content; # Steven Haryanto

 # automatic parameter checking
 $resp = YourModule::pow(base=>2);
 say $resp;                # Error: Missing required parameter 'exp'
 say YourModule::pow("a"); # Error: Parameter 'base' must be an integer

 # do not wrap with response object, return content directly, throws exception on
 # non-success responses.
 use YourModule -unwrap => 1;
 my $name = YourModule::cpanid2name('SHARYANTO'); # Steven Haryanto

 ### using hypersubs in REST API ###
 $ plackup ....
 $ curl http://localhost/yourmodule/pow?base=2&exp=8
 256

 ### using hypersubs in command line ###
 $ hcmdline YourModule yourmod.pl
 $ yourmod.pl cpanid2name --help
 cpanid2name - Convert CPAN author ID to full name
 Usage: cpanid2name [options]
   --help              print this message
   --cpanid            CPAN author ID

 $ yourmod.pl foo; echo $?
 Error: Unknown option --foo
 100

 $ yourmod.pl cpanid2name --foo bar; echo $?
 Error: Unknown option --foo
 100

 $ yourmod.pl cpanid2name FOOBAR; echo $?
 Error: Not Found
 104

 $ yourmod.pl cpanid2name --cpanid SHARYANTO; echo $?
 Steven Haryanto
 0

=head1 DESCRIPTION

Subroutine is an excellent unit of reuse. Sub::Extras leverages exporter, and
utilities to provide various functionalities for your subs:

=over 4

=item * fast and flexible parameter checking

=item * positional as well as named arguments calling style

=item * flexible exporting

=item * easy switching between exception-based and return-code error handling

=item * command-line access (including --help and pretty-printing of structures)

=item * HTTP REST access

=item * easy generation of API documentation (POD, etc)

=item * execution time limits

=item * automatic retries

=item * logging

=item * and more

=back

It's basically a combo of many Sub::...

All you have to do is provide some metadata for your subs and follow some simple
convention.

=head1 WHAT EXTRAS

=head2 Flexible exporter

If your module does not have an "import" subroutine, Sub::Extras will install
one for you. This exporter is pretty flexible:

Import individual subroutines:

 use MyModule qw(foo bar);

Import sets of subroutines via tags:

 use MyModule qw(:sometag);

Import into another name:

 use MyModule foo => { as => newfoo }, bar => { as => foo };

Add extra clauses into a sub:

 use MyModule foo => { as => newfoo, timeout => 5 };

Add extra clauses to all imported subs:

 use MyModule 'foo', 'bar', ':sometag', -add => { timeout => 5 };

By default all subs that have a metadata in %SUB_EXTRAS will be exportable, but
none will be exported by default.

=head1 HOW TO USE

=head2 In Your Module

Your module does not require Sub::Extras at all. All you need to do is:

=over 4

=item * provide metadata for your subroutines (in %SUB_EXTRAS)

 our %SUB_EXTRAS;

 $SUB_EXTRAS{foo} = {
 };
 sub foo {
 }

=item * accept arguments in your subroutine to a hash

Instead of:

 my ($foo, $bar) = @_;

do:

 my (%args) = @_;
 my $foo = $args{foo};
 my $bar = $args{bar};

This way, you can accept extra arguments in the future more easily.

Accept other arguments.

=item * return a 2+-element array in your subroutine

This array can return error code/message as well as result data (and optionally
extra stuffs).

=item * optionally provide metadata for your module

 $SUB_EXTRAS{""} = {
 };

=back

=head2 In Your Application/Consumer Module

=head1 FUNCTIONS

=head2 hsub(%params)

Declare a hypersub. Parameter:

=over 4

=item * name => NAME

=item * code => CODEREF

=item * target => SUBNAME

=item * args => SCHEMAS

See L<Data::Sah> for more information.

SCHEMAS is a hash of argument name and argument schema. Schema uses Data::Sah,
with additional clauses provided by Data::Sah::TypeX::Base::HsubArg:

arg_pos => [int => {min=>1}]

=item * log => 1|0

Enable logging.

=item * log_level => LEVEL

=item * retry => INT|CODEREF

Enable automatic retries

=item * retry_delay => INT

=item * private => BOOL

default is 0. If set to 1, this sub cannot be exported. Note that you can also
name your sub with _ prefix to mark it as private.

=item * timeout => SECS|CODEREF

=item * supports_dryrun

=item * supports_transaction

=item * supports_undo

=back

Internally, it places the spec in %YourModule::hsubs

=head2 resp

Create a sub response object. See L<Sub::Extras::Response>.

=head2 rwrap

Wraps a response object.

=head1 HOW IT WORKS

After loaded, Sub::Extras will install a code in @INC. From there on, all 'use'
statements will execute this code when searching for module. Sub::Extras will
then search the module and load it. And, if the module has an %SUB_EXTRAS
package variable in it, it will install extras into the module.

Sub::Extras wraps your subroutine with some code to: 1) check arguments and
prepare request object; 2) invoke your subroutine with the prepared request
object; 3) catch exception and convert subroutine return value as response
object.

=head1 CLAUSES

Some clauses are immediately applied and cannot be overriden by the importer,
like 'args' and 'return'.

Some clauses can be overriden (e.g. 'timeout').

Some clauses can also be given on a per-invocation basis (e.g. 'timeout').

=head1 IMPORTING

 use Sub::EXtras;
 use MyModule;
 use MyOtherModule;

Options for Sub::Extras:

- what to do if there is an unknown clause?

- whether to install an automatic installer in @INC

- whether to install a default importer in modules


---

 use YourModule;

this will not import anything (unless you use other exporting mechanism in
YourModule, like Exporter or Sub::Exporter).

 use YourModule qw(sub1 sub2);

This will import sub1 and sub2. Normal subs (not declared using 'hsub') is not
be importable. Private subs named with _ prefix is not importable. Subs marked
as C<private> (see L</"hsub"> is also not importable.

Import parameters:

=over 4

=item * -exception => BOOL|{SUBNAME=>BOOL, ...} (XXX -raise_error ?)

Default is 0, which means exceptions happening in your sub will be caught using
eval (or Try::Tiny) and passed as a response object. This means the caller of
your sub will be relatively certain that your sub won't die.

Sometimes this behaviour is not desirable, and you want exceptions to be passed.
If you set this to 0, exception in your sub will not be caught. In addition to
that, your sub will raise an exception for all non-success responses.

 hsub name => foo

 # without -exception
 ...

 # under -exception

=item * -wrap_resp => BOOL|{SUBNAME=>BOOL, ...}

Default is 1. If set to 0, -exception will automatically be turned on and subs
will return response content directly.

 # under -unwrap

=item * -wrap_obj => BOOL

Default is 1. Will not wrap with objects.

 # under -wrap_obj
 sub foo { my $r = shift } # bless([...], 'Sub::Extras::Request')

 # without -wrap_obj
 sub foo { my $r = shift } # [...]

=back

=head1 UTILITIES

See respective manual pages.

=over 4

=item * L<hcmdline>

=item * L<hhttp>

=head1 SEE ALSO

=head2 Modules used

Data::Sah for schema checking.

Log::Any for logging.

Sub::Install to wrap/install.

=head2 Alternative modules

If you just want to give named arguments, you might want to consider
L<Sub::NamedParams>. Sub::MicroSig

L<Sub::Attempts> for wrapping for retries.

Various parameter checking for subroutines: L<Params::Validate>, ...

Sub::Throttle?

=head1 FAQ

=head2 Why Sub::Extras?

flexibility -> clauses can be specified globally, or on a
per-importer, or per-invocation

lower overhead -> only a single wrapping is done (compared to multiple if we use
e.g. Params::Validate + Util Timeout + ... + ...)

small impact -> modules do not need Sub::Extras, they can run normally just
without. we just add some metadata.

=back
