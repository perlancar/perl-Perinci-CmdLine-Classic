=head1 SYNOPSIS

 # in your module

 package MyModule;
 use 5.010;
 use Sub::ExtrasUtil qw(rwrap resp);
 use LWP::UserAgent;
 use HTML::Entities qw(decode_entities);

 our %SUB_EXTRAS;

 $SUB_EXTRAS{cpanid2name} = {
     # ...
 };
 sub cpanid2name {
     my (%args) = @_;

     state $ua = LWP::UserAgent->new;
     my $cpanid = $args{cpanid};
     my $ua_res = $ua->get("http://search.cpan.org/~$cpanid");
     return rwrap(500, "Can't contact CPAN", $ua_res)
         unless $ua_res->is_success;
     return resp(404)
         unless $ua_res->content =~ /CPAN Directory/;
     return [500, "Can't scrape name"]
         unless $ua_res->content =~ m!<div class=t1>(.+?)</div>!;
     resp(200, "OK", decode_entities($1));
 }

=head1 cut
