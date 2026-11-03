#!/usr/bin/perl
# Reduce an NCSBE absentee CSV to non-identifying columns. Reads stdin, writes TSV to stdout.
# The file mixes quoted and unquoted fields and has stray quotes inside site_name, so it is
# tokenised by hand: a quoted field closes only at a quote followed by a comma or line end.
# Output stays outside Dropbox (~/needle_nc01_raw). It has no name, address or voter id column.
use strict;
my %want = map {$_=>1} qw(county_desc race ethnicity gender age voter_party_code precinct_desc cong_dist_desc ballot_req_type ballot_rtn_status site_name sdr);
my @idx; my ($bad,$n,$fix)=(0,0,0);
sub toks { my ($s)=@_; my @f; my $p=0; my $l=length $s;
  while(1){ if(substr($s,$p,1) eq '"'){ my $q=$p+1;
        while(1){ my $e=index($s,'"',$q); if($e<0){ push @f,substr($s,$p+1); return @f }
          my $nx=$e+1<$l?substr($s,$e+1,1):''; 
          if($nx eq '' || ($nx eq ',' && (substr($s,$e+2,1) eq '"' || substr($s,$e+2,1) =~ /[A-Za-z0-9 ]/ || $e+2>=$l))){ push @f,substr($s,$p+1,$e-$p-1); $p=$e+1; last } $q=$e+1 }
        return @f if $p>=$l; $p++; if($p>=$l){push @f,''; return @f} }
     else { my $e=index($s,',',$p); if($e<0){ push @f,substr($s,$p); return @f } push @f,substr($s,$p,$e-$p); $p=$e+1; if($p>=$l){push @f,''; return @f} } } }
while(<STDIN>){ chomp; s/\r$//; my @f=toks($_);
  if($n==0){ for my $i(0..$#f){ push @idx,$i if $want{$f[$i]} } print join("\t",@f[@idx]),"\n"; $n++; next }
  $n++; if(@f!=42){ $bad++; next }
  my @o=@f[@idx]; s/[\t"]/ /g for @o; s/^\s+|\s+$//g for @o; print join("\t",@o),"\n"; }
print STDERR "lines=$n unparsed=$bad\n";
