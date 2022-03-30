#!/usr/bin/perl
use strict;

open(F,'letter-freqs.csv');
while (<F>) {
    chomp;
    my ($letter,@rest) = split(/,\s*/, $_);
    for my $i (0..4) {
        my $n = $i+1;
        my $p = $rest[$i];
        my $np = 1-$p;
        print "wordle:$letter\twordle:Letter$n\t$p\t0\t0\t$np\n";
    }
}
