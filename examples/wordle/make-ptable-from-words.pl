#!/usr/bin/perl
use strict;

open(F,'word-probs.csv');
while (<F>) {
    chomp;
    my ($w,$p) = split(/,\s*/, $_);
    if (!$p) {
        $p = '0.0001';
    }
    my $np = 1-$p;
    print "soln:$w\twordle:TodaysWord\t0\t0\t$p\t$np\n";
}
