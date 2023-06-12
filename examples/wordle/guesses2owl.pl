#!/usr/bin/perl
use strict;

#print "Prefix: : <http://example.org/wordle/>\n";
#print "Prefix: soln: <http://example.org/soln/>\n";

#print "## Constraints from guesses\n";
#print "Ontology: <http://example.org/wordle.owl>\n\n";


while (<>) {
    chomp;
    my ($w, $result) = split(/,\s+/, $_);
    $w = lc($w);
    # we rule out the word in its entirety
    print "Class: soln:$w DisjointWith: :TodaysWord\n";
    for my $i (0..4) {
        my $n = $i+1;
        my $letter = uc(substr($w,$i,1));        
        my $letter_result = substr($result,$i,1);
        if ($letter_result eq 'x') {
            print "Class: :TodaysWord DisjointWith: :hasLetter some :$letter\n";
        }
        elsif ($letter_result eq 'G') {
            print "Class: :TodaysWord SubClassOf: :hasLetter$n some :$letter\n";
        }
        elsif ($letter_result eq 'O') {
            print "Class: :TodaysWord SubClassOf: :hasLetter some :$letter\n";
            print "Class: :TodaysWord DisjointWith: :hasLetter$n some :$letter\n";
        }
        else {
            die "Word: $w results: $result index: $i == $letter_result";
        }
    }
}
open(F, 'word-probs.csv');
my @words = ();
while (<F>) {
    chomp;
    s@,.*@@;
    my $c = "soln:$_";
    push(@words, $c);
    print "Class: $c\n";
}
close(F);
my $wstr = join(', ', @words);
my $union = join(' OR ', @words);
# whelk doesn't support:
print "Class: :TodaysWord DisjointUnionOf: $wstr\n";
print "Class: :TodaysWord SubClassOf: $union\n";
