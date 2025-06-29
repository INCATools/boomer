BASE := today

TEST = 1

words.ptable.tsv: word-probs.csv
	perl make-ptable-from-words.pl $< > $@
letters.ptable.tsv: letter-freqs.csv
	perl make-ptable-from-letter-freqs.pl $< > $@
today.ptable.tsv: words.ptable.tsv letters.ptable.tsv
	cat $^ > $@

ont-%.pre: my-guesses-%.csv
	perl guesses2owl.pl $< > $@

ont-%.omn: ont-%.pre
	cat wordle.omn $< > $@

today.owl: ont-$(TEST).omn
	cp $< $@

include ../Makefile
