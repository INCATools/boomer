BOOMER = boomer
RUNS = 5
WINDOW = 10
SELF_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
STYLE := $(SELF_DIR)/conf/style.json

JSONS=$(wildcard output/*.json)
PNGS=$(patsubst %.json, %.png, $(JSONS))

all: output.ofn pngs output.html

output.ofn output.md: $(BASE).ptable.tsv $(BASE).owl prefixes.yaml
	$(BOOMER) -t $< -a $(BASE).owl -p prefixes.yaml -r $(RUNS) -w $(WINDOW)
.PRECIOUS: output.ofn

%.owl: %.omn
	robot convert -i $< -o $@

# use default
%.omn: 
	echo "using empty file" ;\
	cp $(SELF_DIR)/empty-ontology.ofn $@

# use default
prefixes.yaml:
	cp $(SELF_DIR)/prefixes.yaml .

%.dot: %.json
	og2dot.js -s $(STYLE) $< >$@

%.png: %.dot
	dot $< -Tpng -Grankdir=BT >$@

pngs: $(PNGS)

%.html: %.md
	pandoc $< -o $@

t:
	echo $(SELF_DIR)

clean:
	rm output.ofn

realclean:
	rm output.* output/*
