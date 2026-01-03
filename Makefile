# List your story names here
STORIES = crisis christmas snowable lantern
STDLIB = stdlib.dg

# Generate the list of all targets based on the names above
TARGETS = $(foreach s,$(STORIES),out/$(s).zblorb out/$(s).aastory out/$(s)/index.html)

.PHONY: all clean serve

all: $(TARGETS)

out/%.zblorb: %.dg $(STDLIB) | out
	dialogc -vv $^ -o $@

out/%.aastory: %.dg $(STDLIB) | out
	dialogc -vv -t aa $^ -o $@

out/%/index.html: out/%.aastory | out
	rm -rf out/$*
	aambundle -o out/$* $<
	mv out/$*/play.html $@

# Order-only prerequisite for the output directory
out:
	mkdir -p out

clean:
	rm -Rf out learn_dialog.zip

learn_dialog.zip: all
	rm -f learn_dialog.zip
	( cd out && zip -r ../learn_dialog.zip . )

serve: all
	( cd out; python -m http.server )
