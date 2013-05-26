TOP ?= $(shell pwd)

HOST=$(shell hostname)
ASSIGNMENT=A5
SCHEME=scheme

RELEASE_DIR=release
RELEASE_FILE=$(ASSIGNMENT).tar.gz

MINI_LANG=scheme5.scm
MINI_LANG_TESTS=mini_lang_tests/

.PHONY : clean tags release test view-part-4 view-part-3

# define the run-test function

run-mini-lang = cat $(MINI_LANG) $(1)| $(SCHEME) -batch-mode

test-part-5:
	@for file in $(MINI_LANG_TESTS)*; \
	do                             \
		echo "Running $$file"; \
		$(call run-mini-lang, $$file); \
	done;                          \


release:
	@cd ..; \
	cp -R $(TOP) $(ASSIGNMENT); \
	tar -zcf $(RELEASE_FILE) --exclude .git $(ASSIGNMENT); \
	rm -rf $(ASSIGNMENT); \
	mkdir $(TOP)/$(RELEASE_DIR); \
	mv $(RELEASE_FILE) $(TOP)/$(RELEASE_DIR)

view-part-4:
	@more scheme4.patch dynamic_v_lexical.scm

view-part-3:
	@more scheme3.scm
