TOP ?= $(shell pwd)

HOST=$(shell hostname)
ASSIGNMENT=A5
SCHEME=scheme

RELEASE_DIR=release
RELEASE_FILE=$(ASSIGNMENT).tar.gz

MINI_LANG=scheme6.scm
QUIT=quit.scm
MINI_LANG_TESTS=mini_lang_tests/

.PHONY : clean tags release test

# define the run-test function

run-mini-lang = cat $(MINI_LANG) $(1) $(QUIT) | $(SCHEME) -batch-mode

test:
	@for file in $(MINI_LANG_TESTS)*; \
	do                             \
		$(call run-mini-lang, $$file); \
	done;                          \


release:
	@cd ..; \
	cp -R $(TOP) $(ASSIGNMENT); \
	tar -zcf $(RELEASE_FILE) --exclude .git $(ASSIGNMENT); \
	rm -rf $(ASSIGNMENT); \
	mkdir $(TOP)/$(RELEASE_DIR); \
	mv $(RELEASE_FILE) $(TOP)/$(RELEASE_DIR)
