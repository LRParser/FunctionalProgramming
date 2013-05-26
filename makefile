TOP ?= $(shell pwd)

HOST=$(shell hostname)
ASSIGNMENT=A5
SCHEME=scheme

RELEASE_DIR=release
RELEASE_FILE=$(ASSIGNMENT).tar.gz

BEVAL_TESTS=scheme1.scm
TAUTPROVER_TESTS=scheme2.scm

MINI_LANG=scheme5.scm
MINI_LANG_TESTS=mini_lang_tests/

.PHONY : clean tags release test

# define the run-test function
run-scheme = cat $(1) | $(SCHEME) --batch-mode
run-mini-lang = cat $(MINI_LANG) $(1)| $(SCHEME) -batch-mode

test-part-1:
	@echo "Running $(BEVAL_TESTS)";
	@$(call run-scheme, $(BEVAL_TESTS))

test-part-2:
	@echo "Running $(TAUTPROVER_TESTS)"
	@$(call run-scheme, $(TAUTPROVER_TESTS))

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

.PHONY : view-part-4
view-part-4:
	@more scheme4.patch
