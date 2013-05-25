TOP ?= $(shell pwd)

HOST=$(shell hostname)
ASSIGNMENT=A5
SCHEME=scheme

RELEASE_DIR=release
RELEASE_FILE=$(ASSIGNMENT).tar.gz

.PHONY : clean tags release

release:
	@cd ..; \
	cp -R $(TOP) $(ASSIGNMENT); \
	tar -zcf $(RELEASE_FILE) --exclude .git $(ASSIGNMENT); \
	rm -rf $(ASSIGNMENT); \
	mkdir $(TOP)/$(RELEASE_DIR); \
	mv $(RELEASE_FILE) $(TOP)/$(RELEASE_DIR)
