PROJECT := eshaman
PROJECT_TYPE := application
DOC_DIR := doc
NODE := eshaman_dev@127.0.0.1

include erlang.mk

DOC_SRC = $(wildcard $(DOC_DIR)/*.txt)
DOC_FILES = $(DOC_SRC:%.txt=%.html)
DOC_APPS =

docs: doc $(DOC_FILES)
	for i in $(DOC_APPS); do \
		rm -rf 	doc/$$i; cp -a apps/$$i/doc doc/$$i; \
	done

%.html : %.txt
	a2x -f xhtml $<
