include ../include.mk
include ../vsn.mk

all: $(EBIN_FILES_NO_DOCS)

docs:
	rm -rf $(DOC_DIR)/*
	erl -eval 'edoc:application($(APP_NAME), "..", [{overview,"./overview.edoc"}]).' -run init stop

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)
