## -*- makefile -*-

######################################################################
## Erlang

ERL := erl
ERLC := $(ERL)c

INCLUDE_DIRS := ../include ../../erl_libs $(wildcard ../deps/*/include)
EBIN_DIRS := $(wildcard ../deps/*/ebin)
ERLC_FLAGS := -W $(INCLUDE_DIRS:../%=-I ../%) $(EBIN_DIRS:%=-pa %)

ifndef no_debug_info
  ERLC_FLAGS += +debug_info
endif

ifdef debug
  ERLC_FLAGS += -Ddebug
endif

EBIN_DIR := ../ebin
DOC_DIR  := ../doc
EMULATOR := beam

ERLC_FLAGS += -pz $(EBIN_DIR)

ERL_SOURCES := $(wildcard *.erl)
ERL_HEADERS := $(wildcard *.hrl) $(wildcard ../include/*.hrl)
ERL_OBJECTS := $(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.$(EMULATOR))
ERL_DOCUMENTS := $(ERL_SOURCES:%.erl=$(DOC_DIR)/%.html) $(DOC_DIR)/index.html
ERL_OBJECTS_LOCAL := $(ERL_SOURCES:%.erl=./%.$(EMULATOR))
APP_FILES := $(wildcard *.app)
EBIN_FILES = $(ERL_OBJECTS) $(ERL_DOCUMENTS) $(APP_FILES:%.app=../ebin/%.app)
EBIN_FILES_NO_DOCS = $(ERL_OBJECTS) $(APP_FILES:%.app=../ebin/%.app)
MODULES = $(ERL_SOURCES:%.erl=%)

comma := ,
empty :=
space := $(empty) #end of line after one space
MODULES_COMMA = $(subst $(space),$(comma),$(MODULES))

../ebin/%.app: %.app ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' \
	    -e 's;%APP_NAME%;$(APP_NAME);' \
	    -e 's;%APP_DESCRIPTION%;$(APP_DESCRIPTION);' \
	    -e 's;%MODULES%;$(MODULES_COMMA);' \
	$< >  $@

$(EBIN_DIR)/%.$(EMULATOR): %.erl $(ERL_HEADERS)
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

./%.$(EMULATOR): %.erl $(ERL_HEADERS)
	$(ERLC) $(ERLC_FLAGS) -o . $<
