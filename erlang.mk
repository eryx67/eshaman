ifeq ($(PROJECT),)
$(error "Please set PROJECT variable in your Makefile")
endif

ifeq ($(PROJECT_TYPE),)
$(warning "PROJECT_TYPE is not defined, set to 'application'")
PROJECT_TYPE = application
endif

ifeq ($(PROJECT_DIR),)
PROJECT_DIR := $(CURDIR)
endif

PROJECT_APP_DIRS := $(realpath $(dir $(or $(wildcard $(PROJECT_DIR)/apps/*/src), \
							              $(wildcard $(PROJECT_DIR)/src))))

ARCH?= $(shell uname -m)
PLAT?= $(shell uname -s)
TAG  = ${ARCH}.${PLAT}

define app_src_template =
$(1)/src/*.app.src: $(wildcard $(1)/src/*.erl)
	sed -i 's/{modules, \[[a-zA-Z0-9_, ]*\]}/{modules, \[$$(call comma_join,$$(basename $$(^F)))\]}/' $$@
endef

$(info "Project applications $(PROJECT_APP_DIRS)")

ifeq ($(NODE),)
NODE := $(PROJECT)@127.0.0.1
endif

ifeq ($(NODE_COOKIE),)
NODE_COOKIE := $(PROJECT)
endif

# ERL_FLAGS= -pa $(PROJECT_DIR)/.eunit -pa $(PROJECT_DIR)/ebin \
# 	-pa $(CURDIR)/deps/*/ebin -setcookie $(PROJECT)

ERL_FLAGS= $(patsubst %,-pa %,\
	$(foreach dir,$(PROJECT_APP_DIRS),\
		$(foreach sdir,.eunit test ebin,$(wildcard $(dir)/$(sdir))))) \
	-pa $(CURDIR)/deps/*/ebin

PROJECT_PLT=$(CURDIR)/.project_plt

ERL = $(shell which erl)

ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif

REBAR=./rebar

comma := ,
comma_join = $(subst $(eval) ,$(comma),$(1))
when_project_app = $(and $(filter app%, $(PROJECT_TYPE)), $(1))
when_project_rel = $(and $(filter rel% node%, $(PROJECT_TYPE)), $(1))

ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

ifeq ($(wildcard rel/reltool.config),)
	REL =
	VSN =
	TAR =
	PKG =
else
	REL  = $(shell cat rel/reltool.config | sed -n 's/{target_dir,.*\"\(.*\)\"}./\1/p')
	VSN  = $(shell echo ${REL} | sed -n 's/.*-\(.*\)/\1/p')
ifeq (${VSN},)
	VSN  = $(shell cat rel/reltool.config | sed -n 's/.*{rel,.*\".*\",.*\"\(.*\)\".*/\1/p')
endif
ifeq (${config},)
	RFLAGS  =
	VARIANT =
else
	VARIANT = $(addprefix ., $(notdir $(basename ${config})))
	RFLAGS  = target_dir=${REL}${VARIANT} overlay_vars=${ROOT}/${config}
endif
ifeq (${VSN},)
	TAR = ${REL}${VARIANT}.${TAG}.tgz
	PKG = ${REL}${VARIANT}.${TAG}.bundle
else
	TAR = ${REL}-${VSN}${VARIANT}.${TAG}.tgz
	PKG = ${REL}-${VSN}${VARIANT}.${TAG}.bundle
endif
endif

.PHONY: all build compile doc clean test dialyzer typer shell distclean pdf \
	deps escript clean-common-test-data etop rel

all: deps compile $(call  when_project_rel,build) $(call  when_project_app,escript)

build: deps compile
	$(REBAR) generate skip_deps=true ${RFLAGS}

rel: build
	cd rel ; tar -zcf ../${TAR} ${REL}${VARIANT}/; cd -

deps:
	$(REBAR) get-deps
	$(REBAR) compile

update-deps:
	$(REBAR) update-deps; \
	for dir in $(dir $(wildcard deps/*/.git)); do \
		pushd `pwd`; \
		echo Pulling in $$dir; \
		cd $$dir; \
		git pull; \
		git checkout; \
		popd; \
	done

compile: $(call when_project_app,app-src)
	$(REBAR) skip_deps=true compile

escript: compile
	$(REBAR) skip_deps=true escriptize

doc:
	$(REBAR) skip_deps=true doc

eunit: compile clean-common-test-data
	ERL_FLAGS="$(ERL_FLAGS) -config $(realpath test.config)" \
	$(REBAR) skip_deps=true eunit

ct: compile clean-common-test-data
	$(REBAR) skip_deps=true ct

test: compile eunit ct

tags:
	@ctags -e -R -f TAGS apps deps

$(PROJECT_PLT):
	@echo Building local plt at $(PROJECT_PLT)
	@echo
	dialyzer --output_plt $(PROJECT_PLT) --build_plt \
	--apps erts kernel stdlib -r deps

dialyzer: $(PROJECT_PLT)
	dialyzer --plt $(PROJECT_PLT) --fullpath -Wrace_conditions \
	-I include -pa $(CURDIR)/ebin --src $(PROJECT_DIR)/src

typer:
	typer --plt $(PROJECT_PLT) -r $(PROJECT_DIR)/src

app-src: $(foreach dir,$(PROJECT_APP_DIRS),$(dir)/src/*.app.src)

$(foreach dir,$(PROJECT_APP_DIRS),$(eval $(call app_src_template,$(dir))))

safe-shell: deps eunit shell

shell:
	- mkdir -p $(CURDIR)/data
	$(ERL) -name $(NODE) -setcookie $(NODE_COOKIE) $(ERL_FLAGS) \
	+K true +A 256 +P 512000  +Q 128000 +pc unicode \
	-boot start_sasl -config $(PROJECT).config

etop:
		erl +d -name etop@127.0.0.1 -hidden -s etop -s erlang halt -output text \
		-node $(NODE) -setcookie $(NODE_COOKIE) -tracing off

entop:
		erl -hidden -noinput $(ERL_FLAGS) +d +A 20 -name entop@127.0.0.1 \
		-eval "entop:start('$(NODE)')" -setcookie $(NODE_COOKIE)

pdf:
	pandoc README.md -o README.pdf

clean-common-test-data:
	- rm -rf $(CURDIR)/test/*_SUITE_data

clean: clean-common-test-data
	- rm -rf $(CURDIR)/data
	- rm -rf $(CURDIR)/test/*.beam
	- rm -rf $(CURDIR)/log
	$(REBAR) skip_deps=true clean

distclean: clean
	- rm -f $(CURDIR)/TAGS
	- rm -rf $(PROJECT_PLT)
	- rm -rvf $(CURDIR)/deps/*
