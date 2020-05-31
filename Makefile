REBAR             ?= ./rebar3
REBAR_CMD          = $(REBAR) $(profile:%=as %)
SHELL_CONFIG      ?= config/shell.config
SHELL_CONFIG_TMPL ?= config/shell.config.tmpl

all: compile xref eunit

compile:
	@$(REBAR_CMD) compile

xref:
	@$(REBAR_CMD) xref

clean:
	@$(REBAR_CMD) clean

eunit: export ERL_FLAGS=-config $(PWD)/eunit
eunit:
	@$(REBAR_CMD) do eunit,cover

edoc:
	@$(REBAR_CMD) edoc

shell: compile $(SHELL_CONFIG)
	@$(REBAR_CMD) shell

dialyze: compile
	@$(REBAR_CMD) dialyzer

$(SHELL_CONFIG):
	cp $(SHELL_CONFIG_TMPL) $(SHELL_CONFIG)

