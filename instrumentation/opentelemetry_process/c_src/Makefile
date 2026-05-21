# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT ?= $(notdir $(BASEDIR))
PROJECT := $(strip $(PROJECT))

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~s/erts-~s/include/\", [code:root_dir(), erlang:system_info(version)])." -s erlang halt)
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~s\", [code:lib_dir(erl_interface, include)])." -s erlang halt)
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -eval "io:format(\"~s\", [code:lib_dir(erl_interface, lib)])." -s erlang halt)

C_SRC_DIR = $(CURDIR)
C_SRC_OUTPUT ?= $(CURDIR)/../priv/$(PROJECT).so
MEMTEST_OUTPUT ?= $(CURDIR)/../_build/memory_test

.PHONY: memory-test

# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CXX ?= c++
	CXXFLAGS += -O3 -finline-functions
	LDFLAGS += -flat_namespace -undefined suppress
	PSOURCES = otel_process_info_macos.cc
else ifeq ($(UNAME_SYS), FreeBSD)
	CXX ?= c++
	CXXEFLAGS += -D__STANDALONE_TEST__ -std=c++11 -Wall
	CXXFLAGS += -O3 -finline-functions
	PSOURCES = otel_process_info_freebsd.cc
else ifeq ($(UNAME_SYS), Linux)
	CXX ?= g++
	CXXEFLAGS += -D__STANDALONE_TEST__ -std=c++11 -Wall
	CXXFLAGS += -O3 -finline-functions
	PSOURCES = otel_process_info_linux.cc
endif

SOURCES = otel_process_metrics_nif.cc $(PSOURCES)

CXXFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -std=c++11 -Wall
LDLIBS += -L $(ERL_INTERFACE_LIB_DIR) -lei

ifeq ($(UNAME_SYS), OpenBSD)
	LDLIBS += -lestdc++
else
	LDLIBS += -lstdc++
endif

LDFLAGS += -shared

# Verbosity.

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))

COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

$(C_SRC_OUTPUT): $(OBJECTS)
	@mkdir -p $(BASEDIR)/priv/
	$(link_verbose) $(CC) $(OBJECTS) $(LDFLAGS) $(LDLIBS) -o $(C_SRC_OUTPUT)

%.o: %.cc
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.cpp
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

memory-test:
	@mkdir -p $(BASEDIR)/_build/
	$(CXX) $(CXXEFLAGS) $(LDLIBS) otel_process_info_mt.cc $(PSOURCES) -o $(MEMTEST_OUTPUT)
	valgrind --leak-check=full --error-exitcode=1 $(MEMTEST_OUTPUT) 2

clean:
	@rm -f $(C_SRC_OUTPUT) $(OBJECTS)
