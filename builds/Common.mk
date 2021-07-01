# Melodica Makefile for Unit-level TBs
# To be run from the builds/<Unit> directory where Unit refers to DUT
# --------
#  Common compilation flags
BSC_COMPILATION_FLAGS = -keep-fires -aggressive-conditions -no-warn-action-shadowing -check-assert -no-show-timestamps -show-range-conflict

# Melodica-specific compilation flags
# Select P8 for 8-bit posits, P16 for 16-bit and P32 for 32-bit
BSC_COMPILATION_FLAGS += \
		 -D $(STIMULUS) \
		 -D $(PLATFORM) \
		 -D P$(POSIT_SIZE)
OBJ = .o

TOPMOD = mkTestbench

BLUESPEC_LIB = %/Prelude:%/Libraries

CXXFAMILY=$(shell $(BLUESPECDIR)/bin/bsenv c++_family)

SOFTPOSIT_OBJPATH = $(DISTRO)/SoftPosit/build/Linux-x86_64-GCC

# From bluespec installation
BSIM_INCDIR=$(BLUESPECDIR)/Bluesim
BSIM_LIBDIR=$(BSIM_INCDIR)/$(CXXFAMILY)

TB_PATH = $(DISTRO)/src_bsv/tb
RTL_PATH= $(DISTRO)/src_bsv

# ---------------
#  PATH and Variable settings for individual pipelines
#
RTL_DIRS = $(RTL_PATH)/Fused_Op:$(RTL_PATH)/lib:$(RTL_PATH)/common
BSC_PATH = $(RTL_DIRS):$(TB_PATH):+

# For final C++ link with main.cxx driver for non-BlueTcl version
# (needed for SoftPosits)
CPP_FLAGS += \
	-static \
	-D_GLIBCXX_USE_CXX11_ABI=0 \
        -DNEW_MODEL_MKFOO=new_MODEL_$(TOPMOD) \
        -DMODEL_MKFOO_H=\"model_$(TOPMOD).h\" \
	-I$(BSIM_INCDIR) \
	-O3 \

# -------------------------------------------------------------------------------------
# Compilation Targets -- Here starts the real work
default: compile rtl sim

TMP_DIRS = -bdir build_dir -simdir build_dir -info-dir build_dir -vdir Verilog_RTL

build_dir:
	mkdir -p $@

Verilog_RTL:
	mkdir -p $@

.PHONY: compile
compile: build_dir Verilog_RTL
	@echo "INFO: Compile BSV Source"
	bsc -u -elab -sim $(TMP_DIRS) $(BSC_COMPILATION_FLAGS) -p $(BSC_PATH) $(TOPFILE)
	bsc -u -elab -verilog $(TMP_DIRS) $(BSC_COMPILATION_FLAGS) -p $(BSC_PATH) -g $(TOPMOD) $(TOPFILE)
	@echo "INFO: Compile complete"

SIM_EXE_FILE = exe_HW_sim
BSC_CFLAGS = \
		-Xl -v \
		-Xc -O3\
		-Xc++ -O3\
		-Xc -lm\
		-Xc++ -D_GLIBCXX_USE_CXX11_ABI=0

.PHONY: simulator
simulator:
	@echo "INFO: Link bsc-compiled objects into Bluesim executable"
	bsc -sim -parallel-sim-link 4 \
	   $(TMP_DIRS) \
	   -e $(TOPMOD) -o ./$(SIM_EXE_FILE) \
	   $(BSC_C_FLAGS)
	@echo "INFO: Linking complete"

.PHONY: clean
clean:
	rm -r build_dir/*

.PHONY: deep_clean
deep_clean: clean
	rm -r $(SIM_EXE_FILE).so $(SIM_EXE_FILE) Verilog_RTL/*
