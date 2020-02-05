### Makefile for the ExtNorTest project
### Generated by Bluespec Workstation on Tue Apr 16 22:58:49 IST 2019

#make to compile link add,mul,mac,fdp,q-to-p
default: adder multiplier mac fdp qtop
#make to compile link adder only
adder: compile_adder link_adder link_adder_d 
#make to compile link multiplier
multiplier: compile_multiplier link_multiplier link_multiplier_d
#make to compile link mac
mac: compile_mac link_mac link_mac_d
#make to compile link fdp
fdp: compile_fdp link_fdp link_fdp_d 
#make to compile link q-to-p
qtop: compile_qtop link_qtop link_qtop_d 
#make to compile link p-to-q
ptoq: compile_ptoq link_ptoq link_ptoq_d 
#BSC_COMPILATION_FLAGS += \
#			 -D FPGA \
# Random or exhaustive testbench. Comment following line for exhaustive
BSC_COMPILATION_FLAGS += \
#		 -D RANDOM

OBJ = .o

TOPMOD = mkTestbench

BLUESPEC_LIB = %/Prelude:%/Libraries:%/Libraries/BlueNoC


CXXFAMILY=$(shell $(BLUESPECDIR)/bin/bsenv c++_family)


# Change me -- Where are the DISTRO objects
DISTRO ?= ./
SOFTPOSIT_OBJPATH = $(DISTRO)SoftPosit/build/Linux-x86_64-GCC

BSC_CFLAGS = \
		-Xc -lm\
		-Xc -I$(DISTRO)SoftPosit/source/include \
		-Xc++ -D_GLIBCXX_USE_CXX11_ABI=0

SOFTPOSIT_OBJS = \
$(SOFTPOSIT_OBJPATH)/s_addMagsP8$(OBJ) \
$(SOFTPOSIT_OBJPATH)/s_subMagsP8$(OBJ) \
$(SOFTPOSIT_OBJPATH)/s_mulAddP8$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p8_add$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p8_sub$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p8_mul$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p8_div$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p8_sqrt$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p8_to_p16$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p8_to_p32$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p8_to_pX2$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p8_to_i32$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p8_to_i64$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p8_to_ui32$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p8_to_ui64$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p8_roundToInt$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p8_mulAdd$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p8_eq$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p8_le$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p8_lt$(OBJ) \
$(SOFTPOSIT_OBJPATH)/quire8_fdp_add$(OBJ) \
$(SOFTPOSIT_OBJPATH)/quire8_fdp_sub$(OBJ) \
$(SOFTPOSIT_OBJPATH)/ui32_to_p8$(OBJ) \
$(SOFTPOSIT_OBJPATH)/ui64_to_p8$(OBJ) \
$(SOFTPOSIT_OBJPATH)/i32_to_p8$(OBJ) \
$(SOFTPOSIT_OBJPATH)/i64_to_p8$(OBJ) \
$(SOFTPOSIT_OBJPATH)/s_addMagsP16$(OBJ) \
$(SOFTPOSIT_OBJPATH)/s_subMagsP16$(OBJ) \
$(SOFTPOSIT_OBJPATH)/s_mulAddP16$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p16_to_ui32$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p16_to_ui64$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p16_to_i32$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p16_to_i64$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p16_to_p8$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p16_to_p32$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p16_to_pX2$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p16_roundToInt$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p16_add$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p16_sub$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p16_mul$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p16_mulAdd$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p16_div$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p16_eq$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p16_le$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p16_lt$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p16_sqrt$(OBJ) \
$(SOFTPOSIT_OBJPATH)/quire16_fdp_add$(OBJ) \
$(SOFTPOSIT_OBJPATH)/quire16_fdp_sub$(OBJ) \
$(SOFTPOSIT_OBJPATH)/quire_helper$(OBJ) \
$(SOFTPOSIT_OBJPATH)/ui32_to_p16$(OBJ) \
$(SOFTPOSIT_OBJPATH)/ui64_to_p16$(OBJ) \
$(SOFTPOSIT_OBJPATH)/i32_to_p16$(OBJ) \
$(SOFTPOSIT_OBJPATH)/i64_to_p16$(OBJ) \
$(SOFTPOSIT_OBJPATH)/s_addMagsP32$(OBJ) \
$(SOFTPOSIT_OBJPATH)/s_subMagsP32$(OBJ) \
$(SOFTPOSIT_OBJPATH)/s_mulAddP32$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p32_to_ui32$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p32_to_ui64$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p32_to_i32$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p32_to_i64$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p32_to_p8$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p32_to_p16$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p32_to_pX2$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p32_roundToInt$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p32_add$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p32_sub$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p32_mul$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p32_mulAdd$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p32_div$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p32_eq$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p32_le$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p32_lt$(OBJ) \
$(SOFTPOSIT_OBJPATH)/p32_sqrt$(OBJ) \
$(SOFTPOSIT_OBJPATH)/quire32_fdp_add$(OBJ) \
$(SOFTPOSIT_OBJPATH)/quire32_fdp_sub$(OBJ) \
$(SOFTPOSIT_OBJPATH)/ui32_to_p32$(OBJ) \
$(SOFTPOSIT_OBJPATH)/ui64_to_p32$(OBJ) \
$(SOFTPOSIT_OBJPATH)/i32_to_p32$(OBJ) \
$(SOFTPOSIT_OBJPATH)/i64_to_p32$(OBJ) \
$(SOFTPOSIT_OBJPATH)/s_approxRecipSqrt_1Ks$(OBJ) \
$(SOFTPOSIT_OBJPATH)/c_convertDecToPosit8$(OBJ) \
$(SOFTPOSIT_OBJPATH)/c_convertPosit8ToDec$(OBJ) \
$(SOFTPOSIT_OBJPATH)/c_convertDecToPosit16$(OBJ) \
$(SOFTPOSIT_OBJPATH)/c_convertPosit16ToDec$(OBJ) \
$(SOFTPOSIT_OBJPATH)/c_convertQuire8ToPosit8$(OBJ) \
$(SOFTPOSIT_OBJPATH)/c_convertQuire16ToPosit16$(OBJ) \
$(SOFTPOSIT_OBJPATH)/c_convertQuire32ToPosit32$(OBJ) \
$(SOFTPOSIT_OBJPATH)/c_convertDecToPosit32$(OBJ) \
$(SOFTPOSIT_OBJPATH)/c_convertPosit32ToDec$(OBJ) \
$(SOFTPOSIT_OBJPATH)/c_int$(OBJ) \
$(SOFTPOSIT_OBJPATH)/s_addMagsPX2$(OBJ) \
$(SOFTPOSIT_OBJPATH)/s_subMagsPX2$(OBJ) \
$(SOFTPOSIT_OBJPATH)/s_mulAddPX2$(OBJ) \
$(SOFTPOSIT_OBJPATH)/pX2_add$(OBJ) \
$(SOFTPOSIT_OBJPATH)/pX2_sub$(OBJ) \
$(SOFTPOSIT_OBJPATH)/pX2_mul$(OBJ) \
$(SOFTPOSIT_OBJPATH)/pX2_div$(OBJ) \
$(SOFTPOSIT_OBJPATH)/pX2_mulAdd$(OBJ) \
$(SOFTPOSIT_OBJPATH)/pX2_roundToInt$(OBJ) \
$(SOFTPOSIT_OBJPATH)/pX2_sqrt$(OBJ) \
$(SOFTPOSIT_OBJPATH)/pX2_eq$(OBJ) \
$(SOFTPOSIT_OBJPATH)/pX2_le$(OBJ) \
$(SOFTPOSIT_OBJPATH)/pX2_lt$(OBJ) \
$(SOFTPOSIT_OBJPATH)/ui32_to_pX2$(OBJ) \
$(SOFTPOSIT_OBJPATH)/ui64_to_pX2$(OBJ) \
$(SOFTPOSIT_OBJPATH)/i32_to_pX2$(OBJ) \
$(SOFTPOSIT_OBJPATH)/i64_to_pX2$(OBJ) \
$(SOFTPOSIT_OBJPATH)/c_convertQuireX2ToPositX2$(OBJ) 
 

# From bluespec installation
BSIM_INCDIR=$(BLUESPECDIR)/Bluesim
BSIM_LIBDIR=$(BSIM_INCDIR)/$(CXXFAMILY)
Testbench_Path = src_bsv/tb
# Change me - in case you change where compile-time files are created

#ADDER------------
ADDER_PATH = .:$(BLUESPEC_LIB):src_bsv/Adder:src_bsv/lib:src_bsv/common:$(Testbench_Path) 
BUILD_DIR_ADDER=builds/Adder
BUILD_BSIM_DIR_ADDER=builds/Adder
BSC_BUILDDIR_ADDER=-simdir $(BUILD_BSIM_DIR_ADDER) -bdir $(BUILD_DIR_ADDER) -info-dir $(BUILD_DIR_ADDER)
VERILOG_CODE_DIR_ADDER=VerilogCode/Adder
OUTPUT_ADDER = builds/Adder/output

#MULTIPLIER------------
MULTIPLIER_PATH = .:$(BLUESPEC_LIB):src_bsv/Multiplier:src_bsv/lib:src_bsv/common:$(Testbench_Path)  
BUILD_DIR_MULTIPLIER=builds/Multiplier
BUILD_BSIM_DIR_MULTIPLIER=builds/Multiplier
BSC_BUILDDIR_MULTIPLIER=-simdir $(BUILD_BSIM_DIR_MULTIPLIER) -bdir $(BUILD_DIR_MULTIPLIER) -info-dir $(BUILD_DIR_MULTIPLIER)
VERILOG_CODE_DIR_MULTIPLIER=VerilogCode/Multiplier
OUTPUT_MULTIPLIER = builds/Multiplier/output

#MAC------------
MAC_PATH = .:$(BLUESPEC_LIB):src_bsv/Mac:src_bsv/lib:src_bsv/common:$(Testbench_Path)  
BUILD_DIR_MAC=builds/Mac
BUILD_BSIM_DIR_MAC=builds/Mac
BSC_BUILDDIR_MAC=-simdir $(BUILD_BSIM_DIR_MAC) -bdir $(BUILD_DIR_MAC) -info-dir $(BUILD_DIR_MAC)
VERILOG_CODE_DIR_MAC=VerilogCode/Mac
OUTPUT_MAC = builds/Mac/output

#FDP------------
FDP_PATH = .:$(BLUESPEC_LIB):src_bsv/Fdp:src_bsv/lib:src_bsv/common:$(Testbench_Path)  
BUILD_DIR_FDP=builds/Fdp
BUILD_BSIM_DIR_FDP=builds/Fdp
BSC_BUILDDIR_FDP=-simdir $(BUILD_BSIM_DIR_FDP) -bdir $(BUILD_DIR_FDP) -info-dir $(BUILD_DIR_FDP)
VERILOG_CODE_DIR_FDP=VerilogCode/Fdp
OUTPUT_FDP = builds/Fdp/output

#QtoP------------
QtoP_PATH = .:$(BLUESPEC_LIB):src_bsv/QtoP:src_bsv/lib:src_bsv/common:$(Testbench_Path) 
BUILD_DIR_QtoP=builds/QtoP
BUILD_BSIM_DIR_QtoP=builds/QtoP
BSC_BUILDDIR_QtoP=-simdir $(BUILD_BSIM_DIR_QtoP) -bdir $(BUILD_DIR_QtoP) -info-dir $(BUILD_DIR_QtoP)
VERILOG_CODE_DIR_QtoP=VerilogCode/QtoP
OUTPUT_QtoP = builds/QtoP/output

#PtoQ------------
PtoQ_PATH = .:$(BLUESPEC_LIB):src_bsv/PtoQ:src_bsv/lib:src_bsv/common:$(Testbench_Path) 
BUILD_DIR_PtoQ=builds/PtoQ
BUILD_BSIM_DIR_PtoQ=builds/PtoQ
BSC_BUILDDIR_PtoQ=-simdir $(BUILD_BSIM_DIR_PtoQ) -bdir $(BUILD_DIR_PtoQ) -info-dir $(BUILD_DIR_PtoQ)
VERILOG_CODE_DIR_PtoQ=VerilogCode/PtoQ
OUTPUT_PtoQ = builds/PtoQ/output

# For final C++ link with main.cxx driver for non-BlueTcl version
CPP_FLAGS += \
	-static \
	-D_GLIBCXX_USE_CXX11_ABI=0 \
        -DNEW_MODEL_MKFOO=new_MODEL_$(TOPMOD) \
        -DMODEL_MKFOO_H=\"model_$(TOPMOD).h\" \
	-I$(BSIM_INCDIR) \
	-L$(BSIM_LIBDIR) \
	-L$(SOFTPOSIT_OBJPATH) \
	-O3 \

#COMPILE-------------------------------------------------------------------------------------
#ADDER
.PHONY: compile_adder
compile_adder:
	@echo Compiling...
	mkdir -p $(VERILOG_CODE_DIR_ADDER) $(BUILD_DIR_ADDER) $(BUILD_BSIM_DIR_ADDER) $(OUTPUT_ADDER)
	bsc -u -sim $(BSC_BUILDDIR_ADDER) $(BSC_COMPILATION_FLAGS) -keep-fires -aggressive-conditions -p $(ADDER_PATH) -g $(TOPMOD) $(Testbench_Path)/Add_Tb.bsv 
	bsc -u -elab -verilog $(BSC_BUILDDIR_ADDER) -vdir $(VERILOG_CODE_DIR_ADDER) $(BSC_COMPILATION_FLAGS) -keep-fires -aggressive-conditions -p $(ADDER_PATH) -g $(TOPMOD)  $(Testbench_Path)/Add_Tb.bsv
	@echo Compilation finished

#MULTIPLIER
.PHONY: compile_multiplier
compile_multiplier:
	@echo Compiling...
	mkdir -p $(VERILOG_CODE_DIR_MULTIPLIER) $(BUILD_DIR_MULTIPLIER) $(BUILD_BSIM_DIR_MULTIPLIER) $(OUTPUT_MULTIPLIER)
	bsc -u -sim $(BSC_BUILDDIR_MULTIPLIER) $(BSC_COMPILATION_FLAGS) -keep-fires -aggressive-conditions -p $(MULTIPLIER_PATH) -g $(TOPMOD)  $(Testbench_Path)/Mul_Tb.bsv 
	bsc -u -elab -verilog $(BSC_BUILDDIR_MULTIPLIER) -vdir $(VERILOG_CODE_DIR_MULTIPLIER) $(BSC_COMPILATION_FLAGS) -keep-fires -aggressive-conditions -p $(MULTIPLIER_PATH) -g $(TOPMOD)  $(Testbench_Path)/Mul_Tb.bsv
	@echo Compilation finished

#MAC
.PHONY: compile_mac
compile_mac:
	@echo Compiling...
	mkdir -p $(VERILOG_CODE_DIR_MAC) $(BUILD_DIR_MAC) $(BUILD_BSIM_DIR_MAC) $(OUTPUT_MAC)
	bsc -u -sim $(BSC_BUILDDIR_MAC) $(BSC_COMPILATION_FLAGS) -keep-fires -aggressive-conditions -p $(MAC_PATH) -g $(TOPMOD)  $(Testbench_Path)/Mac_Tb.bsv 
	bsc -u -elab -verilog $(BSC_BUILDDIR_MAC) -vdir $(VERILOG_CODE_DIR_MAC) $(BSC_COMPILATION_FLAGS) -keep-fires -aggressive-conditions -p $(MAC_PATH) -g $(TOPMOD)  $(Testbench_Path)/Mac_Tb.bsv
	@echo Compilation finished

#FDP
.PHONY: compile_fdp
compile_fdp:
	@echo Compiling...
	mkdir -p $(VERILOG_CODE_DIR_FDP) $(BUILD_DIR_FDP) $(BUILD_BSIM_DIR_FDP) $(OUTPUT_FDP)
	bsc -u -sim $(BSC_BUILDDIR_FDP) $(BSC_COMPILATION_FLAGS) -keep-fires -aggressive-conditions -p $(FDP_PATH) -g $(TOPMOD)  $(Testbench_Path)/Fdp_Tb.bsv 
	bsc -u -elab -verilog $(BSC_BUILDDIR_FDP) -vdir $(VERILOG_CODE_DIR_FDP) $(BSC_COMPILATION_FLAGS) -keep-fires -aggressive-conditions -p $(FDP_PATH) -g $(TOPMOD)  $(Testbench_Path)/Fdp_Tb.bsv
	@echo Compilation finished

#QtoP
.PHONY: compile_qtop
compile_qtop:
	@echo Compiling...
	mkdir -p $(VERILOG_CODE_DIR_QtoP) $(BUILD_DIR_QtoP) $(BUILD_BSIM_DIR_QtoP) $(OUTPUT_QtoP)
	bsc -u -sim $(BSC_BUILDDIR_QtoP) $(BSC_COMPILATION_FLAGS) -keep-fires -aggressive-conditions -p $(QtoP_PATH) -g $(TOPMOD)  $(Testbench_Path)/QtoP_Tb.bsv 
	bsc -u -elab -verilog $(BSC_BUILDDIR_QtoP) -vdir $(VERILOG_CODE_DIR_QtoP) $(BSC_COMPILATION_FLAGS) -keep-fires -aggressive-conditions -p $(QtoP_PATH) -g $(TOPMOD)  $(Testbench_Path)/QtoP_Tb.bsv
	@echo Compilation finished

#PtoQ
.PHONY: compile_ptoq
compile_ptoq:
	@echo Compiling...
	mkdir -p $(VERILOG_CODE_DIR_PtoQ) $(BUILD_DIR_PtoQ) $(BUILD_BSIM_DIR_PtoQ) $(OUTPUT_PtoQ)
	bsc -u -sim $(BSC_BUILDDIR_PtoQ) $(BSC_COMPILATION_FLAGS) -keep-fires -aggressive-conditions -p $(PtoQ_PATH) -g $(TOPMOD)  $(Testbench_Path)/PtoQ_Tb.bsv 
	bsc -u -elab -verilog $(BSC_BUILDDIR_PtoQ) -vdir $(VERILOG_CODE_DIR_PtoQ) $(BSC_COMPILATION_FLAGS) -keep-fires -aggressive-conditions -p $(PtoQ_PATH) -g $(TOPMOD)  $(Testbench_Path)/PtoQ_Tb.bsv
	@echo Compilation finished


#LINK----------------------------------------------------------------------------------------------
#ADDER
.PHONY: link_adder
link_adder:
	@echo Linking...
	bsc -e $(TOPMOD) -sim -o $(OUTPUT_ADDER)/out -simdir $(BUILD_DIR_ADDER) -p $(ADDER_PATH) -bdir $(BUILD_DIR_ADDER) -keep-fires -aggressive-conditions  $(BSC_CFLAGS) $(DISTRO)src_c/softposit_wrappers.c $(OBJS_OTHERS) 
	@echo Linking finished

#MULTIPLIER
.PHONY: link_multiplier
link_multiplier:
	@echo Linking...
	bsc -e $(TOPMOD) -sim -o $(OUTPUT_MULTIPLIER)/out -simdir $(BUILD_DIR_MULTIPLIER) -p $(MULTIPLIER_PATH) -bdir $(BUILD_DIR_MULTIPLIER) -keep-fires -aggressive-conditions  $(BSC_CFLAGS) $(DISTRO)src_c/softposit_wrappers.c $(OBJS_OTHERS) 
	@echo Linking finished

#MAC
.PHONY: link_mac
link_mac:
	@echo Linking...
	bsc -e $(TOPMOD) -sim -o $(OUTPUT_MAC)/out -simdir $(BUILD_DIR_MAC) -p $(MAC_PATH) -bdir $(BUILD_DIR_MAC) -keep-fires -aggressive-conditions  $(BSC_CFLAGS) $(DISTRO)src_c/softposit_wrappers.c $(OBJS_OTHERS) 
	@echo Linking finished

#FDP
.PHONY: link_fdp
link_fdp:
	@echo Linking...
	bsc -e $(TOPMOD) -sim -o $(OUTPUT_FDP)/out -simdir $(BUILD_DIR_FDP) -p $(FDP_PATH) -bdir $(BUILD_DIR_FDP) -keep-fires -aggressive-conditions  $(BSC_CFLAGS) $(DISTRO)src_c/softposit_wrappers.c $(OBJS_OTHERS) 
	@echo Linking finished

#QtoP
.PHONY: link_qtop
link_qtop:
	@echo Linking...
	bsc -e $(TOPMOD) -sim -o $(OUTPUT_QtoP)/out -simdir $(BUILD_DIR_QtoP) -p $(QtoP_PATH) -bdir $(BUILD_DIR_QtoP) -keep-fires -aggressive-conditions  $(BSC_CFLAGS) $(DISTRO)src_c/softposit_wrappers.c$(OBJS_OTHERS) 
	@echo Linking finished

#PtoQ
.PHONY: link_ptoq
link_ptoq:
	@echo Linking...
	bsc -e $(TOPMOD) -sim -o $(OUTPUT_PtoQ)/out -simdir $(BUILD_DIR_PtoQ) -p $(PtoQ_PATH) -bdir $(BUILD_DIR_PtoQ) -keep-fires -aggressive-conditions  $(BSC_CFLAGS) $(DISTRO)src_c/softposit_wrappers.c $(OBJS_OTHERS) 
	@echo Linking finished

#LINK_d------------------------------------------------------------------------------------------------
BLUESIM_MAIN_CXX = $(DISTRO)/BSV_Additional_Libs/C++/bluesim_main.cxx

#ADDER
.PHONY: link_adder_d
link_adder_d:
	@echo 'Linking for distributable Bluesim (without Bluetcl driver)'
	c++ $(CPP_FLAGS) \
		-I$(BUILD_BSIM_DIR_ADDER) \
		-o $(OUTPUT_ADDER)/out_adder \
		$(BLUESIM_MAIN_CXX) \
		$(BUILD_BSIM_DIR_ADDER)/*.o \
		$(SOFTPOSIT_OBJS) \
		$(DISTRO)src_c/softposit_wrappers.o \
		-static-libgcc \
		-static-libstdc++ \
		-lbskernel -lbsprim \
		-lpthread
	@echo 'Linking finished for distributable Bluesim (without Bluetcl driver)'

#MULTIPLIER
.PHONY: link_multiplier_d
link_multiplier_d:
	@echo 'Linking for distributable Bluesim (without Bluetcl driver)'
	c++ $(CPP_FLAGS) \
		-I$(BUILD_BSIM_DIR_MULTIPLIER) \
		-o $(OUTPUT_MULTIPLIER)/out_multiplier \
		$(BLUESIM_MAIN_CXX) \
		$(BUILD_BSIM_DIR_MULTIPLIER)/*.o \
		$(SOFTPOSIT_OBJS) \
		$(DISTRO)src_c/softposit_wrappers.o \
		-static-libgcc \
		-static-libstdc++ \
		-lbskernel -lbsprim \
		-lpthread
	@echo 'Linking finished for distributable Bluesim (without Bluetcl driver)'


#MAC
.PHONY: link_mac_d
link_mac_d:
	@echo 'Linking for distributable Bluesim (without Bluetcl driver)'
	c++ $(CPP_FLAGS) \
		-I$(BUILD_BSIM_DIR_MAC) \
		-o $(OUTPUT_MAC)/out_mac \
		$(BLUESIM_MAIN_CXX) \
		$(BUILD_BSIM_DIR_MAC)/*.o \
		$(SOFTPOSIT_OBJS) \
		$(DISTRO)src_c/softposit_wrappers.o \
		-static-libgcc \
		-static-libstdc++ \
		-lbskernel -lbsprim \
		-lpthread
	@echo 'Linking finished for distributable Bluesim (without Bluetcl driver)'

#FDP
.PHONY: link_fdp_d
link_fdp_d:
	@echo 'Linking for distributable Bluesim (without Bluetcl driver)'
	c++ $(CPP_FLAGS) \
		-I$(BUILD_BSIM_DIR_FDP) \
		-o $(OUTPUT_FDP)/out_fdp \
		$(BLUESIM_MAIN_CXX) \
		$(BUILD_BSIM_DIR_FDP)/*.o \
		$(SOFTPOSIT_OBJS) \
		$(DISTRO)src_c/softposit_wrappers.o \
		-static-libgcc \
		-static-libstdc++ \
		-lbskernel -lbsprim \
		-lpthread
	@echo 'Linking finished for distributable Bluesim (without Bluetcl driver)'

#QtoP
.PHONY: link_qtop_d
link_qtop_d:
	@echo 'Linking for distributable Bluesim (without Bluetcl driver)'
	c++ $(CPP_FLAGS) \
		-I$(BUILD_BSIM_DIR_QtoP) \
		-o $(OUTPUT_QtoP)/out_qtop \
		$(BLUESIM_MAIN_CXX) \
		$(BUILD_BSIM_DIR_QtoP)/*.o \
		$(SOFTPOSIT_OBJS) \
		$(DISTRO)src_c/softposit_wrappers.o \
		-static-libgcc \
		-static-libstdc++ \
		-lbskernel -lbsprim \
		-lpthread
	@echo 'Linking finished for distributable Bluesim (without Bluetcl driver)'

#PtoQ
.PHONY: link_ptoq_d
link_ptoq_d:
	@echo 'Linking for distributable Bluesim (without Bluetcl driver)'
	c++ $(CPP_FLAGS) \
		-I$(BUILD_BSIM_DIR_PtoQ) \
		-o $(OUTPUT_PtoQ)/out_ptoq \
		$(BLUESIM_MAIN_CXX) \
		$(BUILD_BSIM_DIR_PtoQ)/*.o \
		$(SOFTPOSIT_OBJS) \
		$(DISTRO)src_c/softposit_wrappers.o \
		-static-libgcc \
		-static-libstdc++ \
		-lbskernel -lbsprim \
		-lpthread
	@echo 'Linking finished for distributable Bluesim (without Bluetcl driver)'

#SIMULATE---------------------------------------------------------------------------------------------
#ADDER
.PHONY: simulate_adder
simulate_adder:
	@echo Simulation...
	./$(OUTPUT_ADDER)/out_adder 
	@echo Simulation finished

#MULTIPLIER
.PHONY: simulate_multiplier
simulate_multiplier:
	@echo Simulation...
	./$(OUTPUT_MULTIPLIER)/out_multiplier 
	@echo Simulation finished

#MAC
.PHONY: simulate_mac
simulate_mac:
	@echo Simulation...
	./$(OUTPUT_MAC)/out_mac 
	@echo Simulation finished

#FDP
.PHONY: simulate_fdp
simulate_fdp:
	@echo Simulation...
	./$(OUTPUT_FDP)/out_fdp 
	@echo Simulation finished

#Q-TO-P
.PHONY: simulate_qtop
simulate_qtop:
	@echo Simulation...
	./$(OUTPUT_QtoP)/out_qtop 
	@echo Simulation finished

#P-TO-Q
.PHONY: simulate_ptoq
simulate_ptoq:
	@echo Simulation...
	./$(OUTPUT_PtoQ)/out_ptoq 
	@echo Simulation finished


#CLEAN------------------------------------------------------------------------------------------------------------
#ADDER
.PHONY: clean_adder
clean_adder:
	rm -r ./builds/Adder ./VerilogCode/Adder $(OUTPUT_ADDER)

#MULTIPLIER
.PHONY: clean_multiplier
clean_multiplier:
	rm -r ./builds/Multiplier ./VerilogCode/Multiplier $(OUTPUT_MULTIPLIER)

#MAC
.PHONY: clean_mac
clean_mac:
	rm -r ./builds/Mac ./VerilogCode/Mac $(OUTPUT_MAC)

#FDP
.PHONY: clean_fdp
clean_fdp:
	rm -r ./builds/Fdp ./VerilogCode/Fdp $(OUTPUT_FDP)

#Q-TO-P
.PHONY: clean_qtop
clean_qtop:
	rm -r ./builds/QtoP ./VerilogCode/QtoP $(OUTPUT_QtoP)

#P-to-Q
.PHONY: clean_ptoq
clean_ptoq:
	rm -r ./builds/PtoQ ./VerilogCode/PtoQ $(OUTPUT_PtoQ)

 
.PHONY: full_clean
full_clean:
	rm -r ./builds ./VerilogCode $(OUTPUT_ADDER) $(OUTPUT_MULTIPLIER) $(OUTPUT_MAC) $(OUTPUT_FDP) $(OUTPUT_QtoP) 
