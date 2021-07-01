###  -*-Makefile-*-

DISTRO ?= ../..

# --------
# Mandatory command-line argument -- use to fix posit size
POSIT_SIZE ?= 16

# Mandatory command line arg -- select between RANDOM and Counter based stimulus
# STIMULUS ?= RANDOM
STIMULUS ?= COUNTER

# Mandatory command line arg -- select between SIM and FPGA
PLATFORM ?= SIM

# Mandatory command line arg -- The TB being built
TOPFILE ?= $(DISTRO)/src_bsv/tb/Quire_Tb.bsv

include $(DISTRO)/builds/Common.mk
