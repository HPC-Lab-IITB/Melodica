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

# Mandatory command line arg -- the top-level bsv file which contains the TB
TOPFILE ?= $(DISTRO)/src_bsv/tb/Quire_Tb.bsv

# Mandatory command line arg -- the TB being built
TOPMOD ?= mkInitReadTb

include $(DISTRO)/builds/Common.mk
