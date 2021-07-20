package Scheduler_types;

// --------------------------------------------------------------
// This package defines:
//
// Types used in Scheduler module
// --------------------------------------------------------------

import Posit_User_Types :: *;
import Posit_Numeric_Types :: *;

//=====================================================================================================
//Type definitions
typedef 64 TCM_XLEN;	//	length of TCM_Word
typedef Bit #(TCM_XLEN)		TCM_Word;
typedef TDiv #(TCM_XLEN, 8)   Bytes_per_TCM_Word;	//8 bits per byte
typedef 16 TCM_ADDR;								//TCM address length
typedef Bit #(TCM_ADDR)		Addr;
Integer bytes_per_tcm_word        = valueOf (Bytes_per_TCM_Word);

// TCM Sizing
//-------------------------------------------
 Integer kB_per_TCM = 'h4;         // 4KB
//   Integer kB_per_TCM = 'h40;     // 64KB
// Integer kB_per_TCM = 'h80;     // 128KB
// Integer kB_per_TCM = 'h400;    // 1 MB
// Integer kB_per_TCM = 'h4000;    // 16 MB
Integer bytes_per_TCM = kB_per_TCM * 'h400;
//-------------------------------------------

Integer mem_size = ((bytes_per_TCM + bytes_per_tcm_word - 1) / bytes_per_tcm_word);	//an Integer specifying the memory size in number of words of type data.		
//Integer mem_size = 2048;				//No. of TCM words

typedef Bit #(TCM_ADDR) Mem_addr;

typedef Bit #(32) Vec_len;

typedef 16 PositWidth;

typedef 2 N_melodica; 			// No. of Melodicas

typedef TDiv#(TCM_XLEN,PositWidth) Vec_size;	// # of posits per TCM word
						// Vec_size should be greater than equal N_melodica

typedef TDiv#(Vec_size, N_melodica) Op_count; // 32


endpackage
