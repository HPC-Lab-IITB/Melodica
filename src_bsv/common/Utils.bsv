package Utils;

// ================================================================
// Copyright (c) 2013-2016 Bluespec, Inc. All Rights Reserved.

// Misc. useful definitions, not app-specific

// ================================================================
// A convenience function to return the current cycle number during Bluesim simulations

ActionValue #(Bit #(32)) cur_cycle = actionvalue
					Bit #(32) t <- $stime;
					return t / 10;
				     endactionvalue;

// ================================================================

interface LED_IFC;
   (* always_ready *)
   method Bool running;
   (* always_ready *)
   method Bool test_pass;
   (* always_ready *)
   method Bool test_fail;
endinterface

endpackage
