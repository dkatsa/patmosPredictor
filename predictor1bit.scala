package patmos

import Chisel._
import Node._
import Constants._

class predictor1bit() extends Module {
   val io = new Bundle {
      val PC_Fe = UInt(INPUT, PC_SIZE) // PC 
      val isBranch_Dec = Bool(INPUT) // Identify branches from Decode
      val exfe = new ExFe().asInput // branchPC and doBranch from EX
      val prex = new PrEx().asOutput 
      val test = Bool(OUTPUT)
      val choose_PC = UInt(OUTPUT,1)
      val correct_PC = UInt(OUTPUT,1)
      val target_out = UInt(OUTPUT,PC_SIZE)
      
      def defaults() = {
         correct_PC := UInt(0)
      }
      
   }
   // Constant ADDRESSES
   val ADDR = 1 << PREDICTOR_INDEX // in VHDL : 2 ** PREDICTOR_INDEX - 1 
   // Fetch #########################################################################
   //    The main memory 
   val PC_BTB = Vec.fill(ADDR) { Reg(UInt(width=PC_SIZE-ADDR)) } // Store PC
   val targetPC_Reg = Vec.fill(ADDR) { Reg(UInt(width=PC_SIZE)) } // Store target_PC
   val predictor = Vec.fill(ADDR) { Reg(UInt(width=PREDICTOR_WIDTH)) } // Store predictor
   // Decode #########################################################################
   val found_Dec = Reg(init = Bool(false), next = PC_BTB(io.PC_Fe(PREDICTOR_INDEX-1,0)) === io.PC_Fe(PC_SIZE-1,PREDICTOR_INDEX))
   val PC_Dec = Reg(init = UInt(0,PC_SIZE), next = io.PC_Fe)
   
   val PC_BTB_Dec = Reg(init = UInt(0,width=PC_SIZE-PREDICTOR_INDEX), next = PC_BTB(io.PC_Fe(PREDICTOR_INDEX-1,0)))  // Store PC
   val targetPC_Reg_Dec = Reg(init = UInt(0,width=PC_SIZE), next = targetPC_Reg(io.PC_Fe(PREDICTOR_INDEX-1,0)))  // Store target_PC
   val predictor_Dec = Reg(init = UInt(0,width=PREDICTOR_WIDTH), next = predictor(io.PC_Fe(PREDICTOR_INDEX-1,0)))  // Store predictor
   // Execute #########################################################################
   val found_Ex = Reg(init = Bool(false), next = found_Dec)
   val PC_Ex = Reg(init = UInt(0,PC_SIZE), next = PC_Dec)
   
   val isBranch_Ex = Reg(init = Bool(false), next = io.isBranch_Dec)
   
   val predictor_Ex = Reg(init = UInt(0,width=PREDICTOR_WIDTH), next = predictor_Dec)  // Store predictor
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
   // Fetch #########################################################################

   
   // Decode #########################################################################
   
   when ( io.isBranch_Dec && predictor_Dec === UInt(1)){
      io.choose_PC := UInt(1)
      io.target_out := targetPC_Reg_Dec
   }.otherwise{ 
      io.choose_PC := UInt(0)
      io.target_out := UInt(0)
   }
   
   // Execute #########################################################################
   
   // There isn't inside the memory.
   when( isBranch_Ex && io.exfe.doBranch && !found_Ex){
      PC_BTB(PC_Ex(PREDICTOR_INDEX-1,0)) := PC_Ex
      predictor(PC_Ex(PREDICTOR_INDEX-1,0)) := UInt(1)
      targetPC_Reg(PC_Ex(PREDICTOR_INDEX-1,0)) := io.exfe.branchPc
   // Else there is inside the memory and it misspredict.  
   }.elsewhen( isBranch_Ex && found_Ex ){
      when( predictor_Ex =/= io.exfe.doBranch ){
         predictor(PC_Ex(PREDICTOR_INDEX-1,0)) := ~predictor(PC_Ex(PREDICTOR_INDEX-1,0))
      }
   }
    
   // io.target_out := targetPC_Reg(PC_Ex)
    
   io.test :=  isBranch_Ex && io.isBranch_Dec
   
   
   
   // Logic to control the manual flush
   when( predictor(PC_Ex(PREDICTOR_INDEX-1,0)) === UInt(1) && found_Ex ){
      when( io.exfe.doBranch){
        io.correct_PC := UInt(0)
        io.prex.override_brflush := Bool(true)
        io.prex.override_brflush_value := Bool(false)
       }.otherwise{ 
        io.correct_PC := UInt(1)
        io.prex.override_brflush := Bool(true)
        io.prex.override_brflush_value := Bool(true)
      }
   } .otherwise{
      io.correct_PC := UInt(0)
      io.prex.override_brflush := Bool(false)
      io.prex.override_brflush_value := Bool(false)
   }
   
   
   
   // predictor(io.PC_Fe(ADDR-1,0))
   // io.PC_Fe(ADDR-1,0)
   
   // prex
   // val override_brflush = Bool()
   // val override_brflush_value = Bool()
   
   // exfe
   // io.exfe.doBranch
   // val branchPc 
   
   
   
   // Pointer for the memory
   // val pointer = Reg(init = UInt(0, PREDICTOR_INDEX)) 
   
   // val PC_Fe_sig = Reg(init = UInt(0, PC_SIZE))
   // val PC_feDec = Reg(init = UInt(0, PC_SIZE)) // But why ????? one extra delay here !!!!  
   // val PC_decEx = Reg(init = UInt(0, PC_SIZE))
   // var found = UInt(width = ADDR) // One-hot signal to identify the data
   // var found_OR = orR(found)
   // val found_feDec = Reg(init = UInt(0, 1) )
   // val found_decEx = Reg(init = UInt(0, 1) )
   // val isBranch_decEx = Reg(init = UInt(0, 1))
   // val index = UInt(width = PREDICTOR_INDEX)
   // val index_feDec = Reg(init = UInt(0, PREDICTOR_INDEX))
   // val index_decEx = Reg(init = UInt(0, PREDICTOR_INDEX))
   // val target_feDec = Reg(init = UInt(0, PC_SIZE)) // TODO Compare new with old target.
   // val predictor_feDec = Reg(init = UInt(0, PREDICTOR_WIDTH))
   // val predictor_decEx = Reg(init = UInt(0, PREDICTOR_WIDTH))
   // val debugging = Reg(Bool())
   // debugging := Bool(false)
   // io.test := debugging
   // PC_Fe_sig := io.PC_Fe
   // PC_feDec := PC_Fe_sig
   // PC_decEx := PC_feDec
   
   // found_feDec := found_OR
   // found_decEx := found_feDec
   // isBranch_decEx := io.isBranch_Dec
   // index := OHToUInt(found)
   // index_feDec := index
   // index_decEx := index_feDec
   // target_feDec := targetPC_Reg(OHToUInt(found))
   // predictor_feDec := predictor(OHToUInt(found))
   // predictor_decEx := predictor_feDec
   // We will science the shit out of it!!!!!!!!!!!!
   
   // io.choose_PC := found_feDec === UInt(1) && predictor_feDec === UInt(1) 
   // io.target_out := target_feDec
   
   // Logic to control the manual flush
   // when( predictor_decEx === UInt(1) && found_decEx === UInt(1) ){
      // when( io.exfe.doBranch === UInt(1) ){
        // io.correct_PC := UInt(0)
        // io.prex.override_brflush := Bool(true)
        // io.prex.override_brflush_value := Bool(false)
       // }.otherwise{ 
        // io.correct_PC := UInt(1)
        // io.prex.override_brflush := Bool(true)
        // io.prex.override_brflush_value := Bool(true)
      // }
   // } .otherwise{
//         when( io.exfe.doBranch === UInt(1) ){
//             io.correct_PC := UInt(0)
//             io.prex.override_brflush := Bool(false)
//             io.prex.override_brflush_value := Bool(false)
//         } .otherwise{ // all zeros
            // io.correct_PC := UInt(0)
            // io.prex.override_brflush := Bool(false)
            // io.prex.override_brflush_value := Bool(false)
//         }
   // }
   
   // debugging :=  isBranch_decEx === UInt(1) && found_decEx === UInt(0) && io.exfe.doBranch === UInt(1) 
   // The pointer increases one each time a new write operations occurs
   // WRITE!!!!
   
   // when( isBranch_decEx===UInt(1) && io.exfe.doBranch===UInt(1) && predictor(UInt(PC_decEx))===UInt(0) ){ // Science bitches !!!
      // targetPC_Reg(UInt(PC_decEx)) := io.exfe.branchPc
      // predictor(UInt(PC_decEx)) := UInt(1)
      // when( found_decEx === UInt(0) { 
         // pointer  := pointer + UInt(1)
         // PC_Reg(UInt(pointer)) := PC_decEx
         // targetPC_Reg(UInt(pointer)) := io.exfe.branchPc
         // predictor(UInt(pointer)) := UInt(1)
      // }.elsewhen( io.exfe.doBranch =/= predictor(PC_decEx) ){
         // predictor(index_decEx) := ~predictor(index_decEx)
      // }
   // }
   
   
   // Find if there is the PC inside the CAM memory
   // for (k <- 0 until ADDR) {
      // Read over Writing : Take the new data! Way too Expensive!
      // when( UInt(k, PREDICTOR_INDEX)  === pointer && isBranch_decEx === UInt(1) && found_decEx === UInt(0) ){
         // when (PC_decEx === io.PC_Fe) {
            // found(k) := UInt(1)
         // }.otherwise{
            // found(k) := UInt(0)
         // }
      // }.otherwise{
         // when (PC_Reg(UInt(k)) === io.PC_Fe) {
            // found(k) := UInt(1)
         // }.otherwise{
            // found(k) := UInt(0)
         // }
      // }
   // }
}
