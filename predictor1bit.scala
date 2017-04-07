package patmos

import Chisel._
import Node._
import Constants._

class predictor1bit() extends Module {
// class predictor1bit(PC_SIZE: Int , PREDICTOR_INDEX: Int , PREDICTOR_WIDTH: Int) extends Module {
   val io = new Bundle {
      val PC_Fe = UInt(INPUT, PC_SIZE) // PC 
      val isBranch_Dec = UInt(INPUT, 1) // Identify branches from Decode
      // branch from EX
      val exfe = new ExFe().asInput
      val choose_PC = UInt(OUTPUT, 1)
      val target_out = UInt(OUTPUT, PC_SIZE) 
      val correct_PC = UInt(OUTPUT, 1)
      val prex = new PrEx().asOutput 
   }
   // Constant ADDRESSES
   val ADDR = 1 << PREDICTOR_INDEX // in VHDL : 2 ** PREDICTOR_INDEX - 1 
   // The main memory 
   val PC_Reg = Vec.fill(ADDR) { Reg(UInt(width=PC_SIZE)) } // Store PC
   val targetPC_Reg = Vec.fill(ADDR) { Reg(UInt(width=PC_SIZE)) } // Store target_PC
   val predictor = Vec.fill(ADDR) { Reg(UInt(width=PREDICTOR_WIDTH)) } // Store predictor
   // Pointer for the memory
   val pointer = Reg(init = UInt(0, PREDICTOR_INDEX)) 
   
   val PC_Fe_sig = Reg(init = UInt(0, PC_SIZE))
   PC_Fe_sig := io.PC_Fe
   val PC_feDec = Reg(init = UInt(0, PC_SIZE)) // But why ????? one extra delay here !!!!  
   PC_feDec := PC_Fe_sig
   val PC_decEx = Reg(init = UInt(0, PC_SIZE))
   PC_decEx := PC_feDec
   var found = UInt(width = ADDR) // One-hot signal to identify the data
   found := UInt(0) // some default
   // A tree of ORs between the bits of signal 'found'
   var found_OR = orR(found)
   
   val found_feDec = Reg(init = UInt(0, 1) )
   found_feDec := found_OR
   val found_decEx = Reg(init = UInt(0, 1) )
   found_decEx := found_feDec
   val isBranch_decEx_sig = Reg(init = UInt(0, 1)) // WTF is going on?
   isBranch_decEx_sig := io.isBranch_Dec
   val isBranch_decEx = Reg(init = UInt(0, 1))
   isBranch_decEx := isBranch_decEx_sig
   val index = UInt(width = PREDICTOR_INDEX)
   index := OHToUInt(found)
   val index_feDec = Reg(init = UInt(0, PREDICTOR_INDEX))
   index_feDec := index
   val index_decEx = Reg(init = UInt(0, PREDICTOR_INDEX))
   index_decEx := index_feDec
   val target_feDec = Reg(init = UInt(0, PC_SIZE)) // TODO Compare new with old target.
   target_feDec := targetPC_Reg(OHToUInt(found))
   val predictor_feDec = Reg(init = UInt(0, PREDICTOR_WIDTH))
   predictor_feDec := predictor(OHToUInt(found))
   val predictor_decEx = Reg(init = UInt(0, PREDICTOR_WIDTH))
   predictor_decEx := predictor_feDec
   // We will science the shit out of it!!!!!!!!!!!!
   
   io.choose_PC := found_feDec === UInt(1) && predictor_feDec === UInt(1) 
   io.target_out := target_feDec
   
   // Logic to control the manual flush
   when( predictor_decEx === UInt(1) && found_decEx === UInt(1) ){
      when( io.exfe.doBranch === UInt(1) ){
        io.correct_PC := UInt(0)
        io.prex.override_brflush := Bool(true)
        io.prex.override_brflush_value := Bool(false)
       }.otherwise{ 
        io.correct_PC := UInt(1)
        io.prex.override_brflush := Bool(true)
        io.prex.override_brflush_value := Bool(true)
      }
   } .otherwise{
//         when( io.exfe.doBranch === UInt(1) ){
//             io.correct_PC := UInt(0)
//             io.prex.override_brflush := Bool(false)
//             io.prex.override_brflush_value := Bool(false)
//         } .otherwise{ // all zeros
            io.correct_PC := UInt(0)
            io.prex.override_brflush := Bool(false)
            io.prex.override_brflush_value := Bool(false)
//         }
   }
   
   // The pointer increases one each time a new write operations occurs
   // WRITE!!!!
   when( isBranch_decEx === UInt(1) && found_decEx === UInt(0) && io.exfe.doBranch){ // Science bitches !!!
      pointer  := pointer + UInt(1)
      PC_Reg(UInt(pointer)) := PC_decEx
      targetPC_Reg(UInt(pointer)) := io.exfe.branchPc
      predictor(UInt(pointer)) := UInt(1)
   }.elsewhen( isBranch_decEx === UInt(1) && found_decEx === UInt(1) ){   
      when( io.exfe.doBranch =/= predictor(index_decEx) ){
         predictor(index_decEx) := ~predictor(index_decEx)
      }
   }.otherwise{
      pointer := pointer 
      PC_Reg(UInt(pointer)) := PC_Reg(UInt(pointer))
      targetPC_Reg(UInt(pointer)) := targetPC_Reg(UInt(pointer))
      predictor(UInt(pointer)) := predictor(UInt(pointer))
      // predictor(index_decEx) := predictor(index_decEx)
   }
   
   
   // Find if there is the PC inside the CAM memory
   for (k <- 0 until ADDR) {
      // Read over Writing : Take the new data! Way too Expensive!
      // when( UInt(k, PREDICTOR_INDEX)  === pointer && isBranch_decEx === UInt(1) && found_decEx === UInt(0) ){
         // when (PC_decEx === io.PC_Fe) {
            // found(k) := UInt(1)
         // }.otherwise{
            // found(k) := UInt(0)
         // }
      // }.otherwise{
         when (PC_Reg(UInt(k)) === io.PC_Fe) {
            found(k) := UInt(1)
         }.otherwise{
            found(k) := UInt(0)
         }
      // }
   }
}
