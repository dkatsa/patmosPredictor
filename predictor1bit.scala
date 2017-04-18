package patmos

import Chisel._
import Node._
import Constants._

class predictor1bit() extends Module {
   val io = new Bundle {
      val PC_Fe = UInt(INPUT, PC_SIZE) // PC 
      val isBranch_Dec = Bool(INPUT) // Identify branches from Decode
      val exfe = new ExFe().asInput // branchPC and doBranch from EX
      val pr_ex = new PrEx().asOutput 
      val choose_PC = UInt(OUTPUT,1)
      val correct_PC = UInt(OUTPUT,1)
      val target_out = UInt(OUTPUT,PC_SIZE)
      
      
      // Outputs for Debugging 2017 \/\/\/\/\/
      val PC_Dec_deb = UInt(OUTPUT, PC_SIZE) 
      val PC_Ex_deb = UInt(OUTPUT, PC_SIZE) 
      val found_Ex_deb = Bool(OUTPUT)
      val isBranch_Ex_deb = Bool(OUTPUT)
      val predictor_Ex_deb = UInt(OUTPUT,1)
      val testWhen = Bool(OUTPUT)
      // Outputs for Debugging 2017 /\/\/\/\/\
      
      def defaults() = {
         choose_PC := UInt(0)
         correct_PC := UInt(0)
         target_out := UInt(0,PC_SIZE)
         testWhen := Bool(false)
      }
   }
   
   // default values
   io.defaults()
   io.pr_ex.defaults()
   
   // Constant ADDRESSES
   val ADDR = 1 << PREDICTOR_INDEX // in VHDL : 2 ** PREDICTOR_INDEX - 1  => 2**6 = 64
   val MSB = PC_SIZE - PREDICTOR_INDEX
   val PREDICTOR_INDEX_ONE = PREDICTOR_INDEX - 1
   val PC_SIZE_ONE = PC_SIZE - 1
//####### Fetch #########################################################################
   //    The main memory 
   val PC_BTB = Vec.fill(ADDR) { Reg(UInt(width=MSB)) } // Store PC     # 30-6 = 24
   val targetPC_Reg = Vec.fill(ADDR) { Reg(UInt(width=PC_SIZE)) } // Store target_PC # 30
   val predictor = Vec.fill(ADDR) { Reg(UInt(width=PREDICTOR_WIDTH)) } // Store predictor # 1
//####### Decode #########################################################################
   // Find inside BTB the respective PC 
   val found_D = Reg(init = Bool(false), next = (PC_BTB(io.PC_Fe(PREDICTOR_INDEX_ONE,0)) === io.PC_Fe(PC_SIZE_ONE,PREDICTOR_INDEX)) )
   val PC_Dec = Reg(init = UInt(0,PC_SIZE), next = io.PC_Fe)
   io.PC_Dec_deb := PC_Dec
   val PC_BTB_Dec = Reg(init = UInt(0,width=MSB), next = PC_BTB(io.PC_Fe(PREDICTOR_INDEX_ONE,0)))  // Store PC
   val targetPC_Reg_Dec = Reg(init = UInt(0,width=PC_SIZE), next = targetPC_Reg(io.PC_Fe(PREDICTOR_INDEX_ONE,0)))  // Store target_PC
   val predictor_Dec = Reg(init = UInt(0,width=PREDICTOR_WIDTH), next = predictor(io.PC_Fe(PREDICTOR_INDEX_ONE,0)))  // Store predictor
   
   val found_Dec = Mux(targetPC_Reg_Dec === UInt(0,PC_SIZE), Bool(false), found_D) // Exception for small PC with MSB all zeros
//####### Execute #########################################################################
   val found_Ex = Reg(init = Bool(false), next = found_Dec)
   io.found_Ex_deb := found_Ex
   val PC_Ex = Reg(init = UInt(0,PC_SIZE), next = PC_Dec)
   io.PC_Ex_deb := PC_Ex
   val targetPC_Reg_Ex = Reg(init = UInt(0,width=PC_SIZE), next = targetPC_Reg_Dec)  // Store target_PC in Execute
   val isBranch_Ex = Reg(init = Bool(false), next = io.isBranch_Dec)
   io.isBranch_Ex_deb := isBranch_Ex
   val predictor_Ex = Reg(init = UInt(0,width=PREDICTOR_WIDTH), next = predictor_Dec)  // Store predictor
   io.predictor_Ex_deb := predictor_Ex
   // val correct_PC_sig = Mux(( found_Ex && isBranch_Ex && (!io.exfe.doBranch) && (predictor_Ex === UInt(1))),UInt(1),UInt(0)) 
   
   // delay overrides
   val override_brflush_sig = Bool()
   val override_brflush_value_sig = Bool()
   val override_brflush_reg = Reg(init = Bool(false), next = override_brflush_sig)
   val override_brflush_value_reg = Reg(init = Bool(false), next = override_brflush_value_sig )
   override_brflush_sig := Bool(false)
   override_brflush_value_sig := Bool(false)
   
   io.pr_ex.override_brflush := override_brflush_reg || override_brflush_sig
   io.pr_ex.override_brflush_value := override_brflush_value_reg || override_brflush_value_sig
   
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//####### Debugging #########################################################################
   
   
   // io.testCorrect := predictor_Ex === UInt(1) && found_Ex && isBranch_Ex && !io.exfe.doBranch
   

//####### Fetch #########################################################################


   
//####### Decode #########################################################################
   
   when ( io.isBranch_Dec && (predictor_Dec === UInt(1)) && found_Dec){
      io.choose_PC := UInt(1)
      io.target_out := targetPC_Reg_Dec
   }.otherwise{ 
      io.choose_PC := UInt(0)
      io.target_out := UInt(0)
   }
   
   
   
//####### Execute #########################################################################
   
   // Logic for upadating the memories of BTB
   // There isn't inside the memory.
   when( isBranch_Ex && io.exfe.doBranch && !found_Ex){
      PC_BTB(PC_Ex(PREDICTOR_INDEX_ONE,0)) := PC_Ex(PC_SIZE_ONE,PREDICTOR_INDEX)
      predictor(PC_Ex(PREDICTOR_INDEX_ONE,0)) := UInt(1)
      targetPC_Reg(PC_Ex(PREDICTOR_INDEX_ONE,0)) := io.exfe.branchPc
   // Else there is inside the memory and it misspredict.  
   }.otherwise{ 
      when( isBranch_Ex && found_Ex && ((predictor_Ex === UInt(1)) ^ io.exfe.doBranch) ){
         predictor(PC_Ex(PREDICTOR_INDEX_ONE,0)) := ~predictor_Ex
      }
      when( isBranch_Ex && found_Ex && (predictor_Ex === UInt(1)) && io.exfe.doBranch && (io.exfe.branchPc =/= targetPC_Reg_Ex) ){
         targetPC_Reg(PC_Ex(PREDICTOR_INDEX_ONE,0)) := io.exfe.branchPc
      }.elsewhen( isBranch_Ex && found_Ex && (predictor_Ex === UInt(0)) && io.exfe.doBranch ){
         targetPC_Reg(PC_Ex(PREDICTOR_INDEX_ONE,0)) := io.exfe.branchPc
      }
   }
   
   // Logic to control the manual flush
   when( found_Ex && isBranch_Ex && (predictor_Ex === UInt(1))){
      when( io.exfe.doBranch){
        when( io.exfe.branchPc =/= targetPC_Reg_Ex ){
           override_brflush_sig := Bool(false) 
           override_brflush_value_sig := Bool(false) // Dont care
        }.otherwise{
           override_brflush_sig := Bool(true) 
           override_brflush_value_sig := Bool(false) 
        }
      }.otherwise{
        override_brflush_sig := Bool(true) 
        override_brflush_value_sig := Bool(true) 
      }
   }.otherwise{
      override_brflush_sig := Bool(false) 
      override_brflush_value_sig := Bool(false) 
   }
   
   
   io.testWhen := Bool(false)
   when( (io.found_Ex_deb && io.isBranch_Ex_deb) && ((! io.exfe.doBranch) && (io.predictor_Ex_deb === UInt(1)))){
      io.testWhen := Bool(true)
   }.otherwise{
      io.testWhen := Bool(false)
   }
   
   
   // io.correct_PC := correct_PC_sig
   when( (found_Ex && isBranch_Ex) && ((! io.exfe.doBranch) && (predictor_Ex === UInt(1)))){
      io.correct_PC := UInt(1) 
   }.otherwise{
      io.correct_PC := UInt(0)
   }
}
