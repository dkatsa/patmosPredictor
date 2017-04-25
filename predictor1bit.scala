package patmos

import Chisel._
import Node._
import Constants._

class predictor1bit() extends Module {
   val io = new Bundle {
      val ena = Bool(INPUT)
      val PC_Fe = UInt(INPUT, PC_SIZE) // PC 
      val isBranch_Dec = Bool(INPUT) // Identify branches from Decode
      val exfe = new ExFe().asInput // branchPC and doBranch from EX
      val pr_ex = new PrEx().asOutput 
      val choose_PC = UInt(OUTPUT,1)
      val correct_PC = UInt(OUTPUT,1)
      val target_out = UInt(OUTPUT,PC_SIZE)
      
      // call from MEM
      val memfe = new MemFe().asInput
      // Stall correct
      val Stall_correct = Bool(OUTPUT)
      
      def defaults() = {
         choose_PC := UInt(0)
         correct_PC := UInt(0)
         target_out := UInt(0,PC_SIZE)
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
   val correct_Reg = Reg(init = Bool(false), next = io.correct_PC === UInt(1))
   val enableReg = Reg(init = Bool(false), next = io.ena)
   val PC_BTB = Vec.fill(ADDR) { Reg(UInt(width=MSB)) } // Store PC     # 30-6 = 24
   val targetPC_Reg = Vec.fill(ADDR) { Reg(UInt(width=PC_SIZE)) } // Store target_PC # 30
   val predictor = Vec.fill(ADDR) { Reg(UInt(width=PREDICTOR_WIDTH)) } // Store predictor # 1
   
   val correct_stall = Reg(init = Bool(false) )
   
//####### Decode #########################################################################
   // Find inside BTB the respective PC 
   val Correct_Enable = Reg(init = Bool(false))
   val found_D = Reg(init = Bool(false), next = ((PC_BTB(io.PC_Fe(PREDICTOR_INDEX_ONE,0)) === io.PC_Fe(PC_SIZE_ONE,PREDICTOR_INDEX)) && io.ena  ))
   val PC_Dec = Reg(init = UInt(0,PC_SIZE), next = io.PC_Fe)
   val PC_BTB_Dec = Reg(init = UInt(0,width=MSB), next = PC_BTB(io.PC_Fe(PREDICTOR_INDEX_ONE,0)))  // Store PC
   val targetPC_Reg_Dec = Reg(init = UInt(0,width=PC_SIZE), next = targetPC_Reg(io.PC_Fe(PREDICTOR_INDEX_ONE,0)))  // Store target_PC
   val predictor_Dec_Res = Reg(init = UInt(0,width=PREDICTOR_WIDTH), next = predictor(io.PC_Fe(PREDICTOR_INDEX_ONE,0)))  // Store predictor
   //Forwarding 
   val predictor_Dec = Mux(io.exfe.doBranch && (! io.pr_ex.override_brflush) && (!io.pr_ex.override_brflush_value) && io.ena ,UInt(0), predictor_Dec_Res)
   // Delay doCallRet
   val doCallRet_Dec = Reg(init = Bool(false), next = io.memfe.doCallRet)
   // Avoid pseudoFounds for small PC. Fix me with more efficiency way!!!!!! 
   val found_Dec = Mux(targetPC_Reg_Dec === UInt(0,PC_SIZE), Bool(false), found_D) // Exception for small PC with MSB all zeros. 
//####### Execute #########################################################################
   val found_Ex = Reg(init = Bool(false), next = found_Dec)
   val PC_Ex = Reg(init = UInt(0,PC_SIZE), next = PC_Dec)
   val targetPC_Reg_Ex = Reg(init = UInt(0,width=PC_SIZE), next = targetPC_Reg_Dec)  // Store target_PC in Execute
   val isBranch_Ex = Reg(init = Bool(false), next = (io.isBranch_Dec && io.ena) )
   val predictor_Ex = Reg(init = UInt(0,width=PREDICTOR_WIDTH), next = predictor_Dec)  // Store predictor
   // Delay doCallRet
   val doCallRet_Ex = Reg(init = Bool(false), next = doCallRet_Dec)
   
   
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
//####### Debugging ###########################################################
   

//####### Fetch ###############################################################

 // (correct_Reg && enableReg && !io.ena)
   
 // On the edge when correct is about to close store Information for after enable  
   when(correct_Reg && enableReg && !io.ena){
      correct_stall := Bool(true)
   }.elsewhen(io.ena){
      correct_stall := Bool(false)
   }
   io.Stall_correct := correct_stall
   
//####### Decode ##############################################################
   
   when ( (predictor(io.PC_Fe(PREDICTOR_INDEX_ONE,0)) === UInt(1)) && (PC_BTB(io.PC_Fe(PREDICTOR_INDEX_ONE,0)) === io.PC_Fe(PC_SIZE_ONE,PREDICTOR_INDEX)) && io.ena){
      io.choose_PC := UInt(1)
      io.target_out := targetPC_Reg(io.PC_Fe(PREDICTOR_INDEX_ONE,0))
   }.otherwise{ 
      io.choose_PC := UInt(0)
      io.target_out := UInt(0)
   }
      
//####### Execute #############################################################
   
   // Logic for upadating the memories of BTB
   // There isn't inside the memory.
   when(io.ena){
      when( isBranch_Ex && io.exfe.doBranch && !found_Ex){
         PC_BTB(PC_Ex(PREDICTOR_INDEX_ONE,0)) := PC_Ex(PC_SIZE_ONE,PREDICTOR_INDEX)
         predictor(PC_Ex(PREDICTOR_INDEX_ONE,0)) := UInt(1)
         targetPC_Reg(PC_Ex(PREDICTOR_INDEX_ONE,0)) := io.exfe.branchPc
      // Else there is inside the memory and it misspredict.  
      }.otherwise{ 
         // when( isBranch_Ex && found_Ex && ((predictor_Ex === UInt(1)) ^ io.exfe.doBranch) ){
         when( isBranch_Ex && found_Ex && ((predictor_Ex === UInt(1)) && (! io.exfe.doBranch)) ){
            predictor(PC_Ex(PREDICTOR_INDEX_ONE,0)) := UInt(0)
            PC_BTB(PC_Ex(PREDICTOR_INDEX_ONE,0)) := UInt(0,MSB)
         }.elsewhen( isBranch_Ex && found_Ex && ((predictor_Ex === UInt(0)) && io.exfe.doBranch) ){ // Maybe remove it!
            predictor(PC_Ex(PREDICTOR_INDEX_ONE,0)) := UInt(1)
            PC_BTB(PC_Ex(PREDICTOR_INDEX_ONE,0)) := PC_Ex(PC_SIZE_ONE,PREDICTOR_INDEX)
         }
         
         
         // Different Target with the predicted one 
         when( isBranch_Ex && found_Ex && io.exfe.doBranch ){
            targetPC_Reg(PC_Ex(PREDICTOR_INDEX_ONE,0)) := io.exfe.branchPc
         }
      }
      when((found_Dec && ((predictor_Dec_Res === UInt(1)) && (!io.isBranch_Dec) && io.ena))) {
         predictor(PC_Dec(PREDICTOR_INDEX_ONE,0)) := UInt(0)
         PC_BTB(PC_Dec(PREDICTOR_INDEX_ONE,0)) := UInt(0,MSB)
      }
   }
   
   // when( (found_Ex && (predictor_Ex === UInt(1)) && (!doCallRet_Ex))  || ((Correct_Enable || correct_stall ) && io.ena ) ){
<<<<<<< HEAD
<<<<<<< HEAD
   when( (found_Ex && (predictor_Ex === UInt(1)) && (!doCallRet_Ex))  || ((Correct_Enable ) && io.ena ) || (found_Dec && ((predictor_Dec_Res === UInt(1)) && (!io.isBranch_Dec) && io.ena)) ){
=======
   when( (found_Ex && (predictor_Ex === UInt(1)) && (!doCallRet_Ex))  || ((Correct_Enable ) && io.ena ) ){
>>>>>>> parent of 43db3c1... correct on decode!
=======
   when( (found_Ex && (predictor_Ex === UInt(1)) && (!doCallRet_Ex))  || ((Correct_Enable ) && io.ena ) ){
>>>>>>> parent of 43db3c1... correct on decode!
      when( io.exfe.doBranch){
        when( io.exfe.branchPc =/= targetPC_Reg_Ex ){
           io.pr_ex.override_brflush := Bool(false) 
           io.pr_ex.override_brflush_value := Bool(false) // Dont care
        }.otherwise{
           io.pr_ex.override_brflush := Bool(true) 
           io.pr_ex.override_brflush_value := Bool(false) 
        }
      }.otherwise{
        io.pr_ex.override_brflush := Bool(true) 
        io.pr_ex.override_brflush_value := Bool(true) 
      }
   }.otherwise{
      io.pr_ex.override_brflush := Bool(false) 
      io.pr_ex.override_brflush_value := Bool(false) 
   }
   
<<<<<<< HEAD
<<<<<<< HEAD
   when(( (found_Ex ) && ((! io.exfe.doBranch) && (predictor_Ex === UInt(1))) && (!doCallRet_Ex)) || (Correct_Enable && io.ena) || (found_Dec && ((predictor_Dec_Res === UInt(1)) && (!io.isBranch_Dec) && io.ena)) ) {
=======
   when(( (found_Ex ) && ((! io.exfe.doBranch) && (predictor_Ex === UInt(1))) && (!doCallRet_Ex)) || (Correct_Enable && io.ena) ) {
>>>>>>> parent of 43db3c1... correct on decode!
=======
   when(( (found_Ex ) && ((! io.exfe.doBranch) && (predictor_Ex === UInt(1))) && (!doCallRet_Ex)) || (Correct_Enable && io.ena) ) {
>>>>>>> parent of 43db3c1... correct on decode!
      Correct_Enable := ! io.ena
      io.correct_PC := UInt(1) 
   }.otherwise{
      io.correct_PC := UInt(0)
   }
}




// FSM
  
// val s_idle :: s_5 :: s_10 :: s_15 :: s_ok :: Nil = Enum(5){ UFix() } // Count the Enums !!!!!!!!
// val state = Reg(init = s_idle)
  
// when ( state === s_idle ){
   // when(  ) {
   // }
// }
  
  
  
  
  