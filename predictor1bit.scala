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
      
      val correct_on_decode_EN = Bool(OUTPUT)
      // call from MEM
      val memfe = new MemFe().asInput
      // Stall correct
      val flush = Bool(INPUT)
      // NonDelayed Comands
      val decex_jmpOp_branch = Bool(INPUT)
      val decex_nonDelayed = Bool(INPUT)
      
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
   val ADDR = 1 << PREDICTOR_INDEX // 2 ** PREDICTOR_INDEX - 1  => 2**6 = 64
   val MSB = PC_SIZE - PREDICTOR_INDEX
   val PREDICTOR_INDEX_ONE = PREDICTOR_INDEX - 1
   val PC_SIZE_ONE = PC_SIZE - 1
//####### Fetch #########################################################################
   //    The main memory 
   val PC_BTB = Vec.fill(ADDR) { Reg(UInt(width=MSB)) } // Store PC     # 30-6 = 24
   val targetPC_Reg = Vec.fill(ADDR) { Reg(UInt(width=PC_SIZE)) } // Store target_PC # 30
   val predictor = Vec.fill(ADDR) { Reg(init = UInt(1,width=2) ) } // Store predictor # 1
   
   
//####### Decode #########################################################################
   // Find inside BTB the respective PC 
   val found_D = Reg(init = Bool(false) )
   val PC_Dec = Reg(init = UInt(0,PC_SIZE) )
   val PC_BTB_Dec = Reg(init = UInt(0,width=MSB) )  // Store PC
   val targetPC_Reg_Dec = Reg(init = UInt(0,width=PC_SIZE) )  // Store target_PC
   val predictor_Dec_Res = Reg(init = UInt(0,width=2) )  // Store predictor
   //Forwarding ... When a choose happened , on decode, check the state of the overrides
   val predictor_Dec = Mux(io.exfe.doBranch && (! io.pr_ex.override_brflush) && (!io.pr_ex.override_brflush_value) && io.ena ,UInt(0,2), predictor_Dec_Res) /// 2bit change value
   // Delay doCallRet
   
   val choose_PC_Dec = Reg(init = Bool(false) )
   // Avoid pseudoFounds for small PC. Fix me with more efficiency way!!!!!! 
   val found_Dec = Mux(targetPC_Reg_Dec === UInt(0,PC_SIZE), Bool(false), found_D) // Exception for small PC with MSB all zeros. 
   
   val correct_on_decode = Mux( ! io.isBranch_Dec , Bool(true), Bool(false))
//####### Execute #########################################################################
   val correct_on_decode_Ex = Reg(init = Bool(false) )
   val choose_PC_Ex = Reg(init = Bool(false) )
   
   val found_Ex = Reg(init = Bool(false) )
   val PC_Ex = Reg(init = UInt(0,PC_SIZE) )
   val targetPC_Reg_Ex = Reg(init = UInt(0,width=PC_SIZE) )  // Store target_PC in Execute
   val isBranch_Ex = Reg(init = Bool(false) )
   val predictor_Ex = Reg(init = UInt(0,width=2) )  // Store predictor
   
   
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
//####### Stall with enable closed ############################################
 
   // Those are the nexts of all the Flip-Flops
   when(io.ena){
   // Fetch
   // Decode
      found_D := ((PC_BTB(io.PC_Fe(PREDICTOR_INDEX_ONE,0)) === io.PC_Fe(PC_SIZE_ONE,PREDICTOR_INDEX)) && io.ena  )
      PC_Dec := io.PC_Fe
      PC_BTB_Dec := PC_BTB(io.PC_Fe(PREDICTOR_INDEX_ONE,0))
      targetPC_Reg_Dec := targetPC_Reg(io.PC_Fe(PREDICTOR_INDEX_ONE,0))
      predictor_Dec_Res := predictor(io.PC_Fe(PREDICTOR_INDEX_ONE,0))
      choose_PC_Dec := (io.choose_PC === UInt(1)) && (! io.memfe.doCallRet) && (! io.flush) && (! io.exfe.doBranch)
   // Execute
      correct_on_decode_Ex := correct_on_decode
      choose_PC_Ex := (choose_PC_Dec && (! io.flush) ) 
      found_Ex := (found_Dec && (!(correct_on_decode && (choose_PC_Dec && (! io.flush)))) && (! io.exfe.doBranch))
      PC_Ex := PC_Dec
      targetPC_Reg_Ex := targetPC_Reg_Dec
      isBranch_Ex := (io.isBranch_Dec && io.ena)
      predictor_Ex := predictor_Dec
   }.otherwise{
      found_D := found_D
      PC_Dec := PC_Dec
      PC_BTB_Dec := PC_BTB_Dec
      predictor_Dec_Res := predictor_Dec_Res
      targetPC_Reg_Dec := targetPC_Reg_Dec
      choose_PC_Dec := choose_PC_Dec
      correct_on_decode_Ex := correct_on_decode_Ex
      choose_PC_Ex := choose_PC_Ex
      found_Ex := found_Ex
      PC_Ex := PC_Ex
      targetPC_Reg_Ex := targetPC_Reg_Ex
      isBranch_Ex := isBranch_Ex
      predictor_Ex := predictor_Ex
   }
 
//####### Fetch ###############################################################

       
//####### Decode ##############################################################
   
   when ( (predictor(io.PC_Fe(PREDICTOR_INDEX_ONE,0)) === UInt(2,2) || predictor(io.PC_Fe(PREDICTOR_INDEX_ONE,0)) === UInt(3,2))
           && (PC_BTB(io.PC_Fe(PREDICTOR_INDEX_ONE,0)) === io.PC_Fe(PC_SIZE_ONE,PREDICTOR_INDEX)) 
           && (!(io.decex_jmpOp_branch && (! io.decex_nonDelayed))) && io.ena && (! io.flush)
           && (targetPC_Reg(io.PC_Fe(PREDICTOR_INDEX_ONE,0)) =/= UInt(0,PC_SIZE))){  // Avoid target with Zero
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
         targetPC_Reg(PC_Ex(PREDICTOR_INDEX_ONE,0)) := io.exfe.branchPc
         when(predictor_Ex === UInt(1,2)){
            predictor(PC_Ex(PREDICTOR_INDEX_ONE,0)) := UInt(3,2)
         }.elsewhen(predictor_Ex === UInt(0,2) || predictor_Ex === UInt(2,2)){
            predictor(PC_Ex(PREDICTOR_INDEX_ONE,0)) := predictor_Ex + UInt(1,2)
         }
      // Else there is inside the memory and it misspredict.  
      }.otherwise{ 
         when( isBranch_Ex && found_Ex && choose_PC_Ex && (! io.exfe.doBranch)) {
            PC_BTB(PC_Ex(PREDICTOR_INDEX_ONE,0)) := UInt(0,MSB)
            when(predictor_Ex === UInt(2,2)){
               predictor(PC_Ex(PREDICTOR_INDEX_ONE,0)) := UInt(0,2)
            }.elsewhen(predictor_Ex === UInt(1,2) || predictor_Ex === UInt(3,2)){
                  predictor(PC_Ex(PREDICTOR_INDEX_ONE,0)) := predictor_Ex - UInt(1,2)
            }
         }.elsewhen( isBranch_Ex && found_Ex && choose_PC_Ex && io.exfe.doBranch) { // Maybe remove it! Maybe is the proper predict.
            PC_BTB(PC_Ex(PREDICTOR_INDEX_ONE,0)) := PC_Ex(PC_SIZE_ONE,PREDICTOR_INDEX) // ??????????
            when(predictor_Ex === UInt(1,2)){
               predictor(PC_Ex(PREDICTOR_INDEX_ONE,0)) := UInt(3,2)
            }.elsewhen(predictor_Ex === UInt(0,2) || predictor_Ex === UInt(2,2)){
               predictor(PC_Ex(PREDICTOR_INDEX_ONE,0)) := predictor_Ex + UInt(1,2)
            }
         }
         
         // Different Target with the predicted one 
         when( isBranch_Ex && found_Ex && choose_PC_Ex && io.exfe.doBranch && (io.exfe.branchPc =/= targetPC_Reg_Ex )){
            targetPC_Reg(PC_Ex(PREDICTOR_INDEX_ONE,0)) := io.exfe.branchPc
         }
      }
      when(correct_on_decode && (choose_PC_Dec && (! io.flush) && (! io.exfe.doBranch))){
         PC_BTB(PC_Dec(PREDICTOR_INDEX_ONE,0)) := UInt(0,MSB)
         when(predictor_Dec === UInt(2,2)){
            predictor(PC_Dec(PREDICTOR_INDEX_ONE,0)) := UInt(0,2)
         }.elsewhen(predictor_Dec === UInt(1,2) || predictor_Dec === UInt(3,2)){
            predictor(PC_Dec(PREDICTOR_INDEX_ONE,0)) := predictor_Dec - UInt(1,2)
         }
      }      
   }
   
   when((found_Ex && (predictor_Ex === UInt(3,2) || predictor_Ex === UInt(2,2)) && choose_PC_Ex ) || (correct_on_decode && (choose_PC_Dec && (! io.flush) )&& (! io.exfe.doBranch) ) ){
      when( io.exfe.doBranch){
        when( io.exfe.branchPc =/= targetPC_Reg_Ex ){ // Check if we predict with different target.
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
   
   when( (found_Ex && (! io.exfe.doBranch) && choose_PC_Ex )|| (correct_on_decode && (choose_PC_Dec && (! io.flush) )&& (! io.exfe.doBranch) )) {
      io.correct_PC := UInt(1) 
   }.otherwise{
      io.correct_PC := UInt(0)
   }
   
   io.correct_on_decode_EN := (correct_on_decode && (choose_PC_Dec && (! io.flush) )&& (! io.exfe.doBranch))
   
   
}
  