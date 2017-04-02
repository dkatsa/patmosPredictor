import Chisel._
import Node._
import Constants._
// import util.log2Up
// import Constants._

class predictor1bit(PC_SIZE: Int , PREDICTOR_INDEX: Int , PREDICTOR_WIDTH: Int) extends Module {
   val io = new Bundle {
      val PC_Fe = UInt(INPUT, PC_SIZE) // PC 
      val isBranch_Dec = UInt(INPUT, 1) // Identify branches from Decode
      val isFlush_Ex = UInt(INPUT, 1) // TODO : find which signal generate flush!
      val target_Ex = UInt(INPUT, PC_SIZE) 
      //
      val choose_PC = UInt(OUTPUT, 1)
      val target_out = UInt(OUTPUT, PC_SIZE) 
   }
   // Constant ADDRESSES
   val ADDR = 1 << PREDICTOR_INDEX // in VHDL : 2 ** PREDICTOR_INDEX - 1 
   // The main memory 
   val PC_Reg = Vec.fill(ADDR) { Reg(UInt(width=PC_SIZE)) } // Store PC
   val targetPC_Reg = Vec.fill(ADDR) { Reg(UInt(width=PC_SIZE)) } // Store target_PC
   val predictor = Vec.fill(ADDR) { Reg(UInt(width=PREDICTOR_WIDTH)) } // Store predictor
   // Pointer for the memory
   val pointer = Reg(init = UInt(0, PREDICTOR_INDEX)) 
   
   val PC_feDec = Reg(init = UInt(0, PC_SIZE), next = io.PC_Fe)
   val PC_decEx = Reg(init = UInt(0, PC_SIZE), next = PC_feDec)
   
   var found = UInt(width = ADDR) // One-hot signal to identify the data
   found := UInt(0) // some default
   // A tree of ORs between the bits of signal 'found'
   var found_OR = orR(found)
   
   val found_feDec = Reg(init = UInt(0, 1), next = found_OR)
   val found_decEx = Reg(init = UInt(0, 1), next = found_feDec)
   
   val isBranch_decEx = Reg(init = UInt(0, 1), next = io.isBranch_Dec )
   
   
   
   val index = UInt(width = PREDICTOR_INDEX)
   index := OHToUInt(found)
   val index_feDec = Reg(init = UInt(0, PREDICTOR_INDEX), next =  index )
   val index_decEx = Reg(init = UInt(0, PREDICTOR_INDEX), next =  index_feDec )
   
 
   val target_feDec = Reg(init = UInt(0, PC_SIZE), next = targetPC_Reg(OHToUInt(found)) )
   val predictor_feDec = Reg(init = UInt(0, PREDICTOR_WIDTH), next =  predictor(OHToUInt(found))  )
   val predictor_decEx = Reg(init = UInt(0, PREDICTOR_WIDTH), next =  predictor_feDec  )
   
   io.choose_PC := found_feDec === UInt(1)  && predictor_feDec === UInt(1) 
   io.target_out := target_feDec
  
   
   // The pointer increases one each time a new write operations occurs
   // WRITE!!!!
   when( isBranch_decEx === UInt(1) && found_decEx === UInt(0) ){
      pointer  := pointer + UInt(1)
      PC_Reg(UInt(pointer)) := PC_decEx
      targetPC_Reg(UInt(pointer)) := io.target_Ex
      predictor(UInt(pointer)) := io.isFlush_Ex
   }.elsewhen( isBranch_decEx === UInt(1) && found_decEx === UInt(1) && io.isFlush_Ex === UInt(1) ){
      predictor(index_decEx) := ~predictor(index_decEx)
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
      when( UInt(k, PREDICTOR_INDEX)  === pointer && isBranch_decEx === UInt(1) && found_decEx === UInt(0) ){
         when (PC_decEx === io.PC_Fe) {
            found(k) := UInt(1)
         }.otherwise{
            found(k) := UInt(0)
         }
      }.otherwise{
         when (PC_Reg(UInt(k)) === io.PC_Fe) {
            found(k) := UInt(1)
         }.otherwise{
            found(k) := UInt(0)
         }
      }
   }
}

// object predictor1bit {
   // def main(args: Array[String]): Unit = {
      // chiselMain(Array("--backend", "v"), () => Module(new predictor1bit(6, 4, 1)))
   // }
// }


// Notes Nice Syntax

/*
  val addrEven = UInt()
  val addrOdd = UInt()
  val addrEvenReg = Reg(init = UInt(2, PC_SIZE), next = addrEven)
  val addrOddReg = Reg(init = UInt(1, PC_SIZE), next = addrOdd)
*/



