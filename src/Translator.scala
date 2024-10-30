
package translator

import scala.io.Source
import java.io.File
import java.io.PrintWriter

object Translator {

  def main(args : Array[String]) : Unit = {
    val vmFile = Source.fromFile(File(args(0)))
    val asmFile = PrintWriter(args(1))
    val initInstrs = List("@256", "D=A", "@SP", "M=D")
    for instr <- getAssembly(vmFile.getLines(), initInstrs) do asmFile.println(instr)
    asmFile.close()
  }

  def getAssembly(vmLineIter : Iterator[String], instrs : List[String]) : List[String] = {
    if vmLineIter.hasNext == false then return instrs
    val vmLine = vmLineIter.next().strip()
    val newInstrs = if vmLine.startsWith("//") then Nil else getInstructions(vmLine, instrs.size)
    getAssembly(vmLineIter, instrs ++ newInstrs)
  }

  def getInstructions(vmLine : String, numInstrs : Int) : List[String] = {
    val tokens = vmLine.split(" ")
    if tokens(0) == "push" then {
      if tokens(1) == "constant" then
        List("@" + tokens(2), "D=A", "@SP", "A=M", "M=D", "@SP", "D=M", "M=D+1")
      else
        Nil
    }
    else if tokens(0) == "add" then
      List("@SP", "A=M-1", "D=M", "A=A-1", "D=D+M", "M=D", "D=A+1", "@SP", "M=D")
    else if tokens(0) == "sub" then
      List("@SP", "A=M-1", "D=M", "A=A-1", "D=D-M", "M=D", "D=A+1", "@SP", "M=D")
    else if tokens(0) == "and" then
      List("@SP", "A=M-1", "D=M", "A=A-1", "D=D&M", "M=D", "D=A+1", "@SP", "M=D")
    else if tokens(0) == "or" then
      List("@SP", "A=M-1", "D=M", "A=A-1", "D=D|M", "M=D", "D=A+1", "@SP", "M=D")
    else if tokens(0) == "neg" then
      List("@SP", "A=M-1", "M=-M")
    else if tokens(0) == "not" then
      List("@SP", "A=M-1", "M=!M")
    else if tokens(0) == "eq" then
      List("@SP",
        "A=M-1",
        "D=M",
        "A=A-1",
        "D=D-M",
        "@EQ_" + numInstrs.toString(),
        "D;JEQ",
        "@SP",
        "A=M-1",
        "A=A-1",
        "M=0",
        "@END_" + numInstrs.toString(),
        "0;JMP",
        "(EQ_" + numInstrs.toString() + ")",
        "@SP",
        "A=M-1",
        "A=A-1",
        "M=-1",
        "(END_" + numInstrs.toString() + ")",
        "@SP",
        "D=M-1",
        "@SP",
        "M=D")
    else if tokens(0) == "lt" then
      List("@SP",
        "A=M-1",
        "D=M",
        "A=A-1",
        "D=D-M",
        "@LT_" + numInstrs.toString(),
        "D;JLT",
        "@SP",
        "A=M-1",
        "A=A-1",
        "M=0",
        "@END_" + numInstrs.toString(),
        "0;JMP",
        "(LT_" + numInstrs.toString() + ")",
        "@SP",
        "A=M-1",
        "A=A-1",
        "M=-1",
        "(END_" + numInstrs.toString() + ")",
        "@SP",
        "D=M-1",
        "@SP",
        "M=D")
    else if tokens(0) == "gt" then
      List("@SP",
        "A=M-1",
        "D=M",
        "A=A-1",
        "D=D-M",
        "@GT_" + numInstrs.toString(),
        "D;JGT",
        "@SP",
        "A=M-1",
        "A=A-1",
        "M=0",
        "@END_" + numInstrs.toString(),
        "0;JMP",
        "(GT_" + numInstrs.toString() + ")",
        "@SP",
        "A=M-1",
        "A=A-1",
        "M=-1",
        "(END_" + numInstrs.toString() + ")",
        "@SP",
        "D=M-1",
        "@SP",
        "M=D")
    else
      Nil
  }
}
