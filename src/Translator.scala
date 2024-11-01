
package translator

import scala.io.Source
import java.io.File
import java.io.PrintWriter

object Translator {

  def main(args : Array[String]) : Unit = {
    val vmFile = Source.fromFile(File(args(0)))
    val asmFile = PrintWriter(args(1))
    //val initInstrs = initSegment("SP", 256) ++ initSegment("LCL", 1024) ++ initSegment("ARG", 2048) ++ initSegment("THIS", 3072) ++ initSegment("THAT", 4096)
    for instr <- getAssembly(vmFile.getLines(), Nil, Map()) do asmFile.println(instr)
    asmFile.close()
  }

  def initSegment(name : String, baseAddr : Int) : List[String] = {
    List("@" + baseAddr.toString(), "D=A", "@" + name, "M=D")
  }

  def getAssembly(vmLineIter : Iterator[String], instrs : List[String], staticVars : Map[Int, Int]) : List[String] = {
    if vmLineIter.hasNext == false then return instrs
    val vmLine = vmLineIter.next().strip()
    val isComment = vmLine.startsWith("//")
    val newStaticVars = if isComment then staticVars else getStaticVars(vmLine, staticVars)
    val newInstrs = if isComment then Nil else getInstructions(vmLine, instrs.size, newStaticVars)
    getAssembly(vmLineIter, instrs ++ newInstrs, newStaticVars)
  }

  def getStaticVars(vmLine : String, staticVars : Map[Int, Int]) : Map[Int, Int] = {
    val tokens = vmLine.split(" ")
    if tokens.length != 3 then
      staticVars
    else {
      if ((tokens(0) == "push") || (tokens(0) == "pop")) && (tokens(1) == "static") then {
        val index = tokens(2).toInt
        if staticVars.contains(index) then
          staticVars
        else
          staticVars + (index -> (staticVars.size + 16))
      }
      else {
        staticVars
      }
    }
  }

  def getInstructions(vmLine : String, numInstrs : Int, staticVars : Map[Int, Int]) : List[String] = {
    val tokens = vmLine.split(" ")
    if tokens(0) == "push" then {
      if tokens(1) == "constant" then
        List("@" + tokens(2), "D=A", "@SP", "A=M", "M=D", "@SP", "D=M", "M=D+1")
      else if tokens(1) == "local" then
        List("@" + tokens(2), "D=A", "@LCL", "A=D+M", "D=M", "@SP", "A=M", "M=D", "@SP", "D=M", "M=D+1")
      else if tokens(1) == "argument" then
        List("@" + tokens(2), "D=A", "@ARG", "A=D+M", "D=M", "@SP", "A=M", "M=D", "@SP", "D=M", "M=D+1")
      else if tokens(1) == "this" then
        List("@" + tokens(2), "D=A", "@THIS", "A=D+M", "D=M", "@SP", "A=M", "M=D", "@SP", "D=M", "M=D+1")
      else if tokens(1) == "that" then
        List("@" + tokens(2), "D=A", "@THAT", "A=D+M", "D=M", "@SP", "A=M", "M=D", "@SP", "D=M", "M=D+1")
      else if tokens(1) == "temp" then
        List("@" + tokens(2), "D=A", "@5", "A=D+A", "D=M", "@SP", "A=M", "M=D", "@SP", "D=M", "M=D+1")
      else if tokens(1) == "pointer" then
        List("@" + tokens(2), "D=A", "@THIS", "A=D+A", "D=M", "@SP", "A=M", "M=D", "@SP", "D=M", "M=D+1")
      else if tokens(1) == "static" then
        List("@" + staticVars(tokens(2).toInt), "D=M", "@SP", "A=M", "M=D", "@SP", "D=M", "M=D+1")
      else
        Nil
    }
    else if tokens(0) == "pop" then {
      if tokens(1) == "local" then
        List("@" + tokens(2), "D=A", "@LCL", "D=D+M", "@R13", "M=D", "@SP", "A=M-1", "D=M", "@R13", "A=M", "M=D", "@SP", "D=M", "M=D-1")
      else if tokens(1) == "argument" then
        List("@" + tokens(2), "D=A", "@ARG", "D=D+M", "@R13", "M=D", "@SP", "A=M-1", "D=M", "@R13", "A=M", "M=D", "@SP", "D=M", "M=D-1")
      else if tokens(1) == "this" then
        List("@" + tokens(2), "D=A", "@THIS", "D=D+M", "@R13", "M=D", "@SP", "A=M-1", "D=M", "@R13", "A=M", "M=D", "@SP", "D=M", "M=D-1")
      else if tokens(1) == "that" then
        List("@" + tokens(2), "D=A", "@THAT", "D=D+M", "@R13", "M=D", "@SP", "A=M-1", "D=M", "@R13", "A=M", "M=D", "@SP", "D=M", "M=D-1")
      else if tokens(1) == "temp" then
        List("@" + tokens(2), "D=A", "@5", "D=D+A", "@R13", "M=D", "@SP", "A=M-1", "D=M", "@R13", "A=M", "M=D", "@SP", "D=M", "M=D-1")
      else if tokens(1) == "pointer" then
        List("@" + tokens(2), "D=A", "@THIS", "D=D+A", "@R13", "M=D", "@SP", "A=M-1", "D=M", "@R13", "A=M", "M=D", "@SP", "D=M", "M=D-1")
      else if tokens(1) == "static" then
        List("@" + staticVars(tokens(2).toInt), "D=A", "@R13", "M=D", "@SP", "A=M-1", "D=M", "@R13", "A=M", "M=D", "@SP", "D=M", "M=D-1")
      else
        Nil
    }
    else if tokens(0) == "add" then
      List("@SP", "A=M-1", "D=M", "A=A-1", "D=D+M", "M=D", "D=A+1", "@SP", "M=D")
    else if tokens(0) == "sub" then
      List("@SP", "A=M-1", "D=M", "A=A-1", "D=M-D", "M=D", "D=A+1", "@SP", "M=D")
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
        "D=M-D",
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
        "D=M-D",
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
        "D=M-D",
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
