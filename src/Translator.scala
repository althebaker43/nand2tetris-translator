
package translator

import scala.io.Source
import java.io.File
import java.io.PrintWriter

class VMLineIterator(rawVMFile : File) {
  val vmFileIter : Iterator[File] = if rawVMFile.isFile() then Iterator[File](rawVMFile) else rawVMFile.listFiles().iterator
  var vmLineIter : Iterator[String] = Iterator[String]()
  var fileName : String = ""

  def hasNext() : Boolean = {
    if vmLineIter.hasNext then {
      true
    }
    else {
      while vmFileIter.hasNext do {
        val nextVMFile = vmFileIter.next()
        if nextVMFile.getName().endsWith(".vm") then {
          vmLineIter = Source.fromFile(nextVMFile).getLines()
          fileName = nextVMFile.getName()
          if (vmLineIter.hasNext) {
            return true
          }
        }
      }
      false
    }
  }

  def next() : String  = {
    vmLineIter.next()
  }
}

object Translator {

  def main(args : Array[String]) : Unit = {
    val asmFile = PrintWriter(args(1))
    val vmLineIter = VMLineIterator(File(args(0)))
    val initInstrs = List("@256", "D=A", "@SP", "M=D", "@Sys.init", "0;JMP")
    for instr <- getAssembly(vmLineIter, initInstrs, Map(), initInstrs.size) do asmFile.println(instr)
    asmFile.close()
  }

  def initSegment(name : String, baseAddr : Int) : List[String] = {
    List("@" + baseAddr.toString(), "D=A", "@" + name, "M=D")
  }

  def getAssembly(vmLineIter : VMLineIterator, instrs : List[String], staticVars : Map[String, Int], numInstrs : Int) : List[String] = {
    if vmLineIter.hasNext() == false then return instrs
    val vmLine = vmLineIter.next().strip()
    val isComment = vmLine.startsWith("//")
    val newStaticVars = if isComment then staticVars else getStaticVars(vmLine, vmLineIter.fileName, staticVars)
    val newInstrs = if isComment then Nil else getInstructions(vmLine, vmLineIter.fileName, numInstrs, newStaticVars)
    val numNewInstrs = getNumNewInstrs(newInstrs, 0)
    val annotatedInstrs = getAnnotatedInstructions(newInstrs, numInstrs)
    val vmLineComment = if isComment then List(vmLine) else List("// " + vmLine)
    getAssembly(vmLineIter, instrs ++ vmLineComment ++ annotatedInstrs, newStaticVars, numInstrs + numNewInstrs)
  }

  def getStaticVars(vmLine : String, fileName : String, staticVars : Map[String, Int]) : Map[String, Int] = {
    val tokens = vmLine.split(" ")
    if tokens.length != 3 then
      staticVars
    else {
      if ((tokens(0) == "push") || (tokens(0) == "pop")) && (tokens(1) == "static") then {
        val fileIndex = fileName + "." + tokens(2)
        if staticVars.contains(fileIndex) then
          staticVars
        else
          staticVars + (fileIndex -> (staticVars.size + 16))
      }
      else {
        staticVars
      }
    }
  }

  def getAnnotatedInstructions(instrs : List[String], beginIdx : Int) : List[String] = {
    if instrs.isEmpty then return instrs
    val headInstr = instrs.head
    val skipLine = headInstr.startsWith("//") || headInstr.startsWith("(")
    val annotatedHead = if skipLine then headInstr else f"$headInstr%20s // PC $beginIdx"
    val newIdx = if skipLine then beginIdx else beginIdx+1
    List(annotatedHead) ++ getAnnotatedInstructions(instrs.tail, newIdx)
  }

  def getNumNewInstrs(instrs : List[String], beginCount : Int) : Int = {
    if instrs.isEmpty then return beginCount
    val headInstr = instrs.head
    val skipLine = headInstr.startsWith("//") || headInstr.startsWith("(")
    val instrCount = if skipLine then 0 else 1
    getNumNewInstrs(instrs.tail, beginCount+instrCount)
  }

  def getInstructions(vmLine : String, fileName : String, numInstrs : Int, staticVars : Map[String, Int]) : List[String] = {
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
        List("@" + staticVars(fileName + "." + tokens(2)), "D=M", "@SP", "A=M", "M=D", "@SP", "D=M", "M=D+1")
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
        List("@" + staticVars(fileName + "." + tokens(2)), "D=A", "@R13", "M=D", "@SP", "A=M-1", "D=M", "@R13", "A=M", "M=D", "@SP", "D=M", "M=D-1")
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
    else if tokens(0) == "label" then
      List("(" + tokens(1) + ")")
    else if tokens(0) == "goto" then
      List("@" + tokens(1), "0;JMP")
    else if tokens(0) == "if-goto" then
      List("@SP", "A=M-1", "D=M", "@R13", "M=D", "@SP", "D=M", "M=D-1", "@R13", "D=M", "@" + tokens(1), "D;JNE")
    else if tokens(0) == "function" then
      List("(" + tokens(1) + ")") ++ List.range(1, tokens(2).toInt + 1).map(_ => List("@0", "D=A", "@SP", "A=M", "M=D", "@SP", "M=M+1")).flatten
    else if tokens(0) == "return" then
      List("// return: Save return value to R13",
        "@SP",
        "M=M-1",
        "A=M",
        "D=M",
        "@R13",
        "M=D",
        "// return: Save ARG to R15",
        "@ARG",
        "D=M",
        "@R15",
        "M=D",
        "// return: Set SP to just before LCL",
        "@LCL",
        "D=M-1",
        "@SP",
        "M=D",
        "A=D",
        "D=M",
        "// return: Restore THAT",
        "@THAT",
        "M=D",
        "@SP",
        "M=M-1",
        "A=M",
        "D=M",
        "// return: Restore THIS",
        "@THIS",
        "M=D",
        "@SP",
        "M=M-1",
        "A=M",
        "D=M",
        "// return: Restore ARG",
        "@ARG",
        "M=D",
        "@SP",
        "M=M-1",
        "A=M",
        "D=M",
        "// return: Restore LCL",
        "@LCL",
        "M=D",
        "// return: Save return address to R14",
        "@SP",
        "M=M-1",
        "A=M",
        "D=M",
        "@R14",
        "M=D",
        "// return: Set SP to old ARG",
        "@R15",
        "D=M",
        "@SP",
        "M=D",
        "// return: Push return value on stack",
        "@R13",
        "D=M",
        "@SP",
        "A=M",
        "M=D",
        "@SP",
        "M=M+1",
        "// return: Return to caller",
        "@R14",
        "A=M",
        "0;JMP")
    else if tokens(0) == "call" then {
      val callLabel = "CALL_" + tokens(1) + "_" + numInstrs.toString()
      List("// call: Push return address",
        "@" + callLabel,
        "D=A",
        "@SP",
        "A=M",
        "M=D",
        "// call: Calculate new ARG",
        "@" + tokens(2),
        "D=A",
        "@SP",
        "D=M-D",
        "// call: Save new ARG",
        "@R13",
        "M=D",
        "// call: Increment SP",
        "@SP",
        "M=M+1",
        "// call: Save LCL",
        "@LCL",
        "D=M",
        "@SP",
        "A=M",
        "M=D",
        "@SP",
        "M=M+1",
        "// call: Save ARG",
        "@ARG",
        "D=M",
        "@SP",
        "A=M",
        "M=D",
        "@SP",
        "M=M+1",
        "// call: Save THIS",
        "@THIS",
        "D=M",
        "@SP",
        "A=M",
        "M=D",
        "@SP",
        "M=M+1",
        "// call: Save THAT",
        "@THAT",
        "D=M",
        "@SP",
        "A=M",
        "M=D",
        "@SP",
        "M=M+1",
        "// call: Update LCL",
        "@SP",
        "D=M",
        "@LCL",
        "M=D",
        "// call: Update ARG",
        "@R13",
        "D=M",
        "@ARG",
        "M=D",
        "@" + tokens(1),
        "0;JMP",
        "(" + callLabel + ")")
    }
    else
      Nil
  }
}
