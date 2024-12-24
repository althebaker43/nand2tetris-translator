
package translator

import org.scalatest.flatspec.AnyFlatSpec
import scala.io.Source

class TranslatorSpec extends AnyFlatSpec {

  "The translator" should "generate instructions" in {

    val listing = """
push constant 7
push constant 8
add
"""

    val lineIter = Source.fromString(listing).getLines()
    val instrs = Translator.getAssembly(lineIter, Nil, Map())

    val expectedAsm = List(
      "@7",
      "D=A",
      "@SP",
      "A=M",
      "M=D",
      "@SP",
      "D=M",
      "M=D+1",
      "@8",
      "D=A",
      "@SP",
      "A=M",
      "M=D",
      "@SP",
      "D=M",
      "M=D+1",
      "@SP",
      "A=M-1",
      "D=M",
      "A=A-1",
      "D=D+M",
      "M=D",
      "D=A+1",
      "@SP",
      "M=D")

    assert(instrs.isEmpty == false)
    assert(expectedAsm == instrs)
  }
}
