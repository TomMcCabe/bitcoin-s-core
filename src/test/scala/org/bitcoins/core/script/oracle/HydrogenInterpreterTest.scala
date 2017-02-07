package org.bitcoins.core.script.oracle

import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.constant.{ScriptNumber, ScriptToken}
import org.bitcoins.core.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 2/7/17.
  */
class HydrogenInterpreterTest extends FlatSpec with MustMatchers {

  "HydrogenInterpreter" must "execute an OP_APIQUERY1ID and return the number of passing yds, zero, to the stack" in {
    //playerid is sam bradfords and game is minnesota's game week 1 against TEN
    val stack: List[ScriptToken] = List(ScriptNumber(27854),ScriptNumber(2016091108))
    val script: List[ScriptToken] = List(OP_APIQUERY1ID)
    val program = ScriptProgram(TestUtil.testProgram,stack,script)
    val executed = HydrogenInterpreter.opApiQuery1Id(program)
    executed.stack must be (List(ScriptNumber.zero))
    executed.script.isEmpty must be (true)
  }

  it must "execute an OP_APIQUERY1ID and return the number of passing yds, , to the stack" in {
    val stack: List[ScriptToken] = List(ScriptNumber(27854),ScriptNumber(2016091813))
    val script: List[ScriptToken] = List(OP_APIQUERY1ID)
    val program = ScriptProgram(TestUtil.testProgram,stack,script)
    val executed = HydrogenInterpreter.opApiQuery1Id(program)
    executed.stack must be (List(ScriptNumber(286)))
    executed.script.isEmpty must be (true)
  }

}
