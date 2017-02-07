package org.bitcoins.core.script.interpreter

import org.bitcoins.core.consensus.Consensus
import org.bitcoins.core.crypto.{BaseTransactionSignatureComponent, WitnessV0TransactionSignatureComponent}
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.{BaseTransaction, EmptyTransactionOutPoint, Transaction, WitnessTransaction}
import org.bitcoins.core.script._
import org.bitcoins.core.script.arithmetic._
import org.bitcoins.core.script.bitwise._
import org.bitcoins.core.script.constant.{ScriptToken, _}
import org.bitcoins.core.script.control._
import org.bitcoins.core.script.crypto._
import org.bitcoins.core.script.flag._
import org.bitcoins.core.script.locktime.{LockTimeInterpreter, OP_CHECKLOCKTIMEVERIFY, OP_CHECKSEQUENCEVERIFY}
import org.bitcoins.core.script.oracle.{OP_APIQUERY1ID, HydrogenInterpreter}
import org.bitcoins.core.script.reserved._
import org.bitcoins.core.script.result._
import org.bitcoins.core.script.splice._
import org.bitcoins.core.script.stack._
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil, BitcoinScriptUtil}

import scala.annotation.tailrec

/**
 * Created by chris on 1/6/16.
 */
trait ScriptInterpreter extends CryptoInterpreter with StackInterpreter with ControlOperationsInterpreter
  with BitwiseInterpreter with ConstantInterpreter with ArithmeticInterpreter with SpliceInterpreter
  with LockTimeInterpreter with BitcoinSLogger {

  /**
   * Currently bitcoin core limits the maximum number of non-push operations per script
   * to 201
   */
  private lazy val maxScriptOps = 201

  /** We cannot push an element larger than 520 bytes onto the stack */
  private lazy val maxPushSize = 520

  /**
   * Runs an entire script though our script programming language and
   * returns a [[ScriptResult]] indicating if the script was valid, or if not what error it encountered
   */
  def run(program : PreExecutionScriptProgram) : ScriptResult = {
    val scriptSig = program.txSignatureComponent.scriptSignature
    val scriptPubKey = program.txSignatureComponent.scriptPubKey
    val flags = program.flags
    val p2shEnabled = ScriptFlagUtil.p2shEnabled(flags)
    val segwitEnabled = ScriptFlagUtil.segWitEnabled(flags)
    val executedProgram : ExecutedScriptProgram = if (ScriptFlagUtil.requirePushOnly(flags)
      && !BitcoinScriptUtil.isPushOnly(program.script)) {
      logger.error("We can only have push operations inside of the script sig when the SIGPUSHONLY flag is set")
      ScriptProgram(program,ScriptErrorSigPushOnly)
    } else if (scriptSig.isInstanceOf[P2SHScriptSignature] && p2shEnabled &&
      !BitcoinScriptUtil.isPushOnly(scriptSig.asm)) {
      logger.error("P2SH scriptSigs are required to be push only by definition - see BIP16, got: " + scriptSig.asm)
      ScriptProgram(program,ScriptErrorSigPushOnly)
    } else {
      val scriptSigExecutedProgram = loop(program,0)
      val t = scriptSigExecutedProgram.txSignatureComponent
      val scriptPubKeyProgram = ScriptProgram(t, scriptSigExecutedProgram.stack, t.scriptPubKey.asm,
        t.scriptPubKey.asm)
      val scriptPubKeyExecutedProgram : ExecutedScriptProgram = loop(scriptPubKeyProgram,0)
      if (scriptSigExecutedProgram.error.isDefined) {
        scriptSigExecutedProgram
      } else if (scriptPubKeyExecutedProgram.error.isDefined || scriptPubKeyExecutedProgram.stackTopIsFalse) {
        scriptPubKeyExecutedProgram
      } else {
        scriptPubKey match {
          case witness : WitnessScriptPubKey =>
            if (segwitEnabled) executeSegWitScript(scriptPubKeyExecutedProgram,witness)
            else scriptPubKeyExecutedProgram
          case p2sh : P2SHScriptPubKey =>
            if (p2shEnabled) executeP2shScript(scriptSigExecutedProgram, program, p2sh)
            else scriptPubKeyExecutedProgram
          case _ : P2PKHScriptPubKey | _: P2PKScriptPubKey | _: MultiSignatureScriptPubKey | _: CSVScriptPubKey |
              _ : CLTVScriptPubKey | _ : NonStandardScriptPubKey | _ : WitnessCommitment | EmptyScriptPubKey =>
            scriptPubKeyExecutedProgram
        }
      }
    }
    logger.debug("Executed Script Program: " + executedProgram)
    if (executedProgram.error.isDefined) executedProgram.error.get
    else if (hasUnexpectedWitness(program)) {
      //note: the 'program' value we pass above is intentional, we need to check the original program
      //as the 'executedProgram' may have had the scriptPubKey value changed to the rebuilt ScriptPubKey of the witness program

      ScriptErrorWitnessUnexpected
    }
    else if (executedProgram.stackTopIsTrue && flags.contains(ScriptVerifyCleanStack)) {
      //require that the stack after execution has exactly one element on it
      if (executedProgram.stack.size == 1) ScriptOk
      else ScriptErrorCleanStack
    } else if (executedProgram.stackTopIsTrue) ScriptOk
    else ScriptErrorEvalFalse
  }

  /**
    * P2SH scripts are unique in their evaluation, first the scriptSignature must be added to the stack, next the
    * p2sh scriptPubKey must be run to make sure the serialized redeem script hashes to the value found in the p2sh
    * scriptPubKey, then finally the serialized redeemScript is decoded and run with the arguments in the p2sh script signature
    * a p2sh script returns true if both of those intermediate steps evaluate to true
    *
    * @param scriptPubKeyExecutedProgram the program with the script signature pushed onto the stack
    * @param originalProgram the original program, used for setting errors & checking that the original script signature contains push only tokens
    * @param p2shScriptPubKey the p2sh scriptPubKey that contains the value the redeemScript must hash to
    * @return the executed program
    */
  private def executeP2shScript(scriptPubKeyExecutedProgram : ExecutedScriptProgram, originalProgram : ScriptProgram, p2shScriptPubKey : P2SHScriptPubKey) : ExecutedScriptProgram = {

    /** Helper function to actually run a p2sh script */
    def run(p: ExecutedScriptProgram, stack : Seq[ScriptToken], s: ScriptPubKey): ExecutedScriptProgram = {
      logger.debug("Running p2sh script: " + stack)
      val p2shRedeemScriptProgram = ScriptProgram(p.txSignatureComponent,stack.tail,
        s.asm)
      if (ScriptFlagUtil.requirePushOnly(p2shRedeemScriptProgram.flags) && !BitcoinScriptUtil.isPushOnly(s.asm)) {
        logger.error("p2sh redeem script must be push only operations whe SIGPUSHONLY flag is set")
        ScriptProgram(p2shRedeemScriptProgram,ScriptErrorSigPushOnly)
      } else loop(p2shRedeemScriptProgram,0)
    }

    val scriptSig = scriptPubKeyExecutedProgram.txSignatureComponent.scriptSignature
    val scriptSigAsm : Seq[ScriptToken] = scriptSig.asm
    //need to check if the scriptSig is push only as required by bitcoin core
    //https://github.com/bitcoin/bitcoin/blob/528472111b4965b1a99c4bcf08ac5ec93d87f10f/src/script/interpreter.cpp#L1419
    if (!BitcoinScriptUtil.isPushOnly(scriptSigAsm)) {
      ScriptProgram(scriptPubKeyExecutedProgram,ScriptErrorSigPushOnly)
    } else if (scriptPubKeyExecutedProgram.error.isDefined) {
      scriptPubKeyExecutedProgram
    } else {
      scriptPubKeyExecutedProgram.stackTopIsTrue match {
        case true =>
          logger.debug("Hashes matched between the p2shScriptSignature & the p2shScriptPubKey")
          //we need to run the deserialized redeemScript & the scriptSignature without the serialized redeemScript
          val stack = scriptPubKeyExecutedProgram.stack
          val redeemScriptBytes = stack.head.bytes
          val c = CompactSizeUInt.calculateCompactSizeUInt(redeemScriptBytes)
          val redeemScript = ScriptPubKey(c.bytes ++ redeemScriptBytes)
          redeemScript match {
            case w : WitnessScriptPubKey =>
              val pushOp = BitcoinScriptUtil.calculatePushOp(redeemScriptBytes)
              val expectedScriptBytes = pushOp.flatMap(_.bytes) ++ redeemScriptBytes
              val flags = scriptPubKeyExecutedProgram.flags
              val segwitEnabled = ScriptFlagUtil.segWitEnabled(flags)
              if (segwitEnabled && (scriptSig.asmBytes == expectedScriptBytes)) {
                // The scriptSig must be _exactly_ a single push of the redeemScript. Otherwise we
                // reintroduce malleability.
                logger.info("redeem script was witness script pubkey, segwit was enabled, scriptSig was single push of redeemScript")
                executeSegWitScript(scriptPubKeyExecutedProgram,w)
              } else if (segwitEnabled && (scriptSig.asmBytes != expectedScriptBytes)) {
                logger.error("Segwit was enabled, but p2sh redeem script was malleated")
                logger.error("ScriptSig bytes: " + scriptSig.hex)
                logger.error("expected scriptsig bytes: " + BitcoinSUtil.encodeHex(expectedScriptBytes))
                ScriptProgram(scriptPubKeyExecutedProgram, ScriptErrorWitnessMalleatedP2SH)
              } else {
                logger.warn("redeem script was witness script pubkey, segwit was NOT enabled")
                //treat the segwit scriptpubkey as any other redeem script
                run(scriptPubKeyExecutedProgram,stack,w)
              }
            case s @ (_ : P2SHScriptPubKey | _ : P2PKHScriptPubKey | _ : P2PKScriptPubKey | _ : MultiSignatureScriptPubKey |
              _ : CLTVScriptPubKey | _ : CSVScriptPubKey | _: NonStandardScriptPubKey | _ : WitnessCommitment | EmptyScriptPubKey) =>
              logger.debug("redeemScript: " + s.asm)
              run(scriptPubKeyExecutedProgram,stack,s)
          }
        case false =>
          logger.warn("P2SH scriptPubKey hash did not match the hash for the serialized redeemScript")
          scriptPubKeyExecutedProgram
      }
    }

  }

  /** Runs a segwit script through our interpreter, mimics this functionality in bitcoin core:
    * [[https://github.com/bitcoin/bitcoin/blob/528472111b4965b1a99c4bcf08ac5ec93d87f10f/src/script/interpreter.cpp#L1441-L1452]]
    * @param scriptPubKeyExecutedProgram the program with the [[ScriptPubKey]] executed
    * @return
    */
  private def executeSegWitScript(scriptPubKeyExecutedProgram: ExecutedScriptProgram, witnessScriptPubKey: WitnessScriptPubKey): ExecutedScriptProgram = {
    scriptPubKeyExecutedProgram.txSignatureComponent match {
      case b : BaseTransactionSignatureComponent =>
        logger.error("Cannot verify witness program with a BaseTransactionSignatureComponent")
        ScriptProgram(scriptPubKeyExecutedProgram,ScriptErrorWitnessProgramWitnessEmpty)
      case w : WitnessV0TransactionSignatureComponent =>
        val scriptSig = scriptPubKeyExecutedProgram.txSignatureComponent.scriptSignature
        val (witnessVersion,witnessProgram) = (witnessScriptPubKey.witnessVersion, witnessScriptPubKey.witnessProgram)
        val witness = w.witness

        //scriptsig must be empty if we have raw p2wsh
        //if script pubkey is a P2SHScriptPubKey then we have P2SH(P2WSH)
        if (scriptSig != EmptyScriptSignature && !w.scriptPubKey.isInstanceOf[P2SHScriptPubKey]) ScriptProgram(scriptPubKeyExecutedProgram,ScriptErrorWitnessMalleated)
        else if (witness.stack.exists(_.size > maxPushSize)) ScriptProgram(scriptPubKeyExecutedProgram, ScriptErrorPushSize)
        else verifyWitnessProgram(witnessVersion, witness, witnessProgram, w)
    }
  }

  /** Verifies a segregated witness program by running it through the interpreter
    * [[https://github.com/bitcoin/bitcoin/blob/f8528134fc188abc5c7175a19680206964a8fade/src/script/interpreter.cpp#L1302]]*/
  private def verifyWitnessProgram(witnessVersion: WitnessVersion, scriptWitness: ScriptWitness, witnessProgram: Seq[ScriptToken],
                                   witnessTxSigComponent: WitnessV0TransactionSignatureComponent): ExecutedScriptProgram = {

    /** Helper function to run the post segwit execution checks */
    def postSegWitProgramChecks(evaluated: ExecutedScriptProgram): ExecutedScriptProgram = {
      logger.debug("Stack after evaluating witness: " + evaluated.stack)
      if (evaluated.error.isDefined) evaluated
      else if (evaluated.stack.size != 1 || evaluated.stackTopIsFalse) ScriptProgram(evaluated,ScriptErrorEvalFalse)
      else evaluated
    }

    witnessVersion match {
      case WitnessVersion0 =>
        val either: Either[(Seq[ScriptToken], ScriptPubKey),ScriptError] = witnessVersion.rebuild(scriptWitness, witnessProgram)
        either match {
          case Left((stack,scriptPubKey)) =>
            val w = witnessTxSigComponent
            val newProgram = ScriptProgram(w.transaction,scriptPubKey,w.inputIndex,stack,scriptPubKey.asm, scriptPubKey.asm,Nil,
              w.flags,w.sigVersion,w.amount)
            val evaluated = loop(newProgram,0)
            postSegWitProgramChecks(evaluated)
          case Right(err) =>
            val program = ScriptProgram(witnessTxSigComponent)
            ScriptProgram(program,err)
        }
      case UnassignedWitness =>
        logger.warn("Unassigned witness inside of witness script pubkey")
        val w = witnessTxSigComponent
        val flags = w.flags
        val discourageUpgradableWitnessVersion = ScriptFlagUtil.discourageUpgradableWitnessProgram(flags)
        val program = ScriptProgram(w.transaction,w.scriptPubKey,w.inputIndex,Nil,Nil, w.scriptPubKey.asm,Nil,
          w.flags,w.sigVersion,w.amount)
        if (discourageUpgradableWitnessVersion) {
          ScriptProgram(program,UnassignedWitness.rebuild(scriptWitness, witnessProgram).right.get)
        } else {
          //if we are not discouraging upgradable ops, we just trivially return the program with an OP_TRUE on the stack
          //see: https://github.com/bitcoin/bitcoin/blob/b83264d9c7a8ddb79f64bd9540caddc8632ef31f/src/script/interpreter.cpp#L1386-L1389
          val evaluated = loop(ScriptProgram(program,Seq(OP_TRUE),ScriptProgram.Stack),0)
          evaluated
        }

    }
  }

  /**
    * The execution loop for a script
    *
    * @param program the program whose script needs to be evaluated
    * @return program the final state of the program after being evaluated by the interpreter
    */
  @tailrec
  private def loop(program : ScriptProgram, opCount: Int) : ExecutedScriptProgram = {
    logger.debug("Stack: " + program.stack)
    logger.debug("Script: " + program.script)
    if (opCount > maxScriptOps && !program.isInstanceOf[ExecutedScriptProgram]) {
      logger.error("We have reached the maximum amount of script operations allowed")
      logger.error("Here are the remaining operations in the script: " + program.script)
      loop(ScriptProgram(program,ScriptErrorOpCount),opCount)
    } else if (program.script.flatMap(_.bytes).size > 10000 && !program.isInstanceOf[ExecutedScriptProgram]) {
      logger.error("We cannot run a script that is larger than 10,000 bytes")
      program match {
        case p : PreExecutionScriptProgram =>
          loop(ScriptProgram(ScriptProgram.toExecutionInProgress(p), ScriptErrorScriptSize),opCount)
        case _ : ExecutionInProgressScriptProgram | _ : ExecutedScriptProgram =>
          loop(ScriptProgram(program, ScriptErrorScriptSize),opCount)
      }
    } else {
      program match {
        case p : PreExecutionScriptProgram => loop(ScriptProgram.toExecutionInProgress(p,Some(p.stack)),opCount)
        case p : ExecutedScriptProgram =>
          val countedOps = program.originalScript.map(BitcoinScriptUtil.countsTowardsScriptOpLimit(_)).count(_ == true)
          logger.info("Counted ops: " + countedOps)
          if (countedOps > maxScriptOps && p.error.isEmpty) {
            loop(ScriptProgram(p,ScriptErrorOpCount),opCount)
          } else p

        case p : ExecutionInProgressScriptProgram =>
          p.script match {
            //if at any time we see that the program is not valid
            //cease script execution
            case _ if p.script.intersect(Seq(OP_VERIF, OP_VERNOTIF)).nonEmpty =>
              logger.error("Script is invalid even when a OP_VERIF or OP_VERNOTIF occurs in an unexecuted OP_IF branch")
              loop(ScriptProgram(p, ScriptErrorBadOpCode),opCount)
            //disabled splice operation
            case _ if p.script.intersect(Seq(OP_CAT, OP_SUBSTR, OP_LEFT, OP_RIGHT)).nonEmpty =>
              logger.error("Script is invalid because it contains a disabled splice operation")
              loop(ScriptProgram(p, ScriptErrorDisabledOpCode),opCount)
            //disabled bitwise operations
            case _ if p.script.intersect(Seq(OP_INVERT, OP_AND, OP_OR, OP_XOR)).nonEmpty =>
              logger.error("Script is invalid because it contains a disabled bitwise operation")
              loop(ScriptProgram(p, ScriptErrorDisabledOpCode),opCount)
            //disabled arithmetic operations
            case _ if p.script.intersect(Seq(OP_MUL, OP_2MUL, OP_DIV, OP_2DIV, OP_MOD, OP_LSHIFT, OP_RSHIFT)).nonEmpty =>
              logger.error("Script is invalid because it contains a disabled arithmetic operation")
              loop(ScriptProgram(p, ScriptErrorDisabledOpCode),opCount)
            //program cannot contain a push operation > 520 bytes
            case _ if (p.script.exists(token => token.bytes.size > maxPushSize)) =>
              logger.error("We have a script constant that is larger than 520 bytes, this is illegal: " + p.script)
              loop(ScriptProgram(p, ScriptErrorPushSize),opCount)
            //program stack size cannot be greater than 1000 elements
            case _ if ((p.stack.size + p.altStack.size) > 1000) =>
              logger.error("We cannot have a stack + alt stack size larger than 1000 elements")
              loop(ScriptProgram(p, ScriptErrorStackSize),opCount)

            //stack operations
            case OP_DUP :: t => loop(opDup(p),calcOpCount(opCount,OP_DUP))
            case OP_DEPTH :: t => loop(opDepth(p),calcOpCount(opCount,OP_DEPTH))
            case OP_TOALTSTACK :: t => loop(opToAltStack(p),calcOpCount(opCount,OP_TOALTSTACK))
            case OP_FROMALTSTACK :: t => loop(opFromAltStack(p),calcOpCount(opCount,OP_FROMALTSTACK))
            case OP_DROP :: t => loop(opDrop(p),calcOpCount(opCount,OP_DROP))
            case OP_IFDUP :: t => loop(opIfDup(p),calcOpCount(opCount,OP_IFDUP))
            case OP_NIP :: t => loop(opNip(p),calcOpCount(opCount,OP_NIP))
            case OP_OVER :: t => loop(opOver(p),calcOpCount(opCount,OP_OVER))
            case OP_PICK :: t => loop(opPick(p),calcOpCount(opCount,OP_PICK))
            case OP_ROLL :: t => loop(opRoll(p),calcOpCount(opCount,OP_ROLL))
            case OP_ROT :: t => loop(opRot(p),calcOpCount(opCount,OP_ROT))
            case OP_2ROT :: t => loop(op2Rot(p),calcOpCount(opCount,OP_2ROT))
            case OP_2DROP :: t => loop(op2Drop(p),calcOpCount(opCount,OP_2DROP))
            case OP_SWAP :: t => loop(opSwap(p),calcOpCount(opCount,OP_SWAP))
            case OP_TUCK :: t => loop(opTuck(p),calcOpCount(opCount,OP_TUCK))
            case OP_2DUP :: t => loop(op2Dup(p),calcOpCount(opCount,OP_2DUP))
            case OP_3DUP :: t => loop(op3Dup(p),calcOpCount(opCount,OP_3DUP))
            case OP_2OVER :: t => loop(op2Over(p),calcOpCount(opCount,OP_2OVER))
            case OP_2SWAP :: t => loop(op2Swap(p),calcOpCount(opCount,OP_2SWAP))

            //arithmetic operations
            case OP_ADD :: t => loop(opAdd(p),calcOpCount(opCount,OP_ADD))
            case OP_1ADD :: t => loop(op1Add(p),calcOpCount(opCount,OP_1ADD))
            case OP_1SUB :: t => loop(op1Sub(p),calcOpCount(opCount,OP_1SUB))
            case OP_SUB :: t => loop(opSub(p),calcOpCount(opCount,OP_SUB))
            case OP_ABS :: t => loop(opAbs(p),calcOpCount(opCount,OP_ABS))
            case OP_NEGATE :: t => loop(opNegate(p),calcOpCount(opCount,OP_NEGATE))
            case OP_NOT :: t => loop(opNot(p),calcOpCount(opCount,OP_NOT))
            case OP_0NOTEQUAL :: t => loop(op0NotEqual(p),calcOpCount(opCount,OP_0NOTEQUAL))
            case OP_BOOLAND :: t => loop(opBoolAnd(p),calcOpCount(opCount,OP_BOOLAND))
            case OP_BOOLOR :: t => loop(opBoolOr(p),calcOpCount(opCount,OP_BOOLOR))
            case OP_NUMEQUAL :: t => loop(opNumEqual(p),calcOpCount(opCount,OP_NUMEQUAL))
            case OP_NUMEQUALVERIFY :: t => loop(opNumEqualVerify(p),calcOpCount(opCount,OP_NUMEQUALVERIFY))
            case OP_NUMNOTEQUAL :: t => loop(opNumNotEqual(p),calcOpCount(opCount,OP_NUMNOTEQUAL))
            case OP_LESSTHAN :: t => loop(opLessThan(p),calcOpCount(opCount,OP_LESSTHAN))
            case OP_GREATERTHAN :: t => loop(opGreaterThan(p),calcOpCount(opCount,OP_GREATERTHAN))
            case OP_LESSTHANOREQUAL :: t => loop(opLessThanOrEqual(p),calcOpCount(opCount,OP_LESSTHANOREQUAL))
            case OP_GREATERTHANOREQUAL :: t => loop(opGreaterThanOrEqual(p),calcOpCount(opCount,OP_GREATERTHANOREQUAL))
            case OP_MIN :: t => loop(opMin(p),calcOpCount(opCount,OP_MIN))
            case OP_MAX :: t => loop(opMax(p),calcOpCount(opCount,OP_MAX))
            case OP_WITHIN :: t => loop(opWithin(p),calcOpCount(opCount,OP_WITHIN))

            //bitwise operations
            case OP_EQUAL :: t => loop(opEqual(p),calcOpCount(opCount,OP_EQUAL))

            case OP_EQUALVERIFY :: t => loop(opEqualVerify(p),calcOpCount(opCount,OP_EQUALVERIFY))

            case OP_0 :: t => loop(ScriptProgram(p, ScriptNumber.zero :: p.stack, t),calcOpCount(opCount,OP_0))
            case (scriptNumberOp : ScriptNumberOperation) :: t =>
              loop(ScriptProgram(p, ScriptNumber(scriptNumberOp.underlying) :: p.stack, t),calcOpCount(opCount,scriptNumberOp))
            case (bytesToPushOntoStack: BytesToPushOntoStack) :: t =>
              loop(pushScriptNumberBytesToStack(p),calcOpCount(opCount,bytesToPushOntoStack))
            case (scriptNumber: ScriptNumber) :: t =>
              loop(ScriptProgram(p, scriptNumber :: p.stack, t),calcOpCount(opCount,scriptNumber))
            case OP_PUSHDATA1 :: t => loop(opPushData1(p),calcOpCount(opCount,OP_PUSHDATA1))
            case OP_PUSHDATA2 :: t => loop(opPushData2(p),calcOpCount(opCount,OP_PUSHDATA2))
            case OP_PUSHDATA4 :: t => loop(opPushData4(p),calcOpCount(opCount,OP_PUSHDATA4))

            case (x : ScriptConstant) :: t => loop(ScriptProgram(p, x :: p.stack, t),calcOpCount(opCount,x))

            //control operations
            case OP_IF :: t => loop(opIf(p),calcOpCount(opCount,OP_IF))
            case OP_NOTIF :: t => loop(opNotIf(p),calcOpCount(opCount,OP_NOTIF))
            case OP_ELSE :: t => loop(opElse(p),calcOpCount(opCount,OP_ELSE))
            case OP_ENDIF :: t => loop(opEndIf(p),calcOpCount(opCount,OP_ENDIF))
            case OP_RETURN :: t => loop(opReturn(p),calcOpCount(opCount,OP_RETURN))

            case OP_VERIFY :: t => loop(opVerify(p),calcOpCount(opCount,OP_VERIFY))

            //crypto operations
            case OP_HASH160 :: t => loop(opHash160(p),calcOpCount(opCount,OP_HASH160))
            case OP_CHECKSIG :: t => loop(opCheckSig(p),calcOpCount(opCount,OP_CHECKSIG))
            case OP_CHECKSIGVERIFY :: t => loop(opCheckSigVerify(p),calcOpCount(opCount,OP_CHECKSIGVERIFY))
            case OP_SHA1 :: t => loop(opSha1(p),calcOpCount(opCount,OP_SHA1))
            case OP_RIPEMD160 :: t => loop(opRipeMd160(p),calcOpCount(opCount,OP_RIPEMD160))
            case OP_SHA256 :: t => loop(opSha256(p),calcOpCount(opCount,OP_SHA256))
            case OP_HASH256 :: t => loop(opHash256(p),calcOpCount(opCount,OP_HASH256))
            case OP_CODESEPARATOR :: t => loop(opCodeSeparator(p),calcOpCount(opCount,OP_CODESEPARATOR))
            case OP_CHECKMULTISIG :: t =>
              opCheckMultiSig(p) match {
                case newProgram : ExecutedScriptProgram =>
                  //script was marked invalid for other reasons, don't need to update the opcount
                  loop(newProgram,opCount)
                case newProgram @ (_ : ExecutionInProgressScriptProgram | _ : PreExecutionScriptProgram) =>
                  val newOpCount = calcOpCount(opCount,OP_CHECKMULTISIG) + BitcoinScriptUtil.numPossibleSignaturesOnStack(program).toInt
                  loop(newProgram,newOpCount)
              }
            case OP_CHECKMULTISIGVERIFY :: t =>
              opCheckMultiSigVerify(p) match {
                case newProgram : ExecutedScriptProgram =>
                  //script was marked invalid for other reasons, don't need to update the opcount
                  loop(newProgram,opCount)
                case newProgram @ (_ : ExecutionInProgressScriptProgram | _ : PreExecutionScriptProgram) =>
                  val newOpCount = calcOpCount(opCount,OP_CHECKMULTISIGVERIFY) + BitcoinScriptUtil.numPossibleSignaturesOnStack(program).toInt
                  loop(newProgram,newOpCount)
              }
            //reserved operations
            case OP_NOP :: t =>
              //script discourage upgradeable flag does not apply to a OP_NOP
              loop(ScriptProgram(p, p.stack, t),calcOpCount(opCount,OP_NOP))

            //if we see an OP_NOP and the DISCOURAGE_UPGRADABLE_OP_NOPS flag is set we must fail our program
            case (nop: NOP) :: t if ScriptFlagUtil.discourageUpgradableNOPs(p.flags) =>
              logger.error("We cannot execute a NOP when the ScriptVerifyDiscourageUpgradableNOPs is set")
              loop(ScriptProgram(p, ScriptErrorDiscourageUpgradableNOPs),calcOpCount(opCount,nop))
            case (nop: NOP) :: t => loop(ScriptProgram(p, p.stack, t),calcOpCount(opCount,nop))
            case OP_RESERVED :: t =>
              logger.error("OP_RESERVED automatically marks transaction invalid")
              loop(ScriptProgram(p,ScriptErrorBadOpCode),calcOpCount(opCount,OP_RESERVED))
            case OP_VER :: t =>
              logger.error("Transaction is invalid when executing OP_VER")
              loop(ScriptProgram(p,ScriptErrorBadOpCode),calcOpCount(opCount,OP_VER))
            case OP_RESERVED1 :: t =>
              logger.error("Transaction is invalid when executing OP_RESERVED1")
              loop(ScriptProgram(p,ScriptErrorBadOpCode),calcOpCount(opCount,OP_RESERVED1))
            case OP_RESERVED2 :: t =>
              logger.error("Transaction is invalid when executing OP_RESERVED2")
              loop(ScriptProgram(p,ScriptErrorBadOpCode),calcOpCount(opCount,OP_RESERVED2))

            case (reservedOperation : ReservedOperation) :: t =>
              logger.error("Undefined operation found which automatically fails the script: " + reservedOperation)
              loop(ScriptProgram(p,ScriptErrorBadOpCode),calcOpCount(opCount,reservedOperation))
            //splice operations
            case OP_SIZE :: t => loop(opSize(p),calcOpCount(opCount,OP_SIZE))

            //locktime operations
            case OP_CHECKLOCKTIMEVERIFY :: t =>
              //check if CLTV is enforced yet
              if (ScriptFlagUtil.checkLockTimeVerifyEnabled(p.flags)) loop(opCheckLockTimeVerify(p),calcOpCount(opCount,OP_CHECKLOCKTIMEVERIFY))
              //if not, check to see if we should discourage p
              else if (ScriptFlagUtil.discourageUpgradableNOPs(p.flags)) {
                logger.error("We cannot execute a NOP when the ScriptVerifyDiscourageUpgradableNOPs is set")
                loop(ScriptProgram(p, ScriptErrorDiscourageUpgradableNOPs),calcOpCount(opCount,OP_CHECKLOCKTIMEVERIFY))
              }
              //in this case, just reat OP_CLTV just like a NOP and remove it from the stack
              else loop(ScriptProgram(p, p.script.tail, ScriptProgram.Script),calcOpCount(opCount,OP_CHECKLOCKTIMEVERIFY))
            case OP_CHECKSEQUENCEVERIFY :: t =>
              //check if CLTV is enforced yet
              if (ScriptFlagUtil.checkSequenceVerifyEnabled(p.flags)) loop(opCheckSequenceVerify(p),calcOpCount(opCount,OP_CHECKSEQUENCEVERIFY))
              //if not, check to see if we should discourage p
              else if (ScriptFlagUtil.discourageUpgradableNOPs(p.flags)) {
                logger.error("We cannot execute a NOP when the ScriptVerifyDiscourageUpgradableNOPs is set")
                loop(ScriptProgram(p, ScriptErrorDiscourageUpgradableNOPs),calcOpCount(opCount,OP_CHECKSEQUENCEVERIFY))
              }
              //in this case, just read OP_CSV just like a NOP and remove it from the stack
              else loop(ScriptProgram(p, p.script.tail, ScriptProgram.Script),calcOpCount(opCount,OP_CHECKSEQUENCEVERIFY))
            //oracle operations
            case OP_APIQUERY1ID :: t =>
              loop(HydrogenInterpreter.opApiQuery1Id(program), calcOpCount(opCount, OP_APIQUERY1ID))
            //no more script operations to run, return whether the program is valid and the final state of the program
            case Nil => loop(ScriptProgram.toExecutedProgram(p),opCount)
            case h :: t => throw new RuntimeException(h + " was unmatched")
          }
      }
    }
  }

  /** Checks the validity of a transaction in accordance to bitcoin core's CheckTransaction function
    * https://github.com/bitcoin/bitcoin/blob/f7a21dae5dbf71d5bc00485215e84e6f2b309d0a/src/main.cpp#L939. */
  def checkTransaction(transaction : Transaction) : Boolean = {
    val inputOutputsNotZero = !(transaction.inputs.isEmpty || transaction.outputs.isEmpty)
    val txNotLargerThanBlock = transaction.bytes.size < Consensus.maxBlockSize
    val outputsSpendValidAmountsOfMoney = !transaction.outputs.exists(o =>
      o.value < CurrencyUnits.zero || o.value > Consensus.maxMoney)

    val outputValues = transaction.outputs.map(_.value)
    val totalSpentByOutputs : CurrencyUnit = outputValues.fold(CurrencyUnits.zero)(_ + _)
    val allOutputsValidMoneyRange = validMoneyRange(totalSpentByOutputs)
    val prevOutputTxIds = transaction.inputs.map(_.previousOutput.txId)
    val noDuplicateInputs = prevOutputTxIds.distinct.size == prevOutputTxIds.size

    val isValidScriptSigForCoinbaseTx = transaction.isCoinbase match {
      case true => transaction.inputs.head.scriptSignature.asmBytes.size >= 2 &&
        transaction.inputs.head.scriptSignature.asmBytes.size <= 100
      case false =>
        //since this is not a coinbase tx we cannot have any empty previous outs inside of inputs
        !transaction.inputs.exists(_.previousOutput == EmptyTransactionOutPoint)
    }
    inputOutputsNotZero && txNotLargerThanBlock && outputsSpendValidAmountsOfMoney && noDuplicateInputs &&
      allOutputsValidMoneyRange && noDuplicateInputs && isValidScriptSigForCoinbaseTx
  }

  /** Determines if the given currency unit is within the valid range for the system */
  def validMoneyRange(currencyUnit : CurrencyUnit) : Boolean = {
    currencyUnit >= CurrencyUnits.zero && currencyUnit <= Consensus.maxMoney
  }

  /**  Calculates the new op count after the execution of the given [[ScriptToken]] */
  private def calcOpCount(oldOpCount: Int, token: ScriptToken):Int = BitcoinScriptUtil.countsTowardsScriptOpLimit(token) match {
    case true => oldOpCount + 1
    case false => oldOpCount
  }

  /** Checks if the transaction contained a witness that we did not use
    * [[https://github.com/bitcoin/bitcoin/blob/528472111b4965b1a99c4bcf08ac5ec93d87f10f/src/script/interpreter.cpp#L1515-L1523]]
    * Return true if witness was NOT used, return false if witness was used. */
  private def hasUnexpectedWitness(program: ScriptProgram): Boolean =  {
    val txSigComponent = program.txSignatureComponent
    logger.debug("TxSigComponent: " + txSigComponent)
    val unexpectedWitness = txSigComponent match {
      case b : BaseTransactionSignatureComponent =>
        b.transaction match {
          case wtx : WitnessTransaction =>
            wtx.witness.witnesses(txSigComponent.inputIndex.toInt).stack.nonEmpty
          case _ : BaseTransaction => false
        }
      case w : WitnessV0TransactionSignatureComponent =>
        val witnessedUsed = w.scriptPubKey match {
          case _ : WitnessScriptPubKey => true
          case _ : P2SHScriptPubKey =>
            val p2shScriptSig = P2SHScriptSignature(txSigComponent.scriptSignature.bytes)
            p2shScriptSig.redeemScript.isInstanceOf[WitnessScriptPubKey]
          case _ : CLTVScriptPubKey | _ : CSVScriptPubKey | _ : MultiSignatureScriptPubKey | _ : NonStandardScriptPubKey |
            _ : P2PKScriptPubKey | _ : P2PKHScriptPubKey | _ : WitnessCommitment | EmptyScriptPubKey =>
            w.witness.stack.isEmpty
        }
        !witnessedUsed
    }

    if (unexpectedWitness) logger.error("Found unexpected witness that was not used by the ScriptProgram: " + program)
    unexpectedWitness
  }
}
object ScriptInterpreter extends ScriptInterpreter