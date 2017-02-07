package org.bitcoins.core.script.oracle
import akka.pattern.ask
import akka.actor.{ActorRef, ActorSystem, PoisonPill}
import akka.util.Timeout
import com.github.nfldb.config.NflDbApiDbConfigWorkstation
import com.github.nfldb.models.{PlayPlayerDAO, PlayPlayerPassing}
import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.constant._

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.DurationInt

import scala.util.{Failure, Success}

/**
  * Created by chris on 2/1/17.
  */
trait HydrogenInterpreter {

  /** Takes the identifier and time from the stack then queries our oracle, finally pushes the result from the oracle onto the stack */
  def opApiQuery1Id(program: ScriptProgram) : ScriptProgram = {
    require(program.script.headOption == Some(OP_APIQUERY1ID), "Next script operation must be OP_APIQUERY1ID")
    require(program.stack.size > 1, "We require the stack to have a time and id for OP_APIQUERY1ID")
    val time: Long = parseTime(program.stack(1))
    val id: String = parseId(program.stack.head)
    val actualNum: Long = sendQuery(id,time)
    val result = ScriptNumber(actualNum)
    ScriptProgram(program, result :: program.stack.tail.tail, program.script.tail)
  }

  /** This function parses the unique identifier for the script, this identifier will be sent to the oracle
    * to retrieve information */
  def parseId(token: ScriptToken): String = {
    val tokenWithoutPrefix = token match {
      case number: ScriptNumber => number.underlying
      case x @ (_: ScriptConstant | _: ScriptOperation) =>
        throw new IllegalArgumentException("NFL sidechain expects a number as the player id, got: " + x)
    }
    val neededDigits = 10
    val playerIdBeforePadding = "00-" + tokenWithoutPrefix
    val neededPadding = neededDigits - playerIdBeforePadding.size
    val padding = "0" * neededPadding
    "00-" + padding + tokenWithoutPrefix
  }

  /** Sends an http request to our oracle requesting information about the given id at time t */
  def sendQuery(id: String, time: Long): Long = {
    val context = ActorSystem("bitcoin-s-system")
    val dbConfig = NflDbApiDbConfigWorkstation
    implicit val timeout = Timeout(5.seconds)
    val playDAO: ActorRef = PlayPlayerDAO(context, dbConfig)
    val gameIdString: String = time.toString
    val statsFuture: Future[Seq[PlayPlayerPassing]] = playDAO.ask(PlayPlayerDAO.PlayerPassingStatsForGameByID(gameIdString, id)).mapTo[Seq[PlayPlayerPassing]]
    killActorOnComplete(statsFuture, playDAO, context.dispatcher)
    val stats = Await.result(statsFuture,5.seconds)
    val stat = stats.head
    stat.passingYds
  }

  /** Parses the number that the counter parties agreed to */
  def parseAgreedNumber(token: ScriptToken): Long = token match {
    case number: ScriptNumber => number.underlying
    case x @ (_: ScriptConstant | _ : ScriptOperation) =>
      throw new IllegalArgumentException("OP_TICKERQUERY expects a number as the agreed price, got: " + x)
  }

  /** Parses the agreed upon time from the given [[ScriptToken]] */
  def parseTime(token: ScriptToken): Long = token match {
    case number: ScriptNumber => number.underlying
    case x @ (_: ScriptConstant | _: ScriptOperation) =>
      throw new IllegalArgumentException("OP_TICKERQUERY expects a number as the time, got: " + x)
  }

  private def killActorOnComplete(f: Future[Any], actor: ActorRef, context: ExecutionContext): Unit = f.onComplete {
    case Success(_) | Failure(_) => actor ! PoisonPill
  }(context)

}


object HydrogenInterpreter extends HydrogenInterpreter