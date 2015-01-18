package com.example.loto.actors

import akka.actor.{ActorSystem, Props}
import org.apache.commons.io.FileUtils
import scala.collection.JavaConversions._

object LoggerActor
{
    val system = ActorSystem("testakka")
    lazy val ref = system.actorOf(Props(new LoggerActor))
}

class LoggerActor extends ActorBase
{
    val logFile = FileUtils.getFile("log.txt")

    val resultsFile = FileUtils.getFile("resultsLog.txt")

    def receive =
    {
        case LogIterationResults(prefix, results) =>
            FileUtils.writeLines(logFile, prefix :: results.toList, "\n", true)
        case LogResults(results: Seq[String]) =>
            FileUtils.writeLines(resultsFile, results, "\n", true)
    }
}

case class LogIterationResults(prefix: String, results: Seq[String])
case class LogResults(results: Seq[String])