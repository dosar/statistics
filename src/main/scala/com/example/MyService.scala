package com.example

import akka.actor.Actor
import com.example.loto.model.RunResults
import com.example.loto._
import spray.http.MediaTypes._
import spray.json.{DefaultJsonProtocol, pimpAny}
import spray.routing._

// we don't implement our route structure directly in the service actor because
// we want to be able to test it independently, without having to spin up an actor
class MyServiceActor extends Actor with MyService {

  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing
  // or timeout handling
  def receive = runRoute(myRoute)
}


// this trait defines our service behavior independently from the service actor
trait MyService extends HttpService with DefaultJsonProtocol
{
  val pWindow = 10
  val fWindow = 20
  implicit val figureOrderStatistics = jsonFormat4(FigureOrderStatistics)
  implicit val figureOrderFrequencyOneRun = jsonFormat4(FigureOrderFrequencyOneRun)
  implicit val figureDiapasonStatistics = jsonFormat4(FigureDiapasonStatistics)

  val metrics = new Metrics()
  val simpleGraphics = new SimpleGraphics(RunResults.runResults)
  import metrics._
  import simpleGraphics._

  val myRoute =
  {
    pathPrefix("web") {
      getFromDirectory("/home/alespuh/work/loto/src/main/resources/web/")
    } ~
    path("graphicdata") {
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val data = graficData1(topFigures.take(10).toArray)
            data.toArray.toJson.toString
          }
        }
      }
    } ~
    (path("graphicdata1") & parameters('pW.as[Int], 'pF.as[Int])) { (pWindow, fWindow) =>
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val data = graficData2(pWindow, fWindow).toArray
            data.map(_._2).toJson.toString
          }
        }
      }
    } ~
    (path("graphicdata2") & parameters('pW.as[Int], 'pF.as[Int])) { (pWindow, fWindow) =>
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val data = graficData4(pWindow, fWindow).toArray
            data.map(_._2).toJson.toString
          }
        }
      }
    } ~
    (path("graphicdata3") & parameters('pW.as[Int], 'pF.as[Int])) { (pWindow, fWindow) =>
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val data = graficData4(pWindow, fWindow).toArray
            data.toArray.toJson.toString
          }
        }
      }
    } ~
    (path("graphicdata4") & parameters('pW.as[Int], 'sF.as[Int], 'pF.as[Int])) { (pWindow, skipWindow, fWindow) =>
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val data = new Strategy1(RunResults.runResults).withTopNonZeroFigures(pWindow, skipWindow, fWindow).map(_._2)
            data.toArray.toJson.toString
          }
        }
      }
    } ~
    (path("graphicdata5") & parameters('pW.as[Int], 'sF.as[Int], 'pF.as[Int])) { (pWindow, skipWindow, fWindow) =>
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val data = new Strategy1(RunResults.runResults)
                .withTopNonZeroFiguresWithoutNotPopular(pWindow, skipWindow, fWindow).map(_._2)
            data.toArray.toJson.toString
          }
        }
      }
    } ~
    (path("graphicdata6") & parameters('pW.as[Int], 'sF.as[Int], 'pF.as[Int])) { (pWindow, skipWindow, fWindow) =>
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val data = new Strategy3(RunResults.runResults, 12)
                .withTopNonZeroFiguresWithoutNotPopular(pWindow, skipWindow, fWindow).map(_._2)
            data.toArray.toJson.toString
          }
        }
      }
    } ~
    path("figureOrderStatistics1") {
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val data = figureOrderStatistics1(RunResults.runResults)
            data.toJson.toString
          }
        }
      }
    } ~
    path("figureOrderStatistics2") {
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val data = figureOrderStatistics2(RunResults.runResults)
            data.toJson.toString
          }
        }
      }
    } ~
    path("figureDiapasonStatistics") {
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val data = figureDiapasonStatistics1(RunResults.runResults)
            data.toJson.toString
          }
        }
      }
    } ~
    path("figureIntersectionStatistics") {
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            figureIntersectionStatistics(RunResults.runResults).toJson.toString
          }
        }
      }
    } ~
    path("import") {
      get {
        respondWithMediaType(`text/html`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            LotoImporter.fillDb
            <html>
              <body>
                <h1>Ok!</h1>
              </body>
            </html>
          }
        }
      }
    }
  }
}