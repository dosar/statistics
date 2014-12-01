package com.example

import akka.actor.Actor
import com.example.loto.LotoImporter
import com.example.loto.Metrics.{FigureDiapasonStatistics, FigureOrderFrequencyOneRun, FigureOrderStatistics}
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
  val pastWindow = 10
  val futureWindow = 200
  implicit val figureOrderStatistics = jsonFormat4(FigureOrderStatistics)
  implicit val figureOrderFrequencyOneRun = jsonFormat4(FigureOrderFrequencyOneRun)
  implicit val figureDiapasonStatistics = jsonFormat4(FigureDiapasonStatistics)

  import com.example.loto.Metrics._
  val myRoute =
  {
    pathPrefix("web") {
      getFromDirectory("/home/alespuh/work/loto/src/main/resources/web/")
    } ~
    path("graphicdata") {
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val data = graficData1(topFigures(runResults).take(10).toArray)
            data.toArray.toJson.toString
          }
        }
      }
    } ~
    path("graphicdata1") {
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val data = graficData2(pastWindow, futureWindow).toArray
            data.toArray.toJson.toString
          }
        }
      }
    } ~
    path("graphicdata2") {
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val data = graficData4(pastWindow, futureWindow).toArray
            data.toArray.toJson.toString
          }
        }
      }
    } ~
    path("graphicdata3") {
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val data = graficData4(pastWindow, futureWindow).toArray
            data.toArray.toJson.toString
          }
        }
      }
    } ~
    path("graphicdata4") {
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val data = graficData6.toArray
            data.toJson.toString
          }
        }
      }
    } ~
    path("figureOrderStatistics1") {
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val data = figureOrderStatistics1
            data.toJson.toString
          }
        }
      }
    } ~
    path("figureOrderStatistics2") {
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val data = figureOrderStatistics2
            data.toJson.toString
          }
        }
      }
    } ~
    path("figureDiapasonStatistics") {
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val data = figureDiapasonStatistics1
            data.toJson.toString
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