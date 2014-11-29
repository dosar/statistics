package com.example

import akka.actor.Actor
import com.example.loto.{LotoImporter, Metrics}
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
  import Metrics._
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