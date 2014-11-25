package com.example

import akka.actor.Actor
import com.example.loto.{Metrics, LotoImporter}
import spray.json.{BasicFormats, pimpAny}
import spray.routing._
import spray.http._
import MediaTypes._

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
trait MyService extends HttpService with BasicFormats
{
  val myRoute =
    path("") {
      get {
        respondWithMediaType(`text/html`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            <html>
              <body>
                <h1>Say hello to <i>spray-routing</i> on <i>spray-can</i>!</h1>
              </body>
            </html>
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
    } ~
    path("graphic/data") {
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
//            implicit val listJson = DoubleJsonFormat
            val data = Metrics.graficData(Metrics.topFigures.take(10).toArray)
            data.toList.map(_.toList).toJson.toString()
          }
        }
      }
    }

}