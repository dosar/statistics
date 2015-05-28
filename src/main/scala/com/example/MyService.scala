package com.example

import akka.actor.Actor
import com.example.loto._
import com.example.loto.metrics._
import com.example.loto.model.RunResults
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
  implicit val figureOrderStatistics = jsonFormat4(FigureOrderStatistics)
  implicit val figureOrderFrequencyOneRun = jsonFormat4(FigureOrderFrequencyOneRun)
  implicit val figureDiapasonStatistics = jsonFormat4(FigureDiapasonStatistics)
  implicit val figureOccurency = jsonFormat2(FigureOccurency)
  implicit val figureIntersection = jsonFormat2(FigureIntersection)
  implicit val runResultItem = jsonFormat3(RunResultItem)
  implicit val strategyIteration = jsonFormat3(StrategyIteration)

  val metrics = new Metrics(){ override val betSizeLimit = 6 }
  val simpleGraphics = new SimpleGraphics(RunResults.runResults)
  import metrics._
  import simpleGraphics._

  val myRoute =
  {
    pathPrefix("web") {
      getFromResourceDirectory("web")
    } ~
    (path("trustedintervals") & parameters('pw.as[Int], 'p1.as[Int], 'p2.as[Int], 'p3.as[Int], 'p4.as[Int], 'p5.as[Int]))
    { (window, p1, p2, p3, p4, p5) =>
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            new SimpleGraphics(RunResults.runResults)
                .trustedIntervals(window, p1 / 100.0, p2 / 100.0, p3 / 100.0, p4 / 100.0, p5 / 100.0).toJson.toString
          }
        }
      }
    } ~
    path("occurencies")
    {
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val metrics = new Metrics{ override val betSizeLimit = 6 }
            metrics.figuresOccurencies(RunResults.runResults, 1, 36).toJson.toString
          }
        }
      }
    } ~
    path("runresults") {
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val data = RunResults.runResults.sortBy(- _.run).map(_.result.map(_ -> false))
            for(i <- 1 until data.length)
            {
              val current = data(i)
              val previous = data(i - 1)
              val intersection = current.intersect(previous)
              for(j <- 0 until intersection.length)
              {
                val (figure, _) = intersection(j)
                current(current.indexWhere(_._1 == figure)) = figure -> true
                previous(previous.indexWhere(_._1 == figure)) = figure -> true
              }
            }
            data.toArray.toJson.toString
          }
        }
      }
    } ~
    (path("strategydebug") & parameters('pW.as[Int], 'sW.as[Int], 'fW.as[Int], 'tFC.as[Int], 'sF.as[Int], 'eF.as[Int], 'page.as[Int], 'iPP.as[Int]))
    { (pw, sw, fw, takeCount, sf, ef, page, iPP) =>
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val strategy = new Strategy4(RunResults.runResults, takeCount, sf, ef) with ComplexBetFigureMetrics
            {
              override val endSize = 3
            }
            val data: Array[StrategyIteration] = strategy.debug(pw, sw, fw)(strategy.middleOccurencyFigures)
            val start = page * iPP - iPP
            val end = start + iPP
              (data.length, data.slice(start, end)).toJson.toString
          }
        }
      }
    } ~
    (path("graphicdata") & parameters('sf.as[Int], 'ef.as[Int], 'tfc.as[Int], 'excludeFigures.as[String], 'pw.as[Int],
      'sw.as[Int], 'fw.as[Int], 'sType.as[String], 'mType.as[String], 'endSize.as[Int]))
    { (sf, ef, tfc, excludeFigures, pw, sw, fw, strType, mType, endSize) =>
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            new StrategySelector(sf, ef, tfc, excludeFigures, pw, sw, fw, strType, mType, endSize).result.toJson.toString
          }
        }
      }
    } ~
    path("graphicdata0") {
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val data = RunResults.runResults.map(_.result)
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
    (path("graphicdata6") & parameters('pW.as[Int], 'sW.as[Int], 'fW.as[Int], 'tFC.as[Int], 'sF.as[Int], 'eF.as[Int]))
    { (pWindow, skipWindow, fWindow, tfCount, sFigure, eFigure) =>
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val data = new Strategy3(RunResults.runResults, betSizeLimit = tfCount, startFigure = sFigure, endFigure = eFigure)
                .withTopNonZeroFiguresWithoutNotPopular(pWindow, skipWindow, fWindow).map(_._2)
            data.toArray.toJson.toString
          }
        }
      }
    } ~
    (path("graphicdata7") & parameters('pW.as[Int]))
    { pWindow =>
      get {
        respondWithMediaType(`application/json`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            val data = new Metrics{ override val betSizeLimit = 6 }.figureIntervals(RunResults.runResults, pWindow)
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