package com.example

import java.util.Date

import com.example.loto.Metrics.{FigureDiapasonStatistics, FigureOrderFrequencyOneRun, FigureOrderStatistics}
import com.example.loto.{Metrics, RunResult}
import org.scalatest._

import scala.collection.mutable.ListBuffer

/**
 * Created by alex on 30.11.14.
 */
class MetricsSpec extends FlatSpec with Matchers with TestHelper
{
    val initialSeq = (1 to 15).toSeq
    val arrays = genSeqs(1, 5, initialSeq).map(_.toArray)
    val runResults = ListBuffer[RunResult]()
    val metrics = new Metrics(runResults.toVector)
    arrays.zipWithIndex.map(el => RunResult(el._2, new Date(), el._1 )).foreach(runResults += _)

    "figuresOccurences" should "produce the correct result" in
    {
        metrics.figuresOccurencies(runResults).foldLeft(0)((acc, map) => acc + map._2) should be (0)
    }

    "graphicData1" should "produce the correct result" in
    {
        val result = metrics.graficData1(Seq(1, 6))
        result.length should be(5)
        result should contain (Array(6))
        result should contain (Array(1))
    }

    "graphicData2" should "produce the correct result" in {

      val result = metrics.graficData2(1, 2)

      result.toSeq should be(Seq(0, 0, 0, 0, 0))
    }

    "graphicData4" should "produce the correct result" in {

      val result = metrics.graficData4(1, 2)

      result.toSeq should be(Seq(0, 0, 0, 0, 0))
    }


    "graphicData5" should "produce the correct result" in {

      val result = metrics.graficData5(1, 2)

      result.toSeq should be(Seq(0, 0, 0, 0, 0))
    }

    "figureOrderStatistics1" should "produce the correct result" in {

      val result = metrics.figureOrderStatistics1

      result should be(FigureOrderStatistics(9, 6, 0, 0))
    }

    "figureOrderStatistics2" should "produce the correct result" in {

      val result = metrics.figureOrderStatistics2

      result should be(FigureOrderFrequencyOneRun(4, 3, 1, 1))
    }

    "figureDiapasonStatistics1" should "produce the correct result" in {

      val result = metrics.figureDiapasonStatistics1

      result should be(FigureDiapasonStatistics(6, 9, 0, 0))
    }
}
