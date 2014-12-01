package com.example

import java.util.Date

import com.example.loto.Metrics.{FigureDiapasonStatistics, FigureOrderFrequencyOneRun, FigureOrderStatistics}
import com.example.loto.{Metrics, RunResult}
import org.scalatest._

import scala.collection.mutable.ListBuffer

/**
 * Created by alex on 30.11.14.
 */
class MetricsSpec extends FlatSpec with Matchers with TestHelper {
    val initialSeq = (for(i <- 1 to 15)yield(i)).toSeq
    val arrays = genSeqs(1, 5, initialSeq).map(_.toArray)
    val runResults: ListBuffer[RunResult] = ListBuffer()
    arrays.zipWithIndex.map( el => RunResult(new Date(), el._2, el._1 )).foreach(runResults += _)

    "figuresOccurences" should "produce the correct result" in {

      Metrics.figuresOccurencies(runResults).foldLeft(0)((acc, map) => acc + map._2) should be (0)
    }

    "graphicData1" should "produce the correct result" in {

        val result = Metrics.graficData1(Seq(1, 6), runResults)

        result.length should be(5)
        result should contain (Array(6))
        result should contain (Array(1))
    }

    "graphicData2" should "produce the correct result" in {

      val result = Metrics.graficData2(1, 2, runResults)

      result.toSeq should be(Seq(0, 0, 0, 0, 0))
    }

    "graphicData4" should "produce the correct result" in {

      val result = Metrics.graficData4(1, 2, runResults)

      result.toSeq should be(Seq(0, 0, 0, 0, 0))
    }


    "graphicData5" should "produce the correct result" in {

      val result = Metrics.graficData5(1, 2, runResults)

      result.toSeq should be(Seq(0, 0, 0, 0, 0))
    }

    "graphicData6" should "produce the correct result" in {

      val result = Metrics.graficData6(runResults)

      result should be(ListBuffer(5, 3, 3, 2, 1))
    }

    "figureOrderStatistics1" should "produce the correct result" in {

      val result = Metrics.figureOrderStatistics1(runResults)

      result should be(FigureOrderStatistics(9, 6, 0, 0))
    }

    "figureOrderStatistics2" should "produce the correct result" in {

      val result = Metrics.figureOrderStatistics2(runResults)

      result should be(FigureOrderFrequencyOneRun(4, 3, 1, 1))
    }

    "figureDiapasonStatistics1" should "produce the correct result" in {

      val result = Metrics.figureDiapasonStatistics1(runResults)

      result should be(FigureDiapasonStatistics(6, 9, 0, 0))
    }
}
