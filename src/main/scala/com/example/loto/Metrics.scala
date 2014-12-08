package com.example.loto

import java.text.SimpleDateFormat
import java.util
import java.util.Date

import com.example.loto.model.RunResult
import com.orientechnologies.orient.core.record.impl.ODocument

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Metrics extends MetricsTypes
{
    case class FigureOrderStatistics(order0Frequency: Int, order1Frequency: Int, order2Frequency: Int, order3Frequency: Int)
    {
        def max = Math.max(order0Frequency, Math.max(order1Frequency, Math.max(order2Frequency, order3Frequency)))
    }

    case class FigureOrderFrequencyOneRun(figure2: Int, figure3: Int, figure4: Int, figure5: Int)

    case class FigureDiapasonStatistics(diapason1: Int, diapason2: Int, diapason3: Int, diapason4: Int)

    def figuresOccurencies(rrs: Seq[RunResult], startFigure: Figure = 1): mutable.Map[Figure, HitCount] =
    {
        var ind = 0
        val figuresMap = new Array[Int](36)

        while(ind < rrs.length)
        {
            val rrResult = rrs(ind).result
            figuresMap(rrResult(0) - 1) += 1
            figuresMap(rrResult(1) - 1) += 1
            figuresMap(rrResult(2) - 1) += 1
            figuresMap(rrResult(3) - 1) += 1
            figuresMap(rrResult(4) - 1) += 1
            ind += 1
        }
        ind = startFigure - 1
        val result = mutable.Map[Int, Int]()
        while(ind < figuresMap.length)
        {
            result += (ind + 1) -> figuresMap(ind)
            ind += 1
        }
        result
    }
}

class SimpleGraphics()
{

}

/*
* runResults - выборка на которой считаем все метрики
* topFiguresCount - сколько чисел брать из набора чисел
* */
class Metrics(topFiguresCount: Int = 12) extends MetricsTypes
{
    import com.example.loto.Metrics._

    def topFigures: Seq[Figure] =
        figuresOccurencies(runResults).toVector.sortBy(_._2).map(_._1)

    def allFigureOccurencies = figuresOccurencies(runResults)

    private val emptyFigureOccurencis = (1 to 36).map(_ -> 0)

    /*
    * берем топ популярных чисел и смотрим когда они повторяются в тиражах
    * */
    def graficData1(figures: Seq[Figure]): Seq[Array[Figure]] =
        runResults.map(rr => rr.result.filter(figures.contains(_)))

    /*
    * берем топ популярных чисел и смотрим сколько из них повторяется в тиражах по окнам
    * */
    def graficData2(topIntervalSize: Int, testIntervalSize: Int) =
        pastWindowToFutureWindow(topIntervalSize, testIntervalSize)(topNonZeroFigures)

    /*
    * просто покажем облако результатов
    * */
    def graficData3(rrs: Seq[RunResult]): Seq[Array[Figure]] = rrs.map(_.result)

    /*
    * берем топ невыпадающих чисел за несколько пред. тиражей и смотрим выпадают ли они в нескольких след. тиражах
    * */
    def graficData4(topIntervalSize: Int, testIntervalSize: Int) = pastWindowToFutureWindow(topIntervalSize, testIntervalSize)
    { rrs =>
        figuresOccurencies(rrs).filter(_._2 == 0).map(_._1).toSeq
    }

    /*
    * берем топ наименее выпадающих чисел за несколько пред. тиражей и смотрим выпадают ли они в нескольких след. тиражах
    * */
    def graficData5(topIntervalSize: Int, testIntervalSize: Int) = pastWindowToFutureWindow(topIntervalSize, testIntervalSize)
    { rrs =>
        figuresOccurencies(rrs).filter(_._2 > 0).toSeq.sortBy(_._2).map(_._1)
    }

    /*
    * степень пересечения тиражей внутри окна
    * */
    def graphicData6(intervalSize: Int) = ???

    def figureIntersectionStatistics =
    {
        val intersections = for((p, f) <- runResults.zip(runResults.drop(1))) yield
            p.result.intersect(f.result).size
        intersections.groupBy(x => x)
            .map{ case (intersectionSize, intersections) => (intersectionSize, intersections.size) }
            .toArray
    }

    def topNonZeroFigures(rrs: Seq[RunResult]): Array[Figure] = topNonZeroFiguresGeneric(rrs, 1, 36)

    def topNonZeroFiguresWithoutNotPopular(rrs: Seq[RunResult]): Array[Figure] = topNonZeroFiguresGeneric(rrs, 16, 36)

    def topNonZeroFiguresGeneric(rrs: Seq[RunResult], startFigure: Figure, endFigure: Figure): Array[Figure] =
    {
        val occs = figuresOccurencies(rrs)
        var list = List[(Figure, HitCount)]()
        var figure = startFigure
        while(figure <= 36)
        {
            val hits = occs(figure)
            if(hits > 0)
                list = (figure, hits) :: list
            figure += 1
        }
        val sorted = list.sortBy(- _._2)
        sorted.map(_._1).take(topFiguresCount).toArray
    }

    /*
    * betCandidate должен быть отсортирован в нужном порядке
    * */
    def topNonZeroFiguresWithoutPrevious(betCandidate: Array[(Figure, HitCount)], index: Int): Array[Figure] =
    {
        val previous = runResults(index - 1).result
        val buffer = new Array[Figure](topFiguresCount)
        var fi = 0
        var arrInd = 0
        while(fi < betCandidate.length && arrInd < topFiguresCount)
        {
            val (figure, hitCount) = betCandidate(fi)
            val previousContainsFigure =
                figure == previous(0) || figure == previous(1) || figure == previous(2) || figure == previous(3) || figure == previous(4)
            if(hitCount != 0 && !previousContainsFigure)
            {
                buffer(arrInd) = figure
                arrInd += 1
            }
            fi += 1
        }
        buffer
    }

    /*
             * сколько выпало чисел разных разрядов за все тиражи
             */
    def figureOrderStatistics1: FigureOrderStatistics = figureOrderStatistics(runResults)

    /*
     * количество выпадений 2, 3, 4, 5 чисел одного разряда
     */
    def figureOrderStatistics2: FigureOrderFrequencyOneRun =
    {
        val runFigures = runResults.map(rr => figureOrderStatistics(Seq(rr)))
        val figure2 = runFigures.count(_.max > 1)
        val figure3 = runFigures.count(_.max > 2)
        val figure4 = runFigures.count(_.max > 3)
        val figure5 = runFigures.count(_.max > 4)
        FigureOrderFrequencyOneRun(figure2, figure3, figure4, figure5)
    }

    def figureDiapasonStatistics1: FigureDiapasonStatistics =
    {
        val runFigures = runResults.flatMap(_.result)
        val diapason1 = runFigures.count(_ < 7)
        val diapason2 = runFigures.count(f => f > 6 && f < 17)
        val diapason3 = runFigures.count(f => f > 16 && f < 27)
        val diapason4 = runFigures.count(f => f > 26 && f < 37)
        FigureDiapasonStatistics(diapason1, diapason2, diapason3, diapason4)
    }

    private def figureOrderStatistics(runResults: Seq[RunResult]): FigureOrderStatistics =
    {
        val runFigures = runResults.flatMap(_.result)
        val order0Figures = runFigures.count(_ < 10)
        val order1Figures = runFigures.count(f => f > 9 && f < 20)
        val order2Figures = runFigures.count(f => f > 19 && f < 30)
        val order3Figures = runFigures.count(f => f > 29 && f < 40)
        FigureOrderStatistics(order0Figures, order1Figures, order2Figures, order3Figures)
    }

    private def pastWindowToFutureWindow(pastIntervalSize: Int, futureIntervalSize: Int)(
        figuresExtractor: Seq[RunResult] => Seq[Figure]) =
    {
        (for(pastWindow <- runResults.take(runResults.length - futureIntervalSize).zipWithIndex.sliding(pastIntervalSize)) yield
        {
            val topIntervalFigures = figuresExtractor(pastWindow.map(_._1)).take(topFiguresCount)
            val last = pastWindow.last._2 + 1
            val futureRuns = runResults.drop(last).take(futureIntervalSize)
            val intersections = futureRuns.map(fr => fr.result.intersect(topIntervalFigures)).map(_.size)
            (topIntervalFigures, if(intersections.isEmpty) 0 else intersections.max)
        }).toVector
    }

}

