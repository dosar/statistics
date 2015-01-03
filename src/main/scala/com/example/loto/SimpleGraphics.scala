package com.example.loto

import com.example.loto.model.RunResult
import com.example.loto.sorter.FiguresByHitSorter

class SimpleGraphics(runResults: Vector[RunResult], override val topFiguresCount: Int = 12, override val startFigure: Int = 1,
    override val endFigure: Int = 36) extends MetricsTypes
{
    def topFigures: Seq[Figure] =
        FiguresByHitSorter.topFigures(figuresOccurencies(runResults))

    def allFigureOccurencies = figuresOccurencies(runResults)

    /*
    * берем топ популярных чисел и смотрим когда они повторяются в тиражах
    * */
    def graficData1(figures: Seq[Figure]): Seq[Array[Figure]] =
        runResults.map(rr => rr.result.filter(figures.contains(_)))

    /*
    * берем топ популярных чисел и смотрим сколько из них повторяется в тиражах по окнам
    * */
    def graficData2(topIntervalSize: Int, testIntervalSize: Int) =
        pastWindowToFutureWindow(topIntervalSize, testIntervalSize)(topNonZeroFiguresGeneric)

    /*
    * берем топ невыпадающих чисел за несколько пред. тиражей и смотрим выпадают ли они в нескольких след. тиражах
    * */
    def graficData4(topIntervalSize: Int, testIntervalSize: Int) = pastWindowToFutureWindow(topIntervalSize, testIntervalSize)
    { rrs =>
        figuresOccurencies(rrs).zipWithIndex.filter(_._1 == 0).map(_._2).toSeq
    }

    /*
    * берем топ наименее выпадающих чисел за несколько пред. тиражей и смотрим выпадают ли они в нескольких след. тиражах
    * */
    def graficData5(topIntervalSize: Int, testIntervalSize: Int) = pastWindowToFutureWindow(topIntervalSize, testIntervalSize)
    { rrs =>
        FiguresByHitSorter.topFiguresWithFilter(figuresOccurencies(rrs), _ > 0).reverse
    }

    /*
    * степень пересечения тиражей внутри окна
    * */
    def graphicData6(intervalSize: Int) = ???

    def trustedIntervals(window: Int, p1: Double, p2: Double, p3: Double, p4: Double, p5: Double): Array[Array[(Int, Int)]] =
    {
        val metrics = new Metrics()
        for(chunk <- 1 to runResults.length / window) yield
        {
            val wrr = runResults.drop(chunk * window).take(window)
            Array(
                metrics.figuresTrustedInterval(wrr.map(_.result(0)).toVector, p1),
                metrics.figuresTrustedInterval(wrr.map(_.result(1)).toVector, p2),
                metrics.figuresTrustedInterval(wrr.map(_.result(2)).toVector, p3),
                metrics.figuresTrustedInterval(wrr.map(_.result(3)).toVector, p4),
                metrics.figuresTrustedInterval(wrr.map(_.result(4)).toVector, p5)
            )
        }
    }.toArray

    private def pastWindowToFutureWindow(pastIntervalSize: Int, futureIntervalSize: Int)(
        figuresExtractor: Vector[RunResult] => Seq[Figure]): Vector[(Seq[Figure], Int)] =
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
