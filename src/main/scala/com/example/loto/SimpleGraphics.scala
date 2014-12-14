package com.example.loto

import com.example.loto.model.RunResult

class SimpleGraphics(runResults: Vector[RunResult], override val topFiguresCount: Int = 12) extends MetricsTypes
{
    def topFigures: Seq[Figure] =
        figuresOccurencies(runResults).toVector.sortBy(_._2).map(_._1)

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
        pastWindowToFutureWindow(topIntervalSize, testIntervalSize)(topNonZeroFigures)

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

    private def pastWindowToFutureWindow(pastIntervalSize: Int, futureIntervalSize: Int)(
        figuresExtractor: Vector[RunResult] => Seq[Figure]) =
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
