package com.example.loto

import java.util
import java.util.Date

import com.orientechnologies.orient.core.record.impl.ODocument

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

/**
 * Created by alespuh on 25.11.14.
 */
object Metrics
{
    type Figure = Int

    def topFigures(runResults: Seq[RunResult]): Seq[Figure] =
        figuresOccurencies(runResults).toList.sortBy(_._2).map(_._1)

    def figuresOccurencies(runResults: Seq[RunResult]) =
    {
        val results = (1 to 36).map(_ -> 0).toMap
        def update(map: Map[Int, Int], figure: Int) =
            map.updated(figure, results(figure) + 1)

        val result = runResults.foldLeft(results)((map, rr) => rr.result.foldLeft(map)(update))
        result.filter(_._1 > 16)
    }


    lazy val runResults = RunResultService.list.sortBy(_.run)

    /*
    * берем топ популярных чисел и смотрим когда они повторяются в тиражах
    * */
    def graficData1(figures: Seq[Figure]): Seq[Array[Figure]] =
        runResults.map(rr => rr.result.filter(figures.contains(_)))

    /*
    * берем топ популярных чисел и смотрим когда они повторяются в тиражах по окнам
    * */
    def graficData2(topIntervalSize: Int, testIntervalSize: Int): Seq[Int] = pastWindowToFutureWindow(topIntervalSize, testIntervalSize)
    { runResults =>
        figuresOccurencies(runResults).filter(_._2 > 0).toSeq.sortBy(_._2).map(_._1)
    }

    /*
    * просто покажем облако результатов
    * */
    def graficData3: Seq[Array[Figure]] = runResults.map(_.result)

    /*
    * берем топ невыпадающих чисел за несколько пред. тиражей и смотрим выпадают ли они в нескольких след. тиражах
    * */
    def graficData4(topIntervalSize: Int, testIntervalSize: Int): Seq[Int] = pastWindowToFutureWindow(topIntervalSize, testIntervalSize)
    { runResults =>
        figuresOccurencies(runResults).filter(_._2 == 0).map(_._1).toSeq
    }

    /*
    * берем топ наименее выпадающих чисел за несколько пред. тиражей и смотрим выпадают ли они в нескольких след. тиражах
    * */
    def graficData5(topIntervalSize: Int, testIntervalSize: Int): Seq[Int] = pastWindowToFutureWindow(topIntervalSize, testIntervalSize)
    { runResults =>
        figuresOccurencies(runResults).filter(_._2 > 0).toSeq.sortBy(_._2).reverse.map(_._1)
    }

    /*
     * сколько выпало чисел разных разрядов за все тиражи
     */
    def figureOrderStatistics1(): FigureOrderStatistics = figureOrderStatistics(runResults)

    /*
     * количество выпадений 2, 3, 4, 5 чисел одного разряда
     */
    def figureOrderStatistics2(): FigureOrderFrequencyOneRun =
    {
        val runFigures = runResults.map(rr => figureOrderStatistics(Seq(rr)))
        val figure2 = runFigures.count(_.max > 1)
        val figure3 = runFigures.count(_.max > 2)
        val figure4 = runFigures.count(_.max > 3)
        val figure5 = runFigures.count(_.max > 4)
        FigureOrderFrequencyOneRun(figure2, figure3, figure4, figure5)
    }

    def figureDiapasonStatistics1(): FigureDiapasonStatistics =
    {
        val runFigures = runResults.flatMap(_.result)
        val diapson1 = runFigures.count(_ < 7)
        val diapson2 = runFigures.count(f => f > 6 && f < 17)
        val diapson3 = runFigures.count(f => f > 16 && f < 27)
        val diapson4 = runFigures.count(f => f > 26 && f < 37)
        FigureDiapasonStatistics(diapson1, diapson2, diapson3, diapson4)
    }

    /*
    * сколько выпало чисел одного разряда для каждого тиража
    * */
    def graficData6 = runResults.map(rr => figureOrderStatistics(Seq(rr)).max)


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
        (for(pastWindow <- runResults.zipWithIndex.sliding(pastIntervalSize)) yield
        {
            val topIntervalFigures = figuresExtractor(pastWindow.map(_._1)).take(9).toArray
            val last = pastWindow.last._2 + 1
            val futureRuns = runResults.drop(last).take(futureIntervalSize)
            val intersections = futureRuns.map(fr => fr.result.intersect(topIntervalFigures)).map(_.size)
            if(intersections.isEmpty) 0 else intersections.max
        }).toSeq
    }

    case class FigureOrderStatistics(order0Frequency: Int, order1Frequency: Int, order2Frequency: Int, order3Frequency: Int)
    {
        def max = Math.max(order0Frequency, Math.max(order1Frequency, Math.max(order2Frequency, order3Frequency)))
    }

    case class FigureOrderFrequencyOneRun(figure2: Int, figure3: Int, figure4: Int, figure5: Int)

    case class FigureDiapasonStatistics(diapason1: Int, diapason2: Int, diapason3: Int, diapason4: Int)
}

case class RunResult(date: Date, run: Int, result : Array[Int])

object RunResultService extends OrientDb
{
    def from(doc: ODocument): RunResult =
    {
        val integers = doc.field[util.ArrayList[Integer]]("result")
        RunResult(doc.field[Date]("date"), doc.field[Int]("run"), integers.map(_.intValue()).toArray)
    }

    def list = tx
    { db =>
        val table = db.browseClass("loto_5_36")
        val result = ListBuffer[RunResult]()
        while(table.hasNext) result += from(table.next())
        result
    }
}