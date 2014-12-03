package com.example.loto

import java.text.SimpleDateFormat
import java.util
import java.util.Date

import com.orientechnologies.orient.core.record.impl.ODocument

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

object Metrics
{
    case class FigureOrderStatistics(order0Frequency: Int, order1Frequency: Int, order2Frequency: Int, order3Frequency: Int)
    {
        def max = Math.max(order0Frequency, Math.max(order1Frequency, Math.max(order2Frequency, order3Frequency)))
    }

    case class FigureOrderFrequencyOneRun(figure2: Int, figure3: Int, figure4: Int, figure5: Int)

    case class FigureDiapasonStatistics(diapason1: Int, diapason2: Int, diapason3: Int, diapason4: Int)
}

/*
* runResults - выборка на которой считаем все метрики
* topFiguresCount - сколько чисел брать из набора чисел
* */
class Metrics(runResults: Seq[RunResult], topFiguresCount: Int = 9)
{
    import Metrics._

    type Figure = Int

    def topFigures: Seq[Figure] =
        figuresOccurencies(runResults).toList.sortBy(_._2).map(_._1)

    def allFigureOccurencies = figuresOccurencies(runResults)

    def figuresOccurencies(rrs: Seq[RunResult]): Map[Int, Int] =
    {
        val results = (1 to 36).map(_ -> 0).toMap

        def update(map: Map[Int, Int], figure: Int) =
            map.updated(figure, map(figure) + 1)

        val result = rrs.foldLeft(results)((map, rr) => rr.result.foldLeft(map)(update))
        result
//            .filter(_._1 > 16)
    }

    /*
    * берем топ популярных чисел и смотрим когда они повторяются в тиражах
    * */
    def graficData1(figures: Seq[Figure]): Seq[Array[Figure]] =
        runResults.map(rr => rr.result.filter(figures.contains(_)))

    /*
    * берем топ популярных чисел и смотрим сколько из них повторяется в тиражах по окнам
    * */
    def graficData2(topIntervalSize: Int, testIntervalSize: Int) = pastWindowToFutureWindow(topIntervalSize, testIntervalSize)
    { rrs =>
        val sorted = figuresOccurencies(rrs).filter(_._2 > 0).toSeq.sortBy(- _._2)
        sorted.map(_._1)
    }

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

    def strategy1(pastWindow: Int, skipWindow: Int, betWindow: Int) = ???

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
        }).toList
    }

}

case class RunResult(run: Int, date: Date, result: Array[Int])

object RunResult
{
    def apply(run: String, date: String, result: String) =
        new RunResult(run.toInt, format.parse(date), result.split(" ").filter(_.trim != "").take(5).map(_.toInt))

    private val format = new SimpleDateFormat("dd.MM.yyyy hh:mm")
}

object RunResultService extends OrientDb
{
    def from(doc: ODocument): RunResult =
    {
        val integers = doc.field[util.ArrayList[Integer]]("result")
        RunResult(doc.field[Int]("run"), doc.field[Date]("date"), integers.map(_.intValue()).toArray)
    }

    def list = tx
    { db =>
        val table = db.browseClass("loto_5_36")
        val result = ListBuffer[RunResult]()
        while(table.hasNext) result += from(table.next())
        result
    }
}

object RunResults
{
  lazy val runResults = RunResultService.list.sortBy(_.run)
}