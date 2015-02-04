package com.example.loto.metrics

import com.example.loto.model.RunResult

import scala.collection.mutable.ArrayBuffer

trait Metrics extends MetricsTypes
{
    def figureIntersectionStatistics(runResults: Array[RunResult]) =
    {
        val intersections = for((p, f) <- runResults.zip(runResults.drop(1))) yield
            p.result.intersect(f.result).size
        intersections.groupBy(x => x)
            .map{ case (intersectionSize, intersections) => (intersectionSize, intersections.size) }
            .toArray
    }

    def figureIntervals(runResults: Array[RunResult], window: Int): ArrayBuffer[(Array[Int], Array[Int])] =
    {
        var windowRrs = runResults
        val result = new ArrayBuffer[(Array[Int], Array[Int])]()
        while(windowRrs.length > 0)
        {
            result += figureIntervals(windowRrs.take(window))
            windowRrs = windowRrs.drop(window)
        }
        result
    }

    /*
     * сколько выпало чисел разных разрядов за все тиражи
     */
    def figureOrderStatistics1(runResults: Seq[RunResult]): FigureOrderStatistics = figureOrderStatistics(runResults)

    /*
     * количество выпадений 2, 3, 4, 5 чисел одного разряда
     */
    def figureOrderStatistics2(runResults: Seq[RunResult]): FigureOrderFrequencyOneRun =
    {
        val runFigures = runResults.map(rr => figureOrderStatistics(Seq(rr)))
        val figure2 = runFigures.count(_.max > 1)
        val figure3 = runFigures.count(_.max > 2)
        val figure4 = runFigures.count(_.max > 3)
        val figure5 = runFigures.count(_.max > 4)
        FigureOrderFrequencyOneRun(figure2, figure3, figure4, figure5)
    }

    def figureDiapasonStatistics1(runResults: Seq[RunResult]): FigureDiapasonStatistics =
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
}

case class FigureOrderStatistics(order0Frequency: Int, order1Frequency: Int, order2Frequency: Int, order3Frequency: Int)
{
    def max = Math.max(order0Frequency, Math.max(order1Frequency, Math.max(order2Frequency, order3Frequency)))
}

case class FigureOrderFrequencyOneRun(figure2: Int, figure3: Int, figure4: Int, figure5: Int)

case class FigureDiapasonStatistics(diapason1: Int, diapason2: Int, diapason3: Int, diapason4: Int)

