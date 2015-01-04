package com.example.loto

import com.example.loto.model.RunResult
import com.example.loto.sorter.FiguresByHitSorter

import scala.collection.mutable.ArrayBuffer

/*
* перестаем ставить по старой ставке если получили хит на 5 чисел
* */
class Strategy4(runResults: Vector[RunResult], override val topFiguresCount: Int = 7, override val startFigure: Int = 16,
    override val endFigure: Int = 36)
extends MetricsTypes with StrategyWithMoneyStatistics[Vector[RunResult], Array[Int]]
{
    def apply(pastWindow: Int, skipWindow: Int, betWindow: Int)(betGenerator: Vector[RunResult] => Array[Int]): ArrayBuffer[(Array[Figure], (IntersectionCount2, IntersectionCount3, IntersectionCount4, IntersectionCount5, MoneyPlus, MoneyMinus))] =
    {
        val startIndex = pastWindow
        val sliceSize = skipWindow + betWindow
        var index = startIndex
        val buffer = ArrayBuffer[(Array[Figure], (IntersectionCount2, IntersectionCount3, IntersectionCount4, IntersectionCount5, MoneyPlus, MoneyMinus))]()
        while (index <= runResults.length - sliceSize)
        {
            val pastRrs = runResults.slice(index - pastWindow, index)
            val bet = betGenerator(pastRrs)
            val futureRrs = runResults.slice(index + skipWindow, index + sliceSize)
            val (statistics, indexIncrement) = getIntersectionStatistics(futureRrs, bet)
            buffer += ((bet, statistics))
            index += indexIncrement
        }
        buffer
    }

    def debug(pastWindow: Int, skipWindow: Int, betWindow: Int)(betGenerator: Vector[RunResult] => Array[Int]): Array[StrategyIteration] =
    {
        val startIndex = pastWindow
        val sliceSize = skipWindow + betWindow
        var index = startIndex
        val siBuffer = ArrayBuffer[StrategyIteration]()

        def runResultItems(pastRrs: Vector[RunResult], skipRrs: Vector[RunResult], futureRrs: Vector[RunResult], bet: Array[Figure]) =
        {
            val refinedPastRrs = if (index == startIndex)
                pastRrs.map(rr => RunResultItem(rr.result.map(figure => FigureIntersection(figure, false)), "white"))
            else Nil
            refinedPastRrs ++
                skipRrs.map(rr => RunResultItem(rr.result.map(figure => FigureIntersection(figure, false)), "grey")) ++
                futureRrs.map
                { rr =>
                    val color = if(bet.size < 5) "orange" else "darkorange"
                    RunResultItem(rr.result.map(figure => FigureIntersection(figure, bet.contains(figure))), color)
                }
        }

        while (index <= runResults.length - sliceSize)
        {
            val pastRrs = runResults.slice(index - pastWindow, index)
            //от 1 до 36, чтобы видеть все хиты
            val (betHits, betFiguresCandidate) = FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(pastRrs, 1, 36))
            val bet = betGenerator(pastRrs).sorted
            val skipRrs = runResults.slice(index, index + skipWindow)
            val futureRrs = runResults.slice(index + skipWindow, index + sliceSize)
            val (_, indexIncrement) = getIntersectionStatistics(futureRrs, bet)
            val runItems = runResultItems(pastRrs, skipRrs, futureRrs.take(indexIncrement), bet)
            siBuffer += StrategyIteration((0 until betFiguresCandidate.length)
                .map(i => FigureOccurency(betFiguresCandidate(i), betHits(i))).toArray, bet, runItems.toArray)
            index += indexIncrement
        }
        siBuffer.toArray
    }
}

case class StrategyIteration(betCandidate: Array[FigureOccurency], bet: Array[Int], runResults: Array[RunResultItem])
case class FigureOccurency(figure: Int, hit: Int)
case class RunResultItem(result: Array[FigureIntersection], color: String)
case class FigureIntersection(figure: Int, intersected: Boolean)
