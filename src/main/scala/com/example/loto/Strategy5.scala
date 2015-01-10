package com.example.loto

import com.example.loto.model.RunResult
import com.example.loto.sorter.FiguresByHitSorter

import scala.collection.mutable.ArrayBuffer

/*
* скип есть, но ставить продолжаем, просто отматываем назад больше на скип для расчета метрик
* */
class Strategy5(runResults: Array[RunResult], override val betSizeLimit: Int = 7, override val startFigure: Int = 16,
    override val endFigure: Int = 36)
extends MetricsTypes with StrategyWithMoneyStatistics[Array[RunResult], Array[Int]]
{
    def apply(pastWindow: Int, skipWindow: Int, betWindow: Int)(betGenerator: Array[RunResult] => Array[Int])(implicit
        intersectionStatistics: (Array[RunResult], Bet) => (StrategyStatistics, SliceSize) = getIntersectionStatistics) =
    {
        val startIndex = pastWindow + skipWindow
        var index = startIndex
        val buffer = ArrayBuffer[(Bet, StrategyStatistics)]()
        while (index <= runResults.length - betWindow)
        {
            val pastRrs = runResults.drop(index - pastWindow - skipWindow).take(pastWindow)
            val bet = betGenerator(pastRrs)
            val futureRrs = runResults.slice(index, index + betWindow)
            val (statistics, indexIncrement) = intersectionStatistics(futureRrs, bet)
            buffer += ((bet, statistics))
            index += indexIncrement
        }
        buffer
    }

    def debug(pastWindow: Int, skipWindow: Int, betWindow: Int)(betGenerator: Array[RunResult] => Array[Int]): Array[StrategyIteration] =
    {
        val startIndex = pastWindow + skipWindow
        var index = startIndex
        val siBuffer = ArrayBuffer[StrategyIteration]()

        def runResultItems(pastRrs: Array[RunResult], skipRrs: Array[RunResult], futureRrs: Array[RunResult], bet: Array[Figure]) =
        {
            val refinedPastRrs =
            {
                if (index == startIndex)
                    pastRrs.map(rr => RunResultItem(rr.result.map(figure => FigureIntersection(figure, false)), "white", rr.run)).toList
                else Nil
            }
            val refinedSkipRrs =
            {
                if (index == startIndex)
                    skipRrs.map(rr => RunResultItem(rr.result.map(figure => FigureIntersection(figure, false)), "grey", rr.run)).toList
                else Nil
            }
            refinedPastRrs ++ refinedSkipRrs ++ futureRrs.map
            { rr =>
                val color = if(bet.size < 5) "orange" else "darkorange"
                RunResultItem(rr.result.map(figure => FigureIntersection(figure, bet.contains(figure))), color, rr.run)
            }
        }

        while (index <= runResults.length - betWindow)
        {
            val pastRrs = runResults.drop(index - pastWindow - skipWindow).take(pastWindow)
            //от 1 до 36, чтобы видеть все хиты
            val (betHits, betFiguresCandidate) = FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(pastRrs, 1, 36))
            val bet = betGenerator(pastRrs).sorted
            val skipRrs = runResults.slice(index - skipWindow, index)
            val futureRrs = runResults.slice(index, index + betWindow)
            val (_, indexIncrement) = getIntersectionStatistics(futureRrs, bet)
            val runItems = runResultItems(pastRrs, skipRrs, futureRrs.take(indexIncrement), bet)
            siBuffer += StrategyIteration((0 until betFiguresCandidate.length)
                .map(i => FigureOccurency(betFiguresCandidate(i), betHits(i))).toArray, bet, runItems.toArray)
            index += indexIncrement
        }
        siBuffer.toArray
    }
}
