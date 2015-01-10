package com.example.loto

import com.example.loto.model.RunResult

import scala.collection.mutable.ArrayBuffer

/*
* Фильтруем значения используя доверительные интервалы
* */
class Strategy6(runResults: Array[RunResult], override val betSizeLimit: Int = 7,
    override val startFigure: Int = 16, override val endFigure: Int = 36)
extends MetricsTypes with StrategyWithMoneyStatistics[(Array[Int], Array[RunResult]), Array[Int]]
{
    type PastWindow = Array[RunResult]

    override def apply(pastWindow: Int, skipWindow: Int, betWindow: Int)(
        betGenerator: ((FigureHitsArray, PastWindow)) => Array[Int])(implicit intersectionStatistics:
        (Array[RunResult], Bet) => (StrategyStatistics, SliceSize) = getIntersectionStatistics) =
    {
        val startIndex = pastWindow + skipWindow
        var index = startIndex
        val buffer = ArrayBuffer[(Array[Figure], (IntersectionCount2, IntersectionCount3, IntersectionCount4, IntersectionCount5, MoneyPlus, MoneyMinus))]()
        while (index <= runResults.length - betWindow)
        {
            val pastRrs = runResults.drop(index - pastWindow - skipWindow).take(pastWindow)
            val betCandidate = figuresOccurencies(pastRrs)
            val bet = betGenerator(betCandidate, pastRrs)
            val futureRrs = runResults.slice(index, index + betWindow)
            val (statistics, indexIncrement) = intersectionStatistics(futureRrs, bet)
            buffer += ((bet, statistics))
            index += indexIncrement
        }
        buffer
    }
}