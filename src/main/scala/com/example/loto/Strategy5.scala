package com.example.loto

import com.example.loto.model.RunResult

import scala.collection.mutable.ArrayBuffer

class Strategy5(runResults: Vector[RunResult], override val topFiguresCount: Int = 7, override val startFigure: Int = 16,
    override val endFigure: Int = 36)
extends MetricsTypes with StrategyWithMoneyStatistics[Vector[RunResult], Array[Int]]
{
    def apply(pastWindow: Int, skipWindow: Int, betWindow: Int)(betGenerator: Vector[RunResult] => Array[Figure]) =
    {
        val startIndex = pastWindow + skipWindow
        var index = startIndex
        val buffer = ArrayBuffer[(Array[Figure], (IntersectionCount2, IntersectionCount3, IntersectionCount4, IntersectionCount5, MoneyPlus, MoneyMinus))]()
        while (index <= runResults.length - betWindow)
        {
            val pastRrs = runResults.drop(index - pastWindow - skipWindow).take(pastWindow)
            val bet = betGenerator(pastRrs)
            val futureRrs = runResults.slice(index, index + betWindow)
            val (statistics, indexIncrement) = getIntersectionStatistics(futureRrs, bet)
            buffer += ((bet, statistics))
            index += indexIncrement
        }
        buffer
    }
}
