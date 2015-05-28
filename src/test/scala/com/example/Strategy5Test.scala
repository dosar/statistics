package com.example

import com.example.loto.metrics.{StrategyStatistics, ProbabalisticIntervalsMetrics}
import com.example.loto.model.RunResults
import com.example.loto.Strategy5

/**
 * Created by alespuh on 15.12.14.
 */
class Strategy5Test extends TestBase
{
    test("strategy 5 test apply")
    {
        val strategy = new Strategy5(Array(
            (1, 2, 8, 9, 10),
            (1, 2, 3, 9, 10),
            (1, 2, 3, 9, 10),
            (1, 2, 3, 4, 10),
            (11, 15, 8, 9, 10),
            (1, 2, 3, 4, 5),
            (1, 2, 3, 4, 5)
        ), betSizeLimit = 4, startFigure = 1)
        val result = strategy.apply(2, 1, 2)(strategy.topNonZeroFiguresGeneric)
        assert(result(0)._1.toSet == Set(1, 2, 9, 10))
        assert(result(0)._2 == StrategyStatistics(1, 1, 0, 0, 0, 0))
        assert(result(1)._1.toSet == Set(1, 2, 3, 10))
        assert(result(1)._2 == StrategyStatistics(0, 2, 0, 0, 0, 0))
    }

    def calcFor(point: Array[Int]) = test("test " + point.mkString("(", ", ", ")"))
    {
        val (sf, ef, p1, p2, p3, p4, p5, pw, sw, fw) = (point(0), point(1), point(2), point(3), point(4), point(5),
            point(6), point(7), point(8), point(9))
        val strategy = new Strategy5(RunResults.runResults, 6, sf, ef) with ProbabalisticIntervalsMetrics
        {
            override val probabilities: Array[Double] = Array(p1 / 10.0, p2 / 10.0, p3 / 10.0, p4 / 10.0, p5 / 10.0)
        }
        val strategyResult = strategy.apply(pw, sw, fw)(strategy.probabalisticIntervalFigures)(strategy.getCombinedBetIntersectionStatistics)
        println(StrategyStatistics.aggregateStatistics(strategyResult))
    }

    calcFor(Array(4,34,10,10,7,10,10,13,6,15))

    calcFor(Array(5,35,8,10,9,8,9,10,3,12))

    calcFor(Array(5,36,6,9,10,10,10,54,28,54))

    calcFor(Array(17, 36, 10, 10, 10, 10, 10, 200, 200, 200))

    calcFor(Array(9, 25, 8, 10, 3, 9, 10, 42, 0, 157))

    calcFor(Array(11, 25, 10, 10, 3, 5, 10, 42, 0, 156))

    calcFor(Array(1, 36, 10, 9, 10, 6, 5, 16, 0, 23))
}
