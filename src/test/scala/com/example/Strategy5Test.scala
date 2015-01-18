package com.example

import com.example.loto.{ProbabalisticIntervalsMetrics, Strategy5}
import com.example.loto.model.RunResults

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
        assert(result(0)._2 == (1, 1, 0, 0, 0, 0))
        assert(result(1)._1.toSet == Set(1, 2, 3, 10))
        assert(result(1)._2 == (0, 2, 0, 0, 0, 0))
    }

    test("test (4,34,1.0,1.0,0.7,1.0,1.0,13,6,15)")
    {
        val strategy = new Strategy5(RunResults.runResults, 6, 4, 34) with ProbabalisticIntervalsMetrics
        {
            override val probabilities: Array[Double] = Array(1.0, 1.0, 0.7, 1.0, 1.0)
        }
        val strategyResult = strategy.apply(13, 6, 15)(strategy.probabalisticIntervalFigures)(strategy.getCombinedBetIntersectionStatistics)
        val (hit2, hit3, hit4, hit5, mplus, mminus) = strategyResult.foldLeft(0, 0, 0, 0, 0, 0)
        { case ((ah2, ah3, ah4, ah5, amp, amm), (_, (h2, h3, h4, h5, mp, mm))) =>
            (ah2 + h2, ah3 + h3, ah4 + h4, ah5 + h5, amp + mp, amm + mm)
        }
        println((hit2, hit3, hit4, hit5, mplus, mminus))
    }

    test("test (5,35,0.8,1.0,0.9,0.8,0.9,10,3,12)")
    {
        val strategy = new Strategy5(RunResults.runResults, 6, 5, 35) with ProbabalisticIntervalsMetrics
        {
            override val probabilities: Array[Double] = Array(0.8, 1.0, 0.9, 0.8, 0.9)
        }
        val strategyResult = strategy.apply(10, 3, 12)(strategy.probabalisticIntervalFigures)(strategy.getCombinedBetIntersectionStatistics)
        val (hit2, hit3, hit4, hit5, mplus, mminus) = strategyResult.foldLeft(0, 0, 0, 0, 0, 0)
        { case ((ah2, ah3, ah4, ah5, amp, amm), (_, (h2, h3, h4, h5, mp, mm))) =>
            (ah2 + h2, ah3 + h3, ah4 + h4, ah5 + h5, amp + mp, amm + mm)
        }
        println((hit2, hit3, hit4, hit5, mplus, mminus))
    }

    test("test (5,36,0.6,0.9,1.0,1.0,1.0,54,28,54)")
    {
        val strategy = new Strategy5(RunResults.runResults, 6, 5, 36) with ProbabalisticIntervalsMetrics
        {
            override val probabilities: Array[Double] = Array(0.6, 0.9, 1.0, 1.0, 1.0)
        }
        val strategyResult = strategy.apply(54, 28, 54)(strategy.probabalisticIntervalFigures)(strategy.getCombinedBetIntersectionStatistics)
        val (hit2, hit3, hit4, hit5, mplus, mminus) = strategyResult.foldLeft(0, 0, 0, 0, 0, 0)
        { case ((ah2, ah3, ah4, ah5, amp, amm), (_, (h2, h3, h4, h5, mp, mm))) =>
            (ah2 + h2, ah3 + h3, ah4 + h4, ah5 + h5, amp + mp, amm + mm)
        }
        println((hit2, hit3, hit4, hit5, mplus, mminus))
    }
}
