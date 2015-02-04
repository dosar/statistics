package com.example

import com.example.loto.metrics.{StrategyStatistics, ProbabalisticIntervalsMetrics}
import com.example.loto.{Strategy4, Strategy5}
import com.example.loto.model.RunResults
import com.example.loto.optimization.StrategyPartialOptimization

/**
 * Created by alespuh on 18.01.15.
 */
class StrategyPartialOptimizationTest extends TestBase
{
    implicit def toArray(range: Range) = range.toArray

    test("optimize strategy4 middle occurencies figures")
    {
        while(true)
        {
            val data = Array[(Int, Array[Int])](
                (10, 1 to 200), //pw
                (0,  0 to 200), //sw
                (10, 1 to 200), //fw
                (1,  1 to 17),  //sf
                (1,  36 to 36)) //ef
            var (startPoint, ranges) = data.unzip
            val optimizer = new StrategyPartialOptimization(startPoint, ranges)(
            { arr =>
                val (pw, sw, fw, sf, ef) = (arr(0), arr(1), arr(2), arr(3), arr(4))
                val strategy = new Strategy4(RunResults.runResults, 6, sf, ef)
                val strategyResult = strategy.apply(pw, sw, fw)(strategy.middleOccurencyFigures)(strategy.getIntersectionStatistics)
                StrategyStatistics.aggregateStatistics(strategyResult)
            })
            val (array, statistics) = optimizer.optimize
            println((array.toList, statistics))
            startPoint = array
        }
    }


    test("optimize strategy5 probabilistic intervals")
    {
        //sf, ef, p1, p2, p3, p4, p5, pw, sw, fw - features in array
        while(true)
        {
            val data = Array[(Int, Array[Int])](
                (10, 1 to 200), //pw
                (0,  0 to 200), //sw
                (10, 1 to 200), //fw
                (10, 2 to 10),  //p1
                (10, 2 to 10),  //p2
                (10, 2 to 10),  //p3
                (10, 2 to 10),  //p4
                (10, 2 to 10),  //p5
                (1,  1 to 1),   //sf
                (1,  36 to 36)) //ef
            var (startPoint, ranges) = data.unzip
            val optimizer = new StrategyPartialOptimization(startPoint, ranges)(
            { arr =>
                val (pw, sw, fw, p1, p2, p3, p4, p5, sf, ef) = (arr(0), arr(1), arr(2), arr(3), arr(4), arr(5), arr(6), arr(7), arr(8), arr(9))
                val strategy = new Strategy5(RunResults.runResults, 6, sf, ef) with ProbabalisticIntervalsMetrics
                {
                    override val probabilities: Array[Double] = Array(p1, p2, p3, p4, p5).map(_ / 10.0)
                }
                val strategyResult = strategy.apply(pw, sw, fw)(strategy.probabalisticIntervalFigures)(strategy.getCombinedBetIntersectionStatistics)
                StrategyStatistics.aggregateStatistics(strategyResult)
            })
            val (array, statistics) = optimizer.optimize
            println((array.toList, statistics))
            startPoint = array
        }
    }
}