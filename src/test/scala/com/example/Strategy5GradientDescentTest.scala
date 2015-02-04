package com.example

import com.example.loto.metrics.{StrategyStatistics, ProbabalisticIntervalsMetrics}
import com.example.loto.Strategy5
import com.example.loto.model.RunResults
import com.example.loto.optimization.{StrategyPartialOptimization, Strategy5GradientDescent}
import org.scalatest.FunSuite

/**
 * Created by alespuh on 16.01.15.
 */
class Strategy5GradientDescentTest extends FunSuite
{
    test("test strategy5 probabilistic intervals")
    {
        val descent = new Strategy5GradientDescent((1, 17, 0.2, 0.2, 0.2, 0.2, 0.2, 1, 0, 1), (16, 36, 1.0, 1.0, 1.0, 1.0, 1.0, 100, 100, 100))(
        { case (startFigure, endFigure, p1, p2, p3, p4, p5, pw, sw, fw) =>
            val strategy = new Strategy5(RunResults.runResults, 6, startFigure, endFigure) with ProbabalisticIntervalsMetrics
            {
                override val probabilities: Array[Double] = Array(p1, p2, p3, p4, p5)
            }
            val strategyResult = strategy.apply(pw, sw, fw)(strategy.probabalisticIntervalFigures)(strategy.getCombinedBetIntersectionStatistics)
            StrategyStatistics.aggregateStatistics(strategyResult)
        })
        println(descent.optimize((2, 36, 0.4, 0.4, 0.4, 0.4, 0.4, 50, 25, 50))(1000))
    }
}
