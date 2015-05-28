package com.example.strategy4optimization

import com.example.StrategyOptimizationBase
import com.example.loto.Strategy4
import com.example.loto.metrics.{DistinctBetFigureMetrics, ComplexBetFigureMetrics}
import com.example.loto.model.{RunResult, RunResults}
import org.scalatest.FunSuite

/**
 * Created by alespuh on 31.01.15.
 */
class Strategy4DistinctBetMetricsOptimization extends FunSuite with StrategyOptimizationBase
{
    test("all5WithFixedBet")
    {
        testStrategy4ComplexBetMetrics(_.all5WithFixedBet(_))
    }

    def testStrategy4ComplexBetMetrics(
        metric: (Strategy4 with DistinctBetFigureMetrics, Array[RunResult]) => Array[Int]) =
    {
        var result = List[((Int, Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 4; endFigure <- 33 to 36)
        {
            println((betSize, startFigure, endFigure))
            val strategy = new Strategy4(RunResults.runResults, betSize, startFigure, endFigure) with DistinctBetFigureMetrics
            def test = testStrategy[Array[RunResult], Strategy4 with DistinctBetFigureMetrics](strategy, 25) _
            result = accumulateResult((betSize, startFigure, endFigure), result, test(metric)(strategy.getCombinedBetIntersectionStatistics))
        }
        result foreach println
    }
}
