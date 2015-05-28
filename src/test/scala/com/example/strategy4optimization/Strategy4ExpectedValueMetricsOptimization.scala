package com.example.strategy4optimization

import com.example.StrategyOptimizationBase
import com.example.loto.Strategy4
import com.example.loto.actors.{LogResults, LoggerActor}
import com.example.loto.metrics.ExpectedValueMetrics
import com.example.loto.model.{RunResults, RunResult}
import org.scalatest.FunSuite

class Strategy4ExpectedValueMetricsOptimization extends FunSuite with StrategyOptimizationBase
{
    test("optimize strategy4 topFromStatOneBet")
    {
        testStrategy4WithExceptNonPopularFiguresMetrics("optimize strategy4 topFromStatOneBet")(_.topFromStatOneBet(_))
    }

    test("optimize strategy4 middleFromStatOneBet")
    {
        testStrategy4WithExceptNonPopularFiguresMetrics("optimize strategy4 middleFromStatOneBet")(_.middleFromStatOneBet(_))
    }

    def testStrategy4WithExceptNonPopularFiguresMetrics(description: String)(
        metric: (Strategy4 with ExpectedValueMetrics, Array[RunResult]) => Array[Int]) =
    {
        LoggerActor.ref ! LogResults(Seq(description))
        var result = List[((Int, Int, Int), Statistics)]()
        val rrs = RunResults.runResults
        for(betSize <- 6 to 6; startFigure <- 1 to 6; endFigure <- 30 to 36)
        {
            println((betSize, startFigure, endFigure))
            val strategy = new Strategy4(rrs, betSize, 1, 36) with ExpectedValueMetrics
            {
                override val betCount = -1 //пока не используется
            }
            def test = testStrategy[Array[RunResult], Strategy4 with ExpectedValueMetrics](strategy, 25)(metric) _
            result = accumulateResult((betSize, startFigure, endFigure), result, test(strategy.getIntersectionStatistics))
        }
        result foreach println
        LoggerActor.ref ! LogResults(result.map(_.toString))
    }
}
