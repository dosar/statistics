package com.example

import com.example.loto.metrics.{Metrics, ProbabalisticIntervalsMetrics}
import com.example.loto.model.RunResult

/**
 * Created by alespuh on 10.01.15.
 */
class ProbabalisticIntervalsMetricsTest extends TestBase
{
    val input: Array[RunResult] = Array(
        ( 9, 14, 27, 28, 36),
        ( 2, 12, 16, 21, 31),
        ( 5, 10, 14, 31, 35),
        ( 1, 11, 13, 20, 28),
        (11, 13, 16, 18, 24),
        ( 3,  4,  9, 11, 20),
        ( 2, 10, 11, 20, 25),
        (10, 16, 20, 28, 33),
        (11, 14, 17, 25, 30),
        ( 2,  6, 11, 14, 18),
        ( 5,  6, 11, 18, 33),
        ( 3, 15, 24, 30, 36),
        ( 2,  6,  8, 25, 36)
    )

    test("probabalisticIntervalFigures betSize 6, start 1, end 36")
    {
        val metrics = new Metrics with ProbabalisticIntervalsMetrics
        {
            override val betSizeLimit: Int = 6
            override val startFigure = 1
            override val endFigure = 36
            override val probabilities = Array(1.0, 1.0, 1.0, 1.0, 1.0)
        }

        val bet = metrics.probabalisticIntervalFigures(input)
        assertResult(null)(metrics.getCombinedBetIntersectionStatistics(input, bet))
        assertResult(Array(3, 10, 16, 18, 32))(bet)
    }
}
