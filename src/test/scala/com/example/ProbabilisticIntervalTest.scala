package com.example

import com.example.loto.metrics.MetricsTypes

class ProbabilisticIntervalTest extends TestBase
{
    test("trustInterval")
    {
        val metrics = new MetricsTypes{override val betSizeLimit = 0}
        val (actualMin, actualMax) = metrics.figureIntervals(Array((1, 2, 3, 4, 5), (5, 7, 10, 11, 15), (3, 5, 9, 12, 20)))
        assert((Array(1, 2, 3, 4, 5).toSet, Array(5, 7, 10, 12, 20).toSet) === (actualMin.toSet, actualMax.toSet))
    }
}