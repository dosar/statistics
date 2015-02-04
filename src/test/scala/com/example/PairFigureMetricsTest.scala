package com.example

import com.example.loto.metrics.{MetricsTypes, PairFigureMetrics}
import com.example.loto.model.RunResult

/**
 * Created by alespuh on 09.01.15.
 */
class PairFigureMetricsTest extends TestBase
{
    val input: Array[RunResult] = Array(
        (1, 3, 4, 2, 5),
        (4, 2, 3, 5, 1),
        (2, 3, 4, 5, 9),
        (3, 4, 5, 10, 11),
        (4, 5, 12, 13, 14),
        (5, 15, 16, 17, 18)
    )

    test("test topFigureCombinedWithPair take 6 start 1 end 36")
    {
        val metrics = new PairFigureMetrics with MetricsTypes
        {
            override val betSizeLimit: Int = 6
            override val startFigure = 1
            override val endFigure = 36
        }

        assertResult(Array(5, 4, 3, 2, 1, 9))(metrics.topFigureCombinedWithPair(input))
    }

    test("test pairFiguresOccurencies start 1 end 36")
    {
        val metrics = new PairFigureMetrics with MetricsTypes
        {
            override val betSizeLimit: Int = 6
            override val startFigure = 1
            override val endFigure = 36
        }

        val expectedReadable = Array(
            (1, 2) -> 2, (1, 3) -> 2, (1, 4) -> 2, (1, 5) -> 2,
            (2, 3) -> 3, (2, 4) -> 3, (2, 5) -> 3, (2, 9) -> 1,
            (3, 4) -> 4, (3, 5) -> 4, (3, 9) -> 1, (3, 10) -> 1, (3, 11) -> 1,
            (4, 5) -> 5, (4, 9) -> 1, (4, 10) -> 1, (4, 11) -> 1, (4, 12) -> 1, (4, 13) -> 1, (4, 14) -> 1,
            (5, 9) -> 1, (5, 10) -> 1, (5, 11) -> 1, (5, 12) -> 1, (5, 13) -> 1,
            (5, 14) -> 1, (5, 15) -> 1, (5, 16) -> 1, (5, 17) -> 1, (5, 18) -> 1,
            (10, 11) -> 1,
            (12, 13) -> 1, (12, 14) -> 1,
            (13, 14) -> 1,
            (15, 16) -> 1, (15, 17) -> 1, (15, 18) -> 1,
            (16, 17) -> 1, (16, 18) -> 1,
            (17, 18) -> 1
        )

        val expected = new Array[Int](36 * 36)
        expectedReadable.foreach{ case ((fig1, fig2), hit) => expected((fig1 - 1) * 36 + fig2 - 1) = hit }

        val result = metrics.pairFiguresOccurencies(input.map(rr => rr.copy(result = rr.result.sorted)))
        assert(expected.length === result.length)
        for(i <- 0 until expected.length)
            assertResult(expected(i), "(index = " + i + ")")(result(i))
    }

    test("test pairFiguresOccurencies start 3 end 16")
    {
        val metrics = new PairFigureMetrics with MetricsTypes
        {
            override val betSizeLimit: Int = 6
            override val startFigure = 3
            override val endFigure = 16
        }

        val expectedReadable = Array(
            (3, 4) -> 4, (3, 5) -> 4, (3, 9) -> 1, (3, 10) -> 1, (3, 11) -> 1,
            (4, 5) -> 5, (4, 9) -> 1, (4, 10) -> 1, (4, 11) -> 1, (4, 12) -> 1, (4, 13) -> 1, (4, 14) -> 1,
            (5, 9) -> 1, (5, 10) -> 1, (5, 11) -> 1, (5, 12) -> 1, (5, 13) -> 1,
            (5, 14) -> 1, (5, 15) -> 1, (5, 16) -> 1,
            (10, 11) -> 1,
            (12, 13) -> 1, (12, 14) -> 1,
            (13, 14) -> 1,
            (15, 16) -> 1
        )

        val expected = Array.fill(36 * 36)(-1)
        (metrics.startFigure to metrics.endFigure).foreach
        { fig1 =>
            (metrics.startFigure to metrics.endFigure).foreach(fig2 => expected((fig1 - 1) * 36 + fig2 - 1) = 0)
        }
        expectedReadable.foreach{ case ((fig1, fig2), hit) => expected((fig1 - 1) * 36 + fig2 - 1) = hit }
        val result = metrics.pairFiguresOccurencies(input.map(rr => rr.copy(result = rr.result.sorted)))
        assert(expected.length === result.length)
        for(i <- 0 until expected.length)
            assertResult(expected(i), "(index = " + i + ")")(result(i))
    }

    test("test topPairFigures bet 4 start 1 end 36")
    {
        val metrics = new PairFigureMetrics with MetricsTypes
        {
            override val betSizeLimit: Int = 4
            override val startFigure = 1
            override val endFigure = 36
        }

        assertResult(Array(4, 5, 3, 2))(metrics.topPairFigures(input))
    }

    test("test topPairFigures bet 4 start 3 end 9")
    {
        val metrics = new PairFigureMetrics with MetricsTypes
        {
            override val betSizeLimit: Int = 4
            override val startFigure = 3
            override val endFigure = 9
        }

        assertResult(Array(4, 5, 3, 9))(metrics.topPairFigures(input))
    }
}
