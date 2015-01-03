package com.example

import com.example.loto.MetricsTypes
import com.example.loto.model.RunResult

class MetricsTypesTest extends TestBase
{
    val input: Vector[RunResult] = Vector(
        (1, 3, 4, 2, 5),
        (4, 2, 3, 5, 1),
        (2, 3, 4, 5, 9),
        (3, 4, 5, 10, 11),
        (4, 5, 12, 13, 14),
        (5, 15, 16, 17, 18)
    )

    test("topFiguresExceptSome take 5")
    {
        val metrics = new MetricsTypes
        {
            override val topFiguresCount: Int = 5
            override val startFigure = 1
            override val endFigure = 36
        }
        val result = metrics.topNonZeroFiguresExceptSome(input, Array(1))
        assert(List(5, 4, 3, 2, 12) === result.toList)
    }

    test("topFiguresExceptSome take 4")
    {
        val metrics = new MetricsTypes
        {
            override val topFiguresCount: Int = 4
            override val startFigure = 1
            override val endFigure = 36
        }
        val result = metrics.topNonZeroFiguresExceptSome(input, Array(1))
        assert(List(5, 4, 3, 2) === result.toList)
    }

    test("test topNonZeroFiguresGeneric")
    {
        val metrics = new MetricsTypes
        {
            override val topFiguresCount: Int = 5
            override val startFigure = 1
            override val endFigure = 36
        }
        val result = metrics.topNonZeroFiguresGeneric(input)
        assert(List(5, 4, 3, 2, 1) === result.toList)
    }

    test("test topNonZeroFiguresGeneric with interval")
    {
        val metrics = new MetricsTypes
        {
            override val topFiguresCount: Int = 3
            override val startFigure = 2
            override val endFigure = 4
        }
        val result = metrics.topNonZeroFiguresGeneric(input)
        assert(List(4, 3, 2) === result.toList)
    }

    test("test topNonZeroFiguresGeneric1")
    {
        val metrics = new MetricsTypes
        {
            override val topFiguresCount: Int = 5
            override val startFigure = 1
            override val endFigure = 36
        }
        val result = metrics.topNonZeroFiguresGeneric1(input)
        assert(List(5, 4, 3, 2, 1) === result.toList)
    }

    test("test topNonZeroFiguresGeneric1 with interval")
    {
        val metrics = new MetricsTypes
        {
            override val topFiguresCount: Int = 3
            override val startFigure = 2
            override val endFigure = 4
        }
        val result = metrics.topNonZeroFiguresGeneric1(input)
        assert(List(4, 3, 2) === result.toList)
    }
}
