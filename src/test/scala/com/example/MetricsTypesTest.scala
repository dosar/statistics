package com.example

import com.example.loto.MetricsTypes
import com.example.loto.betgenerator.FromMiddleOccurenciesBetGenerator
import com.example.loto.model.RunResult

class MetricsTypesTest extends TestBase
{
    val input: Array[RunResult] = Array(
        (1, 3, 4, 2, 5),
        (4, 2, 3, 5, 1),
        (2, 3, 4, 5, 9),
        (3, 4, 5, 10, 11),
        (4, 5, 12, 13, 14),
        (5, 15, 16, 17, 18)
    )

    val fromMiddleOccurenciesInput = Array(
        null,
        Array(2, 19, 21, 0, 0, 0),
        Array(3, 9, 12, 26, 0, 0),
        Array(3, 3, 7, 23),
        Array(13, 4, 8, 11, 14, 20, 22, 25, 27, 29, 30, 31, 35, 36),
        Array(4, 1, 18, 24, 28),
        Array(3, 16, 32, 33),
        Array(5, 5, 6, 10, 13, 17),
        Array(1, 34),
        Array(1, 15),
        null,
        Array(1, 2)
    )

    test("backFigureOccurencies")
    {
        val metrics = new MetricsTypes
        {
            override val betSizeLimit: Int = 5
            override val startFigure = 1
            override val endFigure = 36
        }
        val result = metrics.backFigureOccurencies(Array((1, 2, 3, 4, 5), (2, 3, 4, 5, 6), (2, 3, 4, 5, 12), (7, 8, 9, 10, 11), (32, 33, 34, 35, 36)))

        def checkInnerArray(ind: Int, size: Int, expected: Seq[Int]) =
        {
            assert(size === result(ind)(0))
            assert(expected.toSet === result(ind).drop(1).take(result(ind)(0)).toSet)
        }

        checkInnerArray(0, 19, 13 to 31)
        checkInnerArray(1, 13, Seq(1, 6, 7, 8, 9, 10, 11, 12, 32, 33, 34, 35, 36))
        assertResult(null)(result(2))
        checkInnerArray(3, 4, Seq(2, 3, 4, 5))
    }

    test("fromMiddleOccurencies take 5 start 1 end 36")
    {
        def fromMiddleOccurencies(x: Array[Array[Int]]) = new FromMiddleOccurenciesBetGenerator(x, 5, 1, 36).generate()

        assert(fromMiddleOccurencies(fromMiddleOccurenciesInput) === Array(16, 32, 33, 5, 1))
    }

    test("fromMiddleOccurencies take 5 start 17 end 36")
    {
        def fromMiddleOccurencies(x: Array[Array[Int]]) = new FromMiddleOccurenciesBetGenerator(x, 5, 17, 36).generate()

        assert(fromMiddleOccurencies(fromMiddleOccurenciesInput) === Array(32, 33, 17, 18, 34))
    }

    test("fromMiddleOccurencies take 5 start 1 end 17")
    {
        def fromMiddleOccurencies(x: Array[Array[Int]]) = new FromMiddleOccurenciesBetGenerator(x, 5, 1, 17).generate()

        assert(fromMiddleOccurencies(fromMiddleOccurenciesInput) === Array(16, 5, 1, 6, 4))
    }

    test("topFiguresExceptSome take 5")
    {
        val metrics = new MetricsTypes
        {
            override val betSizeLimit: Int = 5
            override val startFigure = 1
            override val endFigure = 36
        }
        val result = metrics.topNonZeroFiguresExceptSome(input, Array(1, 0))
        assert(List(5, 4, 3, 2, 12) === result.toList)
    }

    test("topFiguresExceptSome take 4")
    {
        val metrics = new MetricsTypes
        {
            override val betSizeLimit: Int = 4
            override val startFigure = 1
            override val endFigure = 36
        }
        val result = metrics.topNonZeroFiguresExceptSome(input, Array(1, 0))
        assert(List(5, 4, 3, 2) === result.toList)
    }

    test("test topNonZeroFiguresGeneric")
    {
        val metrics = new MetricsTypes
        {
            override val betSizeLimit: Int = 5
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
            override val betSizeLimit: Int = 3
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
            override val betSizeLimit: Int = 5
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
            override val betSizeLimit: Int = 3
            override val startFigure = 2
            override val endFigure = 4
        }
        val result = metrics.topNonZeroFiguresGeneric1(input)
        assert(List(4, 3, 2) === result.toList)
    }

    test("test trueMiddleOccurenciesFigures take 3 start 1 end 20")
    {
        val metrics = new MetricsTypes
        {
            override val betSizeLimit: Int = 3
            override val startFigure = 1
            override val endFigure = 20
        }
        val result = metrics.trueMiddleOccurencyFigures(input)
        assertResult(List(16, 14, 15))(result.toList)
    }
}
