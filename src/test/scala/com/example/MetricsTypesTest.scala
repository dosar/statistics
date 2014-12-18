package com.example

import com.example.loto.MetricsTypes

class MetricsTypesTest extends TestBase
{
    test("test topNonZeroFiguresGeneric")
    {
        val metrics = new MetricsTypes
        {
            override val topFiguresCount: Int = 5
            override val startFigure = 1
            override val endFigure = 36
        }
        val result = metrics.topNonZeroFiguresGeneric(Vector(
            (1, 3, 4, 2, 5),
            (4, 2, 3, 5, 1),
            (2, 3, 4, 5, 9),
            (3, 4, 5, 10, 11),
            (4, 5, 12, 13, 14),
            (5, 15, 16, 17, 18)
        ), 1, 36)
        assert(List(5, 4, 3, 2, 1) === result.toList)
    }

    test("test topNonZeroFiguresGeneric with interval")
    {
        val metrics = new MetricsTypes { override val topFiguresCount: Int = 3}
        val result = metrics.topNonZeroFiguresGeneric(Vector(
            (1, 3, 4, 2, 5),
            (4, 2, 3, 5, 1),
            (2, 3, 4, 5, 9),
            (3, 4, 5, 10, 11),
            (4, 5, 12, 13, 14),
            (5, 15, 16, 17, 18)
        ), 2, 4)
        assert(List(4, 3, 2) === result.toList)
    }
}
