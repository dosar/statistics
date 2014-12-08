package com.example

import com.example.loto.Strategy1

class Strategy1Test extends TestBase
{
    test("strategy1 3, 0, 2")
    {
        val strategy = new Strategy1(Vector(
            (1, 2, 3, 4, 5),
            (6, 7, 8, 9, 10),
            (6, 7, 8, 9, 10),
            (12, 11, 1, 5, 3),
            (12, 13, 14, 15, 16),
            (20, 13, 11, 9, 35)
        ), 5)
        checkArr(strategy.withTopNonZeroFigures(3, 0, 2), Seq((Array(6, 7, 8, 9, 10), (0, 0))))
    }

    test("strategy1 2, 0, 2")
    {
        val strategy = new Strategy1(Vector(
            (6, 7, 8, 9, 10),
            (6, 7, 8, 9, 10),
            (1, 2, 3, 4, 5),
            (6, 7, 1, 5, 3),
            (12, 13, 14, 15, 16),
            (20, 13, 11, 9, 35)
        ), 5)
        checkArr(strategy.withTopNonZeroFigures(2, 0, 2), Seq((Array(6, 7, 8, 9, 10), (2, 1)), (Array(5, 1, 2, 3, 4), (0, 0))))
    }

    test("strategy1 2, 1, 2")
    {
        val strategy = new Strategy1(Vector(
            (6, 7, 8, 9, 10),
            (6, 7, 8, 9, 10),
            (1, 2, 3, 4, 5),
            (6, 7, 1, 5, 3),
            (6, 7, 1, 15, 16),
            (20, 13, 1, 5, 3),
            (12, 13, 14, 15, 16),
            (1, 2, 3, 4, 5)
        ), 5)
        checkArr(strategy.withTopNonZeroFigures(2, 1, 2), Seq((Array(6, 7, 8, 9, 10), (2, 2)), (Array(5, 1, 6, 7, 16), (2, 1))))
    }
}
