package com.example

import com.example.loto.Strategy5

/**
 * Created by alespuh on 15.12.14.
 */
class Strategy5Test extends TestBase
{
    test("strategy 5 test apply")
    {
        val strategy = new Strategy5(Vector(
            (1, 2, 8, 9, 10),
            (1, 2, 3, 9, 10),
            (1, 2, 3, 9, 10),
            (1, 2, 3, 4, 10),
            (11, 15, 8, 9, 10),
            (1, 2, 3, 4, 5),
            (1, 2, 3, 4, 5)
        ), topFiguresCount = 4, startFigure = 1)
        val result = strategy.apply(2, 1, 2)(strategy.topNonZeroFiguresGeneric)
        assert(result(0)._1.toSet == Set(1, 2, 9, 10))
        assert(result(0)._2 == (1, 1, 0, 0, 0, 0))
        assert(result(1)._1.toSet == Set(1, 2, 3, 10))
        assert(result(1)._2 == (0, 2, 0, 0, 0, 0))
    }
}
