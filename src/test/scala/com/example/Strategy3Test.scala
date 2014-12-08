package com.example

import com.example.loto.Metrics

class Strategy3Test extends TestBase
{
    test("strategy3 3, 0, 3")
    {
        val metrics: Metrics = new Metrics(Vector(
            (1, 2, 3, 4, 5),
            (6, 7, 8, 9, 10),
            (1, 2, 3, 4, 5),
            (6, 7, 8, 9, 10),
            (1, 2, 3, 4, 5),
            (12, 13, 14, 15, 16),
            (20, 13, 11, 9, 35),
            (1, 13, 2, 9, 35),
            (20, 13, 11, 9, 35)
        ), 5)
//        checkArr(metrics.strategy3(3, 0, 3)(metrics.topNonZeroFigures),
//            Seq((Array(1, 2, 3, 4, 5), (5, 1)), (Array(1, 2, 3, 4, 5), (2, 1))))
    }
}
