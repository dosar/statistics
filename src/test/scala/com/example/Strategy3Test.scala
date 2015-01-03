package com.example

import com.example.loto.Strategy3

class Strategy3Test extends TestBase
{
    test("intersections")
    {
        val (maxIntersection, count, indexIncrement) = new Strategy3(Vector.empty).getIntersections(Vector(
            (1, 2, 3, 4, 6),
            (1, 2, 3, 4, 5),
            (1, 3, 4, 7, 8)
        ), Array(1, 2, 3, 4, 5))
        assert(maxIntersection === 5)
        assert(count === 1)
        assert(indexIncrement === 2)
    }

    test("strategy3 3, 0, 3")
    {
        val metrics = new Strategy3(Vector(
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
        checkArr(metrics.withTopNonZeroFigures(3, 0, 3), Seq((Array(1, 2, 3, 4, 5), (5, 1)), (Array(1, 2, 3, 4, 5), (2, 1))))
    }
}