package com.example

import com.example.loto.Strategy4

class Strategy4Test extends TestBase
{
    test("strategy4 test apply")
    {
        val strategy = new Strategy4(Array(
            (1, 2, 3, 4, 5),
            (1, 2, 3, 4, 5),
            (1, 4, 5, 6, 7),
            (1, 4, 5, 6, 7),
            (1, 2, 3, 4, 5)
        ), 5, 1, 36)
        val applyResult = strategy.apply(2, 0, 2)(strategy.topNonZeroFiguresGeneric)
        assertResult(Set((Set(1, 2, 3, 4, 5), (0, 2, 0, 0, 600, 60)), (Set(1, 4, 5, 6, 7), (0, 1, 0, 0, 300, 30))))(
            applyResult.map{ case(figures, statistics) => (figures.toSet, statistics) }.toSet)
    }

    test("strategy 4 intersections with statistics with 5 hit and zero minus")
    {
        val strategy = new Strategy4(Array())
        val (statistics, _) = strategy.getIntersectionStatistics(Array(
            (1, 2, 8, 9, 10),
            (1, 2, 3, 9, 10),
            (1, 2, 3, 9, 10),
            (1, 2, 3, 4, 10),
            (1, 2, 3, 4, 5),
            (11, 15, 8, 9, 10)
        ), Array(1, 2, 3, 4, 5))
        assert(1 === statistics._1) // посчитали хиты на 2 числа
        assert(2 === statistics._2) // посчитали хиты на 3 числа
        assert(1 === statistics._3) // посчитали хиты на 4 числа
        assert(1 === statistics._4) // посчитали хиты на 5 чисел
        assert((30 + 600 + 3000 + 1000000) === statistics._5) // посчитали на сколько в плюс ушли
        assert(150 === statistics._6) // посчитали на сколько в минус ушли
    }

    test("strategy 4 intersections with statistics with 5 hit and nonzero minus")
    {
        val strategy = new Strategy4(Array())
        val (statistics, _) = strategy.getIntersectionStatistics(Array(
            (1, 2, 8, 9, 10),
            (1, 2, 3, 9, 10),
            (1, 2, 3, 9, 10),
            (1, 2, 3, 4, 10),
            (11, 15, 8, 9, 10),
            (1, 2, 3, 4, 5)
        ), Array(1, 2, 3, 4, 5))
        assert(1 === statistics._1) // посчитали хиты на 2 числа
        assert(2 === statistics._2) // посчитали хиты на 3 числа
        assert(1 === statistics._3) // посчитали хиты на 4 числа
        assert(1 === statistics._4) // посчитали хиты на 5 чисел
        assert((30 + 600 + 3000 + 1000000) === statistics._5) // посчитали на сколько в плюс ушли
        assert(180 === statistics._6) // посчитали на сколько в минус ушли
    }
}
