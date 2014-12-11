package com.example

import com.example.loto.Strategy4

/**
 * Created by alespuh on 09.12.14.
 */
class Strategy4Test extends TestBase
{
    test("strategy 4 intersections with statistics with 5 hit and zero minus")
    {
        val strategy = new Strategy4(Vector())
        val (statistics, _) = strategy.getIntersectionStatistics(Vector(
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
        assert((30 + 600 + 3000 + 300000) === statistics._5) // посчитали на сколько в плюс ушли
        assert(0 === statistics._6) // посчитали на сколько в минус ушли
    }

    test("strategy 4 intersections with statistics with 5 hit and nonzero minus")
    {
        val strategy = new Strategy4(Vector())
        val (statistics, _) = strategy.getIntersectionStatistics(Vector(
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
        assert((30 + 600 + 3000 + 300000) === statistics._5) // посчитали на сколько в плюс ушли
        assert(30 === statistics._6) // посчитали на сколько в минус ушли
    }
}