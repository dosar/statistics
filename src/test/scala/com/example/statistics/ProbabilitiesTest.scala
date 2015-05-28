package com.example.statistics

import com.example.loto.array.ArrayPerformanceUtil
import com.example.loto.metrics.Metrics
import com.example.loto.model.{RunResults, RunResult}
import com.example.loto.sorter.{PairArrayQuickSorter, FiguresByHitSorter}
import org.scalatest.FunSuite

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Created by alespuh on 07.02.15.
 */
class ProbabilitiesTest extends FunSuite
{
    type Figure = Int; type Position = Int; type Probability = Double

    //    List(36, 30, 35, 27, 4, 15, 6, 26, 33, 10, 32, 1, 22, 19, 28, 2, 20, 18, 3, 8, 7, 16, 31, 24, 23, 5, 25, 13, 17, 21, 34, 29, 14, 12, 11, 9)
    test("topFigures on all data")
    {
        val metrics = new Metrics{ override val betSizeLimit = 6 }
        println(FiguresByHitSorter.topFigures(metrics.figuresOccurencies(RunResults.runResults, 1, 36)).toList)
    }

    //    List(36, 30, 35, 27, 4, 15, 6, 26, 33, 10, 32, 1, 22, 19, 28, 2, 20, 18, 3, 8, 7, 16, 31, 24, 23, 5, 25, 13, 17, 21, 34, 29, 14, 12, 11, 9)
    //    List(36, 30, 35, 4, 27, 15, 6, 26, 33, 32, 10, 19, 22, 1, 28, 2, 20, 18, 3, 31, 8, 7, 16, 24, 23, 5, 25, 13, 17, 21, 34, 14, 29, 12, 11, 9, 0)
    test("topFigures on all data test")
    {
        val metrics = new Metrics{ override val betSizeLimit = 6 }
        val figureHits = metrics.figuresOccurencies(RunResults.runResults, 1, 36)
        val figures = ArrayPerformanceUtil.createArray(figureHits.length)(i => i)
        PairArrayQuickSorter.quicksort(figureHits, figures)
        println(figures.toList)
    }

    test("pair occurencies on all data")
    {
        val map = mutable.Map[(Int, Int), Int]() withDefaultValue(0)
        for (rr <- RunResults.runResults)
        {
            for(i <- 0 until rr.result.length - 1; j <- i + 1 until rr.result.length)
                map((rr.result(i), rr.result(j))) += 1
        }
        map.toVector.sortBy(_._2) foreach println
    }


    test("считаем вероятности выпадения числа в определенное место")
    {
        val result = mutable.Map[(Figure, Position), Int]() withDefaultValue 0
        for(rr <- RunResults.runResults; (figure, ind) <- rr.result.zipWithIndex)
            result((figure, ind + 1)) += 1
        result.toList.sortBy(_._2) foreach println
    }

    test("теоретические вероятности выпадения числа в определенное место")
    {
        val result = for(figure <- 1 to 36; position <- 1 to 5) yield
        {
            ((figure, position), cnk(figure - 1, position - 1) * cnk(36 - figure, 5 - position) / cnk(36, 5))
        }
        result.groupBy{ case((fig, pos), p) => pos}.toList.sortBy{case (pos, _) => pos}
            .flatMap{case (_, groupedList) => groupedList.sortBy{case ((fig, pos), p) => p}} foreach println
        println("")
        println("сортировка по числу-позиции")
        println("")
        result foreach println
    }

    test("plot data figures in position 1")
    {
        val result = for(figure <- 1 to 36; position <- 1 to 5) yield
        {
            ((figure, position), cnk(figure - 1, position - 1) * cnk(36 - figure, 5 - position) / cnk(36, 5))
        }

        def byPosition(position: Int) =
        {
            result.filter{ case((fig, pos), p) => pos == position}
                .toList.sortBy{case ((fig, pos), _) => fig}
                .map{case ((fig, pos), prob) => (fig, prob)}.unzip
        }

        def printPosition(posColor: (Int, String)) =
        {
            val (position, color) = posColor
            val (xs, ys) = byPosition(position)
            xs.mkString("[", ", ", "]") + ", " + ys.mkString("[", ", ", "]") + ", \"" + color + "\""
        }
        println{List((1, "k"), (2, "r"), (3, "g"), (4, "b"), (5, "m")).map(printPosition).mkString("plot(" , ", ", ")")}

        val expectedValues = 1 to 5 map
        { pos =>
            val (figs, probs) = byPosition(pos)
            figs.zip(probs).map{ case(fig, prob) => fig * prob }.sum
        }

        val dispersions = 1 to 5 map
        { pos =>
            val (figs, probs) = byPosition(pos)
            val expected = expectedValues(pos - 1)
            figs.zip(probs).map{ case (fig, prob) => (fig - expected).abs * prob }.sum
        }

        println("expected")
        expectedValues foreach println
        println("variance")
        dispersions foreach println
    }

    test("были ли повторные комбинации")//нет
    {
        println(RunResults.runResults.map(_.result.toList).toList.groupBy(x => x).find(_._2.size > 1))
    }

    test("factorial")
    {
        assertResult(1)(factorial(0))
        assertResult(1)(factorial(1))
        assertResult(2)(factorial(2))
        assertResult(6)(factorial(3))
        assertResult(24)(factorial(4))
        assertResult(120)(factorial(5))
    }

    test("cnk")
    {
        assertResult(0)(cnk(0, 1))
        assertResult(1)(cnk(0, 0))
        assertResult(1)(cnk(1, 1))
        assertResult(1)(cnk(36, 36))
        assertResult(36)(cnk(36, 1))
        assertResult(3)(cnk(3, 2))
        assertResult(6)(cnk(6, 5))
    }

    def factorial(n: Int): BigDecimal =
    {
        if(n == 0 || n == 1) 1
        else n * factorial(n - 1)
    }

    def cnk(n: Int, k: Int): BigDecimal =
    {
        if(k < 0 || n < 0) throw new Error(s"k=$k or n=$n < 0")
        if(k > n)
            0
        else
            factorial(n) / (factorial(k) * factorial(n-k))
    }
}
