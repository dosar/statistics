package com.example

import com.example.loto.SimpleParallelSort
import org.scalatest._

class SimpleParallelSortTest extends FunSuite
{
    test("sort 1 chunk")
    {
        val sorter = new SimpleParallelSort[(Int, Int, Int, Int), Int](1, 3, (0, 0, 0, 0))(x => x._4)
        sorter.update(0, (1, 2, 3, 1))
        sorter.update(0, (2, 2, 3, 1))
        sorter.update(0, (3, 2, 3, 1))
        sorter.update(0, (3, 2, 3, 1))
        assert(Array((1, 2, 3, 1), (2, 2, 3, 1), (3, 2, 3, 1)) === sorter.result)
    }

    test("sort 1 chunk replace")
    {
        val sorter = new SimpleParallelSort[(Int, Int, Int, Int), Int](1, 3, (0, 0, 0, 0))(x => x._4)
        sorter.update(0, (1, 2, 3, 1))
        sorter.update(0, (2, 2, 3, 1))
        sorter.update(0, (3, 2, 3, 1))
        sorter.update(0, (3, 2, 3, 2))
        assert(Array((3, 2, 3, 2), (2, 2, 3, 1), (3, 2, 3, 1)) === sorter.result)
    }

    test("sort 2 chunks and combine")
    {
        val sorter = new SimpleParallelSort[(Int, Int, Int, Int), Int](2, 3, (0, 0, 0, 0))(x => x._4)
        sorter.update(0, (1, 2, 3, 1))
        sorter.update(0, (2, 2, 3, 1))
        sorter.update(0, (3, 2, 3, 1))
        sorter.update(0, (3, 2, 3, 2))
        sorter.update(1, (1, 0, 3, 1))
        sorter.update(1, (2, 0, 3, 1))
        sorter.update(1, (3, 0, 3, 1))
        sorter.update(1, (3, 0, 3, 2))
        assert(Array((3, 2, 3, 2), (3, 0, 3, 2), (2, 2, 3, 1), (3, 2, 3, 1), (2, 0, 3, 1), (3, 0, 3, 1)) === sorter.result)
    }
}
