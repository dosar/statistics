package com.example

import com.example.loto.array.ArrayPerformanceUtil
import org.scalatest.FunSuite

class ArrayPerformanceUtilTest extends FunSuite
{
    test("test createFiguresArray filter left chunk")
    {
        assert(ArrayPerformanceUtil.createFiguresArray(Array(1, 2, 3, 0)) === (4 to 36).toArray)
    }

    test("test createFiguresArray filter right chunk")
    {
        assert(ArrayPerformanceUtil.createFiguresArray(Array(32, 33, 34, 35, 36, 0)) === (1 to 31).toArray)
    }

    test("test createFiguresArray filter middle chunk")
    {
        assert(ArrayPerformanceUtil.createFiguresArray(Array(11, 12, 13, 14, 0)) === ((1 to 10) ++ (15 to 36)).toArray)
    }

    test("test createFiguresArray filter random chunk")
    {
        assert(ArrayPerformanceUtil.createFiguresArray(Array(11, 13, 27, 0)) === ((1 to 10) ++ (12 to 12) ++ (14 to 26) ++ (28 to 36)).toArray)
    }

    test("test fill with default")
    {
        assert(ArrayPerformanceUtil.createArray(10, -1) === Array(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1))
    }
}
