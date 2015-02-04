package com.example.loto.optimization

import com.example.loto.array.ArrayPerformanceUtil
import com.example.loto.metrics.StrategyStatistics

/**
 * Created by alespuh on 18.01.15.
 */

class StrategyPartialOptimization(var startPoint: Array[Int], ranges: Array[Array[Int]])(f: Array[Int] => StrategyStatistics)
{
    def optimize =
    {
        var result = f(startPoint)
        val resultPoint = ArrayPerformanceUtil.slice(startPoint, 0, startPoint.length)
        for((i, j) <- selectDimensions)
        {
            for(d1 <- ranges(i))
            {
                startPoint(i) = d1
                for(d2 <- ranges(j))
                {
                    startPoint(j) = d2
                    val ft = f(startPoint)
                    if(ft > result)
                    {
                        result = ft
                        scala.compat.Platform.arraycopy(startPoint, 0, resultPoint, 0, startPoint.length)
                    }
                }
            }
            scala.compat.Platform.arraycopy(resultPoint, 0, startPoint, 0, resultPoint.length)
        }
        (resultPoint, result)
    }

    def selectDimensions =
    {
        for(i <- 0 until startPoint.length; j <- i + 1 until startPoint.length) yield (i, j)
    }
}
