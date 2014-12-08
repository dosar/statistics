package com.example.loto

/**
 * Created by alespuh on 08.12.14.
 */
trait MetricsTypes
{
    type MaxIntersection = Int
    type MaxIntersectionCount = Int
    type Figure = Int
    type HitCount = Int
    type Index = Int

    def intersectionSize(rrResult: Array[Figure], bet: Seq[Figure]): Int =
    {
        var ind = 0
        var result = 0
        while(ind < bet.length)
        {
            val figure = bet(ind)
            if(figure == rrResult(0) || figure == rrResult(1) || figure == rrResult(2) || figure == rrResult(3) || figure == rrResult(4))
                result += 1
            ind += 1
        }
        result
    }
}
