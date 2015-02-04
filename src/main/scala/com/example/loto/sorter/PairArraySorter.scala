package com.example.loto.sorter

import com.example.loto.array.ArrayPerformanceUtil

/**
 * Created by alespuh on 13.01.15.
 */
trait PairArraySorter
{
    type MasterArray = Array[Int]
    type SlaveArray = Array[Int]

    def swap(input: Array[Int], oneIndex: Int, anotherIndex: Int)
    {
        val current = input(oneIndex)
        input(oneIndex) = input(anotherIndex)
        input(anotherIndex) = current
    }

    def subArray(arr: Array[Int], startInd: Int, until: Int) = ArrayPerformanceUtil.slice(arr, startInd, until)
}
