package com.example.loto.sorter

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

    def subArray(arr: Array[Int], startInd: Int, until: Int) =
    {
        val result = new Array[Int](until - startInd)
        scala.compat.Platform.arraycopy(arr, startInd, result, 0, result.length)
        result
    }
}
