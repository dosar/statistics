package com.example.loto.array

/**
 * Created by alespuh on 03.01.15.
 */
object ArrayPerformanceUtil
{
    def take(master: Array[Int], slave: Array[Int], take: Int)(implicit iterations: Int = Math.min(master.length, take)) =
    {
        if(iterations == master.length) (master, slave)
        else
        {
            val (masterResult, slaveResult) = (new Array[Int](iterations), new Array[Int](iterations))
            var ind = 0
            while(ind < iterations)
            {
                masterResult(ind) = master(ind)
                slaveResult(ind) = slave(ind)
                ind += 1
            }
            (masterResult, slaveResult)
        }
    }

    def takeFromEndReversed(master: Array[Int], slave: Array[Int], take: Int)(implicit iterations: Int = Math.min(master.length, take)) =
    {
        val (masterResult, slaveResult) = (new Array[Int](iterations), new Array[Int](iterations))
        val inputStartIndex = master.length - 1
        var ind = 0
        while(ind < iterations)
        {
            masterResult(ind) = master(inputStartIndex - 1)
            slaveResult(ind) = slave(inputStartIndex - 1)
            ind += 1
        }
        (masterResult, slaveResult)
    }

    /*ожидаем, что exceptFigure отсортирован по возрастанию и в конце завершающий 0*/
    def createFiguresArray(exceptFigures: Array[Int]) =
    {
        val resultSize = 36 - exceptFigures.size + 1
        val result = new Array[Int](resultSize)
        var resultInd = 0
        var exceptFiguresInd = 0
        var figure = 1
        while(resultInd < resultSize && figure < 37)
        {
            if(figure != exceptFigures(exceptFiguresInd))
            {
                result(resultInd) = figure
                resultInd += 1
            }
            else exceptFiguresInd += 1
            figure += 1
        }
        result
    }

    type Index = Int
    def createArray(size: Int)(default: Index => Int) =
    {
        var ind = 0
        val result = new Array[Int](size)
        while(ind < size)
        {
            result(ind) = default(ind)
            ind += 1
        }
        result
    }

    def createArray[T <: AnyRef : Manifest](size: Int, default: => T) =
    {
        var ind = 0
        val result = new Array[T](size)
        while(ind < size)
        {
            result(ind) = default
            ind += 1
        }
        result
    }

    def maxForArray(array: Array[Int]) =
    {
        var max = array(0)
        var ind = 1
        while(ind < array.length)
        {
            val value = array(ind)
            if(value > max)
                max = value
            ind += 1
        }
        max
    }

    def sumNonNegativeValues(array: Array[Int]) =
    {
        var sum = 0
        var ind = 0
        while(ind < array.length)
        {
            val value = array(ind)
            if(value > -1)
                sum += value
            ind += 1
        }
        sum
    }

    def lastPositiveInd(array: Array[Int]) =
    {
        var resultInd = 0
        var ind = 0
        while(ind < array.length)
        {
            if(array(ind) > 0)
                resultInd = ind
            ind += 1
        }
        resultInd
    }

    def firstNonNegativeMinInd(array: Array[Int]) =
    {
        var minInd = 0
        var ind = 1
        while(ind < array.length)
        {
            if(array(ind) < array(minInd) && array(ind) >= 0)
                minInd = ind
            ind += 1
        }
        minInd
    }

    def lastNonNegativeMinInd(array: Array[Int]) =
    {
        var minInd = 0
        var ind = 1
        while(ind < array.length)
        {
            if(array(ind) <= array(minInd) && array(ind) >= 0)
                minInd = ind
            ind += 1
        }
        minInd
    }

    type ExpandedArray = Array[Int]
    def safeSetArrayElement(array: Array[Int], ind: Int, value: Int)(ifCapacityExceeded: ExpandedArray => Unit): Unit =
    {
        if(ind < array.length)
            array(ind) = value
        else
        {
            val expandedArray = new Array[Int](array.length * 2)
            scala.compat.Platform.arraycopy(array, 0, expandedArray, 0, array.length)
            expandedArray(ind) = value
            ifCapacityExceeded(expandedArray)
        }
    }

    def slice[T: Manifest](arr: Array[T], from: Int, until: Int) =
    {
        val length = Math.min(until, arr.length) - from
        val result = new Array[T](length)
        scala.compat.Platform.arraycopy(arr, from, result, 0, length)
        result
    }
}