package com.example.loto.array

/**
 * Created by alespuh on 03.01.15.
 */
object ArrayPerformanceUtil
{
    def take(master: Array[Int], slave: Array[Int], take: Int) =
    {
        if(take == master.length) (master, slave)
        else
        {
            val (masterResult, slaveResult) = (new Array[Int](take), new Array[Int](take))
            var ind = 0
            while(ind < take)
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

    def slice[T <: AnyRef : Manifest](arr: Array[T], from: Int, until: Int) =
    {
        val length = Math.min(until, arr.length) - from
        val result = new Array[T](length)
        scala.compat.Platform.arraycopy(arr, from, result, 0, length)
        result
    }

    def slice(arr: Array[Int], from: Int, until: Int) =
    {
        val length = Math.min(until, arr.length) - from
        val result = new Array[Int](length)
        scala.compat.Platform.arraycopy(arr, from, result, 0, length)
        result
    }

    //проверить есть ли дубли
    def checkTallies(arr: Array[Int]): Array[Int] =
    {
        var i = 0
        var j = 1
        while(i < arr.length)
        {
            if(arr(i) == 0)
            {
                println("Недозаполненная ставка. Ебань! " + arr.toList.toString)
                return new Array[Int](arr.length)
            }
            j = i + 1
            while(j < arr.length)
            {
                if(arr(i) == arr(j))
                {
                    println("Дубль ебать! " + arr.toList.toString)
                    return new Array[Int](arr.length)
                }
                j += 1
            }
            i += 1
        }
        return arr
    }

    def reverseInPlace(input: Array[Int]) =
    {
        var ind = 0
        while(ind < input.length / 2)
        {
            val buf = input(ind)
            input(ind) = input(input.length - ind - 1)
            input(input.length - ind - 1) = buf
            ind += 1
        }
    }
}