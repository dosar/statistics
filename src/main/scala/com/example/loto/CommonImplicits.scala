package com.example.loto

object CommonImplicits
{
    implicit class InImplicits[T](obj: T)
    {
        def in(seq: Seq[T]) = seq.contains(obj)

        def in(seq: Product) = seq.productIterator.contains(obj)

        def notIn(seq: Seq[T]) = !in(seq)

        def notIn(seq: Product) = !in(seq)
    }

    class Incrementer(var obj: Int)
    {
        def ++ =
        {
            val res = obj
            obj += 1
            res
        }
    }

    class IncrementerExcept(var obj: Int, except: Set[Int])
    {
        def ++ =
        {
            var res = obj
            while(except.contains(res))
            {
                res += 1
            }
            obj = res + 1
            res
        }
    }
}
