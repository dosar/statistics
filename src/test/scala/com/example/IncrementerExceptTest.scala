package com.example

import com.example.loto.CommonImplicits.IncrementerExcept

/**
 * Created by alespuh on 21.12.14.
 */
class IncrementerExceptTest extends TestBase
{
    test("increment except 1")
    {
        val incrementer = new IncrementerExcept(0, Set(1, 2, 3, 6))
        assert(0 === (incrementer ++))
        assert(4 === (incrementer ++))
        assert(5 === (incrementer ++))
        assert(7 === (incrementer ++))
        assert(8 === (incrementer ++))
    }
}
