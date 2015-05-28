package com.example

class CommonTest extends TestBase
{
    test("sort list")
    {
        assert(List(1, 3, 2, 5).sortBy(x => x) === List(1, 2, 3, 5))
    }
}
