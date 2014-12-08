package com.example.loto.model

import java.util
import java.util.Date
import scala.collection.JavaConversions._
import com.orientechnologies.orient.core.record.impl.ODocument

import scala.collection.mutable.ListBuffer

object RunResultService extends OrientDb
{
    def from(doc: ODocument): RunResult =
    {
        val integers = doc.field[util.ArrayList[Integer]]("result")
        RunResult(doc.field[Int]("run"), doc.field[Date]("date"), integers.map(_.intValue()).toArray)
    }

    def list = tx
    { db =>
        val table = db.browseClass("loto_5_36")
        val result = ListBuffer[RunResult]()
        while(table.hasNext) result += from(table.next())
        result.toVector
    }
}
