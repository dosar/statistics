package com.example.loto

import java.text.SimpleDateFormat

import com.example.loto.CommonImplicits.InImplicits
import com.orientechnologies.orient.core.command.OCommandRequest
import com.orientechnologies.orient.core.record.impl.ODocument
import com.orientechnologies.orient.core.sql.OCommandSQL

import scala.collection.mutable
import scala.io.Source

/**
 * Created by alespuh on 25.11.14.
 */
object LotoImporter extends OrientDb
{
    def fillDb = tx
    { db =>
        db.command[OCommandRequest](new OCommandSQL("delete from " + tableName)).execute()
        val source = Source.fromFile("/home/alespuh/work/loto/src/main/resources/5_36.txt")
        val lines = source.getLines().toList.filter(_ notIn (months ++ otherGarbage))
        val lines1 = lines.zipWithIndex.filter(_._2 % 3 == 0).map(_._1).toList
        val lines2 = lines.zipWithIndex.filter(_._2 % 3 == 1).map(_._1).toList
        val format = new SimpleDateFormat("dd.MM.yyyy hh:mm")
        for((line1, line2) <- lines1.zip(lines2))
        {
            val doc = new ODocument(tableName)
            val run :: date :: Nil = line1.split(" / ").toList
            withoutDoubles(run.toInt)
            {
                doc.field("run", run.toInt)
                doc.field("date", format.parse(date))
                doc.field("result", line2.split(" ").filter(_.trim != "").take(5).map(_.toInt))
                doc.save()
            }
        }
    }

    type Run = Int
    private def withoutDoubles(run: Run)(action: => Unit) =
    {
        if(!runs.contains(run))
        {
            runs += run
            action
        }
    }

    private val runs = mutable.Buffer[Run]()
    private val months = Seq("январь", "февраль", "март", "апрель", "май", "июнь", "июль", "август", "сентябрь",
        "октябрь", "ноябрь", "декабрь")

    private val otherGarbage = Seq("Тираж / Дата", "Выпавшие числа", "Суперприз, руб.", "разыгран")
    private val tableName = "loto_5_36"
}