package com.example.loto.model

import com.orientechnologies.orient.core.db.document.ODatabaseDocumentTx

/**
 * Created by alespuh on 25.11.14.
 */
trait OrientDb
{
    def tx[A](f: ODatabaseDocumentTx => A): A =
    {
        val db: ODatabaseDocumentTx  = new ODatabaseDocumentTx("remote:/loto").open("admin", "admin")
        try
        {
            f(db)
        }
        catch
        {
            case e: Throwable =>
            {
                println(e)
                throw e
            }
        }
        finally db.close()
    }

}
