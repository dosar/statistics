package com.example.loto.actors

import akka.actor.SupervisorStrategy.Resume
import akka.actor.{Actor, OneForOneStrategy, Props, SupervisorStrategy}

import scala.concurrent.duration._

trait ActorBase extends Actor
{
    override def supervisorStrategy: SupervisorStrategy = OneForOneStrategy(maxNrOfRetries = 1, withinTimeRange = 1 minute)
    { case e: Throwable =>
        println(e); Resume
    }

    protected def resendMessage[T <: Actor](nameTokes: String*)(message: Any)(implicit classTag: scala.reflect.ClassTag[T]) =
    {
        val name = nameTokes.mkString("_")
        context.child(name).getOrElse(context.actorOf(Props[T], name)) ! message
    }
}