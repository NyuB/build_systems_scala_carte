package nyub.makette

import nyub.makette.TaskResult.Ok
import nyub.makette.TaskResult.Ko
import scala.reflect.ClassTag

sealed trait TaskResult[+V]:
    def map[B](f: V => B): TaskResult[B] = this match
        case Ok(v) => Ok(f(v))
        case k: Ko => k

    def flatMap[B](f: V => TaskResult[B]): TaskResult[B] = this match
        case Ok(v) => f(v)
        case k: Ko => k

object TaskResult:
    extension [V](t: TaskResult[V])
        inline def cast[B <: V](using c: ClassTag[B]): TaskResult[B] = t.flatMap: v =>
            if v.isInstanceOf[B] then Ok(v.asInstanceOf[B])
            else Ko(s"Unable to cast ${v.getClass()} as expected result ${c.runtimeClass}")

    def ko(reason: Any): TaskResult[Nothing] = Ko(reason)
    def ok[V](v: V): TaskResult[V] = Ok(v)
    case class Ok[V](val v: V) extends TaskResult[V]
    case class Ko(val reason: Any) extends TaskResult[Nothing]
