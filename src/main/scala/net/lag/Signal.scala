package net.lag

import scala.collection.mutable
import java.lang.reflect


object SignalHandlerFactory {
  def apply() = {
    // only one actual implementation for now
    SunSignalHandler.instantiate
  }
}

trait SignalHandler {
  def handle(signal: String, handlers: collection.Map[String, collection.Set[String => Unit]])
}

object SunSignalHandler {
  def instantiate = {
    try {
      Class.forName("sun.misc.Signal")
      Some(new SunSignalHandler)
    } catch {
      case ex: ClassNotFoundException => None
    }
  }
}

class SunSignalHandler extends SignalHandler {
  private val signalHandlerClass = Class.forName("sun.misc.SignalHandler")
  private val signalClass = Class.forName("sun.misc.Signal")
  private val handleMethod = signalClass.getMethod("handle", signalClass, signalHandlerClass)
  private val nameMethod = signalClass.getMethod("getName")

  def handle(signal: String, handlers: scala.collection.Map[String, scala.collection.Set[String => Unit]]) {
    val sunSignal = signalClass.getConstructor(classOf[String]).newInstance(signal).asInstanceOf[Object]
    val proxy = reflect.Proxy.newProxyInstance(signalHandlerClass.getClassLoader, Array[Class[_]](signalHandlerClass),
      new reflect.InvocationHandler {
        def invoke(proxy: Object, method: reflect.Method, args: Array[Object]) = {
          if (method.getName() == "handle") {
            handlers(signal).foreach { x =>
              x(nameMethod.invoke(args(0)).asInstanceOf[String])
            }
          }
          null
        }
      }).asInstanceOf[Object]

    handleMethod.invoke(null, sunSignal, proxy)
  }
}

object HandleSignal {
  private val handlers = mutable.HashMap[String, mutable.Set[String => Unit]]()

  def apply(posixSignal: String)(f: String => Unit) {
    if (!handlers.contains(posixSignal)) {
      handlers.synchronized {
        SignalHandlerFactory().foreach { _.handle(posixSignal, handlers) }
        handlers += ((posixSignal, mutable.HashSet[String => Unit]()))
      }
    }

    handlers.synchronized {
      handlers(posixSignal) += f
    }
  }

  def clear(posixSignal: String) {
    handlers.synchronized {
      handlers(posixSignal).clear
    }
  }
}

