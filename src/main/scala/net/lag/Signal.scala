package net.lag

import java.lang.ref.WeakReference
import scala.collection.mutable
import sun.misc.{Signal, SignalHandler}

object HandleSignal {
  private val handlers = mutable.HashMap[Signal, mutable.Set[Signal => Unit]]()

  private object Handler extends SignalHandler {
    def handle(signal: Signal) {
      synchronized {
        handlers(signal).foreach(_(signal))
      }
    }
  }

  def apply(posixSignal: String)(f: Signal => Unit) {
    val signal = new Signal(posixSignal)
    if (!handlers.contains(signal)) {
      Signal.handle(new Signal(posixSignal), Handler)
      synchronized {
        handlers += ((signal, mutable.HashSet[Signal => Unit]()))
      }
    }

    synchronized {
      handlers(signal) += f
    }
  }

  def clear(posixSignal: String) {
    val signal = new Signal(posixSignal)
    synchronized {
      handlers(signal).clear
    }
  }
}

