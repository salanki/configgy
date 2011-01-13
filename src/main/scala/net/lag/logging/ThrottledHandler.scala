/*
 * Copyright 2009 Robey Pointer <robeypointer@gmail.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain
 * a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.lag.logging

import _root_.scala.collection.mutable
import java.util.{logging => javalog}

class ThrottledHandler(wrapped: Handler, val durationMilliseconds: Int, val maxToDisplay: Int) extends Handler(wrapped.formatter) {
  private class Throttle(now: Long, name: String, level: javalog.Level) {
    var startTime: Long = now
    var count: Int = 0

    override def toString = "Throttle: startTime=" + startTime + " count=" + count

    final def checkFlush(now: Long) {
      if (now - startTime >= durationMilliseconds) {
        if (count > maxToDisplay) {
          val throttledRecord = new javalog.LogRecord(level, "(swallowed %d repeating messages)".format(count - maxToDisplay))
          throttledRecord.setLoggerName(name)
          wrapped.publish(throttledRecord)
        }
        startTime = now
        count = 0
      }
    }
  }

  private val throttleMap = new mutable.HashMap[String, Throttle]

  def reset() {
    throttleMap.synchronized {
      for ((k, throttle) <- throttleMap) {
        throttle.startTime = 0
      }
    }
  }

  def close() = wrapped.close()
  def flush() = wrapped.flush()

  @volatile var lastFlushCheck: Long = 0

  /**
   * Log a message, with sprintf formatting, at the desired level, and
   * attach an exception and stack trace.
   */
  def publish(record: javalog.LogRecord) = {
    val now = System.currentTimeMillis
    if (now - lastFlushCheck > 1000) {
      lastFlushCheck = now
      throttleMap.synchronized {
        throttleMap.values.foreach { t => t.checkFlush(now) }
      }
    }
    val throttle = throttleMap.synchronized {
      throttleMap.getOrElseUpdate(record.getMessage(),
                                  new Throttle(now, record.getLoggerName(), record.getLevel()))
    }
    throttle.synchronized {
      throttle.checkFlush(now)
      throttle.count += 1
      if (throttle.count <= maxToDisplay) {
        wrapped.publish(record)
      }
    }
  }
}
