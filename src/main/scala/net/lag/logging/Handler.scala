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

import java.util.{logging => javalog}
import scala.collection.mutable
import net.lag.extensions._


/**
 * A base log handler for scala. This extends the java built-in handler
 * and connects it with a formatter automatically.
 */
abstract class Handler(val formatter: Formatter) extends javalog.Handler {

  setFormatter(formatter)


  /**
   * Where to truncate log messages (character count). 0 = don't truncate.
   */

  /**
   * Where to truncate log messages (character count). 0 = don't truncate.
   */

  /**
   * Where to truncate stack traces in exception logging (line count).
   */

  /**
   * Where to truncate stack traces in exception logging (line count).
   */

  /**
   * Return <code>true</code> if dates in log messages are being reported
   * in UTC time, or <code>false</code> if they're being reported in local
   * time.
   */

  /**
   * Set whether dates in log messages should be reported in UTC time
   * (<code>true</code>) or local time (<code>false</code>, the default).
   * This variable and <code>timeZone</code> affect the same settings, so
   * whichever is called last will take precedence.
   */

  override def toString = {
    "<%s level=%s formatter=%s>".format(getClass.getName, getLevel, formatter.toString)
  }
}


/**
 * Mostly useful for unit tests: logging goes directly into a
 * string buffer.
 */
class StringHandler(_formatter: Formatter) extends Handler(_formatter) {
  private var buffer = new StringBuilder()

  def publish(record: javalog.LogRecord) = {
    buffer append getFormatter().format(record)
  }

  def close() = { }

  def flush() = { }

  override def toString = buffer.toString

  def clear() = {
    buffer.clear
  }
}


/**
 * Log things to the console.
 */
class ConsoleHandler(_formatter: Formatter) extends Handler(_formatter) {
  def publish(record: javalog.LogRecord) = {
    System.err.print(getFormatter().format(record))
  }

  def close() = { }

  def flush() = Console.flush
}
