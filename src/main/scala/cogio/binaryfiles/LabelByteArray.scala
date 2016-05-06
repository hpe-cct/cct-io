/*
 * (c) Copyright 2016 Hewlett Packard Enterprise Development LP
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cogio.binaryfiles

import java.io.{BufferedInputStream, FileInputStream}

import libcog._

object LabelByteArray {

  private class LabelSensor(path: String, numClasses: Int, numExamples: Int, period: Int, resourcePath: String) {
    assert(numClasses > 1)
    assert(numExamples > 0)
    assert(period >= 1)

    private def getStream: BufferedInputStream = new BufferedInputStream(
      try {
        this.getClass.getClassLoader.getResourceAsStream(path)
      } catch {
        case e: NullPointerException => new FileInputStream(resourcePath + path)
      }
    )

    private val stream = getStream

    stream.mark(numExamples)

    private var currentPosition = 0
    private var periodCounter = period

    private def resetBuffer() {
      stream.reset()
      currentPosition = 0
      periodCounter = period
    }

    private val readNext: () => Option[Iterator[Float]] = () =>
      if (periodCounter < period - 1) {
        periodCounter += 1; None
      }
      else {
        val iter = new Iterator[Float] {
          val clazz = stream.read
          private var i = 0

          def hasNext = i < numClasses

          def next(): Float = {
            val result = if (i == clazz) 1f else 0f
            i += 1
            result
          }
        }
        currentPosition += 1
        if (currentPosition == numExamples) resetBuffer()
        periodCounter = 0
        Some(iter)
      }

    def label: VectorField = {
      val sensor = new Sensor(Shape(numClasses), readNext, () => resetBuffer())
      reshape(sensor, Shape(), Shape(numClasses))
    }
  }


  def apply(path: String, numClasses: Int, numExamples: Int,
            period: Int = 1, resourcePath: String = "src/main/resources/"): VectorField =
    new LabelSensor(path, numClasses, numExamples, period, resourcePath).label
}

