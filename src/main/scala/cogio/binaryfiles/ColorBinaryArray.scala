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

/** Factory to build a ColorSensor from a flat binary file.
  *
  * @author Ben Chandler
  */
object ColorBinaryArray {

  private class Sensor(path: String, shape: Shape, numExamples: Long, period: Int, resourcePath: String) {
    assert(shape.dimensions == 2)
    assert(numExamples > 0)
    assert(period >= 1)

    private def getStream: BufferedInputStream = new BufferedInputStream(
      try {
        this.getClass.getClassLoader.getResourceAsStream(path)
      } catch {
        case e: NullPointerException => new FileInputStream(resourcePath + path)
      }
    )

    private var index = 0
    private var phase = 0
    private var stream = getStream

    private val bufferSize = 4096
    private val buffer = new Array[Byte](bufferSize)

    private def nextValue(): Option[Iterator[Byte]] = {
      val iterator = if (phase == 0) {
        Some(new Iterator[Byte] {
          var bytesToRead = shape.points * 3
          var bytesToWrite = bytesToRead
          var cursor = 0
          var bytesAvailable = 0

          def doRead() {
            if (index == numExamples) {
              stream.close()
              stream = getStream
              index = 0
            }

            val bytesRead = stream.read(buffer, 0, math.min(bytesToRead, bufferSize))
            require(bytesRead >= 0)
            bytesAvailable = bytesRead
            bytesToRead -= bytesRead
          }

          def hasNext: Boolean = bytesToWrite > 0

          def next(): Byte = {
            if (cursor == bytesAvailable && bytesToRead > 0) {
              cursor = 0

              doRead()
            }

            val b = buffer(cursor)
            cursor += 1
            bytesToWrite -= 1

            if (bytesToWrite == 0) {
              index += 1
            }

            b
          }
        })
      } else {
        None
      }

      phase += 1

      if (phase == period) {
        phase = 0
      }

      iterator
    }

    def sensor: ColorField = new ColorSensor(shape, () => nextValue())
  }

  def apply(path: String, shape: Shape, numExamples: Long, period: Int = 1,
            resourcePath: String = "src/main/resources/"): ColorField =
    new Sensor(path, shape, numExamples, period, resourcePath).sensor
}
