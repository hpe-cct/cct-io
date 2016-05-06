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

package cogio.imagefiles

import libcog._

/** Factory which creates a ColorField holding a single static color image
  * read in from a file.
  *
  * @author Greg Snider
  */
object ColorImage {

  /**  Read image from "file" and create a static Sensor that holds it. */
  def apply(file: String): ColorField = {
    val pixels: Array[Array[Array[Float]]] =
      new ImageFileReader(file).color
    val rows = pixels(0).length
    val columns = pixels(0)(0).length
    def pixelColor(row: Int, column: Int): Pixel = {
      val red = pixels(0)(row)(column)
      val green = pixels(1)(row)(column)
      val blue = pixels(2)(row)(column)
      new Pixel(red, green, blue)
    }
    val colorField = ColorField(rows, columns, pixelColor)
    colorField
  }
}

