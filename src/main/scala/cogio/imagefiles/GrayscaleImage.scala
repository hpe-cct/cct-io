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

/** Factory which creates a ScalarField holding a single static grayscale image
  * read in from a file.
  *
  * @author Greg Snider
  */
object GrayscaleImage {

  /**  Read image from "file" and create a static ScalarField that holds it. */
  def apply(file: String): ScalarField = ScalarField(matrix(file))

  /** Read the image from "file" into a matrix. */
  def matrix(file: String): Matrix = {
    val pixels: Array[Array[Float]] =
      new ImageFileReader(file).grayscale
    val rows = pixels.length
    val columns = pixels(0).length
    new Matrix(rows, columns) {
      for (r <- 0 until rows; c <- 0 until columns)
        this(r, c) = pixels(r)(c)
    }
  }
}

