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

import java.io.{FileInputStream, BufferedInputStream, DataInputStream, File}

/** Reads the high-definition, high dynamic range images files created by
  * Russell SA Brinkworth and Steven Wiederman. These files have a suffix of
  * ".bed2".
  *
  * Here's the documentation of the file format:
  *
  * BED2 files are IEEE 32-bit floating point images with a 10 digit header
  * as follows:
  * {{{
  *   1: number of colours in the image
  *   2: number of rows in the image
  *   3: number of columns in the image
  *   4: the depth of data (32-, 16- or 8-bit)
  *   5: if the data is transposed (1 = yes)
  *   6: if the data is in hexagonal format or rectangular (1 = hexagonal)
  *   7 - 10: empty
  * }}}
  *
  * Unlike with other image formats the data is saved with the colour
  * information grouped together.
  *
  * If there is three colour channels then they are always in the order
  * Green, Red and Blue.
  *
  * @author Greg Snider
  */

private class HDImageFileReader(filename: String) {
  /** Degree of subsampling for rows and columns. */
  private var Subsample = 4

  // Open the image file.
  private val file = new File(filename)
  private val in = new DataInputStream(new BufferedInputStream(
    new FileInputStream(file)))

  // Read the header.
  private val colors = in.readFloat.toInt
  private val rows = in.readFloat.toInt
  private val columns = in.readFloat.toInt
  private val depth = in.readFloat.toInt
  private val transposed = (in.readFloat == 1)
  private val rectangular = (in.readFloat != 1)
  in.readFloat
  in.readFloat
  in.readFloat
  in.readFloat
  require(depth == 32)
  require(rectangular)
  require(transposed)
  require(colors == 3)
  private val subColumns = columns / Subsample
  private val subRows = rows / Subsample

  // Create the image buffers.
  private val redPixels = Array.fill(subColumns, subRows)(0.0f)
  private val greenPixels = Array.fill(subColumns, subRows)(0.0f)
  private val bluePixels = Array.fill(subColumns, subRows)(0.0f)
  private val luminancePixels = Array.fill(subColumns, subRows)(0.0f)

  // Read the image data
  for (row <- 0 until rows; col <- 0 until columns)
    greenPixels(col / Subsample)(row / Subsample) += in.readFloat
  for (row <- 0 until rows; col <- 0 until columns)
    redPixels(col / Subsample)(row / Subsample) += in.readFloat
  for (row <- 0 until rows; col <- 0 until columns)
    bluePixels(col / Subsample)(row / Subsample) += in.readFloat

  // Normalize the image data so that all pixels are in the interval [0, 1]
  var maxPixel = 0f
  for (r <- 0 until subRows; c <- 0 until subColumns) {
    maxPixel = maxPixel max greenPixels(c)(r)
    maxPixel = maxPixel max redPixels(c)(r)
    maxPixel = maxPixel max bluePixels(c)(r)
  }
  for (r <- 0 until subRows; c <- 0 until subColumns) {
    greenPixels(c)(r) /= maxPixel
    redPixels(c)(r) /= maxPixel
    bluePixels(c)(r) /= maxPixel
  }

  // Create the luminance image pixels
  for (r <- 0 until subRows; c <- 0 until subColumns) {
    luminancePixels(c)(r) = 0.3811f * redPixels(c)(r) +
      0.5783f * greenPixels(c)(r) + 0.0402f * bluePixels(c)(r)
  }

  /** Get the red image field as a 2D array of Floats. */
  def redImage: Array[Array[Float]] = redPixels

  /** Get the green image field as a 2D array of Floats. */
  def greenImage: Array[Array[Float]] = greenPixels

  /** Get the blue image field as a 2D array of Floats. */
  def blueImage: Array[Array[Float]] = bluePixels

  /** Get the luminance image field as a 2D array of Floats. */
  def luminanceImage: Array[Array[Float]] = luminancePixels

  def print {
    println("Image: " + filename)
    printf("  %d colors\n", colors)
    printf("  %d rows\n", rows)
    printf("  %d columns\n", columns)
    printf("  %d depth\n", depth)
    println("  transposed: " + transposed)
    println("  rectangular: " + rectangular)
  }
}
