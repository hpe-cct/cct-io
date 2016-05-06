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

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

/** Reads standard image files (.jpeg, .gif, .png, .bmp, .wbmp) and provides
  * access to the pixels in the image. The "grayscale" method converts the
  * image to a 2D pixel array with each pixel scaled to the interval [0, 1];
  * this method uses the green channel information as an approximation of
  * black and white.
  *
  * @param filename Full path name of the image file
  *
  * @author Greg Snider
  */
class ImageFileReader(filename: String) {
  val Verbose = false
  private val file = new File(filename)
  if (Verbose)
    println(file.getAbsolutePath)
  val image: BufferedImage = ImageIO.read(file)
  val imageWidth = image.getWidth(null)
  val imageHeight = image.getHeight(null)

  /** Access as a 2D array of grayscale pixels, each pixel in [0.0, 1.0] */
  def grayscale: Array[Array[Float]] = {
    /*
    // Convert image to grayscale. This causes the RGB components to be
    // equal, each in the interval [0, 255]
    val grayImage = new BufferedImage(image.getWidth, image.getHeight,
                                      BufferedImage.TYPE_BYTE_GRAY)
    val g: Graphics = grayImage.getGraphics
    g.drawImage(image, 0, 0, null)
    g.dispose
    */
    // Convert each pixel to the interval [0.0, 1.0]
    val pixels = Array.ofDim[Float](imageHeight, imageWidth)
    val MaxPixel = 255.0f
    for (y <- 0 until imageHeight; x <- 0 until imageWidth) {
      //val pixelValue = grayImage.getRGB(x, y)
      val pixelValue = image.getRGB(x, y)
      val greenComponent = (pixelValue >> 8) & 0xff
      pixels(y)(x) = greenComponent / MaxPixel
    }
    pixels
  }

  /** Get an iterator over the grayscale pixels in an image in row-major order.
    *
    * @return Iterator over pixels
    */
  def grayscaleIterator = new Iterator[Float] {
    // Note: ImageIO uses (x, y) coordinate system, Cog uses (row, column),
    // so we must correct for that here.
    private var row = 0
    private var column = 0
    private val MaxPixel = 255.0f

    /** For speed, hasNext always returns true. */
    def hasNext(): Boolean = {
      true
    }

    /** Get next pixel value in interval (0, 1). If you call this too many
      * times, going past the end of the image, you will take an exception.
      *
      * @return Grayscale pixel value. This uses the green channel as an
      *         approximation.
      */
    def next(): Float = {
      val pixelValue = image.getRGB(column, row)
      val greenComponent = (pixelValue >> 8) & 0xff
      column += 1
      if (column == imageWidth) {
        column = 0
        row += 1
      }
      greenComponent / MaxPixel
    }
  }

  /** Access as a stack of 2D arrays of pixels. The first index is the color
    * plane (0 = red, 1 = green, 2 = blue), the next two indices are the row
    * and column of the pixel, respectively.
    */
  def color: Array[Array[Array[Float]]] = {
    val Colors = 3
    val Red = 0
    val Green = 1
    val Blue = 2
    val pixels = Array.ofDim[Float](Colors, imageHeight, imageWidth)
    val MaxPixel = 255.0f
    for (x <- 0 until imageWidth; y <- 0 until imageHeight) {
      val pixelValue = image.getRGB(x, y)
      val redComponent = (pixelValue >> 16) & 0xff
      val greenComponent = (pixelValue >> 8) & 0xff
      val blueComponent = (pixelValue >> 0) & 0xff
      pixels(Red)(y)(x) = redComponent / MaxPixel
      pixels(Green)(y)(x) = greenComponent / MaxPixel
      pixels(Blue)(y)(x) = blueComponent / MaxPixel
    }
    pixels
  }
}
