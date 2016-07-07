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

package cogio.webcam

import libcog._
import org.bytedeco.javacpp.opencv_imgproc
import org.bytedeco.javacv.{OpenCVFrameConverter, FrameGrabber, VideoInputFrameGrabber, FFmpegFrameGrabber}
import org.bytedeco.javacv.FrameGrabber.ImageMode

/**
  *
  */
object ColorWebcam {
  val Colors = 3
  val lookup = Array.tabulate(256)(_ / 255f)

  var verbose = false

  sealed trait Format
  object Format {
    case object Unknown extends Format { override def toString = "default"}
    case object MJpeg extends Format { override def toString = "mjpeg" }
    case object H264 extends Format { override def toString = "h264" }
    case class Custom(name: String) { override def toString = name }
  }

  /** Returns a color sensor that attempts to capture video from the specified
    * network address. Attempts to guess the video's encoding format, and uses
    * whatever the default resolution provided by the source is.
    *
    * CAUTION! This function tends to hang if it can't make a connection to
    * the target URL! */
  def apply(url: String): ColorSensor = apply(url, Format.Unknown, 0, 0)
  def apply(url: String, format: Format): ColorSensor = apply(url, format, 0, 0)

  /** Returns a color sensor that attempts to capture video from the specified
    * network address. The 'format' parameter is used to coerce the video
    * decoder to assume a particular encoding format, in case it can't guess it
    * correctly on its own. The `width` or `height` and height parameters can
    * be used to request a specific resolution from the video source, but of
    * course the source may not support the requested resolution (or may choose
    * to ignore it).
    *
    * CAUTION! This function tends to hang if it can't make a connection to
    * the target URL! */
  def apply(url: String, format: Format, width: Int, height: Int): ColorSensor = {
    if (verbose) println("Connecting to IP camera at URL: "+url)
    val grabber = new FFmpegFrameGrabber(url)
    grabber.setImageMode(ImageMode.COLOR)
    if (width > 0) grabber.setImageWidth(width)
    if (height > 0) grabber.setImageHeight(height)

    format match {
      case Format.Unknown => // Do nothing (ffmpeg should try to automaticcaly determine stream format?)
      case fmt => grabber.setFormat(fmt.toString)
    }

    grabber.start()

    val imgWidth = grabber.getImageWidth
    val imgHeight = grabber.getImageHeight
    if (verbose) println(s"Video stream has resolution ${imgWidth}x$imgHeight.")
    new ColorSensor(imgHeight, imgWidth, () => readFrame(grabber))
  }
  def apply(url: String, width: Int, height: Int): ColorSensor = apply(url, Format.Unknown, width, height)

  /** Create a color sensor that captures video from the specified device.
    *
    * WARNING: This implementation depends on a Windows-specific library
    *          (videoInput). For a Linux compatible camera sensor, use the
    *          [[LinuxWebcam]] class.
    *
    * Defaults to device 0, which usually corresponds to the first webcam
    * installed in a system. If capture width, height, or framerate are
    * supplied, an attempt is made to apply those parameters to the video
    * stream. Be warned that not all cameras support all image sizes or
    * framerates, and they may ignore those settings (silently or otherwise).
    */
  def apply(width: Int = 0, height: Int = 0, deviceNumber: Int = 0, framerate: Double = 0) = {
    val grabber = new VideoInputFrameGrabber(deviceNumber)

    // This call seemed to get us frame rates > 10 fps in Cog 3, but it
    // doesn't seem to work anymore.
    //grabber.setPixelFormat(com.googlecode.javacv.cpp.avutil.AV_PIX_FMT_VDPAU_H264)
    grabber.setImageMode(ImageMode.COLOR)

    if (width > 0) grabber.setImageWidth(width)
    if (height > 0) grabber.setImageHeight(height)
    if (framerate > 0) grabber.setFrameRate(framerate)

    grabber.start()

    // Actual dimensions of the video stream are only available after the grabber
    // has started.
    val imgWidth = grabber.getImageWidth
    val imgHeight = grabber.getImageHeight

    new ColorSensor(imgHeight, imgWidth, () => readFrame(grabber))
  }

  private def readFrame(grabber: FrameGrabber)() = {
    val converter = new OpenCVFrameConverter.ToIplImage()
    val image = converter.convert(grabber.grab())
    if (image != null) {
      opencv_imgproc.cvCvtColor(image, image, opencv_imgproc.CV_BGR2RGB)
      val bytes = image.getByteBuffer.asReadOnlyBuffer()
      Some(new Iterator[Byte] {
        def hasNext: Boolean = bytes.hasRemaining
        def next(): Byte = bytes.get()
      })
    } else
      None
  }
}
