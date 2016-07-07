package cogio.webcam

import java.nio.ByteBuffer

import org.bytedeco.javacpp.opencv_imgproc
import org.bytedeco.javacv.{Frame, FrameGrabber, OpenCVFrameConverter, OpenCVFrameGrabber}

import libcog.ColorSensor

object LinuxWebcam {

  /** Factory for wrapping a [[LinuxWebcam]] into a
    * [[cogx.compiler.parser.syntaxtree.ColorSensor]] for use inside a
    * [[cogx.runtime.ComputeGraph]]. Unlike [[ColorWebcam]], this sensor should
    * work on Linux platforms (leveraging the Video4Linux(2) library).
    *
    * Attempts to set the device to deliver a video stream at the given
    * resolution and framerate, but no guarantee is made that the settings are
    * actually accepted and applied.
    *
    * @param device Host device index of the camera. Usually 0 if the host has
    *               no other video input devices attached (other cameras,
    *               capture cards, etc.).
    * @param width Request the target device to produce a video stream with the
    *              given pixel width. Only applied if height is also specified.
    * @param height Request the target device to produce a video stream with
    *               the given pixel height. Only applied if width is also
    *               specified.
    * @param frameRate Request the target device to produce a video stream with
    *                  the given framerate.
    */
  def apply(device: Int, width: Int, height: Int, frameRate: Double): ColorSensor = {
    val camera = new LinuxWebcam(device, width, height, frameRate)
    val converter = new OpenCVFrameConverter.ToIplImage()
    def next(): Option[Iterator[Byte]] = {
      camera.nextFrame() match {
        case Some(frame) =>
          // Camera delivers pixels as BGR, but Cog expects RGB, so we have to
          // do some channel swapping. OpenCV provides a method for just that,
          // but we have to convert to an IplImage to use it.
          val iplImage = converter.convert(frame)
          opencv_imgproc.cvCvtColor(iplImage, iplImage, opencv_imgproc.CV_BGR2RGB)

          val buffer = iplImage.createBuffer().asInstanceOf[ByteBuffer].asReadOnlyBuffer()
          val it = new Iterator[Byte] {
            override def hasNext: Boolean = buffer.position() < buffer.limit()
            override def next(): Byte = buffer.get()
          }
          Some(it)
        case None => None
      }
    }
    sys.addShutdownHook(camera.shutdown())
    new ColorSensor(camera.height, camera.width, next, () => {}, 0.0)
  }

  /** Return a ColorSensor that uses a [[LinuxWebcam]] to read from the given
    * device. Attempts to configure the device to produce a video stream of
    * the given dimensions, but no guarantee is made that the settings are
    * accepted and applied. */
  def apply(device: Int, width: Int, height: Int): ColorSensor = apply(device, width, height, 0.0)

  /** Return a ColorSensor that uses a [[LinuxWebcam]] to read from the given
    * device. Attempts to configure the device to produce a video stream with
    * the given framerate, but no guarantee is made that the settings are
    * accepted and applied. */
  def apply(device: Int, frameRate: Double): ColorSensor = apply(device, 0, 0, frameRate)

  /** Return a ColorSensor that uses a [[LinuxWebcam]] to read from the given
    * device, using the device's default settings. */
  def apply(device: Int): ColorSensor = apply(device, 0, 0)

  /** Return a ColorSensor that uses a [[LinuxWebcam]] to read from device 0,
    * using the devices default settings. */
  def apply(): ColorSensor = apply(0, 0, 0)
}

/** Wrapper for the Linux-compatible OpenCVFrameGrabber that fetches video
  * frames from an attached camera. Use the factory methods in the companion
  * object to roll one of these up in a ColorSensor for use inside a
  * ComputeGraph.
  *
  * At attempt is made to coerce the device into producing a video stream of
  * the given resolution and framerate, but as device capabilities and driver
  * compatibilities vary, there is no guarantee that these settings will
  * actually take effect.
  *
  * Setting a preferred width and height of 0 will use the device's default
  * resolution. Similarly, providing a framerate of 0.0 will use the device's
  * default framerate.
  *
  * === Developer's note: ===
  * The videoInput library that powers the [[cogio.webcam.ColorWebcam]] sensor
  * is only available for Windows. This implementation has been demonstrated to
  * work on Ubuntu 14.04 using Video4Linux2 (V4L2) drivers with an HP 4310
  * camera. Unfortunately, we seem unable to coerce the camera to produce
  * anything other than its default 640x480 @ 20fps MJPEG stream.
  *
  * @param device Host device index of the camera. Usually 0 if the host has
  *               no other video input devices attached (other cameras, capture
  *               cards, etc.).
  * @param preferredWidth Request the target device to produce a video stream
  *                       with the given pixel width. Only applied if
  *                       preferredHeight is also specified. Set to 0 to use
  *                       device default setting.
  * @param preferredHeight Request the target device to produce a video stream
  *                        with the given pixel height. Only applied if
  *                        preferredWidth is also specified. Set to 0 to use
  *                        device default setting.
  * @param preferredFrameRate Request the target device to produce a video
  *                           stream with the given framerate. Set to 0.0 to
  *                           use device default setting.
  */
class LinuxWebcam(
    device: Int,
    preferredWidth: Int,
    preferredHeight: Int,
    preferredFrameRate: Double) {

  def this(device: Int, preferredWidth: Int, preferredHeight: Int) =
    this(device, preferredWidth, preferredHeight, 0.0)

  def this(device: Int) = this(device, 0, 0)

  private val Verbose = false
  private def dbg(msg: String) { if (Verbose) println(msg) }

  dbg("Opening device "+device)
  private val grabber = OpenCVFrameGrabber.createDefault(device)
  grabber.setImageMode(FrameGrabber.ImageMode.COLOR)
  //grabber.setFormat("h264")
  if (preferredWidth > 0 && preferredHeight > 0) {
    grabber.setImageWidth(preferredWidth)
    grabber.setImageHeight(preferredHeight)
  }
  if (preferredFrameRate > 0.0) {
    grabber.setFrameRate(preferredFrameRate)
  }

  grabber.start() // Grabber must be started to query stream info

  /** Flag indicating if the internal frame grabber is running. Calls to
    * [[nextFrame()]] will always return None while this flag is false.
    */
  private var running = true

  val format = grabber.getFormat
  val pxFormat = grabber.getPixelFormat
  val frameRate = grabber.getFrameRate
  val width = grabber.getImageWidth
  val height = grabber.getImageHeight
  dbg(s"Format is $format - $pxFormat - ${width}x$height - $frameRate")

  /** Get the next video frame the camera device. Returns None if the camera
    * fails to deliver a frame for some reason or if [[shutdown()]] has ever
    * been called on this object.
    */
  def nextFrame(): Option[Frame] = {
    if (running) {
      val frame = grabber.grab()
      if (frame != null) {
        return Some(frame)
      }
    }
    None
  }

  /** Stop this object's internal frame grabber and release the resources
    * associated with it. All subsequent calls to [[nextFrame()]] will return
    * None.
    *
    * This class does not provide a means to restart the grabber, so after a
    * call to shutdown, this object is largely useless and should be discarded.
    */
  def shutdown(): Unit = {
    grabber.stop()
    grabber.release()
    running = false
  }
}
