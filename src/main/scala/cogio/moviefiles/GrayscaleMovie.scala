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

package cogio.moviefiles

import cogx.platform.types.FieldType
import libcog._
import org.bytedeco.javacpp.opencv_core
import org.bytedeco.javacv.{OpenCVFrameConverter, FrameGrabber, FFmpegFrameGrabber}


/** Factory for creating a sensor that reads in successive frames of a
  * movie from a movie file. The sensor loops back to the beginning of the movie
  * after it hits the last frame, forming an endless loop.  This uses a pipelined
  * sensor, so the ffmpeg processing is done in parallel with any GPU processing
  * of the frames.
  *
  * @author Tobin Gonzalez and Dick Carter
  */
object GrayscaleMovie {

  /** Create a sensor (synchronous or fast-as-possible) that reads a
    * movie file to create a sensor stream.
    *
    * @param filename Name of the movie file.
    * @param initTimestamp The initial frame to display (may not be 0 if the sensor was restored).
    * @param synchronous True if movie is to stream at encoded framerate,
    *        false if movie is to stream as fast as possible.
    * @param stride Step by `stride` movie frames on each Cog step.
    * @return A Sensor that carries the movie's video stream.
    */
  def apply(filename: String, initTimestamp: Long, synchronous: Boolean, stride: Int): Sensor = {
    require(stride >= 1, "stride must be greater than or equal to one")

    /** A timestamp used to save this sensor at its present state. */
    var lastTimeStamp = 0L

    /** The grayscale framegrabber for this sensor. */
    val grabber = new FFmpegFrameGrabber(filename)

    /** In producting the next frame, how many frames should be skipped. Initially 0 so first frame is frame0. */
    var framesToSkip = 0

    Runtime.getRuntime.addShutdownHook(new Thread {
      override def run() {
        println("Shutting down FFmpegFrameGrabber")
        grabber.stop()
      }
    })
    // See the AV_PIX_FMT_* enums in com.googlecode.javacv.cpp.avutil for more
    // about the different pixel formats.
    // Set grabber options here.
    grabber.setImageMode(FrameGrabber.ImageMode.GRAY)

    // Grabber must be started to query stream info, like framerate and
    // resolution.
    grabber.start()
    val width = grabber.getImageWidth
    val height = grabber.getImageHeight
    val pixels = width * height
    val frameRate = if (synchronous) grabber.getFrameRate else 0.0

    /** Set the frame number back to its saved state, not necessarily timestamp 0L. */
    def reset(): Unit = {
      try {
        // There is something strange going on with FFmpeg and its pipeline of grabbed frames and its response
        // to the setFrameNumber call.  The following sequence of grabbing a couple of frames first before
        // reset produced identical results between the grabber's first reset and subsequent user-initiated resets.
        // With this approach, there are no initial black frames or lost frames.  This was troubleshot using
        // the ParkingLot.mp4, action-60fps.mp4, and Wildlife.wmv videos. Wildlife.wmv seems to have an initial
        // black frame.
        if (initTimestamp <= 0L)
          grabber.restart()
        else {
          grabber.grab()
          grabber.grab()
          grabber.grab()
          grabber.setTimestamp(initTimestamp)
        }
      }
      catch {
        case e: Exception => println("Reset of the video stream not available with this decoder.")
      }
      // After reset, don't skip over frames on the first readFrame()
      framesToSkip = 0
    }

    /** Read the next frame of the movie. */
    def readFrame(): Iterator[Float] =
    {
      // Save the current timestamp to be used by the restore- best done right here for pipelined sensors.
      lastTimeStamp = grabber.getTimestamp
      val converter = new OpenCVFrameConverter.ToIplImage()
      var frame: opencv_core.IplImage = null

      // grab `framesToSkip + 1` frames, skipping `framesToSkip`
      for (i <- 0 to framesToSkip) {
        frame = converter.convert(grabber.grabImage())
        if (frame == null) {
          restart(grabber)
          frame = converter.convert(grabber.grabImage())
          require(frame != null)
        }
      }
      // We know we've produced the first frame, so make sure we skip over 'stride - 1' frames from now on
      framesToSkip = stride - 1

      val byteBuffer = frame.getByteBuffer.asReadOnlyBuffer
      byteBuffer.rewind
      new Iterator[Float] {
        def hasNext = true
        def next(): Float = (byteBuffer.get & 0xff) / 255f
      }
    }

    /** The Sensor behind this movie player- overrides hooks to enable saving of its state. */
    new Sensor(Shape(height, width), () => Some(readFrame), reset _, frameRate) {
      // The parameters that define the state of this sensor, packed into a string and separated by spaces.
      override def restoreParameters = Seq[Any](synchronous, lastTimeStamp, stride, filename).mkString(" ")

      // The default restoringClass object instance would identify this as an anonymous subclass of a (pipelined) Sensor.
      // We override this here to point to the GrayscaleMovie factory object (so the restore method will be found).
      override def restoringClass = GrayscaleMovie
    }
  }

  /** An alternate constructor that does not supply an initial timestamp and supplies some defaults for 'synchronous' and 'stride'. */
  def apply(filename: String, synchronous: Boolean = false, stride: Int = 1): Sensor = apply(filename, 0L, synchronous, stride)

  /** Restart the movie from the beginning.
    *
    * OpenCV is not thread safe in some ways.  In particular, multiple
    * FrameGrabbers cannot call restart() simultaneously.  Hence 'synchronized'.
    *
    * @param grabber The framegrabber to be restarted.
    */
  private def restart(grabber: FFmpegFrameGrabber): Unit = synchronized {
    grabber.restart()
  }

  /** The factory method used to create a pipelined sensor from its stored parameter string. */
  def restore(fieldType: FieldType, parameterString: String) = {
    //
    // The 4 parameters that define the state of the GrayscaleMovie are:
    //
    // 1. Whether the movie is being played out synchronously at its natural frame rate (a Boolean).
    // 2. The current frame (an Int).
    // 3. The interval between displayed frames (a.k.a. "stride").
    // 4. The name of the movie file (a String).
    //
    val parameters = parameterString.split(" ")
    require(parameters.length >= 4, "Expecting 4 parameters, found " + parameters.length)
    val synchronous = parameters(0) != "false"
    val initState = parameters(1).toLong
    val stride = parameters(2).toInt
    // Suppose the filename has spaces in it?  By saving the filename last, we handle that case as follows:
    val remainingParameters = parameters.toSeq.drop(3)
    // Put spaces back into filename, if necessary
    val filename = remainingParameters.mkString(" ")
    apply(filename, initState, synchronous, stride)
  }
 }