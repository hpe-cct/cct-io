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

import java.nio.file.{Paths, Path}

import org.bytedeco.javacv.FFmpegFrameGrabber

trait MovieProperties {
  val audioChannels: Int
  val format: String
  val frameRate: Double
  val gamma: Double
  val imageHeight: Int
  val imageWidth: Int
  val lengthInFrames: Int
  val lengthInTime: Long
  val sampleRate: Int
}

object MovieProperties {
  def apply(movie: Path): MovieProperties = {
    val grabber = new FFmpegFrameGrabber(movie.toFile)

    try {
      grabber.start()

      new MovieProperties {
        val audioChannels = grabber.getAudioChannels
        val format = grabber.getFormat
        val frameRate = grabber.getFrameRate
        val gamma = grabber.getGamma
        val imageHeight = grabber.getImageHeight
        val imageWidth = grabber.getImageWidth
        val lengthInFrames = grabber.getLengthInFrames
        val lengthInTime = grabber.getLengthInTime
        val sampleRate = grabber.getSampleRate
      }
    } finally {
      grabber.stop()
    }
  }

  def apply(movie: String): MovieProperties = apply(Paths.get(movie))
}
