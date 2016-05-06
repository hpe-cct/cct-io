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

package object cogio {

  // Image file I/O
  val GrayscaleImage = imagefiles.GrayscaleImage
  val ColorImage = imagefiles.ColorImage

  // Movie file I/O
  val ColorMovie = moviefiles.ColorMovie
  val GrayscaleMovie = moviefiles.GrayscaleMovie

  // Webcam I/O
  val ColorWebcam = webcam.ColorWebcam

  // Persistent CPU-resident representation of a field and associated sensors/actuators
  type FieldState = fieldstate.FieldState
  val FieldState = fieldstate.FieldState
  val VectorQueueSensor = fieldstate.VectorQueueSensor
  val UnpipelinedVectorQueueActuator = fieldstate.UnpipelinedVectorQueueActuator
  val VectorStateInjector = fieldstate.VectorStateInjector

  // Binary file I/O
  val ColorBinaryArray = binaryfiles.ColorBinaryArray
  val LabelByteArray = binaryfiles.LabelByteArray
}
