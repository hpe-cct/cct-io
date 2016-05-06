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

import java.nio.file.Paths

import cogdebugger.CogDebuggerApp
import cogio.moviefiles.ColorMovie
import cogx.runtime.ComputeGraph
import libcog._


object V1 extends CogDebuggerApp(
  new ComputeGraph {
    val filename = Paths.get("src", "test", "resources", "movies", "TheQuad_480x270-60.mp4")

    val camera = toVectorField(ColorMovie(filename.toString, synchronous = true))
    val background = VectorField(camera.fieldShape, Shape(3))
    background <== 0.001f * camera + 0.999f * background
    val suspicious = reduceSum(abs(background - camera))

    // Probes for display
    probe(toColorField(camera), "camera")
    probe(toColorField(background), "background")
    probe(toColorField(suspicious * 5), "suspicious")
  }
)