/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include "catch2/catch.hpp"

#include <sgct/readconfig.h>
#include <openspace/engine/configuration.h>
#include <ghoul/fmt.h>
#include <filesystem>
#include <fstream>

using namespace openspace::configuration;

namespace {
    std::string stringify(const std::string filename) {
        std::ifstream myfile;
        myfile.open(filename);
        std::stringstream buffer;
        buffer << myfile.rdbuf();
        return buffer.str();
    }

    void attemptValidation(const std::string cfgString) {
        std::string schemaString = stringify("../config/schema/sgcteditor.schema.json");
        std::filesystem::path schemaDir = std::filesystem::u8path("../config/schema");
        sgct::validateConfigAgainstSchema(cfgString, schemaString, schemaDir, "Generic");
    }
} // namespace

TEST_CASE("SgctEdit: pass", "[sgctedit]") {
    const std::string config =
R"({
  "generator": {
    "major": 1,
    "minor": 1,
    "name": "SgctWindowConfig"
  },
  "masteraddress": "localhost",
  "nodes": [
    {
      "address": "localhost",
      "port": 20401,
      "windows": [
        {
          "border": true,
          "id": 0,
          "monitor": 0,
          "name": "ffss",
          "pos": {
            "x": 112,
            "y": 77
          },
          "size": {
            "x": 1280,
            "y": 720
          },
          "viewports": [
            {
              "pos": {
                "x": 0.0,
                "y": 0.0
              },
              "projection": {
                "heightoffset": 0.0,
                "quality": "1024",
                "type": "CylindricalProjection"
              },
              "size": {
                "x": 1.0,
                "y": 1.0
              },
              "tracked": true
            }
          ]
        }
      ]
    }
  ],
  "scene": {
    "orientation": {
      "w": 0.0,
      "x": 0.0,
      "y": 0.0,
      "z": 0.0
    }
  },
  "users": [
    {
      "eyeseparation": 0.06499999761581421,
      "pos": {
        "x": 0.0,
        "y": 0.0,
        "z": 4.0
      }
    }
  ],
  "version": 1
})";
    CHECK_NOTHROW(attemptValidation(config));
}
