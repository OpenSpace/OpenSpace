/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_exception.hpp>

#include <ghoul/filesystem/filesystem.h>
#include <nlohmann/json.hpp>
#include <nlohmann/json-schema.hpp>
#include <sgct/readconfig.h>
#include <openspace/engine/configuration.h>
#include <filesystem>
#include <fstream>
#include <sstream>

using namespace openspace;

namespace {
    std::string stringify(const std::filesystem::path& filename) {
        const std::ifstream myfile = std::ifstream(filename);
        std::stringstream buffer;
        buffer << myfile.rdbuf();
        return buffer.str();
    }

    void attemptValidation(const std::string& cfgString) {
        const std::filesystem::path schemaDir = absPath("${TESTDIR}/../config/schema");
        const std::string schemaString = stringify(schemaDir / "sgcteditor.schema.json");
        sgct::validateConfigAgainstSchema(cfgString, schemaString, schemaDir);
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

TEST_CASE("SgctEdit: addedTrailingBracket", "[sgctedit]") {
    const std::string config =
R"({
  "generator": {
    "major": 0,
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
}})";
    CHECK_THROWS_MATCHES(
        attemptValidation(config),
        nlohmann::json::parse_error,
        Catch::Matchers::Message(
            "[json.exception.parse_error.101] parse error at line 67, column 2: "
            "syntax error while parsing value - unexpected '}'; expected "
            "end of input"
        )
    );
}

TEST_CASE("SgctEdit: missingMasterAddress", "[sgctedit]") {
    const std::string config =
R"({
  "generator": {
    "major": 1,
    "minor": 1,
    "name": "SgctWindowConfig"
  },
  "nodes": [
    {
      "address": "localhost",
      "port": 20401,
      "windows": [
        {
          "border": true,
          "id": 0,
          "monitor": 1,
          "name": "name",
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
    CHECK_THROWS_MATCHES(
        attemptValidation(config),
        std::exception,
        Catch::Matchers::Message(
            "At  of {\"generator\":{\"major\":1,\"minor\":1,\"name\":"
            "\"SgctWindowConfig\"},\"nodes\":[{\"address\":\"localhost\",\"port\":"
            "20401,\"windows\":[{\"border\":true,\"id\":0,\"monitor\":1,\"name\":"
            "\"name\",\"pos\":{\"x\":112,\"y\":77},\"size\":{\"x\":1280,\"y\":720},"
            "\"viewports\":[{\"pos\":{\"x\":0.0,\"y\":0.0},\"projection\":"
            "{\"heightoffset\":0.0,\"quality\":\"1024\",\"type\":"
            "\"CylindricalProjection\"},\"size\":{\"x\":1.0,\"y\":1.0},\"tracked\":"
            "true}]}]}],\"scene\":{\"orientation\":{\"w\":0.0,\"x\":0.0,\"y\":0.0,"
            "\"z\":0.0}},\"users\":[{\"eyeseparation\":0.06499999761581421,\"pos\":"
            "{\"x\":0.0,\"y\":0.0,\"z\":4.0}}],\"version\":1} - required property "
            "'masteraddress' not found in object\n"
        )
    );
}

TEST_CASE("SgctEdit: missingPos", "[sgctedit]") {
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
          "name": "name",
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
      "eyeseparation": 0.06499999761581421
    }
  ],
  "version": 1
})";
    CHECK_THROWS_MATCHES(
        attemptValidation(config),
        std::exception,
        Catch::Matchers::Message(
            "At /users/0 of {\"eyeseparation\":0.06499999761581421} - required "
            "property 'pos' not found in object\n"
        )
    );
}

TEST_CASE("SgctEdit: missingGenerator", "[sgctedit]") {
    const std::string config =
R"({
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
          "name": "name",
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
    CHECK_THROWS_MATCHES(
        attemptValidation(config),
        std::exception,
        Catch::Matchers::Message(
            "At  of {\"masteraddress\":\"localhost\",\"nodes\":[{\"address\":"
            "\"localhost\",\"port\":20401,\"windows\":[{\"border\":true,\"id\":"
            "0,\"monitor\":0,\"name\":\"name\",\"pos\":{\"x\":112,\"y\":77},\"size\":"
            "{\"x\":1280,\"y\":720},\"viewports\":[{\"pos\":{\"x\":0.0,\"y\":0.0},"
            "\"projection\":{\"heightoffset\":0.0,\"quality\":\"1024\",\"type\":"
            "\"CylindricalProjection\"},\"size\":{\"x\":1.0,\"y\":1.0},\"tracked\":"
            "true}]}]}],\"scene\":{\"orientation\":{\"w\":0.0,\"x\":0.0,\"y\":0.0,\"z\":"
            "0.0}},\"users\":[{\"eyeseparation\":0.06499999761581421,\"pos\":{\"x\":"
            "0.0,\"y\":0.0,\"z\":4.0}}],\"version\":1} - required property 'generator' "
            "not found in object\n"
        )
    );
}

TEST_CASE("SgctEdit: minimumVersion", "[sgctedit]") {
    const sgct::config::GeneratorVersion minVersion { "SgctWindowConfig", 1, 1 };
    const std::filesystem::path cfg =
        absPath("${TESTDIR}/sgctedit/fails_minimum_version.json");
    const sgct::config::GeneratorVersion ver = sgct::readConfigGenerator(cfg);
    CHECK_FALSE(ver.versionCheck(minVersion));
}

TEST_CASE("SgctEdit: invalidZvalue", "[sgctedit]") {
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
          "monitor": 1,
          "name": "ffss",
          "pos": {
            "x": 112,
            "y": 77
          },
          "size": {
            "x": 1280,
            "y": 720,
	    "z": s
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
    CHECK_THROWS_MATCHES(
        attemptValidation(config),
        std::exception,
        Catch::Matchers::Message(
            "[json.exception.parse_error.101] parse error at line 25, column 11: "
            "syntax error while parsing value - invalid literal; last read: '\"z\": s'"
        )
    );
}

TEST_CASE("SgctEdit: unwelcomeValue", "[sgctedit]") {
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
      "extra": "???",
      "eyeseparation": 0.6,
      "pos": {
        "x": 0.0,
        "y": 0.0,
        "z": 4.0
      }
    }
  ],
  "version": 1
})";
    CHECK_THROWS_MATCHES(
        attemptValidation(config),
        std::exception,
        Catch::Matchers::Message(
            "At /users/0 of {\"extra\":\"???\",\"eyeseparation\":0.6,\"pos\":"
            "{\"x\":0.0,\"y\":0.0,\"z\":4.0}} - validation failed for additional "
            "property 'extra': instance invalid as per false-schema\n"
        )
    );
}
