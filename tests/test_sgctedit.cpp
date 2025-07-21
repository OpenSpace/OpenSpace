/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <openspace/engine/configuration.h>
#include <openspace/json.h>
#include <ghoul/filesystem/filesystem.h>
#include <nlohmann/json-schema.hpp>
#include <sgct/config.h>
#include <filesystem>
#include <fstream>
#include <sstream>

using namespace openspace;

namespace {
    void validate(std::string_view cfgString) {
        const std::filesystem::path schemaDir = absPath("${TESTDIR}/../config/schema");
        const std::filesystem::path schema = schemaDir / "sgct.schema.json";
        std::string err = sgct::validateConfigAgainstSchema(cfgString, schema);
        if (!err.empty()) {
            throw std::runtime_error(err);
        }
    }
} // namespace

TEST_CASE("SgctEdit: pass", "[sgctedit]") {
    constexpr std::string_view Source =
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
    CHECK_NOTHROW(validate(Source));
}

TEST_CASE("SgctEdit: addedTrailingBracket", "[sgctedit]") {
    constexpr std::string_view Source =
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
        validate(Source),
        nlohmann::json::parse_error,
        Catch::Matchers::Message(
            "[json.exception.parse_error.101] parse error at line 67, column 2: "
            "syntax error while parsing value - unexpected '}'; expected "
            "end of input"
        )
    );
}

TEST_CASE("SgctEdit: missingMasterAddress", "[sgctedit]") {
    constexpr std::string_view Source =
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
        validate(Source),
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

TEST_CASE("SgctEdit: minimumVersion", "[sgctedit]") {
    const sgct::config::GeneratorVersion minVersion { "SgctWindowConfig", 1, 1 };
    const std::filesystem::path cfg =
        absPath("${TESTDIR}/sgctedit/fails_minimum_version.json");
    const sgct::config::Cluster cluster = sgct::readConfig(cfg);
    REQUIRE(cluster.generator);
    CHECK_FALSE(cluster.generator->versionCheck(minVersion));
}

TEST_CASE("SgctEdit: invalidZvalue", "[sgctedit]") {
    constexpr std::string_view Source =
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
        validate(Source),
        std::exception,
        Catch::Matchers::Message(
            "[json.exception.parse_error.101] parse error at line 25, column 11: "
            "syntax error while parsing value - invalid literal; last read: '\"z\": s'"
        )
    );
}

TEST_CASE("SgctEdit: unwelcomeValue", "[sgctedit]") {
    constexpr std::string_view Source =
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
        validate(Source),
        std::exception,
        Catch::Matchers::Message(
            "At /users/0 of {\"extra\":\"???\",\"eyeseparation\":0.6,\"pos\":"
            "{\"x\":0.0,\"y\":0.0,\"z\":4.0}} - validation failed for additional "
            "property 'extra': instance invalid as per false-schema\n"
        )
    );
}
