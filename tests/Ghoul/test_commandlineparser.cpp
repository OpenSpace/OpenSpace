/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#include <ghoul/cmdparser/commandlineparser.h>
#include <ghoul/cmdparser/singlecommand.h>
#include <ghoul/cmdparser/multiplecommand.h>

#include <memory>

 /*
  * Test checklist:
  * +++ SingleCommand, MultipleCommand
  * +++     1-4 arguments
  * +++     different types
  * +++     same types
  * +++     calling once
  * +++     calling multiple times
  * +++ Multiple commands in the same command-line result
  * +++ Variable orders should produce the same result
  * +++ Unknown commands
  * +++ Collection of unknown commands with known commands interspersed
  * +++ Error messages when unknown commands are allowed but no receiving vector is
  *     provided (and vice versa)
  */

TEST_CASE("CommandlineParser: Unknown Commands Unhandled", "[commandlineparser]") {
    ghoul::cmdparser::CommandlineParser p;

    const std::vector<std::string> argv = {
        "tests",
        "-cmd1",
        "arg",
        "-cmd2",
        "arg2"
    };

    p.setCommandLine(argv);
    REQUIRE_THROWS_AS(p.execute(), ghoul::RuntimeError);
}

TEST_CASE("CommandlineParser: Unknown Commands Handled Correctly", "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;

    const std::vector<std::string> argv = {
        "tests",
        "-cmd1",
        "arg",
        "-cmd2",
        "arg2"
    };

    p.setAllowUnknownCommands(
        ghoul::cmdparser::CommandlineParser::AllowUnknownCommands::Yes
    );
    p.setCommandLine(argv);
    REQUIRE_NOTHROW(p.execute());
}

TEST_CASE("CommandlineParser: Unknown Commands Interspersed", "[commandlineparser]") {
    ghoul::cmdparser::CommandlineParser p;

    const std::vector<std::string> argv = {
        "tests",
        "-cmd1",
        "arg",
        "-cmd2",
        "arg2",
        "arg3",
        "-cmd3",
        "arg4"
    };
    std::optional<std::string> v1;
    std::optional<std::string> v2;
    using T = ghoul::cmdparser::SingleCommand<std::string, std::string>;
    p.addCommand(std::make_unique<T>(v1, v2, "-cmd2"));

    using ghoul::cmdparser::CommandlineParser;
    p.setAllowUnknownCommands(CommandlineParser::AllowUnknownCommands::Yes);
    const std::vector<std::string>& arguments = p.setCommandLine(argv);

    REQUIRE_NOTHROW(p.execute());

    REQUIRE(arguments.size() == 4);
    CHECK(arguments[0] == "-cmd1");
    CHECK(arguments[1] == "arg");
    CHECK(arguments[2] == "-cmd3");
    CHECK(arguments[3] == "arg4");
    REQUIRE(v1.has_value());
    CHECK(v1 == "arg2");
    REQUIRE(v2.has_value());
    CHECK(v2 == "arg3");
}

TEST_CASE("CommandlineParser: Single Zero Command Arguments", "[commandlineparser]") {
    ghoul::cmdparser::CommandlineParser p;

    std::optional<bool> v;
    using T = ghoul::cmdparser::SingleCommandZeroArguments;
    p.addCommand(std::make_unique<T>(v, "-zero"));

    const std::vector<std::string> argv = {
        "tests",
        "-zero"
    };

    p.setCommandLine(argv);
    REQUIRE_NOTHROW(p.execute());
    REQUIRE(v.has_value());
    CHECK(v);
}

TEST_CASE("CommandlineParser: Single Command One Argument Bool", "[commandlineparser]") {
    ghoul::cmdparser::CommandlineParser p;

    // boolean
    std::optional<bool> v;
    using T = ghoul::cmdparser::SingleCommand<bool>;
    p.addCommand(std::make_unique<T>(v, "-single"));

    SECTION("false") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.has_value());
        CHECK_FALSE(*v);
    }
    SECTION("true") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.has_value());
        CHECK(*v);
    }
}

TEST_CASE(
    "CommandlineParser: Single Command Called Multiple Times",
    "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;

    // boolean
    std::optional<bool> v = false;
    using T = ghoul::cmdparser::SingleCommand<bool>;
    p.addCommand(std::make_unique<T>(v, "-single"));

    const std::vector<std::string> argv = {
        "tests",
        "-single",
        "0",
        "-single",
        "0"
    };

    p.setCommandLine(argv);
    REQUIRE_THROWS_AS(p.execute(), ghoul::RuntimeError);
}

TEST_CASE("CommandlineParser: Multiple Commands Permutation", "[commandlineparser]") {
    ghoul::cmdparser::CommandlineParser p;

    std::optional<int> v1;
    std::optional<int> v2;
    std::optional<int> v3;

    using T = ghoul::cmdparser::SingleCommand<int>;

    p.addCommand(std::make_unique<T>(v1, "-cmd1"));
    p.addCommand(std::make_unique<T>(v2, "-cmd2"));
    p.addCommand(std::make_unique<T>(v3, "-cmd3"));

    SECTION("123") {
        const std::vector<std::string> argv = {
            "tests",
            "-cmd1",
            "1",
            "-cmd2",
            "2",
            "-cmd3",
            "3"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == 1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 2);
        REQUIRE(v3.has_value());
        CHECK(*v3 == 3);
    }

    SECTION("213") {
        const std::vector<std::string> argv = {
            "tests",
            "-cmd2",
            "2",
            "-cmd1",
            "1",
            "-cmd3",
            "3"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == 1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 2);
        REQUIRE(v3.has_value());
        CHECK(*v3 == 3);
    }

    SECTION("321") {
        const std::vector<std::string> argv = {
            "tests",
            "-cmd3",
            "3",
            "-cmd2",
            "2",
            "-cmd1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == 1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 2);
        REQUIRE(v3.has_value());
        CHECK(*v3 == 3);
    }

    SECTION("312") {
        const std::vector<std::string> argv = {
            "tests",
            "-cmd3",
            "3",
            "-cmd1",
            "1",
            "-cmd2",
            "2"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == 1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 2);
        REQUIRE(v3.has_value());
        CHECK(*v3 == 3);
    }
}

TEST_CASE("CommandlineParser: Single Command One Argument Int", "[commandlineparser]") {
    ghoul::cmdparser::CommandlineParser p;

    std::optional<int> v;
    using T = ghoul::cmdparser::SingleCommand<int>;
    p.addCommand(std::make_unique<T>(v, "-single"));

    SECTION("1") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.has_value());
        CHECK(*v == 1);
    }
    SECTION("2") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.has_value());
        CHECK(*v == 0);
    }
}

TEST_CASE("CommandlineParser: Single Command One Argument String", "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;

    std::optional<std::string> v;
    using T = ghoul::cmdparser::SingleCommand<std::string>;
    p.addCommand(std::make_unique<T>(v, "-single"));

    SECTION("foo") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "foo"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.has_value());
        CHECK(*v == "foo");
    }

    SECTION("bar") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "bar"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.has_value());
        CHECK(*v == "bar");
    }
}

TEST_CASE(
    "CommandlineParser: Single Command Two Arguments Bool Bool",
    "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;

    std::optional<bool> v1;
    std::optional<bool> v2;
    using T = ghoul::cmdparser::SingleCommand<bool, bool>;
    p.addCommand(std::make_unique<T>(v1, v2, "-single"));

    SECTION("00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK_FALSE(*v1);
        REQUIRE(v2.has_value());
        CHECK_FALSE(*v2);
    }
    SECTION("10") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1);
        REQUIRE(v2.has_value());
        CHECK_FALSE(*v2);
    }
    SECTION("01") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK_FALSE(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2);
    }
    SECTION("11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2);
    }
}

TEST_CASE(
    "CommandlineParser: Single Command Two Arguments Int Int",
    "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;
    std::optional<int> v1;
    std::optional<int> v2;
    using T = ghoul::cmdparser::SingleCommand<int, int>;
    p.addCommand(std::make_unique<T>(v1, v2, "-single"));

    SECTION("00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == 0);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 0);
    }
    SECTION("10") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == 1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 0);
    }
    SECTION("01") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == 0);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 1);
    }
    SECTION("11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == 1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 1);
    }
}

TEST_CASE(
    "CommandlineParser: Single Command Two Arguments String String",
    "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;

    std::optional<std::string> v1;
    std::optional<std::string> v2;
    using T = ghoul::cmdparser::SingleCommand<std::string, std::string>;
    p.addCommand(std::make_unique<T>(v1, v2, "-single"));

    SECTION("00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == "0");
        REQUIRE(v2.has_value());
        CHECK(*v2 == "0");
    }
    
    SECTION("10") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == "1");
        REQUIRE(v2.has_value());
        CHECK(*v2 == "0");
    }
    SECTION("01") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == "0");
        REQUIRE(v2.has_value());
        CHECK(*v2 == "1");
    }
    SECTION("11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == "1");
        REQUIRE(v2.has_value());
        CHECK(*v2 == "1");
    }
}

TEST_CASE(
    "CommandlineParser: Single Command Two Arguments Bool Int",
    "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;

    std::optional<bool> v1;
    std::optional<int> v2;
    using T = ghoul::cmdparser::SingleCommand<bool, int>;
    p.addCommand(std::make_unique<T>(v1, v2, "-single"));

    SECTION("00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK_FALSE(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 0);
    }
    SECTION("10") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 0);
    }
    SECTION("01") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK_FALSE(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 1);
    }
    SECTION("11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 1);
    }
}

TEST_CASE(
    "CommandlineParser: Single Command Two Arguments Int Bool",
    "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;
    std::optional<int> v1;
    std::optional<bool> v2;
    using T = ghoul::cmdparser::SingleCommand<int, bool>;
    p.addCommand(std::make_unique<T>(v1, v2, "-single"));

    SECTION("00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == 0);
        REQUIRE(v2.has_value());
        CHECK_FALSE(*v2);
    }
    SECTION("10") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == 1);
        REQUIRE(v2.has_value());
        CHECK_FALSE(*v2);
    }
    SECTION("01") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == 0);
        REQUIRE(v2.has_value());
        CHECK(*v2);
    }
    SECTION("11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == 1);
        REQUIRE(v2.has_value());
        CHECK(*v2);
    }
}

TEST_CASE(
    "CommandlineParser: Single Command Two Arguments Int String",
    "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;

    std::optional<int> v1;
    std::optional<std::string> v2;
    using T = ghoul::cmdparser::SingleCommand<int, std::string>;
    p.addCommand(std::make_unique<T>(v1, v2, "-single"));

    SECTION("00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == 0);
        REQUIRE(v2.has_value());
        CHECK(*v2 == "0");
    }
    SECTION("10") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == 1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == "0");
    }
    SECTION("01") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == 0);
        REQUIRE(v2.has_value());
        CHECK(*v2 == "1");
    }
    SECTION("11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == 1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == "1");
    }
}

TEST_CASE(
    "CommandlineParser: Single Command Two Arguments String Int",
    "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;

    std::optional<std::string> v1;
    std::optional<int> v2;
    using T = ghoul::cmdparser::SingleCommand<std::string, int>;
    p.addCommand(std::make_unique<T>(v1, v2, "-single"));

    SECTION("00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == "0");
        REQUIRE(v2.has_value());
        CHECK(*v2 == 0);
    }
    SECTION("10") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == "1");
        REQUIRE(v2.has_value());
        CHECK(*v2 == 0);
    }
    SECTION("01") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == "0");
        REQUIRE(v2.has_value());
        CHECK(*v2 == 1);
    }
    SECTION("11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1 == "1");
        REQUIRE(v2.has_value());
        CHECK(*v2 == 1);
    }
}

TEST_CASE(
    "CommandlineParser: Single Command Three Arguments Bool Int String",
    "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;

    std::optional<bool> v1;
    std::optional<int> v2;
    std::optional<std::string> v3;
    using T = ghoul::cmdparser::SingleCommand<bool, int, std::string>;
    p.addCommand(std::make_unique<T>(v1, v2, v3, "-single"));

    SECTION("000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK_FALSE(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 0);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "0");
    }
    SECTION("100") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 0);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "0");
    }
    SECTION("010") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK_FALSE(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 1);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "0");
    }
    SECTION("110") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 1);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "0");
    }

    SECTION("001") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK_FALSE(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 0);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "1");
    }
    SECTION("101") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 0);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "1");
    }
    SECTION("011") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK_FALSE(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 1);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "1");
    }
    SECTION("111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 1);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "1");
    }
}

TEST_CASE(
    "CommandlineParser: Single Command Four Arguments Bool Int String Float",
    "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;

    std::optional<bool> v1;
    std::optional<int> v2;
    std::optional<std::string> v3;
    std::optional<float> v4;

    using T = ghoul::cmdparser::SingleCommand<bool, int, std::string, float>;
    p.addCommand(std::make_unique<T>(v1, v2, v3, v4, "-single"));

    SECTION("0000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK_FALSE(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 0);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "0");
        REQUIRE(v4.has_value());
        CHECK(*v4 == 0.f);
    }
    SECTION("1000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 0);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "0");
        REQUIRE(v4.has_value());
        CHECK(*v4 == 0.f);
    }
    SECTION("0100") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK_FALSE(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 1);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "0");
        REQUIRE(v4.has_value());
        CHECK(*v4 == 0.f);
    }
    SECTION("1100") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 1);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "0");
        REQUIRE(v4.has_value());
        CHECK(*v4 == 0.f);
    }

    SECTION("0010") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK_FALSE(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 0);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "1");
        REQUIRE(v4.has_value());
        CHECK(*v4 == 0.f);
    }
    SECTION("1010") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 0);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "1");
        REQUIRE(v4.has_value());
        CHECK(*v4 == 0.f);
    }
    SECTION("0110") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK_FALSE(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 1);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "1");
        REQUIRE(v4.has_value());
        CHECK(*v4 == 0.f);
    }
    SECTION("1110") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 1);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "1");
        REQUIRE(v4.has_value());
        CHECK(*v4 == 0.f);
    }

    SECTION("0001") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK_FALSE(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 0);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "0");
        REQUIRE(v4.has_value());
        CHECK(*v4 == 1.f);
    }
    SECTION("1001") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 0);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "0");
        REQUIRE(v4.has_value());
        CHECK(*v4 == 1.f);
    }
    SECTION("0101") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK_FALSE(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 1);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "0");
        REQUIRE(v4.has_value());
        CHECK(*v4 == 1.f);
    }
    SECTION("1101") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 1);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "0");
        REQUIRE(v4.has_value());
        CHECK(*v4 == 1.f);
    }

    SECTION("0011") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK_FALSE(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 0);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "1");
        REQUIRE(v4.has_value());
        CHECK(*v4 == 1.f);
    }
    SECTION("1011") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 0);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "1");
        REQUIRE(v4.has_value());
        CHECK(*v4 == 1.f);
    }
    SECTION("0111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK_FALSE(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 1);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "1");
        REQUIRE(v4.has_value());
        CHECK(*v4 == 1.f);
    }
    SECTION("1111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.has_value());
        CHECK(*v1);
        REQUIRE(v2.has_value());
        CHECK(*v2 == 1);
        REQUIRE(v3.has_value());
        CHECK(*v3 == "1");
        REQUIRE(v4.has_value());
        CHECK(*v4 == 1.f);
    }
}

TEST_CASE(
    "CommandlineParser: Multiple Zero Command Arguments",
    "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;

    int v = 0;
    using T = ghoul::cmdparser::MultipleCommandZeroArguments;
    p.addCommand(std::make_unique<T>(v, "-zero"));

    SECTION("zero zero") {
        const std::vector<std::string> argv = {
            "tests",
            "-zero",
            "-zero"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        CHECK(v == 2);
    }

    SECTION("6x zero") {
        const std::vector<std::string> argv = {
            "tests",
            "-zero",
            "-zero",
            "-zero",
            "-zero",
            "-zero",
            "-zero"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        CHECK(v == 6);
    }
}

TEST_CASE("CommandlineParser: Multiple Command One Argument Bool", "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;

    std::vector<bool> v;
    using T = ghoul::cmdparser::MultipleCommand<bool>;
    p.addCommand(std::make_unique<T>(v, "-single"));

    SECTION("0") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.size() == 1);
        CHECK_FALSE(v[0]);
    }

    SECTION("1") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.size() == 1);
        CHECK(v[0]);
    }

    SECTION("00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "-single",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.size() == 2);
        CHECK_FALSE(v[0]);
        CHECK_FALSE(v[1]);
    }

    SECTION("10") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "-single",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.size() == 2);
        CHECK(v[0]);
        CHECK_FALSE(v[1]);
    }

    SECTION("01") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "-single",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.size() == 2);
        CHECK_FALSE(v[0]);
        CHECK(v[1]);
    }

    SECTION("10") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "-single",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.size() == 2);
        CHECK(v[0]);
        CHECK_FALSE(v[1]);
    }
}

TEST_CASE("CommandlineParser: Multiple Command One Argument Int", "[commandlineparser]") {
    ghoul::cmdparser::CommandlineParser p;

    std::vector<int> v;
    using T = ghoul::cmdparser::MultipleCommand<int>;
    p.addCommand(std::make_unique<T>(v, "-single"));

    SECTION("1") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.size() == 1);
        CHECK(v[0] == 1);
    }

    SECTION("0") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.size() == 1);
        CHECK(v[0] == 0);
    }

    SECTION("10") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "-single",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.size() == 2);
        CHECK(v[0] == 1);
        CHECK(v[1] == 0);
    }

    SECTION("00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "-single",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.size() == 2);
        CHECK(v[0] == 0);
        CHECK(v[1] == 0);
    }

    SECTION("11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "-single",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.size() == 2);
        CHECK(v[0] == 1);
        CHECK(v[1] == 1);
    }

    SECTION("01") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "-single",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.size() == 2);
        CHECK(v[0] == 0);
        CHECK(v[1] == 1);
    }
}

TEST_CASE(
    "CommandlineParser: Multiple Command One Argument String",
    "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;

    std::vector<std::string> v;
    using T = ghoul::cmdparser::MultipleCommand<std::string>;
    p.addCommand(std::make_unique<T>(v, "-single"));

    SECTION("foo") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "foo"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.size() == 1);
        CHECK(v[0] == "foo");
    }

    SECTION("bar") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "bar"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.size() == 1);
        CHECK(v[0] == "bar");
    }

    SECTION("foo foo") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "foo",
            "-single",
            "foo"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.size() == 2);
        CHECK(v[0] == "foo");
        CHECK(v[1] == "foo");
    }

    SECTION("bar foo") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "bar",
            "-single",
            "foo"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.size() == 2);
        CHECK(v[0] == "bar");
        CHECK(v[1] == "foo");
    }

    SECTION("foo bar") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "foo",
            "-single",
            "bar"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.size() == 2);
        CHECK(v[0] == "foo");
        CHECK(v[1] == "bar");
    }

    SECTION("bar bar") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "bar",
            "-single",
            "bar"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v.size() == 2);
        CHECK(v[0] == "bar");
        CHECK(v[1] == "bar");
    }
}

TEST_CASE(
    "CommandlineParser: Multiple Command Two Arguments Bool Bool",
    "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;

    std::vector<bool> v1;
    std::vector<bool> v2;
    using T = ghoul::cmdparser::MultipleCommand<bool, bool>;
    p.addCommand(std::make_unique<T>(v1, v2, "-single"));

    SECTION("00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK_FALSE(v1[0]);
        CHECK_FALSE(v2[0]);
    }

    SECTION("10") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0]);
        CHECK_FALSE(v2[0]);
    }

    SECTION("01") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0]);
    }

    SECTION("11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0]);
        CHECK(v2[0]);
    }

    SECTION("00 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK_FALSE(v2[0]);
        CHECK_FALSE(v1[1]);
        CHECK_FALSE(v2[1]);
    }

    SECTION("10 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0]);
        CHECK_FALSE(v2[0]);
        CHECK_FALSE(v1[1]);
        CHECK_FALSE(v2[1]);
    }

    SECTION("01 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0]);
        CHECK_FALSE(v1[1]);
        CHECK_FALSE(v2[1]);
    }

    SECTION("11 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0]);
        CHECK_FALSE(v1[1]);
        CHECK_FALSE(v2[1]);
    }

    SECTION("00 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK_FALSE(v2[0]);
        CHECK(v1[1]);
        CHECK(v2[1]);
    }

    SECTION("10 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0]);
        CHECK_FALSE(v2[0]);
        CHECK(v1[1]);
        CHECK(v2[1]);
    }

    SECTION("01 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0]);
        CHECK(v1[1]);
        CHECK(v2[1]);
    }

    SECTION("11 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0]);
        CHECK(v1[1]);
        CHECK(v2[1]);
    }
}

TEST_CASE(
    "CommandlineParser: Multiple Command Two Arguments Int Int",
    "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;

    std::vector<int> v1;
    std::vector<int> v2;

    using T = ghoul::cmdparser::MultipleCommand<int, int>;
    p.addCommand(std::make_unique<T>(v1, v2, "-single"));

    SECTION("00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0] == 0);
        CHECK(v2[0] == 0);
    }

    SECTION("10") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0] == 1);
        CHECK(v2[0] == 0);
    }

    SECTION("01") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0] == 0);
        CHECK(v2[0] == 1);
    }

    SECTION("11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0] == 1);
        CHECK(v2[0] == 1);
    }

    SECTION("00 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 0);
        CHECK(v2[0] == 0);
        CHECK(v1[1] == 0);
        CHECK(v2[1] == 0);
    }

    SECTION("10 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 1);
        CHECK(v2[0] == 0);
        CHECK(v1[1] == 0);
        CHECK(v2[1] == 0);
    }

    SECTION("01 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 0);
        CHECK(v2[0] == 1);
        CHECK(v1[1] == 0);
        CHECK(v2[1] == 0);
    }

    SECTION("11 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 1);
        CHECK(v2[0] == 1);
        CHECK(v1[1] == 0);
        CHECK(v2[1] == 0);
    }

    SECTION("00 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 0);
        CHECK(v2[0] == 0);
        CHECK(v1[1] == 1);
        CHECK(v2[1] == 1);
    }

    SECTION("10 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 1);
        CHECK(v2[0] == 0);
        CHECK(v1[1] == 1);
        CHECK(v2[1] == 1);
    }

    SECTION("01 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 0);
        CHECK(v2[0] == 1);
        CHECK(v1[1] == 1);
        CHECK(v2[1] == 1);
    }

    SECTION("11 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 1);
        CHECK(v2[0] == 1);
        CHECK(v1[1] == 1);
        CHECK(v2[1] == 1);
    }
}

TEST_CASE(
    "CommandlineParser: Multiple Command Two Arguments String String",
    "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;

    std::vector<std::string> v1;
    std::vector<std::string> v2;

    using T = ghoul::cmdparser::MultipleCommand<std::string, std::string>;
    p.addCommand(std::make_unique<T>(v1, v2, "-single"));

    SECTION("00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0] == "0");
        CHECK(v2[0] == "0");
    }

    SECTION("10") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0] == "1");
        CHECK(v2[0] == "0");
    }

    SECTION("01") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0] == "0");
        CHECK(v2[0] == "1");
    }

    SECTION("11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0] == "1");
        CHECK(v2[0] == "1");
    }

    SECTION("00 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == "0");
        CHECK(v2[0] == "0");
        CHECK(v1[1] == "0");
        CHECK(v2[1] == "0");
    }

    SECTION("10 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == "1");
        CHECK(v2[0] == "0");
        CHECK(v1[1] == "0");
        CHECK(v2[1] == "0");
    }

    SECTION("01 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == "0");
        CHECK(v2[0] == "1");
        CHECK(v1[1] == "0");
        CHECK(v2[1] == "0");
    }

    SECTION("11 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == "1");
        CHECK(v2[0] == "1");
        CHECK(v1[1] == "0");
        CHECK(v2[1] == "0");
    }

    SECTION("00 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == "0");
        CHECK(v2[0] == "0");
        CHECK(v1[1] == "1");
        CHECK(v2[1] == "1");
    }

    SECTION("10 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == "1");
        CHECK(v2[0] == "0");
        CHECK(v1[1] == "1");
        CHECK(v2[1] == "1");
    }

    SECTION("01 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == "0");
        CHECK(v2[0] == "1");
        CHECK(v1[1] == "1");
        CHECK(v2[1] == "1");
    }

    SECTION("11 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == "1");
        CHECK(v2[0] == "1");
        CHECK(v1[1] == "1");
        CHECK(v2[1] == "1");
    }
}

TEST_CASE(
    "CommandlineParser: Multiple Command Two Arguments Bool Int",
    "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;

    std::vector<bool> v1;
    std::vector<int> v2;

    using T = ghoul::cmdparser::MultipleCommand<bool, int>;
    p.addCommand(std::make_unique<T>(v1, v2, "-single"));

    SECTION("00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 0);
    }

    SECTION("10") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0]);
        CHECK(v2[0] == 0);
    }

    SECTION("01") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 1);
    }

    SECTION("11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0]);
        CHECK(v2[0] == 1);
    }

    SECTION("00 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 0);
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
    }

    SECTION("10 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 0);
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
    }

    SECTION("01 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 1);
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
    }

    SECTION("11 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 1);
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
    }

    SECTION("00 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
    }

    SECTION("10 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
    }

    SECTION("01 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
    }

    SECTION("11 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
    }
}

TEST_CASE(
    "CommandlineParser: Multiple Command Two Arguments Int Bool",
    "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;

    std::vector<int> v1;
    std::vector<bool> v2;

    using T = ghoul::cmdparser::MultipleCommand<int, bool>;
    p.addCommand(std::make_unique<T>(v1, v2, "-single"));

    SECTION("00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0] == 0);
        CHECK_FALSE(v2[0]);
    }

    SECTION("10") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0] == 1);
        CHECK_FALSE(v2[0]);
    }

    SECTION("01") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0] == 0);
        CHECK(v2[0]);
    }

    SECTION("11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0] == 1);
        CHECK(v2[0]);
    }

    SECTION("00 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 0);
        CHECK_FALSE(v2[0]);
        CHECK(v1[1] == 0);
        CHECK_FALSE(v2[1]);
    }

    SECTION("10 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 1);
        CHECK_FALSE(v2[0]);
        CHECK(v1[1] == 0);
        CHECK_FALSE(v2[1]);
    }

    SECTION("01 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 0);
        CHECK(v2[0]);
        CHECK(v1[1] == 0);
        CHECK_FALSE(v2[1]);
    }

    SECTION("11 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 1);
        CHECK(v2[0]);
        CHECK(v1[1] == 0);
        CHECK_FALSE(v2[1]);
    }

    SECTION("00 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 0);
        CHECK_FALSE(v2[0]);
        CHECK(v1[1] == 1);
        CHECK(v2[1]);
    }

    SECTION("10 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 1);
        CHECK_FALSE(v2[0]);
        CHECK(v1[1] == 1);
        CHECK(v2[1]);
    }

    SECTION("01 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 0);
        CHECK(v2[0]);
        CHECK(v1[1] == 1);
        CHECK(v2[1]);
    }

    SECTION("11 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 1);
        CHECK(v2[0]);
        CHECK(v1[1] == 1);
        CHECK(v2[1]);
    }
}

TEST_CASE(
    "CommandlineParser: Multiple Command Two Arguments Int String",
    "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;

    std::vector<int> v1;
    std::vector<std::string> v2;

    using T = ghoul::cmdparser::MultipleCommand<int, std::string>;
    p.addCommand(std::make_unique<T>(v1, v2, "-single"));

    SECTION("00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0] == 0);
        CHECK(v2[0] == "0");
    }

    SECTION("10") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0] == 1);
        CHECK(v2[0] == "0");
    }

    SECTION("01") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0] == 0);
        CHECK(v2[0] == "1");
    }

    SECTION("11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0] == 1);
        CHECK(v2[0] == "1");
    }

    SECTION("00 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 0);
        CHECK(v2[0] == "0");
        CHECK(v1[1] == 0);
        CHECK(v2[1] == "0");
    }

    SECTION("10 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 1);
        CHECK(v2[0] == "0");
        CHECK(v1[1] == 0);
        CHECK(v2[1] == "0");
    }

    SECTION("01 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 0);
        CHECK(v2[0] == "1");
        CHECK(v1[1] == 0);
        CHECK(v2[1] == "0");
    }

    SECTION("11 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 1);
        CHECK(v2[0] == "1");
        CHECK(v1[1] == 0);
        CHECK(v2[1] == "0");
    }

    SECTION("00 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 0);
        CHECK(v2[0] == "0");
        CHECK(v1[1] == 1);
        CHECK(v2[1] == "1");
    }

    SECTION("10 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 1);
        CHECK(v2[0] == "0");
        CHECK(v1[1] == 1);
        CHECK(v2[1] == "1");
    }

    SECTION("01 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 0);
        CHECK(v2[0] == "1");
        CHECK(v1[1] == 1);
        CHECK(v2[1] == "1");
    }

    SECTION("11 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == 1);
        CHECK(v2[0] == "1");
        CHECK(v1[1] == 1);
        CHECK(v2[1] == "1");
    }
}

TEST_CASE(
    "CommandlineParser: Multiple Command Two Arguments String Int",
    "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;

    std::vector<std::string> v1;
    std::vector<int> v2;

    using T = ghoul::cmdparser::MultipleCommand<std::string, int>;
    p.addCommand(std::make_unique<T>(v1, v2, "-single"));

    SECTION("00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0] == "0");
        CHECK(v2[0] == 0);
    }

    SECTION("10") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0] == "1");
        CHECK(v2[0] == 0);
    }

    SECTION("01") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0] == "0");
        CHECK(v2[0] == 1);
    }

    SECTION("11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        CHECK(v1[0] == "1");
        CHECK(v2[0] == 1);
    }

    SECTION("00 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == "0");
        CHECK(v2[0] == 0);
        CHECK(v1[1] == "0");
        CHECK(v2[1] == 0);
    }

    SECTION("10 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == "1");
        CHECK(v2[0] == 0);
        CHECK(v1[1] == "0");
        CHECK(v2[1] == 0);
    }

    SECTION("01 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == "0");
        CHECK(v2[0] == 1);
        CHECK(v1[1] == "0");
        CHECK(v2[1] == 0);
    }

    SECTION("11 00") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "-single",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == "1");
        CHECK(v2[0] == 1);
        CHECK(v1[1] == "0");
        CHECK(v2[1] == 0);
    }

    SECTION("00 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == "0");
        CHECK(v2[0] == 0);
        CHECK(v1[1] == "1");
        CHECK(v2[1] == 1);
    }

    SECTION("10 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == "1");
        CHECK(v2[0] == 0);
        CHECK(v1[1] == "1");
        CHECK(v2[1] == 1);
    }

    SECTION("01 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == "0");
        CHECK(v2[0] == 1);
        CHECK(v1[1] == "1");
        CHECK(v2[1] == 1);
    }

    SECTION("11 11") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "-single",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        CHECK(v1[0] == "1");
        CHECK(v2[0] == 1);
        CHECK(v1[1] == "1");
        CHECK(v2[1] == 1);
    }
}

TEST_CASE(
    "CommandlineParser: Multiple Command Three Arguments Bool Int String",
    "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;

    std::vector<bool> v1;
    std::vector<int> v2;
    std::vector<std::string> v3;

    using T = ghoul::cmdparser::MultipleCommand<bool, int, std::string>;
    p.addCommand(std::make_unique<T>(v1, v2, v3, "-single"));

    SECTION("000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "0");
    }

    SECTION("100") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        CHECK(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "0");
    }

    SECTION("010") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "0");
    }

    SECTION("110") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        CHECK(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "0");
    }

    SECTION("001") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "1");
    }

    SECTION("101") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        CHECK(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "1");
    }

    SECTION("011") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "1");
    }

    SECTION("111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        CHECK(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "1");
    }

    SECTION("000 000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "0",
            "-single",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "0");
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
    }

    SECTION("100 000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "0",
            "-single",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "0");
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
    }

    SECTION("010 000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "0",
            "-single",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "0");
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
    }

    SECTION("110 000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "0",
            "-single",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "0");
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
    }

    SECTION("001 000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "1",
            "-single",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "1");
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
    }

    SECTION("101 000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "1",
            "-single",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "1");
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
    }

    SECTION("011 000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "1",
            "-single",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "1");
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
    }

    SECTION("111 000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "1",
            "-single",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "1");
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
    }

    SECTION("000 111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "0",
            "-single",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "0");
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
    }

    SECTION("100 111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "0",
            "-single",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "0");
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
    }

    SECTION("010 111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "0",
            "-single",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "0");
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
    }

    SECTION("110 111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "0",
            "-single",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "0");
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
    }

    SECTION("001 111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "1",
            "-single",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "1");
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
    }

    SECTION("101 111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "1",
            "-single",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "1");
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
    }

    SECTION("011 111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "1",
            "-single",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "1");
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
    }

    SECTION("111 111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "1",
            "-single",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "1");
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
    }
}

TEST_CASE(
    "CommandlineParser: Multiple Command Four Arguments Bool Int String Float",
    "[commandlineparser]")
{
    ghoul::cmdparser::CommandlineParser p;

    std::vector<bool> v1;
    std::vector<int> v2;
    std::vector<std::string> v3;
    std::vector<float> v4;

    using T = ghoul::cmdparser::MultipleCommand<bool, int, std::string, float>;
    p.addCommand(std::make_unique<T>(v1, v2, v3, v4, "-single"));

    SECTION("0000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        REQUIRE(v4.size() == 1);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 0.f);
    }

    SECTION("1000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        REQUIRE(v4.size() == 1);
        CHECK(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 0.f);
    }

    SECTION("0100") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        REQUIRE(v4.size() == 1);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 0.f);
    }

    SECTION("1100") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        REQUIRE(v4.size() == 1);
        CHECK(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 0.f);
    }

    SECTION("0010") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        REQUIRE(v4.size() == 1);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 0.f);
    }

    SECTION("1010") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        REQUIRE(v4.size() == 1);
        CHECK(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 0.f);
    }

    SECTION("0110") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        REQUIRE(v4.size() == 1);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 0.f);
    }

    SECTION("1110") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "1",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        REQUIRE(v4.size() == 1);
        CHECK(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 0.f);
    }

    SECTION("0001") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        REQUIRE(v4.size() == 1);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 1.f);
    }

    SECTION("1001") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        REQUIRE(v4.size() == 1);
        CHECK(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 1.f);
    }

    SECTION("0101") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        REQUIRE(v4.size() == 1);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 1.f);
    }

    SECTION("1101") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "0",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        REQUIRE(v4.size() == 1);
        CHECK(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 1.f);
    }

    SECTION("0011") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        REQUIRE(v4.size() == 1);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 1.f);
    }

    SECTION("1011") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        REQUIRE(v4.size() == 1);
        CHECK(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 1.f);
    }

    SECTION("0111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        REQUIRE(v4.size() == 1);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 1.f);
    }

    SECTION("1111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 1);
        REQUIRE(v2.size() == 1);
        REQUIRE(v3.size() == 1);
        REQUIRE(v4.size() == 1);
        CHECK(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 1.f);
    }

    SECTION("0000 0000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "0",
            "0",
            "-single",
            "0",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 0.f);
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
        CHECK(v4[1] == 0.f);
    }

    SECTION("1000 0000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "0",
            "0",
            "-single",
            "0",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 0.f);
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
        CHECK(v4[1] == 0.f);
    }

    SECTION("0100 0000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "0",
            "0",
            "-single",
            "0",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 0.f);
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
        CHECK(v4[1] == 0.f);
    }

    SECTION("1100 0000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "0",
            "0",
            "-single",
            "0",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 0.f);
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
        CHECK(v4[1] == 0.f);
    }

    SECTION("0010 0000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "1",
            "0",
            "-single",
            "0",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 0.f);
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
        CHECK(v4[1] == 0.f);
    }

    SECTION("1010 0000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "1",
            "0",
            "-single",
            "0",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 0.f);
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
        CHECK(v4[1] == 0.f);
    }

    SECTION("0110 0000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "1",
            "0",
            "-single",
            "0",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 0.f);
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
        CHECK(v4[1] == 0.f);
    }

    SECTION("1110 0000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "1",
            "0",
            "-single",
            "0",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 0.f);
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
        CHECK(v4[1] == 0.f);
    }

    SECTION("0001 0000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "0",
            "1",
            "-single",
            "0",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 1.f);
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
        CHECK(v4[1] == 0.f);
    }

    SECTION("1001 0000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "0",
            "1",
            "-single",
            "0",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 1.f);
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
        CHECK(v4[1] == 0.f);
    }

    SECTION("0101 0000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "0",
            "1",
            "-single",
            "0",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 1.f);
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
        CHECK(v4[1] == 0.f);
    }

    SECTION("1101 0000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "0",
            "1",
            "-single",
            "0",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 1.f);
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
        CHECK(v4[1] == 0.f);
    }

    SECTION("0011 0000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "1",
            "1",
            "-single",
            "0",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 1.f);
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
        CHECK(v4[1] == 0.f);
    }

    SECTION("1011 0000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "1",
            "1",
            "-single",
            "0",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 1.f);
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
        CHECK(v4[1] == 0.f);
    }

    SECTION("0111 0000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "1",
            "1",
            "-single",
            "0",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 1.f);
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
        CHECK(v4[1] == 0.f);
    }

    SECTION("1111 0000") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "1",
            "1",
            "-single",
            "0",
            "0",
            "0",
            "0"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 1.f);
        CHECK_FALSE(v1[1]);
        CHECK(v2[1] == 0);
        CHECK(v3[1] == "0");
        CHECK(v4[1] == 0.f);
    }

    SECTION("0000 1111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "0",
            "0",
            "-single",
            "1",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 0.f);
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
        CHECK(v4[1] == 1.f);
    }

    SECTION("1000 1111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "0",
            "0",
            "-single",
            "1",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 0.f);
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
        CHECK(v4[1] == 1.f);
    }

    SECTION("0100 1111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "0",
            "0",
            "-single",
            "1",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 0.f);
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
        CHECK(v4[1] == 1.f);
    }

    SECTION("1100 1111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "0",
            "0",
            "-single",
            "1",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 0.f);
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
        CHECK(v4[1] == 1.f);
    }

    SECTION("0010 1111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "1",
            "0",
            "-single",
            "1",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 0.f);
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
        CHECK(v4[1] == 1.f);
    }

    SECTION("1010 1111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "1",
            "0",
            "-single",
            "1",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 0.f);
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
        CHECK(v4[1] == 1.f);
    }

    SECTION("0110 1111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "1",
            "0",
            "-single",
            "1",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 0.f);
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
        CHECK(v4[1] == 1.f);
    }

    SECTION("1110 1111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "1",
            "0",
            "-single",
            "1",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 0.f);
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
        CHECK(v4[1] == 1.f);
    }

    SECTION("0001 1111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "0",
            "1",
            "-single",
            "1",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 1.f);
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
        CHECK(v4[1] == 1.f);
    }

    SECTION("1001 1111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "0",
            "1",
            "-single",
            "1",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 1.f);
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
        CHECK(v4[1] == 1.f);
    }

    SECTION("0101 1111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "0",
            "1",
            "-single",
            "1",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 1.f);
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
        CHECK(v4[1] == 1.f);
    }

    SECTION("1101 1111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "0",
            "1",
            "-single",
            "1",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "0");
        CHECK(v4[0] == 1.f);
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
        CHECK(v4[1] == 1.f);
    }

    SECTION("0011 1111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "0",
            "1",
            "1",
            "-single",
            "1",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 1.f);
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
        CHECK(v4[1] == 1.f);
    }

    SECTION("1011 1111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "0",
            "1",
            "1",
            "-single",
            "1",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 0);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 1.f);
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
        CHECK(v4[1] == 1.f);
    }

    SECTION("0111 1111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "0",
            "1",
            "1",
            "1",
            "-single",
            "1",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK_FALSE(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 1.f);
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
        CHECK(v4[1] == 1.f);
    }

    SECTION("1111 1111") {
        const std::vector<std::string> argv = {
            "tests",
            "-single",
            "1",
            "1",
            "1",
            "1",
            "-single",
            "1",
            "1",
            "1",
            "1"
        };

        p.setCommandLine(argv);
        REQUIRE_NOTHROW(p.execute());
        REQUIRE(v1.size() == 2);
        REQUIRE(v2.size() == 2);
        REQUIRE(v3.size() == 2);
        REQUIRE(v4.size() == 2);
        CHECK(v1[0]);
        CHECK(v2[0] == 1);
        CHECK(v3[0] == "1");
        CHECK(v4[0] == 1.f);
        CHECK(v1[1]);
        CHECK(v2[1] == 1);
        CHECK(v3[1] == "1");
        CHECK(v4[1] == 1.f);
    }
}
