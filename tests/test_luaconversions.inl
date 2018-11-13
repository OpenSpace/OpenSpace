/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <ghoul/lua/ghoul_lua.h>
#include <openspace/properties/propertydelegate.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/charproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/scalar/longdoubleproperty.h>
#include <openspace/properties/scalar/longlongproperty.h>
#include <openspace/properties/scalar/longproperty.h>
#include <openspace/properties/scalar/shortproperty.h>
#include <openspace/properties/scalar/signedcharproperty.h>
#include <openspace/properties/scalar/ucharproperty.h>
#include <openspace/properties/scalar/uintproperty.h>
#include <openspace/properties/scalar/ulonglongproperty.h>
#include <openspace/properties/scalar/ulongproperty.h>
#include <openspace/properties/scalar/ushortproperty.h>
#include <openspace/properties/scalar/wcharproperty.h>
#include <openspace/properties/vector/bvec2property.h>
#include <openspace/properties/vector/bvec3property.h>
#include <openspace/properties/vector/bvec4property.h>

#include <openspace/properties/vector/dvec2property.h>
#include <openspace/properties/vector/dvec3property.h>
#include <openspace/properties/vector/dvec4property.h>

#include <openspace/properties/vector/ivec2property.h>
#include <openspace/properties/vector/ivec3property.h>
#include <openspace/properties/vector/ivec4property.h>

#include <openspace/properties/vector/uvec2property.h>
#include <openspace/properties/vector/uvec3property.h>
#include <openspace/properties/vector/uvec4property.h>

#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/vector/vec4property.h>
#include <openspace/properties/matrix/mat2property.h>
#include <openspace/properties/matrix/mat2x3property.h>
#include <openspace/properties/matrix/mat2x4property.h>
#include <openspace/properties/matrix/mat3x2property.h>
#include <openspace/properties/matrix/mat3property.h>
#include <openspace/properties/matrix/mat3x4property.h>
#include <openspace/properties/matrix/mat4x2property.h>
#include <openspace/properties/matrix/mat4x3property.h>
#include <openspace/properties/matrix/mat4property.h>

#include <openspace/properties/matrix/dmat2property.h>
#include <openspace/properties/matrix/dmat2x3property.h>
#include <openspace/properties/matrix/dmat2x4property.h>
#include <openspace/properties/matrix/dmat3x2property.h>
#include <openspace/properties/matrix/dmat3property.h>
#include <openspace/properties/matrix/dmat3x4property.h>
#include <openspace/properties/matrix/dmat4x2property.h>
#include <openspace/properties/matrix/dmat4x3property.h>
#include <openspace/properties/matrix/dmat4property.h>
#include <openspace/properties/stringproperty.h>

#include <random>

namespace {
    constexpr int NumberFuzzTests = 100000;
} // namespace 

class LuaConversionTest : public testing::Test {
protected:
    lua_State* state;

    LuaConversionTest() {
        state = luaL_newstate();
        luaL_openlibs(state);
    }

    ~LuaConversionTest() {
        lua_close(state);
    }

    void reset() {
        lua_close(state);
        state = luaL_newstate();
        luaL_openlibs(state);
    }
};

TEST_F(LuaConversionTest, LuaExecution) {
    int status = luaL_loadstring(state, "");
    EXPECT_EQ(status, LUA_OK);
}

TEST_F(LuaConversionTest, Bool) {
    using namespace openspace::properties;
    bool success = PropertyDelegate<TemplateProperty<bool>>::toLuaValue<bool>(
        state,
        true
    );
    EXPECT_TRUE(success) << "toLuaValue";
    bool value = static_cast<bool>(0);
    value = PropertyDelegate<TemplateProperty<bool>>::fromLuaValue<bool>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, true) << "fromLuaValue";
}

TEST_F(LuaConversionTest, Char) {
    using namespace openspace::properties;
    using T = char;
    T val = T(1);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, CharFuzz) {
    using namespace openspace::properties;
    using T = char;

    std::mt19937 gen(1337);
    std::uniform_int_distribution<> dis(
        std::numeric_limits<T>::lowest(),
        std::numeric_limits<T>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

//TEST_F(LuaConversionTest, WChar) {
//    using namespace openspace::properties;
//    using T = wchar_t;
//
//    T val = T(1);
//
//    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
//        state,
//        val
//        );
//    EXPECT_TRUE(success) << "toLuaValue";
//    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
//        state,
//        success
//    );
//    EXPECT_TRUE(success) << "fromLuaValue";
//    EXPECT_EQ(value, val) << "fromLuaValue";
//}

//TEST_F(LuaConversionTest, WCharFuzz) {
//    using namespace openspace::properties;
//    using T = wchar_t;
//
//    std::mt19937 gen(1337);
//    std::uniform_int_distribution<> dis(
//        std::numeric_limits<T>::lowest(),
//        std::numeric_limits<T>::max()
//    );
//
//    constexpr int NumberFuzzTests = 10000;
//    for (int i = 0; i < NumberFuzzTests; ++i) {
//        T val = T(dis(gen));
//
//        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
//            state,
//            val
//        );
//        EXPECT_TRUE(success) << "toLuaValue";
//        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
//            state,
//            success
//        );
//        EXPECT_TRUE(success) << "fromLuaValue";
//        EXPECT_EQ(value, val) << "fromLuaValue";
//    }
//}

TEST_F(LuaConversionTest, SignedChar) {
    using namespace openspace::properties;
    using T = signed char;
    T val = T(1);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, SignedCharFuzz) {
    using namespace openspace::properties;
    using T = signed char;

    std::mt19937 gen(1337);
    std::uniform_int_distribution<> dis(
        std::numeric_limits<T>::lowest(),
        std::numeric_limits<T>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, UnsignedChar) {
    using namespace openspace::properties;
    using T = unsigned char;
    T val = T(1);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, UnsignedCharFuzz) {
    using namespace openspace::properties;
    using T = unsigned char;

    std::mt19937 gen(1337);
    std::uniform_int_distribution<> dis(
        std::numeric_limits<T>::lowest(),
        std::numeric_limits<T>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, Short) {
    using namespace openspace::properties;
    using T = short;
    T val = T(1);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, ShortFuzz) {
    using namespace openspace::properties;
    using T = short;

    std::mt19937 gen(1337);
    std::uniform_int_distribution<T> dis(
        std::numeric_limits<T>::lowest(),
        std::numeric_limits<T>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, UnsignedShort) {
    using namespace openspace::properties;
    using T = unsigned short;
    T val = T(1);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, UnsignedShortFuzz) {
    using namespace openspace::properties;
    using T = unsigned short;

    std::mt19937 gen(1337);
    std::uniform_int_distribution<T> dis(
        std::numeric_limits<T>::lowest(),
        std::numeric_limits<T>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, Int) {
    using namespace openspace::properties;
    using T = int;
    T val = T(1);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, IntFuzz) {
    using namespace openspace::properties;
    using T = int;

    std::mt19937 gen(1337);
    std::uniform_int_distribution<T> dis(
        std::numeric_limits<T>::lowest(),
        std::numeric_limits<T>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, UnsignedInt) {
    using namespace openspace::properties;
    using T = unsigned int;
    T val = T(1);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, UnsignedIntFuzz) {
    using namespace openspace::properties;
    using T = unsigned int;

    std::mt19937 gen(1337);
    std::uniform_int_distribution<T> dis(
        std::numeric_limits<T>::lowest(),
        std::numeric_limits<T>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, Long) {
    using namespace openspace::properties;
    using T = long;
    T val = T(1);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, LongFuzz) {
    using namespace openspace::properties;
    using T = long;

    std::mt19937 gen(1337);
    // We need to limit the range of values as Lua uses 'doubles' to store, and some
    // values will not be representable
    std::uniform_int_distribution<T> dis(
        std::numeric_limits<int>::lowest(),
        std::numeric_limits<int>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, UnsignedLong) {
    using namespace openspace::properties;
    using T = unsigned long;
    T val = T(1);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, UnsignedLongFuzz) {
    using namespace openspace::properties;
    using T = unsigned long;

    std::mt19937 gen(1337);
    // We need to limit the range of values as Lua uses 'doubles' to store, and some
    // values will not be representable
    std::uniform_int_distribution<T> dis(
        std::numeric_limits<unsigned int>::lowest(),
        std::numeric_limits<unsigned int>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, LongLong) {
    using namespace openspace::properties;
    using T = long long;
    T val = T(1);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, LongLongFuzz) {
    using namespace openspace::properties;
    using T = long long;

    std::mt19937 gen(1337);
    // We need to limit the range of values as Lua uses 'doubles' to store, and some
    // values will not be representable
    std::uniform_int_distribution<T> dis(
        std::numeric_limits<int>::lowest(),
        std::numeric_limits<int>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, UnsignedLongLong) {
    using namespace openspace::properties;
    using T = unsigned long long;
    T val = T(1);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, UnsignedLongLongFuzz) {
    using namespace openspace::properties;
    using T = unsigned long long;

    std::mt19937 gen(1337);
    // We need to limit the range of values as Lua uses 'doubles' to store, and some
    // values will not be representable
    std::uniform_int_distribution<T> dis(
        std::numeric_limits<unsigned int>::lowest(),
        std::numeric_limits<unsigned int>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, Float) {
    using namespace openspace::properties;
    using T = float;
    T val = T(1.f);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, FloatFuzz) {
    using namespace openspace::properties;
    using T = float;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T> dis(
        0.f,
        std::numeric_limits<T>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, Double) {
    using namespace openspace::properties;
    using T = double;
    T val = T(1.0);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, DoubleFuzz) {
    using namespace openspace::properties;
    using T = double;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T> dis(
        0.0,
        std::numeric_limits<T>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, LongDouble) {
    using namespace openspace::properties;
    using T = long double;
    T val = T(1.0);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, LongDoubleFuzz) {
    using namespace openspace::properties;
    using T = long double;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T> dis(
        0l,
        std::numeric_limits<T>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, Vec2) {
    using namespace openspace::properties;
    using T = glm::vec2;
    T val = T(1.f);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, Vec2Fuzz) {
    using namespace openspace::properties;
    using T = glm::vec2;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, Vec3) {
    using namespace openspace::properties;
    using T = glm::vec3;
    T val = T(1.f);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, Vec3Fuzz) {
    using namespace openspace::properties;
    using T = glm::vec3;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, Vec4) {
    using namespace openspace::properties;
    using T = glm::vec4;
    T val = T(1.f);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, Vec4Fuzz) {
    using namespace openspace::properties;
    using T = glm::vec4;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen), dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, DVec2) {
    using namespace openspace::properties;
    using T = glm::dvec2;
    T val = T(1.0);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, DVec2Fuzz) {
    using namespace openspace::properties;
    using T = glm::dvec2;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, DVec3) {
    using namespace openspace::properties;
    using T = glm::dvec3;
    T val = T(1.0);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, DVec3Fuzz) {
    using namespace openspace::properties;
    using T = glm::dvec3;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, DVec4) {
    using namespace openspace::properties;
    using T = glm::dvec4;
    T val = T(1.0);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, DVec4Fuzz) {
    using namespace openspace::properties;
    using T = glm::dvec4;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen), dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, IVec2) {
    using namespace openspace::properties;
    using T = glm::ivec2;
    T val = T(1);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, IVec2Fuzz) {
    using namespace openspace::properties;
    using T = glm::ivec2;

    std::mt19937 gen(1337);
    std::uniform_int_distribution<T::value_type> dis(
        std::numeric_limits<T::value_type>::lowest(),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, IVec3) {
    using namespace openspace::properties;
    using T = glm::ivec3;
    T val = T(1);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, IVec3Fuzz) {
    using namespace openspace::properties;
    using T = glm::ivec3;

    std::mt19937 gen(1337);
    std::uniform_int_distribution<T::value_type> dis(
        std::numeric_limits<T::value_type>::lowest(),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, IVec4) {
    using namespace openspace::properties;
    using T = glm::ivec4;
    T val = T(1);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, IVec4Fuzz) {
    using namespace openspace::properties;
    using T = glm::ivec4;

    std::mt19937 gen(1337);
    std::uniform_int_distribution<T::value_type> dis(
        std::numeric_limits<T::value_type>::lowest(),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen), dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, UVec2) {
    using namespace openspace::properties;
    using T = glm::uvec2;
    T val = T(1);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, UVec2Fuzz) {
    using namespace openspace::properties;
    using T = glm::uvec2;

    std::mt19937 gen(1337);
    std::uniform_int_distribution<T::value_type> dis(
        std::numeric_limits<T::value_type>::lowest(),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, UVec3) {
    using namespace openspace::properties;
    using T = glm::uvec3;
    T val = T(1);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, UVec3Fuzz) {
    using namespace openspace::properties;
    using T = glm::uvec3;

    std::mt19937 gen(1337);
    std::uniform_int_distribution<T::value_type> dis(
        std::numeric_limits<T::value_type>::lowest(),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, UVec4) {
    using namespace openspace::properties;
    using T = glm::uvec4;
    T val = T(1);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, UVec4Fuzz) {
    using namespace openspace::properties;
    using T = glm::uvec4;

    std::mt19937 gen(1337);
    std::uniform_int_distribution<T::value_type> dis(
        std::numeric_limits<T::value_type>::lowest(),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen), dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, Mat2x2) {
    using namespace openspace::properties;
    using T = glm::mat2x2;
    T val = T(1.f);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, Mat2x2Fuzz) {
    using namespace openspace::properties;
    using T = glm::mat2x2;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen), dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, Mat2x3) {
    using namespace openspace::properties;
    using T = glm::mat2x3;
    T val = T(1.f);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, Mat2x3Fuzz) {
    using namespace openspace::properties;
    using T = glm::mat2x3;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen), dis(gen), dis(gen), dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, Mat2x4) {
    using namespace openspace::properties;
    using T = glm::mat2x4;
    T val = T(1.f);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, Mat2x4Fuzz) {
    using namespace openspace::properties;
    using T = glm::mat2x4;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen), dis(gen),
                  dis(gen), dis(gen), dis(gen), dis(gen)
        );

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, Mat3x2) {
    using namespace openspace::properties;
    using T = glm::mat3x2;
    T val = T(1.f);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, Mat3x2Fuzz) {
    using namespace openspace::properties;
    using T = glm::mat3x2;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen), dis(gen), dis(gen), dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, Mat3x3) {
    using namespace openspace::properties;
    using T = glm::mat3x3;
    T val = T(1.f);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, Mat3x3Fuzz) {
    using namespace openspace::properties;
    using T = glm::mat3x3;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen),
                  dis(gen), dis(gen), dis(gen),
                  dis(gen), dis(gen), dis(gen)
        );

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, Mat3x4) {
    using namespace openspace::properties;
    using T = glm::mat3x4;
    T val = T(1.f);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, Mat3x4Fuzz) {
    using namespace openspace::properties;
    using T = glm::mat3x4;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen), dis(gen),
                  dis(gen), dis(gen), dis(gen), dis(gen),
                  dis(gen), dis(gen), dis(gen), dis(gen)
        );

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, Mat4x2) {
    using namespace openspace::properties;
    using T = glm::mat4x2;
    T val = T(1.f);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, Mat4x2Fuzz) {
    using namespace openspace::properties;
    using T = glm::mat4x2;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen),
                  dis(gen), dis(gen),
                  dis(gen), dis(gen),
                  dis(gen), dis(gen)
        );

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, Mat4x3) {
    using namespace openspace::properties;
    using T = glm::mat4x3;
    T val = T(1.f);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, Mat4x3Fuzz) {
    using namespace openspace::properties;
    using T = glm::mat4x3;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen),
                  dis(gen), dis(gen), dis(gen),
                  dis(gen), dis(gen), dis(gen),
                  dis(gen), dis(gen), dis(gen)
        );

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, Mat4x4) {
    using namespace openspace::properties;
    using T = glm::mat4x4;
    T val = T(1.f);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, Mat4x4Fuzz) {
    using namespace openspace::properties;
    using T = glm::mat4x4;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen), dis(gen),
                  dis(gen), dis(gen), dis(gen), dis(gen),
                  dis(gen), dis(gen), dis(gen), dis(gen),
                  dis(gen), dis(gen), dis(gen), dis(gen)
        );

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, DMat2x2) {
    using namespace openspace::properties;
    using T = glm::dmat2x2;
    T val = T(1.f);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, DMat2x2Fuzz) {
    using namespace openspace::properties;
    using T = glm::dmat2x2;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen), dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, DMat2x3) {
    using namespace openspace::properties;
    using T = glm::dmat2x3;
    T val = T(1.f);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, DMat2x3Fuzz) {
    using namespace openspace::properties;
    using T = glm::dmat2x3;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen), dis(gen), dis(gen), dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, DMat2x4) {
    using namespace openspace::properties;
    using T = glm::dmat2x4;
    T val = T(1.f);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, DMat2x4Fuzz) {
    using namespace openspace::properties;
    using T = glm::dmat2x4;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen), dis(gen),
                  dis(gen), dis(gen), dis(gen), dis(gen)
        );

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, DMat3x2) {
    using namespace openspace::properties;
    using T = glm::dmat3x2;
    T val = T(1.f);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, DMat3x2Fuzz) {
    using namespace openspace::properties;
    using T = glm::dmat3x2;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen), dis(gen), dis(gen), dis(gen));

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, DMat3x3) {
    using namespace openspace::properties;
    using T = glm::dmat3x3;
    T val = T(1.f);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, DMat3x3Fuzz) {
    using namespace openspace::properties;
    using T = glm::dmat3x3;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen),
                  dis(gen), dis(gen), dis(gen),
                  dis(gen), dis(gen), dis(gen)
        );

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, DMat3x4) {
    using namespace openspace::properties;
    using T = glm::dmat3x4;
    T val = T(1.f);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, DMat3x4Fuzz) {
    using namespace openspace::properties;
    using T = glm::dmat3x4;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen), dis(gen),
                  dis(gen), dis(gen), dis(gen), dis(gen),
                  dis(gen), dis(gen), dis(gen), dis(gen)
        );

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, DMat4x2) {
    using namespace openspace::properties;
    using T = glm::dmat4x2;
    T val = T(1.f);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, DMat4x2Fuzz) {
    using namespace openspace::properties;
    using T = glm::dmat4x2;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen),
                  dis(gen), dis(gen),
                  dis(gen), dis(gen),
                  dis(gen), dis(gen)
        );

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, DMat4x3) {
    using namespace openspace::properties;
    using T = glm::dmat4x3;
    T val = T(1.f);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, DMat4x3Fuzz) {
    using namespace openspace::properties;
    using T = glm::dmat4x3;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen),
                  dis(gen), dis(gen), dis(gen),
                  dis(gen), dis(gen), dis(gen),
                  dis(gen), dis(gen), dis(gen)
        );

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, DMat4x4) {
    using namespace openspace::properties;
    using T = glm::dmat4x4;
    T val = T(1.f);

    bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
        state,
        val
    );
    EXPECT_TRUE(success) << "toLuaValue";
    T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
        state,
        success
    );
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, val) << "fromLuaValue";
}

TEST_F(LuaConversionTest, DMat4x4Fuzz) {
    using namespace openspace::properties;
    using T = glm::dmat4x4;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T::value_type> dis(
        T::value_type(0),
        std::numeric_limits<T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; ++i) {
        T val = T(dis(gen), dis(gen), dis(gen), dis(gen),
                  dis(gen), dis(gen), dis(gen), dis(gen),
                  dis(gen), dis(gen), dis(gen), dis(gen),
                  dis(gen), dis(gen), dis(gen), dis(gen)
        );

        bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(
            state,
            val
        );
        EXPECT_TRUE(success) << "toLuaValue";
        T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(
            state,
            success
        );
        EXPECT_TRUE(success) << "fromLuaValue";
        EXPECT_EQ(value, val) << "fromLuaValue";
    }
}

TEST_F(LuaConversionTest, String) {
    using namespace openspace::properties;
    bool success
          = PropertyDelegate<TemplateProperty<std::string>>::toLuaValue<std::string>(
                state, "value");
    EXPECT_TRUE(success) << "toLuaValue";
    std::string value = "";
    value = PropertyDelegate<TemplateProperty<std::string>>::fromLuaValue<std::string>(
          state, success);
    EXPECT_TRUE(success) << "fromLuaValue";
    EXPECT_EQ(value, "value") << "fromLuaValue";
}
