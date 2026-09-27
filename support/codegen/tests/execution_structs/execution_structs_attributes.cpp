/*****************************************************************************************
 *                                                                                       *
 * OpenSpace Codegen                                                                     *
 *                                                                                       *
 * Copyright (c) 2021-2026                                                               *
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

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/documentationengine.h>
#include <openspace/documentation/verifier.h>
#include <openspace/misc/dictionary.h>
#include <filesystem>
#include <fstream>
#include <optional>
#include <variant>
#include <vector>

using namespace openspace;

namespace {
    std::string description = "Description";

    std::vector<std::string> List = { "F", "G", "H", "I", "J" };

    struct [[codegen::Dictionary(Attributes)]] Parameters {
        // keyValue documentation
        float keyValue [[codegen::key("KeyKey")]];

        // keyValueOptional documentation
        std::optional<float> keyValueOptional [[codegen::key("KeyKeyOptional")]];

        // keyValueVector documentation
        std::vector<float> keyValueVector [[codegen::key("KeyKeyVector")]];

        // inRangeValueInt documentation
        int inRangeValueInt [[codegen::inrange(-2, 8)]];

        // inRangeValueIntOptional documentation
        std::optional<int> inRangeValueIntOptional [[codegen::inrange(-2, 8)]];

        // inRangeValueIntVector documentation
        std::vector<int> inRangeValueIntVector [[codegen::inrange(-2, 8)]];

        // inRangeValueFloat documentation
        float inRangeValueFloat [[codegen::inrange(8.f, 9.f)]];

        // inRangeValueFloatOptional documentation
        std::optional<float> inRangeValueFloatOptional [[codegen::inrange(8.f, 9.f)]];

        // inRangeValueFloatVector documentation
        std::vector<float> inRangeValueFloatVector [[codegen::inrange(8.f, 9.f)]];

        // inRangeValueVec2 documentation
        glm::vec2 inRangeValueVec2 [[codegen::inrange(glm::vec2(1.f), glm::vec2(2.f))]];

        // inRangeValueVec2Optional documentation
        std::optional<glm::vec2> inRangeValueVec2Optional
            [[codegen::inrange(glm::vec2(1.f), glm::vec2(2.f))]];

        // inRangeValueVec2Vector documentation
        std::vector<glm::vec2> inRangeValueVec2Vector
            [[codegen::inrange(glm::vec2(1.f), glm::vec2(2.f))]];

        // inRangeValueVec3 documentation
        glm::vec3 inRangeValueVec3 [[codegen::inrange(glm::vec3(1.f), glm::vec3(2.f))]];

        // inRangeValueVec3Optional documentation
        std::optional<glm::vec3> inRangeValueVec3Optional
            [[codegen::inrange(glm::vec3(1.f), glm::vec3(2.f))]];

        // inRangeValueVec3Vector documentation
        std::vector<glm::vec3> inRangeValueVec3Vector
            [[codegen::inrange(glm::vec3(1.f), glm::vec3(2.f))]];

        // inRangeValueVec4 documentation
        glm::vec4 inRangeValueVec4 [[codegen::inrange(glm::vec4(1.f), glm::vec4(2.f))]];

        // inRangeValueVec4Optional documentation
        std::optional<glm::vec4> inRangeValueVec4Optional
            [[codegen::inrange(glm::vec4(1.f), glm::vec4(2.f))]];

        // inRangeValueVec4Vector documentation
        std::vector<glm::vec4> inRangeValueVec4Vector
            [[codegen::inrange(glm::vec4(1.f), glm::vec4(2.f))]];

        // inRangeValueIVec2 documentation
        glm::ivec2 inRangeValueIVec2 [[codegen::inrange(glm::ivec2(1), glm::ivec2(3))]];

        // inRangeValueIVec2Optional documentation
        std::optional<glm::ivec2> inRangeValueIVec2Optional
            [[codegen::inrange(glm::ivec2(1), glm::ivec2(3))]];

        // inRangeValueIVec2Vector documentation
        std::vector<glm::ivec2> inRangeValueIVec2Vector
            [[codegen::inrange(glm::ivec2(1), glm::ivec2(3))]];

        // inRangeValueIVec3 documentation
        glm::ivec3 inRangeValueIVec3 [[codegen::inrange(glm::ivec3(1), glm::ivec3(3))]];

        // inRangeValueIVec3Optional documentation
        std::optional<glm::ivec3> inRangeValueIVec3Optional
            [[codegen::inrange(glm::ivec3(1), glm::ivec3(3))]];

        // inRangeValueIVec3Vector documentation
        std::vector<glm::ivec3> inRangeValueIVec3Vector
            [[codegen::inrange(glm::ivec3(1), glm::ivec3(3))]];

        // inRangeValueIVec4 documentation
        glm::ivec4 inRangeValueIVec4 [[codegen::inrange(glm::ivec4(1), glm::ivec4(3))]];

        // inRangeValueIVec4Optional documentation
        std::optional<glm::ivec4> inRangeValueIVec4Optional
            [[codegen::inrange(glm::ivec4(1), glm::ivec4(3))]];

        // inRangeValueIVec4Vector documentation
        std::vector<glm::ivec4> inRangeValueIVec4Vector
            [[codegen::inrange(glm::ivec4(1), glm::ivec4(3))]];


        // notInRangeValueInt documentation
        int notInRangeValueInt [[codegen::notinrange(5, 7)]];

        // notInRangeValueIntOptional documentation
        std::optional<int> notInRangeValueIntOptional [[codegen::notinrange(5, 7)]];

        // notInRangeValueIntVector documentation
        std::vector<int> notInRangeValueIntVector [[codegen::notinrange(5, 7)]];

        // notInRangeValueFloat documentation
        float notInRangeValueFloat [[codegen::notinrange(-0.5f, 0.5f)]];

        // notInRangeValueFloatOptional documentation
        std::optional<float> notInRangeValueFloatOptional
            [[codegen::notinrange(-0.5f, 0.5f)]];

        // notInRangeValueFloatVector documentation
        std::vector<float> notInRangeValueFloatVector
            [[codegen::notinrange(-0.5f, 0.5f)]];

        // notInRangeValueVec2 documentation
        glm::vec2 notInRangeValueVec2
            [[codegen::notinrange(glm::vec2(1.f), glm::vec2(2.f))]];

        // notInRangeValueVec2Optional documentation
        std::optional<glm::vec2> notInRangeValueVec2Optional
            [[codegen::notinrange(glm::vec2(1.f), glm::vec2(2.f))]];

        // notInRangeValueVec2Vector documentation
        std::vector<glm::vec2> notInRangeValueVec2Vector
            [[codegen::notinrange(glm::vec2(1.f), glm::vec2(2.f))]];

        // notInRangeValueVec3 documentation
        glm::vec3 notInRangeValueVec3
            [[codegen::notinrange(glm::vec3(1.f), glm::vec3(2.f))]];

        // notInRangeValueVec3Optional documentation
        std::optional<glm::vec3> notInRangeValueVec3Optional
            [[codegen::notinrange(glm::vec3(1.f), glm::vec3(2.f))]];

        // notInRangeValueVec3Vector documentation
        std::vector<glm::vec3> notInRangeValueVec3Vector
            [[codegen::notinrange(glm::vec3(1.f), glm::vec3(2.f))]];

        // notInRangeValueVec4 documentation
        glm::vec4 notInRangeValueVec4
            [[codegen::notinrange(glm::vec4(1.f), glm::vec4(2.f))]];

        // notInRangeValueVec4Optional documentation
        std::optional<glm::vec4> notInRangeValueVec4Optional
            [[codegen::notinrange(glm::vec4(1.f), glm::vec4(2.f))]];

        // notInRangeValueVec4Vector documentation
        std::vector<glm::vec4> notInRangeValueVec4Vector
            [[codegen::notinrange(glm::vec4(1.f), glm::vec4(2.f))]];

        // notInRangeValueIVec2 documentation
        glm::ivec2 notInRangeValueIVec2
            [[codegen::notinrange(glm::ivec2(1), glm::ivec2(3))]];

        // notInRangeValueIVec2Optional documentation
        std::optional<glm::ivec2> notInRangeValueIVec2Optional
            [[codegen::notinrange(glm::ivec2(1), glm::ivec2(3))]];

        // notInRangeValueIVec2Vector documentation
        std::vector<glm::ivec2> notInRangeValueIVec2Vector
            [[codegen::notinrange(glm::ivec2(1), glm::ivec2(3))]];

        // notInRangeValueIVec3 documentation
        glm::ivec3 notInRangeValueIVec3
            [[codegen::notinrange(glm::ivec3(1), glm::ivec3(3))]];

        // notInRangeValueIVec3Optional documentation
        std::optional<glm::ivec3> notInRangeValueIVec3Optional
            [[codegen::notinrange(glm::ivec3(1), glm::ivec3(3))]];

        // notInRangeValueIVec3Vector documentation
        std::vector<glm::ivec3> notInRangeValueIVec3Vector
            [[codegen::notinrange(glm::ivec3(1), glm::ivec3(3))]];

        // notInRangeValueIVec4 documentation
        glm::ivec4 notInRangeValueIVec4
            [[codegen::notinrange(glm::ivec4(1), glm::ivec4(3))]];

        // notInRangeValueIVec4Optional documentation
        std::optional<glm::ivec4> notInRangeValueIVec4Optional
            [[codegen::notinrange(glm::ivec4(1), glm::ivec4(3))]];

        // notInRangeValueIVec4Vector documentation
        std::vector<glm::ivec4> notInRangeValueIVec4Vector
            [[codegen::notinrange(glm::ivec4(1), glm::ivec4(3))]];


        // lessValueInt documentation
        int lessValueInt [[codegen::less(20)]];

        // lessValueIntOptional documentation
        std::optional<int> lessValueIntOptional [[codegen::less(20)]];

        // lessValueIntVector documentation
        std::vector<int> lessValueIntVector [[codegen::less(20)]];

        // lessValueFloat documentation
        float lessValueFloat [[codegen::less(21.f)]];

        // lessValueFloatOptional documentation
        std::optional<float> lessValueFloatOptional [[codegen::less(22.f)]];

        // lessValueFloatVector documentation
        std::vector<float> lessValueFloatVector [[codegen::less(23.f)]];

        // lessValueVec2 documentation
        glm::vec2 lessValueVec2 [[codegen::less(glm::vec2(1.f))]];

        // lessValueVec2Optional documentation
        std::optional<glm::vec2> lessValueVec2Optional [[codegen::less(glm::vec2(1.f))]];

        // lessValueVec2Vector documentation
        std::vector<glm::vec2> lessValueVec2Vector [[codegen::less(glm::vec2(1.f))]];

        // lessValueVec3 documentation
        glm::vec3 lessValueVec3 [[codegen::less(glm::vec3(1.f))]];

        // lessValueVec3Optional documentation
        std::optional<glm::vec3> lessValueVec3Optional [[codegen::less(glm::vec3(1.f))]];

        // lessValueVec3Vector documentation
        std::vector<glm::vec3> lessValueVec3Vector [[codegen::less(glm::vec3(1.f))]];

        // lessValueVec4 documentation
        glm::vec4 lessValueVec4 [[codegen::less(glm::vec4(1.f))]];

        // lessValueVec4Optional documentation
        std::optional<glm::vec4> lessValueVec4Optional [[codegen::less(glm::vec4(1.f))]];

        // lessValueVec4Vector documentation
        std::vector<glm::vec4> lessValueVec4Vector [[codegen::less(glm::vec4(1.f))]];

        // lessValueIVec2 documentation
        glm::ivec2 lessValueIVec2 [[codegen::less(glm::ivec2(1))]];

        // lessValueIVec2Optional documentation
        std::optional<glm::ivec2> lessValueIVec2Optional
            [[codegen::less(glm::ivec2(1))]];

        // lessValueIVec2Vector documentation
        std::vector<glm::ivec2> lessValueIVec2Vector [[codegen::less(glm::ivec2(1))]];

        // lessValueIVec3 documentation
        glm::ivec3 lessValueIVec3 [[codegen::less(glm::ivec3(1))]];

        // lessValueIVec3Optional documentation
        std::optional<glm::ivec3> lessValueIVec3Optional [[codegen::less(glm::ivec3(1))]];

        // lessValueIVec3Vector documentation
        std::vector<glm::ivec3> lessValueIVec3Vector [[codegen::less(glm::ivec3(1))]];

        // lessValueIVec4 documentation
        glm::ivec4 lessValueIVec4 [[codegen::less(glm::ivec4(1))]];

        // lessValueIVec4Optional documentation
        std::optional<glm::ivec4> lessValueIVec4Optional [[codegen::less(glm::ivec4(1))]];

        // lessValueIVec4Vector documentation
        std::vector<glm::ivec4> lessValueIVec4Vector [[codegen::less(glm::ivec4(1))]];


        // lessEqualValueInt documentation
        int lessEqualValueInt [[codegen::lessequal(25)]];

        // lessEqualValueIntOptional documentation
        std::optional<int> lessEqualValueIntOptional [[codegen::lessequal(25)]];

        // lessEqualValueIntVector documentation
        std::vector<int> lessEqualValueIntVector [[codegen::lessequal(25)]];

        // lessEqualValueFloat documentation
        float lessEqualValueFloat [[codegen::lessequal(27.f)]];

        // lessEqualValueFloatOptional documentation
        std::optional<float> lessEqualValueFloatOptional [[codegen::lessequal(27.f)]];

        // lessEqualValueFloatVector documentation
        std::vector<float> lessEqualValueFloatVector [[codegen::lessequal(27.f)]];

        // lessEqualValueVec2 documentation
        glm::vec2 lessEqualValueVec2 [[codegen::lessequal(glm::vec2(1.f))]];

        // lessEqualValueVec2Optional documentation
        std::optional<glm::vec2> lessEqualValueVec2Optional
            [[codegen::lessequal(glm::vec2(1.f))]];

        // lessEqualValueVec2Vector documentation
        std::vector<glm::vec2> lessEqualValueVec2Vector
            [[codegen::lessequal(glm::vec2(1.f))]];

        // lessEqualValueVec3 documentation
        glm::vec3 lessEqualValueVec3 [[codegen::lessequal(glm::vec3(1.f))]];

        // lessEqualValueVec3Optional documentation
        std::optional<glm::vec3> lessEqualValueVec3Optional
            [[codegen::lessequal(glm::vec3(1.f))]];

        // lessEqualValueVec3Vector documentation
        std::vector<glm::vec3> lessEqualValueVec3Vector
            [[codegen::lessequal(glm::vec3(1.f))]];

        // lessEqualValueVec4 documentation
        glm::vec4 lessEqualValueVec4 [[codegen::lessequal(glm::vec4(1.f))]];

        // lessEqualValueVec4Optional documentation
        std::optional<glm::vec4> lessEqualValueVec4Optional
            [[codegen::lessequal(glm::vec4(1.f))]];

        // lessEqualValueVec4Vector documentation
        std::vector<glm::vec4> lessEqualValueVec4Vector
            [[codegen::lessequal(glm::vec4(1.f))]];

        // lessEqualValueIVec2 documentation
        glm::ivec2 lessEqualValueIVec2 [[codegen::lessequal(glm::ivec2(1))]];

        // lessEqualValueIVec2Optional documentation
        std::optional<glm::ivec2> lessEqualValueIVec2Optional
            [[codegen::lessequal(glm::ivec2(1))]];

        // lessEqualValueIVec2Vector documentation
        std::vector<glm::ivec2> lessEqualValueIVec2Vector
            [[codegen::lessequal(glm::ivec2(1))]];

        // lessEqualValueIVec3 documentation
        glm::ivec3 lessEqualValueIVec3 [[codegen::lessequal(glm::ivec3(1))]];

        // lessEqualValueIVec3Optional documentation
        std::optional<glm::ivec3> lessEqualValueIVec3Optional
            [[codegen::lessequal(glm::ivec3(1))]];

        // lessEqualValueIVec3Vector documentation
        std::vector<glm::ivec3> lessEqualValueIVec3Vector
            [[codegen::lessequal(glm::ivec3(1))]];

        // lessEqualValueIVec4 documentation
        glm::ivec4 lessEqualValueIVec4 [[codegen::lessequal(glm::ivec4(1))]];

        // lessEqualValueIVec4Optional documentation
        std::optional<glm::ivec4> lessEqualValueIVec4Optional
            [[codegen::lessequal(glm::ivec4(1))]];

        // lessEqualValueIVec4Vector documentation
        std::vector<glm::ivec4> lessEqualValueIVec4Vector
            [[codegen::lessequal(glm::ivec4(1))]];


        // greaterValueInt documentation
        int greaterValueInt [[codegen::greater(25)]];

        // greaterValueIntOptional documentation
        std::optional<int> greaterValueIntOptional [[codegen::greater(25)]];

        // greaterValueIntVector documentation
        std::vector<int> greaterValueIntVector [[codegen::greater(25)]];

        // greaterValueFloat documentation
        float greaterValueFloat [[codegen::greater(-5.f)]];

        // greaterValueFloatOptional documentation
        std::optional<float> greaterValueFloatOptional [[codegen::greater(-5.f)]];

        // greaterValueFloatVector documentation
        std::vector<float> greaterValueFloatVector [[codegen::greater(-5.f)]];

        // greaterValueVec2 documentation
        glm::vec2 greaterValueVec2 [[codegen::greater(glm::vec2(1.f))]];

        // greaterValueVec2Optional documentation
        std::optional<glm::vec2> greaterValueVec2Optional
            [[codegen::greater(glm::vec2(1.f))]];

        // greaterValueVec2Vector documentation
        std::vector<glm::vec2> greaterValueVec2Vector
            [[codegen::greater(glm::vec2(1.f))]];

        // greaterValueVec3 documentation
        glm::vec3 greaterValueVec3 [[codegen::greater(glm::vec3(1.f))]];

        // greaterValueVec3Optional documentation
        std::optional<glm::vec3> greaterValueVec3Optional
            [[codegen::greater(glm::vec3(1.f))]];

        // greaterValueVec3Vector documentation
        std::vector<glm::vec3> greaterValueVec3Vector
            [[codegen::greater(glm::vec3(1.f))]];

        // greaterValueVec4 documentation
        glm::vec4 greaterValueVec4 [[codegen::greater(glm::vec4(1.f))]];

        // greaterValueVec4Optional documentation
        std::optional<glm::vec4> greaterValueVec4Optional
            [[codegen::greater(glm::vec4(1.f))]];

        // greaterValueVec4Vector documentation
        std::vector<glm::vec4> greaterValueVec4Vector
            [[codegen::greater(glm::vec4(1.f))]];

        // greaterValueIVec2 documentation
        glm::ivec2 greaterValueIVec2 [[codegen::greater(glm::ivec2(1))]];

        // greaterValueIVec2Optional documentation
        std::optional<glm::ivec2> greaterValueIVec2Optional
            [[codegen::greater(glm::ivec2(1))]];

        // greaterValueIVec2Vector documentation
        std::vector<glm::ivec2> greaterValueIVec2Vector
            [[codegen::greater(glm::ivec2(1))]];

        // greaterValueIVec3 documentation
        glm::ivec3 greaterValueIVec3 [[codegen::greater(glm::ivec3(1))]];

        // greaterValueIVec3Optional documentation
        std::optional<glm::ivec3> greaterValueIVec3Optional
            [[codegen::greater(glm::ivec3(1))]];

        // greaterValueIVec3Vector documentation
        std::vector<glm::ivec3> greaterValueIVec3Vector
            [[codegen::greater(glm::ivec3(1))]];

        // greaterValueIVec4 documentation
        glm::ivec4 greaterValueIVec4 [[codegen::greater(glm::ivec4(1))]];

        // greaterValueIVec4Optional documentation
        std::optional<glm::ivec4> greaterValueIVec4Optional
            [[codegen::greater(glm::ivec4(1))]];

        // greaterValueIVec4Vector documentation
        std::vector<glm::ivec4> greaterValueIVec4Vector
            [[codegen::greater(glm::ivec4(1))]];


        // greaterEqualValueInt documentation
        int greaterEqualValueInt [[codegen::greaterequal(33)]];

        // greaterEqualValueIntOptional documentation
        std::optional<int> greaterEqualValueIntOptional [[codegen::greaterequal(33)]];

        // greaterEqualValueIntVector documentation
        std::vector<int> greaterEqualValueIntVector [[codegen::greaterequal(33)]];

        // greaterEqualValueFloat documentation
        float greaterEqualValueFloat [[codegen::greaterequal(-25.f)]];

        // greaterEqualValueFloatOptional documentation
        std::optional<float> greaterEqualValueFloatOptional
            [[codegen::greaterequal(-25.f)]];

        // greaterEqualValueFloatVector documentation
        std::vector<float> greaterEqualValueFloatVector [[codegen::greaterequal(-25.f)]];

        // greaterEqualValueVec2 documentation
        glm::vec2 greaterEqualValueVec2 [[codegen::greaterequal(glm::vec2(1.f))]];

        // greaterEqualValueVec2Optional documentation
        std::optional<glm::vec2> greaterEqualValueVec2Optional
            [[codegen::greaterequal(glm::vec2(1.f))]];

        // greaterEqualValueVec2Vector documentation
        std::vector<glm::vec2> greaterEqualValueVec2Vector
            [[codegen::greaterequal(glm::vec2(1.f))]];

        // greaterEqualValueVec3 documentation
        glm::vec3 greaterEqualValueVec3 [[codegen::greaterequal(glm::vec3(1.f))]];

        // greaterEqualValueVec3Optional documentation
        std::optional<glm::vec3> greaterEqualValueVec3Optional
            [[codegen::greaterequal(glm::vec3(1.f))]];

        // greaterEqualValueVec3Vector documentation
        std::vector<glm::vec3> greaterEqualValueVec3Vector
            [[codegen::greaterequal(glm::vec3(1.f))]];

        // greaterEqualValueVec4 documentation
        glm::vec4 greaterEqualValueVec4 [[codegen::greaterequal(glm::vec4(1.f))]];

        // greaterEqualValueVec4Optional documentation
        std::optional<glm::vec4> greaterEqualValueVec4Optional
            [[codegen::greaterequal(glm::vec4(1.f))]];

        // greaterEqualValueVec4Vector documentation
        std::vector<glm::vec4> greaterEqualValueVec4Vector
            [[codegen::greaterequal(glm::vec4(1.f))]];

        // greaterEqualValueIVec2 documentation
        glm::ivec2 greaterEqualValueIVec2 [[codegen::greaterequal(glm::ivec2(1))]];

        // greaterEqualValueIVec2Optional documentation
        std::optional<glm::ivec2> greaterEqualValueIVec2Optional
            [[codegen::greaterequal(glm::ivec2(1))]];

        // greaterEqualValueIVec2Vector documentation
        std::vector<glm::ivec2> greaterEqualValueIVec2Vector
            [[codegen::greaterequal(glm::ivec2(1))]];

        // greaterEqualValueIVec3 documentation
        glm::ivec3 greaterEqualValueIVec3 [[codegen::greaterequal(glm::ivec3(1))]];

        // greaterEqualValueIVec3Optional documentation
        std::optional<glm::ivec3> greaterEqualValueIVec3Optional
            [[codegen::greaterequal(glm::ivec3(1))]];

        // greaterEqualValueIVec3Vector documentation
        std::vector<glm::ivec3> greaterEqualValueIVec3Vector
            [[codegen::greaterequal(glm::ivec3(1))]];

        // greaterEqualValueIVec4 documentation
        glm::ivec4 greaterEqualValueIVec4 [[codegen::greaterequal(glm::ivec4(1))]];

        // greaterEqualValueIVec4Optional documentation
        std::optional<glm::ivec4> greaterEqualValueIVec4Optional
            [[codegen::greaterequal(glm::ivec4(1))]];

        // greaterEqualValueIVec4Vector documentation
        std::vector<glm::ivec4> greaterEqualValueIVec4Vector
            [[codegen::greaterequal(glm::ivec4(1))]];


        // unequalValueInt documentation
        int unequalValueInt [[codegen::unequal(1)]];

        // unequalValueIntOptional documentation
        std::optional<int> unequalValueIntOptional [[codegen::unequal(1)]];

        // unequalValueIntVector documentation
        std::vector<int> unequalValueIntVector [[codegen::unequal(1)]];

        // unequalValueFloat documentation
        float unequalValueFloat [[codegen::unequal(123.f)]];

        // unequalValueFloatOptional documentation
        std::optional<float> unequalValueFloatOptional [[codegen::unequal(123.f)]];

        // unequalValueFloatVector documentation
        std::vector<float> unequalValueFloatVector [[codegen::unequal(123.f)]];

        // unequalValueVec2 documentation
        glm::vec2 unequalValueVec2 [[codegen::unequal(glm::vec2(1.f))]];

        // unequalValueVec2Optional documentation
        std::optional<glm::vec2> unequalValueVec2Optional
            [[codegen::unequal(glm::vec2(1.f))]];

        // unequalValueVec2Vector documentation
        std::vector<glm::vec2> unequalValueVec2Vector
            [[codegen::unequal(glm::vec2(1.f))]];

        // unequalValueVec3 documentation
        glm::vec3 unequalValueVec3 [[codegen::unequal(glm::vec3(1.f))]];

        // unequalValueVec3Optional documentation
        std::optional<glm::vec3> unequalValueVec3Optional
            [[codegen::unequal(glm::vec3(1.f))]];

        // unequalValueVec3Vector documentation
        std::vector<glm::vec3> unequalValueVec3Vector
            [[codegen::unequal(glm::vec3(1.f))]];

        // unequalValueVec4 documentation
        glm::vec4 unequalValueVec4 [[codegen::unequal(glm::vec4(1.f))]];

        // unequalValueVec4Optional documentation
        std::optional<glm::vec4> unequalValueVec4Optional
            [[codegen::unequal(glm::vec4(1.f))]];

        // unequalValueVec4Vector documentation
        std::vector<glm::vec4> unequalValueVec4Vector
            [[codegen::unequal(glm::vec4(1.f))]];

        // unequalValueIVec2 documentation
        glm::ivec2 unequalValueIVec2 [[codegen::unequal(glm::ivec2(1))]];

        // unequalValueIVec2Optional documentation
        std::optional<glm::ivec2> unequalValueIVec2Optional
            [[codegen::unequal(glm::ivec2(1))]];

        // unequalValueIVec2Vector documentation
        std::vector<glm::ivec2> unequalValueIVec2Vector
            [[codegen::unequal(glm::ivec2(1))]];

        // unequalValueIVec3 documentation
        glm::ivec3 unequalValueIVec3 [[codegen::unequal(glm::ivec3(1))]];

        // unequalValueIVec3Optional documentation
        std::optional<glm::ivec3> unequalValueIVec3Optional
            [[codegen::unequal(glm::ivec3(1))]];

        // unequalValueIVec3Vector documentation
        std::vector<glm::ivec3> unequalValueIVec3Vector
            [[codegen::unequal(glm::ivec3(1))]];

        // unequalValueIVec4 documentation
        glm::ivec4 unequalValueIVec4 [[codegen::unequal(glm::ivec4(1))]];

        // unequalValueIVec4Optional documentation
        std::optional<glm::ivec4> unequalValueIVec4Optional
            [[codegen::unequal(glm::ivec4(1))]];

        // unequalValueIVec4Vector documentation
        std::vector<glm::ivec4> unequalValueIVec4Vector
            [[codegen::unequal(glm::ivec4(1))]];


        // [[codegen::verbatim(description)]]
        bool descValue;


        // inListValue1 documentation
        std::string inListValue1 [[codegen::inlist("A", "B", "C", "D", "E")]];

        // inListValue1Optional documentation
        std::optional<std::string> inListValue1Optional
            [[codegen::inlist("A", "B", "C", "D", "E")]];

        // inListValue1Vector documentation
        std::vector<std::string> inListValue1Vector
            [[codegen::inlist("A", "B", "C", "D", "E")]];

        // inListValue2 documentation
        std::string inListValue2 [[codegen::inlist(List)]];

        // inListValue2Optional documentation
        std::optional<std::string> inListValue2Optional [[codegen::inlist(List)]];

        // inListValue2Vector documentation
        std::vector<std::string> inListValue2Vector [[codegen::inlist(List)]];


        // notInListValue1 documentation
        std::string notInListValue1 [[codegen::notinlist("A", "B", "C", "D", "E")]];

        // notInListValue1Optional documentation
        std::optional<std::string> notInListValue1Optional
            [[codegen::notinlist("A", "B", "C", "D", "E")]];

        // notInListValue1Vector documentation
        std::vector<std::string> notInListValue1Vector
            [[codegen::notinlist("A", "B", "C", "D", "E")]];

        // notInListValue2 documentation
        std::string notInListValue2 [[codegen::notinlist(List)]];

        // notInListValue2Optional documentation
        std::optional<std::string> notInListValue2Optional [[codegen::notinlist(List)]];

        // notInListValue2Vector documentation
        std::vector<std::string> notInListValue2Vector [[codegen::notinlist(List)]];


        // unequalValueString documentation
        std::string unequalValueString [[codegen::unequal("abcdef")]];

        // unequalValueStringOptional documentation
        std::optional<std::string> unequalValueStringOptional
            [[codegen::unequal("abcdef")]];

        // unequalValueStringVector documentation
        std::vector<std::string> unequalValueStringVector
            [[codegen::unequal("abcdef")]];


        // string not empty value documentation
        std::string notEmptyString [[codegen::notempty()]];

        // string not empty optional value documentation
        std::optional<std::string> notEmptyStringOptional [[codegen::notempty()]];

        // string not empty vector value documentation
        std::vector<std::string> notEmptyStringVector [[codegen::notempty()]];

        // string not empty optional vector value documentation
        std::optional<std::vector<std::string>> notEmptyStringOptionalVector
            [[codegen::notempty()]];


        // referenceValueOptional documentation
        std::optional<ghoul::Dictionary> referenceValueOptional
            [[codegen::reference("abc")]];

        // referenceValueVector documentation
        std::optional<std::vector<ghoul::Dictionary>> referenceValueVector
            [[codegen::reference("abc")]];

        // dictValue documentation
        ghoul::Dictionary dictValue [[codegen::reference("abc")]];

        // dictValueVector documentation
        std::vector<ghoul::Dictionary> dictValueVector [[codegen::reference("abc")]];

        // dictValueOptional documentation
        std::optional<ghoul::Dictionary> dictValueOptional [[codegen::reference("abc")]];

        // dictValueMap documentation
        std::map<std::string, ghoul::Dictionary> dictValueMap
            [[codegen::reference("abc")]];

        // vectorDictValueMap documentation
        std::vector<std::map<std::string, ghoul::Dictionary>> vectorDictValueMap
            [[codegen::reference("abc")]];

        // optionalDictValueMap documentation
        std::optional<std::map<std::string, ghoul::Dictionary>> optionalDictValueMap
            [[codegen::reference("abc")]];

        // optionalVectorDictValueMap documentation
        std::optional<std::vector<std::map<std::string, ghoul::Dictionary>>>
            optionalVectorDictValueMap [[codegen::reference("abc")]];

        // dictValueVectorMap documentation
        std::map<std::string, std::vector<ghoul::Dictionary>> dictValueVectorMap
            [[codegen::reference("abc")]];

        // vectorDictValueVectorMap documentation
        std::vector<std::map<std::string, std::vector<ghoul::Dictionary>>> vectorDictValueVectorMap
            [[codegen::reference("abc")]];

        // optionalDictValueVectorMap documentation
        std::optional<std::map<std::string, std::vector<ghoul::Dictionary>>> optionalDictValueVectorMap
            [[codegen::reference("abc")]];

        // optionalVectorDictValueVectorMap documentation
        std::optional<std::vector<std::map<std::string, std::vector<ghoul::Dictionary>>>>
            optionalVectorDictValueVectorMap [[codegen::reference("abc")]];

        // annotation documentation
        std::string annotation [[codegen::annotation("abc")]];

        // annotationOptional documentation
        std::optional<std::string> annotationOptional [[codegen::annotation("def")]];

        // annotationVector documentation
        std::vector<std::string> annotationVector [[codegen::annotation("ghi")]];


        // dcolor3Value documentation
        glm::dvec3 dcolor3Value [[codegen::color()]];

        // optionalDcolor3Value documentation
        std::optional<glm::dvec3> optionalDcolor3Value [[codegen::color()]];

        // vectorDcolor3Value documentation
        std::vector<glm::dvec3> vectorDcolor3Value [[codegen::color()]];

        // color3Value documentation
        glm::vec3 color3Value [[codegen::color()]];

        // optionalColor3Value documentation
        std::optional<glm::vec3> optionalColor3Value [[codegen::color()]];

        // vectorColor3Value documentation
        std::vector<glm::vec3> vectorColor3Value [[codegen::color()]];

        // dcolor4Value documentation
        glm::dvec4 dcolor4Value [[codegen::color()]];

        // optionalDcolor4Value documentation
        std::optional<glm::dvec4> optionalDcolor4Value [[codegen::color()]];

        // vectorDcolor4Value documentation
        std::vector<glm::dvec4> vectorDcolor4Value [[codegen::color()]];

        // color4Value documentation
        glm::vec4 color4Value [[codegen::color()]];

        // optionalColor4Value documentation
        std::optional<glm::vec4> optionalColor4Value [[codegen::color()]];

        // vectorColor4Value documentation
        std::vector<glm::vec4> vectorColor4Value [[codegen::color()]];


        // dateTime value documentation
        std::string dateTimeValue [[codegen::datetime()]];

        // optional dateTime value documentation
        std::optional<std::string> optionalDateTimeValue [[codegen::datetime()]];

        // vector dateTime value documentation
        std::vector<std::string> vectorDateTimeValue [[codegen::datetime()]];

        // optional vector dateTime value documentation
        std::optional<std::vector<std::string>> optionalVectorDateTimeValue
            [[codegen::datetime()]];


        // identifier value documentation
        std::string identifierValue [[codegen::identifier()]];

        // optional identifier value documentation
        std::optional<std::string> optionalIdentifierValue [[codegen::identifier()]];

        // vector identifier value documentation
        std::vector<std::string> vectorIdentifierValue [[codegen::identifier()]];

        // optional vector identifier value documentation
        std::optional<std::vector<std::string>> optionalVectorIdentifierValue
            [[codegen::identifier()]];

        // private value documentation
        std::string privateValue [[codegen::private()]];

        // optional private value documentation
        std::optional<std::string> optionalPrivateValue [[codegen::private()]];

        // vector private value documentation
        std::vector<std::string> vectorPrivateValue [[codegen::private()]];

        // optional vector private value documentation
        std::optional<std::vector<std::string>> optionalVectorPrivateValue [[codegen::private()]];


        // default mustexist documentation
        std::filesystem::path defaultMustExistValue;

        // default optional mustexist documentation
        std::optional<std::filesystem::path> defaultOptionalMustExistValue;

        // default vector mustexist documentation
        std::vector<std::filesystem::path> defaultVectorMustExistValue;

        // default optionalvector mustexist documentation
        std::optional<std::vector<std::filesystem::path>> defaultOptionalVectorMustExistValue;


        // yes mustexist documentation
        std::filesystem::path yesMustExistValue [[codegen::mustexist(true)]];

        // yes optional mustexist documentation
        std::optional<std::filesystem::path> yesOptionalMustExistValue [[codegen::mustexist(true)]];

        // yes vector mustexist documentation
        std::vector<std::filesystem::path> yesVectorMustExistValue [[codegen::mustexist(true)]];

        // yes optionalvector mustexist documentation
        std::optional<std::vector<std::filesystem::path>> yesOptionalVectorMustExistValue [[codegen::mustexist(true)]];


        // no mustexist documentation
        std::filesystem::path noMustExistValue [[codegen::mustexist(false)]];

        // no optional mustexist documentation
        std::optional<std::filesystem::path> noOptionalMustExistValue [[codegen::mustexist(false)]];

        // no vector mustexist documentation
        std::vector<std::filesystem::path> noVectorMustExistValue [[codegen::mustexist(false)]];

        // no optionalvector mustexist documentation
        std::optional<std::vector<std::filesystem::path>> noOptionalVectorMustExistValue [[codegen::mustexist(false)]];


        // default mustexist directory documentation
        std::filesystem::path defaultMustExistDirectoryValue [[codegen::directory()]];

        // default optional mustexist directory documentation
        std::optional<std::filesystem::path> defaultOptionalMustExistDirectoryValue [[codegen::directory()]];

        // default vector mustexist directory documentation
        std::vector<std::filesystem::path> defaultVectorMustExistDirectoryValue [[codegen::directory()]];

        // default optionalvector mustexist directory documentation
        std::optional<std::vector<std::filesystem::path>> defaultOptionalVectorMustExistDirectoryValue [[codegen::directory()]];


        // yes mustexist directory documentation
        std::filesystem::path yesMustExistDirectoryValue [[codegen::mustexist(true), codegen::directory()]];

        // yes optional mustexist directory documentation
        std::optional<std::filesystem::path> yesOptionalMustExistDirectoryValue [[codegen::mustexist(true), codegen::directory()]];

        // yes vector mustexist directory documentation
        std::vector<std::filesystem::path> yesVectorMustExistDirectoryValue [[codegen::mustexist(true), codegen::directory()]];

        // yes optionalvector mustexist directory documentation
        std::optional<std::vector<std::filesystem::path>> yesOptionalVectorMustExistDirectoryValue [[codegen::mustexist(true), codegen::directory()]];


        // no mustexist directory documentation
        std::filesystem::path noMustExistDirectoryValue [[codegen::mustexist(false), codegen::directory()]];

        // no optional mustexist directory documentation
        std::optional<std::filesystem::path> noOptionalMustExistDirectoryValue [[codegen::mustexist(false), codegen::directory()]];

        // no vector mustexist directory documentation
        std::vector<std::filesystem::path> noVectorMustExistDirectoryValue [[codegen::mustexist(false), codegen::directory()]];

        // no optionalvector mustexist directory documentation
        std::optional<std::vector<std::filesystem::path>> noOptionalVectorMustExistDirectoryValue [[codegen::mustexist(false), codegen::directory()]];
    };
} // namespace
#include "execution_structs_attributes_codegen.cpp"


TEST_CASE("Execution/Structs/Attributes:  Bake", "[Execution][Structs]") {
    using namespace std::string_literals;

    Documentation doc;
    doc.id = "abc";
    DocEng.addDocumentation(doc);

    ghoul::Dictionary d;
    d.setValue("KeyKey", 2.1);
    d.setValue("KeyKeyOptional", 2.2);
    {
        ghoul::Dictionary e;
        e.setValue("1", 2.3);
        e.setValue("2", 2.4);
        e.setValue("3", 2.5);
        d.setValue("KeyKeyVector", e);
    }
    d.setValue("InRangeValueInt", 3.0);
    d.setValue("InRangeValueIntOptional", 4.0);
    {
        ghoul::Dictionary e;
        e.setValue("1", 5.0);
        e.setValue("2", 6.0);
        e.setValue("3", 7.0);
        d.setValue("InRangeValueIntVector", e);
    }
    d.setValue("InRangeValueFloat", 8.1);
    d.setValue("InRangeValueFloatOptional", 8.2);
    {
        ghoul::Dictionary e;
        e.setValue("1", 8.3);
        e.setValue("2", 8.4);
        e.setValue("3", 8.5);
        d.setValue("InRangeValueFloatVector", e);
    }
    d.setValue("InRangeValueVec2", glm::dvec2(1.5));
    d.setValue("InRangeValueVec2Optional", glm::dvec2(1.5));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec2(1.25));
        e.setValue("2", glm::dvec2(1.5));
        e.setValue("3", glm::dvec2(1.75));
        d.setValue("InRangeValueVec2Vector", e);
    }
    d.setValue("InRangeValueVec3", glm::dvec3(1.5));
    d.setValue("InRangeValueVec3Optional", glm::dvec3(1.5));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec3(1.25));
        e.setValue("2", glm::dvec3(1.5));
        e.setValue("3", glm::dvec3(1.75));
        d.setValue("InRangeValueVec3Vector", e);
    }
    d.setValue("InRangeValueVec4", glm::dvec4(1.5));
    d.setValue("InRangeValueVec4Optional", glm::dvec4(1.5));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec4(1.25));
        e.setValue("2", glm::dvec4(1.5));
        e.setValue("3", glm::dvec4(1.75));
        d.setValue("InRangeValueVec4Vector", e);
    }
    d.setValue("InRangeValueIVec2", glm::dvec2(2));
    d.setValue("InRangeValueIVec2Optional", glm::dvec2(2));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec2(2.0));
        e.setValue("2", glm::dvec2(2.0));
        e.setValue("3", glm::dvec2(2.0));
        d.setValue("InRangeValueIVec2Vector", e);
    }
    d.setValue("InRangeValueIVec3", glm::dvec3(2.0));
    d.setValue("InRangeValueIVec3Optional", glm::dvec3(2.0));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec3(2.0));
        e.setValue("2", glm::dvec3(2.0));
        e.setValue("3", glm::dvec3(2.0));
        d.setValue("InRangeValueIVec3Vector", e);
    }
    d.setValue("InRangeValueIVec4", glm::dvec4(2.0));
    d.setValue("InRangeValueIVec4Optional", glm::dvec4(2.0));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec4(2.0));
        e.setValue("2", glm::dvec4(2.0));
        e.setValue("3", glm::dvec4(2.0));
        d.setValue("InRangeValueIVec4Vector", e);
    }

    d.setValue("NotInRangeValueInt", 9.0);
    d.setValue("NotInRangeValueIntOptional", 10.0);
    {
        ghoul::Dictionary e;
        e.setValue("1", 11.0);
        e.setValue("2", 12.0);
        e.setValue("3", 13.0);
        d.setValue("NotInRangeValueIntVector", e);
    }
    d.setValue("NotInRangeValueFloat", 14.1);
    d.setValue("NotInRangeValueFloatOptional", 14.2);
    {
        ghoul::Dictionary e;
        e.setValue("1", 14.3);
        e.setValue("2", 14.4);
        e.setValue("3", 14.5);
        d.setValue("NotInRangeValueFloatVector", e);
    }
    d.setValue("NotInRangeValueVec2", glm::dvec2(0.5));
    d.setValue("NotInRangeValueVec2Optional", glm::dvec2(0.5));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec2(0.25));
        e.setValue("2", glm::dvec2(0.5));
        e.setValue("3", glm::dvec2(0.75));
        d.setValue("NotInRangeValueVec2Vector", e);
    }
    d.setValue("NotInRangeValueVec3", glm::dvec3(0.5));
    d.setValue("NotInRangeValueVec3Optional", glm::dvec3(0.5));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec3(0.25));
        e.setValue("2", glm::dvec3(0.5));
        e.setValue("3", glm::dvec3(0.75));
        d.setValue("NotInRangeValueVec3Vector", e);
    }
    d.setValue("NotInRangeValueVec4", glm::dvec4(0.5));
    d.setValue("NotInRangeValueVec4Optional", glm::dvec4(0.5));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec4(0.25));
        e.setValue("2", glm::dvec4(0.5));
        e.setValue("3", glm::dvec4(0.75));
        d.setValue("NotInRangeValueVec4Vector", e);
    }
    d.setValue("NotInRangeValueIVec2", glm::dvec2(4.0));
    d.setValue("NotInRangeValueIVec2Optional", glm::dvec2(4.0));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec2(4.0));
        e.setValue("2", glm::dvec2(5.0));
        e.setValue("3", glm::dvec2(6.0));
        d.setValue("NotInRangeValueIVec2Vector", e);
    }
    d.setValue("NotInRangeValueIVec3", glm::dvec3(4.0));
    d.setValue("NotInRangeValueIVec3Optional", glm::dvec3(4.0));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec3(4.0));
        e.setValue("2", glm::dvec3(5.0));
        e.setValue("3", glm::dvec3(6.0));
        d.setValue("NotInRangeValueIVec3Vector", e);
    }
    d.setValue("NotInRangeValueIVec4", glm::dvec4(4.0));
    d.setValue("NotInRangeValueIVec4Optional", glm::dvec4(4.0));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec4(4.0));
        e.setValue("2", glm::dvec4(5.0));
        e.setValue("3", glm::dvec4(6.0));
        d.setValue("NotInRangeValueIVec4Vector", e);
    }


    d.setValue("LessValueInt", 15.0);
    d.setValue("LessValueIntOptional", 16.0);
    {
        ghoul::Dictionary e;
        e.setValue("1", 17.0);
        e.setValue("2", 18.0);
        e.setValue("3", 19.0);
        d.setValue("LessValueIntVector", e);
    }
    d.setValue("LessValueFloat", 20.1);
    d.setValue("LessValueFloatOptional", 20.2);
    {
        ghoul::Dictionary e;
        e.setValue("1", 20.3);
        e.setValue("2", 20.4);
        e.setValue("3", 20.5);
        d.setValue("LessValueFloatVector", e);
    }
    d.setValue("LessValueVec2", glm::dvec2(0.5));
    d.setValue("LessValueVec2Optional", glm::dvec2(0.5));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec2(0.25));
        e.setValue("2", glm::dvec2(0.5));
        e.setValue("3", glm::dvec2(0.75));
        d.setValue("LessValueVec2Vector", e);
    }
    d.setValue("LessValueVec3", glm::dvec3(0.5));
    d.setValue("LessValueVec3Optional", glm::dvec3(0.5));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec3(0.25));
        e.setValue("2", glm::dvec3(0.5));
        e.setValue("3", glm::dvec3(0.75));
        d.setValue("LessValueVec3Vector", e);
    }
    d.setValue("LessValueVec4", glm::dvec4(0.5));
    d.setValue("LessValueVec4Optional", glm::dvec4(0.5));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec4(0.25));
        e.setValue("2", glm::dvec4(0.5));
        e.setValue("3", glm::dvec4(0.75));
        d.setValue("LessValueVec4Vector", e);
    }
    d.setValue("LessValueIVec2", glm::dvec2(0));
    d.setValue("LessValueIVec2Optional", glm::dvec2(0));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec2(0.0));
        e.setValue("2", glm::dvec2(-1.0));
        e.setValue("3", glm::dvec2(-2.0));
        d.setValue("LessValueIVec2Vector", e);
    }
    d.setValue("LessValueIVec3", glm::dvec3(0.0));
    d.setValue("LessValueIVec3Optional", glm::dvec3(0.0));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec3(0.0));
        e.setValue("2", glm::dvec3(-1.0));
        e.setValue("3", glm::dvec3(-2.0));
        d.setValue("LessValueIVec3Vector", e);
    }
    d.setValue("LessValueIVec4", glm::dvec4(0.0));
    d.setValue("LessValueIVec4Optional", glm::dvec4(0.0));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec4(0.0));
        e.setValue("2", glm::dvec4(-1.0));
        e.setValue("3", glm::dvec4(-2.0));
        d.setValue("LessValueIVec4Vector", e);
    }


    d.setValue("LessEqualValueInt", 21.0);
    d.setValue("LessEqualValueIntOptional", 22.0);
    {
        ghoul::Dictionary e;
        e.setValue("1", 23.0);
        e.setValue("2", 24.0);
        e.setValue("3", 25.0);
        d.setValue("LessEqualValueIntVector", e);
    }
    d.setValue("LessEqualValueFloat", 26.1);
    d.setValue("LessEqualValueFloatOptional", 26.2);
    {
        ghoul::Dictionary e;
        e.setValue("1", 26.3);
        e.setValue("2", 26.4);
        e.setValue("3", 26.5);
        d.setValue("LessEqualValueFloatVector", e);
    }
    d.setValue("LessEqualValueVec2", glm::dvec2(0.5));
    d.setValue("LessEqualValueVec2Optional", glm::dvec2(0.5));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec2(0.25));
        e.setValue("2", glm::dvec2(0.5));
        e.setValue("3", glm::dvec2(0.75));
        d.setValue("LessEqualValueVec2Vector", e);
    }
    d.setValue("LessEqualValueVec3", glm::dvec3(0.5));
    d.setValue("LessEqualValueVec3Optional", glm::dvec3(0.5));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec3(0.25));
        e.setValue("2", glm::dvec3(0.5));
        e.setValue("3", glm::dvec3(0.75));
        d.setValue("LessEqualValueVec3Vector", e);
    }
    d.setValue("LessEqualValueVec4", glm::dvec4(0.5));
    d.setValue("LessEqualValueVec4Optional", glm::dvec4(0.5));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec4(0.25));
        e.setValue("2", glm::dvec4(0.5));
        e.setValue("3", glm::dvec4(0.75));
        d.setValue("LessEqualValueVec4Vector", e);
    }
    d.setValue("LessEqualValueIVec2", glm::dvec2(0.0));
    d.setValue("LessEqualValueIVec2Optional", glm::dvec2(0.0));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec2(0.0));
        e.setValue("2", glm::dvec2(-1.0));
        e.setValue("3", glm::dvec2(-2.0));
        d.setValue("LessEqualValueIVec2Vector", e);
    }
    d.setValue("LessEqualValueIVec3", glm::dvec3(0.0));
    d.setValue("LessEqualValueIVec3Optional", glm::dvec3(0.0));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec3(0.0));
        e.setValue("2", glm::dvec3(-1.0));
        e.setValue("3", glm::dvec3(-2.0));
        d.setValue("LessEqualValueIVec3Vector", e);
    }
    d.setValue("LessEqualValueIVec4", glm::dvec4(0.0));
    d.setValue("LessEqualValueIVec4Optional", glm::dvec4(0.0));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec4(0.0));
        e.setValue("2", glm::dvec4(-1.0));
        e.setValue("3", glm::dvec4(-2.0));
        d.setValue("LessEqualValueIVec4Vector", e);
    }


    d.setValue("GreaterValueInt", 27.0);
    d.setValue("GreaterValueIntOptional", 28.0);
    {
        ghoul::Dictionary e;
        e.setValue("1", 29.0);
        e.setValue("2", 30.0);
        e.setValue("3", 31.0);
        d.setValue("GreaterValueIntVector", e);
    }
    d.setValue("GreaterValueFloat", 32.1);
    d.setValue("GreaterValueFloatOptional", 32.2);
    {
        ghoul::Dictionary e;
        e.setValue("1", 32.3);
        e.setValue("2", 32.4);
        e.setValue("3", 32.5);
        d.setValue("GreaterValueFloatVector", e);
    }
    d.setValue("GreaterValueVec2", glm::dvec2(1.5));
    d.setValue("GreaterValueVec2Optional", glm::dvec2(1.5));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec2(1.25));
        e.setValue("2", glm::dvec2(1.5));
        e.setValue("3", glm::dvec2(1.75));
        d.setValue("GreaterValueVec2Vector", e);
    }
    d.setValue("GreaterValueVec3", glm::dvec3(1.5));
    d.setValue("GreaterValueVec3Optional", glm::dvec3(1.5));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec3(1.25));
        e.setValue("2", glm::dvec3(1.5));
        e.setValue("3", glm::dvec3(1.75));
        d.setValue("GreaterValueVec3Vector", e);
    }
    d.setValue("GreaterValueVec4", glm::dvec4(1.5));
    d.setValue("GreaterValueVec4Optional", glm::dvec4(1.5));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec4(1.25));
        e.setValue("2", glm::dvec4(1.5));
        e.setValue("3", glm::dvec4(1.75));
        d.setValue("GreaterValueVec4Vector", e);
    }
    d.setValue("GreaterValueIVec2", glm::dvec2(2.0));
    d.setValue("GreaterValueIVec2Optional", glm::dvec2(2.0));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec2(2.0));
        e.setValue("2", glm::dvec2(3.0));
        e.setValue("3", glm::dvec2(4.0));
        d.setValue("GreaterValueIVec2Vector", e);
    }
    d.setValue("GreaterValueIVec3", glm::dvec3(2.0));
    d.setValue("GreaterValueIVec3Optional", glm::dvec3(2.0));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec3(2.0));
        e.setValue("2", glm::dvec3(3.0));
        e.setValue("3", glm::dvec3(4.0));
        d.setValue("GreaterValueIVec3Vector", e);
    }
    d.setValue("GreaterValueIVec4", glm::dvec4(2.0));
    d.setValue("GreaterValueIVec4Optional", glm::dvec4(2.0));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec4(2.0));
        e.setValue("2", glm::dvec4(3.0));
        e.setValue("3", glm::dvec4(4.0));
        d.setValue("GreaterValueIVec4Vector", e);
    }


    d.setValue("GreaterEqualValueInt", 33.0);
    d.setValue("GreaterEqualValueIntOptional", 34.0);
    {
        ghoul::Dictionary e;
        e.setValue("1", 35.0);
        e.setValue("2", 36.0);
        e.setValue("3", 37.0);
        d.setValue("GreaterEqualValueIntVector", e);
    }
    d.setValue("GreaterEqualValueFloat", 38.1);
    d.setValue("GreaterEqualValueFloatOptional", 38.2);
    {
        ghoul::Dictionary e;
        e.setValue("1", 38.3);
        e.setValue("2", 38.4);
        e.setValue("3", 38.5);
        d.setValue("GreaterEqualValueFloatVector", e);
    }
    d.setValue("GreaterEqualValueVec2", glm::dvec2(1.5));
    d.setValue("GreaterEqualValueVec2Optional", glm::dvec2(1.5));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec2(1.25));
        e.setValue("2", glm::dvec2(1.5));
        e.setValue("3", glm::dvec2(1.75));
        d.setValue("GreaterEqualValueVec2Vector", e);
    }
    d.setValue("GreaterEqualValueVec3", glm::dvec3(1.5));
    d.setValue("GreaterEqualValueVec3Optional", glm::dvec3(1.5));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec3(1.25));
        e.setValue("2", glm::dvec3(1.5));
        e.setValue("3", glm::dvec3(1.75));
        d.setValue("GreaterEqualValueVec3Vector", e);
    }
    d.setValue("GreaterEqualValueVec4", glm::dvec4(1.5));
    d.setValue("GreaterEqualValueVec4Optional", glm::dvec4(1.5));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec4(1.25));
        e.setValue("2", glm::dvec4(1.5));
        e.setValue("3", glm::dvec4(1.75));
        d.setValue("GreaterEqualValueVec4Vector", e);
    }
    d.setValue("GreaterEqualValueIVec2", glm::dvec2(2.0));
    d.setValue("GreaterEqualValueIVec2Optional", glm::dvec2(2.0));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec2(2.0));
        e.setValue("2", glm::dvec2(3.0));
        e.setValue("3", glm::dvec2(4.0));
        d.setValue("GreaterEqualValueIVec2Vector", e);
    }
    d.setValue("GreaterEqualValueIVec3", glm::dvec3(2.0));
    d.setValue("GreaterEqualValueIVec3Optional", glm::dvec3(2.0));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec3(2.0));
        e.setValue("2", glm::dvec3(3.0));
        e.setValue("3", glm::dvec3(4.0));
        d.setValue("GreaterEqualValueIVec3Vector", e);
    }
    d.setValue("GreaterEqualValueIVec4", glm::dvec4(2.0));
    d.setValue("GreaterEqualValueIVec4Optional", glm::dvec4(2.0));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec4(2.0));
        e.setValue("2", glm::dvec4(3.0));
        e.setValue("3", glm::dvec4(4.0));
        d.setValue("GreaterEqualValueIVec4Vector", e);
    }


    d.setValue("UnequalValueInt", 39.0);
    d.setValue("UnequalValueIntOptional", 40.0);
    {
        ghoul::Dictionary e;
        e.setValue("1", 41.0);
        e.setValue("2", 42.0);
        e.setValue("3", 43.0);
        d.setValue("UnequalValueIntVector", e);
    }
    d.setValue("UnequalValueFloat", 44.1);
    d.setValue("UnequalValueFloatOptional", 44.2);
    {
        ghoul::Dictionary e;
        e.setValue("1", 44.3);
        e.setValue("2", 44.4);
        e.setValue("3", 44.5);
        d.setValue("UnequalValueFloatVector", e);
    }
    d.setValue("UnequalValueVec2", glm::dvec2(0.5));
    d.setValue("UnequalValueVec2Optional", glm::dvec2(0.5));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec2(0.25));
        e.setValue("2", glm::dvec2(0.5));
        e.setValue("3", glm::dvec2(0.75));
        d.setValue("UnequalValueVec2Vector", e);
    }
    d.setValue("UnequalValueVec3", glm::dvec3(0.5));
    d.setValue("UnequalValueVec3Optional", glm::dvec3(0.5));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec3(0.25));
        e.setValue("2", glm::dvec3(0.5));
        e.setValue("3", glm::dvec3(0.75));
        d.setValue("UnequalValueVec3Vector", e);
    }
    d.setValue("UnequalValueVec4", glm::dvec4(0.5));
    d.setValue("UnequalValueVec4Optional", glm::dvec4(0.5));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec4(0.25));
        e.setValue("2", glm::dvec4(0.5));
        e.setValue("3", glm::dvec4(0.75));
        d.setValue("UnequalValueVec4Vector", e);
    }
    d.setValue("UnequalValueIVec2", glm::dvec2(0.0));
    d.setValue("UnequalValueIVec2Optional", glm::dvec2(0.0));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec2(0.0));
        e.setValue("2", glm::dvec2(-1.0));
        e.setValue("3", glm::dvec2(-2.0));
        d.setValue("UnequalValueIVec2Vector", e);
    }
    d.setValue("UnequalValueIVec3", glm::dvec3(0.0));
    d.setValue("UnequalValueIVec3Optional", glm::dvec3(0.0));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec3(0.0));
        e.setValue("2", glm::dvec3(-1.0));
        e.setValue("3", glm::dvec3(-2.0));
        d.setValue("UnequalValueIVec3Vector", e);
    }
    d.setValue("UnequalValueIVec4", glm::dvec4(0.0));
    d.setValue("UnequalValueIVec4Optional", glm::dvec4(0.0));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec4(0.0));
        e.setValue("2", glm::dvec4(-1.0));
        e.setValue("3", glm::dvec4(-2.0));
        d.setValue("UnequalValueIVec4Vector", e);
    }


    d.setValue("DescValue", true);


    d.setValue("InListValue1", "A"s);
    d.setValue("InListValue1Optional", "B"s);
    {
        ghoul::Dictionary e;
        e.setValue("1", "C"s);
        e.setValue("2", "D"s);
        e.setValue("3", "E"s);
        d.setValue("InListValue1Vector", e);
    }
    d.setValue("InListValue2", "F"s);
    d.setValue("InListValue2Optional", "G"s);
    {
        ghoul::Dictionary e;
        e.setValue("1", "H"s);
        e.setValue("2", "I"s);
        e.setValue("3", "J"s);
        d.setValue("InListValue2Vector", e);
    }


    d.setValue("NotInListValue1", "F"s);
    d.setValue("NotInListValue1Optional", "G"s);
    {
        ghoul::Dictionary e;
        e.setValue("1", "H"s);
        e.setValue("2", "I"s);
        e.setValue("3", "J"s);
        d.setValue("NotInListValue1Vector", e);
    }
    d.setValue("NotInListValue2", "K"s);
    d.setValue("NotInListValue2Optional", "L"s);
    {
        ghoul::Dictionary e;
        e.setValue("1", "M"s);
        e.setValue("2", "N"s);
        e.setValue("3", "O"s);
        d.setValue("NotInListValue2Vector", e);
    }


    d.setValue("UnequalValueString", "zyxwv"s);
    d.setValue("UnequalValueStringOptional", "zyxwv"s);
    {
        ghoul::Dictionary e;
        e.setValue("1", "zyxwv1"s);
        e.setValue("2", "zyxwv2"s);
        e.setValue("3", "zyxwv3"s);
        d.setValue("UnequalValueStringVector", e);
    }
    d.setValue("NotEmptyString", "def"s);
    d.setValue("NotEmptyStringOptional", "def"s);
    {
        ghoul::Dictionary v;
        v.setValue("1", "jkl"s);
        v.setValue("2", "mno"s);
        v.setValue("3", "pqr"s);
        d.setValue("NotEmptyStringVector", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", "jkl"s);
        v.setValue("2", "mno"s);
        v.setValue("3", "pqr"s);
        d.setValue("NotEmptyStringOptionalVector", v);
    }
    d.setValue("DictValue", ghoul::Dictionary());
    {
        ghoul::Dictionary e;
        e.setValue("1", ghoul::Dictionary());
        e.setValue("2", ghoul::Dictionary());
        e.setValue("3", ghoul::Dictionary());
        d.setValue("DictValueVector", e);
    }
    d.setValue("DictValueOptional", ghoul::Dictionary());
    {
        ghoul::Dictionary e;
        e.setValue("a", ghoul::Dictionary());
        e.setValue("b", ghoul::Dictionary());
        e.setValue("c", ghoul::Dictionary());
        d.setValue("DictValueMap", e);
    }
    {
        ghoul::Dictionary e;
        {
            ghoul::Dictionary f;
            f.setValue("a", ghoul::Dictionary());
            f.setValue("b", ghoul::Dictionary());
            f.setValue("c", ghoul::Dictionary());
            e.setValue("1", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("a", ghoul::Dictionary());
            f.setValue("b", ghoul::Dictionary());
            f.setValue("c", ghoul::Dictionary());
            e.setValue("2", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("a", ghoul::Dictionary());
            f.setValue("b", ghoul::Dictionary());
            f.setValue("c", ghoul::Dictionary());
            e.setValue("3", f);
        }
        d.setValue("VectorDictValueMap", e);
    }
    {
        ghoul::Dictionary e;
        e.setValue("a", ghoul::Dictionary());
        e.setValue("b", ghoul::Dictionary());
        e.setValue("c", ghoul::Dictionary());
        d.setValue("OptionalDictValueMap", e);
    }
    {
        ghoul::Dictionary e;
        {
            ghoul::Dictionary f;
            f.setValue("a", ghoul::Dictionary());
            f.setValue("b", ghoul::Dictionary());
            f.setValue("c", ghoul::Dictionary());
            e.setValue("1", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("a", ghoul::Dictionary());
            f.setValue("b", ghoul::Dictionary());
            f.setValue("c", ghoul::Dictionary());
            e.setValue("2", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("a", ghoul::Dictionary());
            f.setValue("b", ghoul::Dictionary());
            f.setValue("c", ghoul::Dictionary());
            e.setValue("3", f);
        }
        d.setValue("OptionalVectorDictValueMap", e);
    }
    {
        ghoul::Dictionary e;
        e.setValue("a", ghoul::Dictionary());
        e.setValue("b", ghoul::Dictionary());
        e.setValue("c", ghoul::Dictionary());
        d.setValue("DictValueVectorMap", e);
    }
    {
        ghoul::Dictionary e;
        {
            ghoul::Dictionary f;
            f.setValue("a", ghoul::Dictionary());
            f.setValue("b", ghoul::Dictionary());
            f.setValue("c", ghoul::Dictionary());
            e.setValue("1", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("a", ghoul::Dictionary());
            f.setValue("b", ghoul::Dictionary());
            f.setValue("c", ghoul::Dictionary());
            e.setValue("2", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("a", ghoul::Dictionary());
            f.setValue("b", ghoul::Dictionary());
            f.setValue("c", ghoul::Dictionary());
            e.setValue("3", f);
        }
        d.setValue("VectorDictValueVectorMap", e);
    }
    {
        ghoul::Dictionary e;
        e.setValue("a", ghoul::Dictionary());
        e.setValue("b", ghoul::Dictionary());
        e.setValue("c", ghoul::Dictionary());
        d.setValue("OptionalDictValueVectorMap", e);
    }
    {
        ghoul::Dictionary e;
        {
            ghoul::Dictionary f;
            f.setValue("a", ghoul::Dictionary());
            f.setValue("b", ghoul::Dictionary());
            f.setValue("c", ghoul::Dictionary());
            e.setValue("1", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("a", ghoul::Dictionary());
            f.setValue("b", ghoul::Dictionary());
            f.setValue("c", ghoul::Dictionary());
            e.setValue("2", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("a", ghoul::Dictionary());
            f.setValue("b", ghoul::Dictionary());
            f.setValue("c", ghoul::Dictionary());
            e.setValue("3", f);
        }
        d.setValue("OptionalVectorDictValueVectorMap", e);
    }


    d.setValue("Annotation", "annotation_abc"s);
    d.setValue("AnnotationOptional", "annotation_def"s);
    {
        ghoul::Dictionary e;
        e.setValue("1", "annotation_ghi"s);
        e.setValue("2", "annotation_jkl"s);
        e.setValue("3", "annotation_mno"s);
        d.setValue("AnnotationVector", e);
    }
    d.setValue("Dcolor3Value", glm::dvec3(0.0, 0.1, 0.2));
    d.setValue("OptionalDcolor3Value", glm::dvec3(0.3, 0.4, 0.5));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec3(0.60, 0.61, 0.62));
        e.setValue("2", glm::dvec3(0.63, 0.64, 0.65));
        e.setValue("3", glm::dvec3(0.66, 0.67, 0.68));
        d.setValue("VectorDcolor3Value", e);
    }
    d.setValue("Color3Value", glm::dvec3(0.70, 0.71, 0.72));
    d.setValue("OptionalColor3Value", glm::dvec3(0.73, 0.74, 0.75));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec3(0.80, 0.81, 0.82));
        e.setValue("2", glm::dvec3(0.83, 0.84, 0.85));
        e.setValue("3", glm::dvec3(0.86, 0.87, 0.88));
        d.setValue("VectorColor3Value", e);
    }
    d.setValue("Dcolor4Value", glm::dvec4(0.0, 0.1, 0.2, 0.3));
    d.setValue("OptionalDcolor4Value", glm::dvec4(0.4, 0.5, 0.6, 0.7));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec4(0.60, 0.61, 0.62, 0.63));
        e.setValue("2", glm::dvec4(0.64, 0.65, 0.66, 0.67));
        e.setValue("3", glm::dvec4(0.68, 0.69, 0.70, 0.71));
        d.setValue("VectorDcolor4Value", e);
    }
    d.setValue("Color4Value", glm::dvec4(0.80, 0.81, 0.82, 0.83));
    d.setValue("OptionalColor4Value", glm::dvec4(0.84, 0.85, 0.86, 0.87));
    {
        ghoul::Dictionary e;
        e.setValue("1", glm::dvec4(0.90, 0.91, 0.92, 0.93));
        e.setValue("2", glm::dvec4(0.94, 0.95, 0.96, 0.97));
        e.setValue("3", glm::dvec4(0.98, 0.99, 0.991, 0.992));
        d.setValue("VectorColor4Value", e);
    }

    d.setValue("DateTimeValue", "1990 03 11 12:40:53"s);
    d.setValue("OptionalDateTimeValue", "1991 03 11 12:40:53"s);
    {
        ghoul::Dictionary e;
        e.setValue("1", "1992 03 11 12:40:53"s);
        e.setValue("2", "1993 03 11 12:40:53"s);
        e.setValue("3", "1994 03 11 12:40:53"s);
        d.setValue("VectorDateTimeValue", e);
    }
    {
        ghoul::Dictionary e;
        e.setValue("1", "1995 03 11 12:40:53"s);
        e.setValue("2", "1996 03 11 12:40:53"s);
        e.setValue("3", "1997 03 11 12:40:53"s);
        d.setValue("OptionalVectorDateTimeValue", e);
    }

    d.setValue("IdentifierValue", "abcdef"s);
    d.setValue("OptionalIdentifierValue", "defghi"s);
    {
        ghoul::Dictionary e;
        e.setValue("1", "ghijkl"s);
        e.setValue("2", "jklomn"s);
        e.setValue("3", "omnpqr"s);
        d.setValue("VectorIdentifierValue", e);
    }
    {
        ghoul::Dictionary e;
        e.setValue("1", "pqrstu"s);
        e.setValue("2", "stuvwx"s);
        e.setValue("3", "vwxyzz"s);
        d.setValue("OptionalVectorIdentifierValue", e);
    }

    d.setValue("PrivateValue", "abcdef"s);
    d.setValue("OptionalPrivateValue", "defghi"s);
    {
        ghoul::Dictionary e;
        e.setValue("1", "ghijkl"s);
        e.setValue("2", "jklomn"s);
        e.setValue("3", "omnpqr"s);
        d.setValue("VectorPrivateValue", e);
    }
    {
        ghoul::Dictionary e;
        e.setValue("1", "pqrstu"s);
        e.setValue("2", "stuvwx"s);
        e.setValue("3", "vwxyzz"s);
        d.setValue("OptionalVectorPrivateValue", e);
    }

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    std::filesystem::path tmpFile = (path / "codegen_execution_structs_attributes.txt");
    {
        std::ofstream f(tmpFile);
        f << "unit test";
    }

    std::filesystem::path noTmpFile = (path / "codegen_execution_structs_attributes_no_exist.txt");


    d.setValue("DefaultMustExistValue", tmpFile);
    d.setValue("DefaultOptionalMustExistValue", tmpFile);
    {
        ghoul::Dictionary e;
        e.setValue("1", tmpFile);
        e.setValue("2", tmpFile);
        e.setValue("3", tmpFile);
        d.setValue("DefaultVectorMustExistValue", e);
    }
    {
        ghoul::Dictionary e;
        e.setValue("1", tmpFile);
        e.setValue("2", tmpFile);
        e.setValue("3", tmpFile);
        d.setValue("DefaultOptionalVectorMustExistValue", e);
    }

    d.setValue("YesMustExistValue", tmpFile);
    d.setValue("YesOptionalMustExistValue", tmpFile);
    {
        ghoul::Dictionary e;
        e.setValue("1", tmpFile);
        e.setValue("2", tmpFile);
        e.setValue("3", tmpFile);
        d.setValue("YesVectorMustExistValue", e);
    }
    {
        ghoul::Dictionary e;
        e.setValue("1", tmpFile);
        e.setValue("2", tmpFile);
        e.setValue("3", tmpFile);
        d.setValue("YesOptionalVectorMustExistValue", e);
    }

    d.setValue("NoMustExistValue", noTmpFile);
    d.setValue("NoOptionalMustExistValue", noTmpFile);
    {
        ghoul::Dictionary e;
        e.setValue("1", noTmpFile);
        e.setValue("2", noTmpFile);
        e.setValue("3", noTmpFile);
        d.setValue("NoVectorMustExistValue", e);
    }
    {
        ghoul::Dictionary e;
        e.setValue("1", noTmpFile);
        e.setValue("2", noTmpFile);
        e.setValue("3", noTmpFile);
        d.setValue("NoOptionalVectorMustExistValue", e);
    }

    std::filesystem::path tmpFolder = (path / "codegen_execution_structs_attributes");
    std::filesystem::create_directories(tmpFolder);
    std::filesystem::path noTmpFolder = (path / "codegen_execution_structs_attributes_no_exist");

    d.setValue("DefaultMustExistDirectoryValue", tmpFolder);
    d.setValue("DefaultOptionalMustExistDirectoryValue", tmpFolder);
    {
        ghoul::Dictionary e;
        e.setValue("1", tmpFolder);
        e.setValue("2", tmpFolder);
        e.setValue("3", tmpFolder);
        d.setValue("DefaultVectorMustExistDirectoryValue", e);
    }
    {
        ghoul::Dictionary e;
        e.setValue("1", tmpFolder);
        e.setValue("2", tmpFolder);
        e.setValue("3", tmpFolder);
        d.setValue("DefaultOptionalVectorMustExistDirectoryValue", e);
    }

    d.setValue("YesMustExistDirectoryValue", tmpFolder);
    d.setValue("YesOptionalMustExistDirectoryValue", tmpFolder);
    {
        ghoul::Dictionary e;
        e.setValue("1", tmpFolder);
        e.setValue("2", tmpFolder);
        e.setValue("3", tmpFolder);
        d.setValue("YesVectorMustExistDirectoryValue", e);
    }
    {
        ghoul::Dictionary e;
        e.setValue("1", tmpFolder);
        e.setValue("2", tmpFolder);
        e.setValue("3", tmpFolder);
        d.setValue("YesOptionalVectorMustExistDirectoryValue", e);
    }

    d.setValue("NoMustExistDirectoryValue", noTmpFolder);
    d.setValue("NoOptionalMustExistDirectoryValue", noTmpFolder);
    {
        ghoul::Dictionary e;
        e.setValue("1", noTmpFolder);
        e.setValue("2", noTmpFolder);
        e.setValue("3", noTmpFolder);
        d.setValue("NoVectorMustExistDirectoryValue", e);
    }
    {
        ghoul::Dictionary e;
        e.setValue("1", noTmpFolder);
        e.setValue("2", noTmpFolder);
        e.setValue("3", noTmpFolder);
        d.setValue("NoOptionalVectorMustExistDirectoryValue", e);
    }


    const Parameters p = codegen::bake<Parameters>(d);

    CHECK(p.keyValue == 2.1f);
    REQUIRE(p.keyValueOptional.has_value());
    CHECK(p.keyValueOptional == 2.2f);
    CHECK(p.keyValueVector == std::vector{ 2.3f, 2.4f, 2.5f });

    CHECK(p.inRangeValueInt == 3);
    REQUIRE(p.inRangeValueIntOptional.has_value());
    CHECK(p.inRangeValueIntOptional == 4);
    CHECK(p.inRangeValueIntVector == std::vector{ 5, 6, 7 });

    CHECK(p.inRangeValueFloat == 8.1f);
    REQUIRE(p.inRangeValueFloatOptional.has_value());
    CHECK(p.inRangeValueFloatOptional == 8.2f);
    CHECK(p.inRangeValueFloatVector == std::vector{ 8.3f, 8.4f, 8.5f });

    CHECK(p.inRangeValueVec2 == glm::vec2(1.5f));
    REQUIRE(p.inRangeValueVec2Optional.has_value());
    CHECK(p.inRangeValueVec2Optional == glm::vec2(1.5f));
    CHECK(
        p.inRangeValueVec2Vector ==
        std::vector{ glm::vec2(1.25f), glm::vec2(1.5f), glm::vec2(1.75f) }
    );

    CHECK(p.inRangeValueVec3 == glm::vec3(1.5f));
    REQUIRE(p.inRangeValueVec3Optional.has_value());
    CHECK(p.inRangeValueVec3Optional == glm::vec3(1.5f));
    CHECK(
        p.inRangeValueVec3Vector ==
        std::vector{ glm::vec3(1.25f), glm::vec3(1.5f), glm::vec3(1.75f) }
    );

    CHECK(p.inRangeValueVec4 == glm::vec4(1.5f));
    REQUIRE(p.inRangeValueVec4Optional.has_value());
    CHECK(p.inRangeValueVec4Optional == glm::vec4(1.5f));
    CHECK(
        p.inRangeValueVec4Vector ==
        std::vector{ glm::vec4(1.25f), glm::vec4(1.5f), glm::vec4(1.75f) }
    );

    CHECK(p.inRangeValueIVec2 == glm::ivec2(2));
    REQUIRE(p.inRangeValueIVec2Optional.has_value());
    CHECK(p.inRangeValueIVec2Optional == glm::ivec2(2));
    CHECK(
        p.inRangeValueIVec2Vector ==
        std::vector{ glm::ivec2(2), glm::ivec2(2), glm::ivec2(2) }
    );

    CHECK(p.inRangeValueIVec3 == glm::ivec3(2));
    REQUIRE(p.inRangeValueIVec3Optional.has_value());
    CHECK(p.inRangeValueIVec3Optional == glm::ivec3(2));
    CHECK(
        p.inRangeValueIVec3Vector ==
        std::vector{ glm::ivec3(2), glm::ivec3(2), glm::ivec3(2) }
    );

    CHECK(p.inRangeValueIVec4 == glm::ivec4(2));
    REQUIRE(p.inRangeValueIVec4Optional.has_value());
    CHECK(p.inRangeValueIVec4Optional == glm::ivec4(2));
    CHECK(
        p.inRangeValueIVec4Vector ==
        std::vector{ glm::ivec4(2), glm::ivec4(2), glm::ivec4(2) }
    );

    CHECK(p.notInRangeValueInt == 9);
    REQUIRE(p.notInRangeValueIntOptional.has_value());
    CHECK(p.notInRangeValueIntOptional == 10);
    CHECK(p.notInRangeValueIntVector == std::vector{ 11, 12, 13 });

    CHECK(p.notInRangeValueFloat == 14.1f);
    REQUIRE(p.notInRangeValueFloatOptional.has_value());
    CHECK(p.notInRangeValueFloatOptional == 14.2f);
    CHECK(p.notInRangeValueFloatVector == std::vector{ 14.3f, 14.4f, 14.5f });

    CHECK(p.notInRangeValueVec2 == glm::vec2(0.5f));
    REQUIRE(p.notInRangeValueVec2Optional.has_value());
    CHECK(p.notInRangeValueVec2Optional == glm::vec2(0.5f));
    CHECK(
        p.notInRangeValueVec2Vector ==
        std::vector{ glm::vec2(0.25f), glm::vec2(0.5f), glm::vec2(0.75f) }
    );

    CHECK(p.notInRangeValueVec3 == glm::vec3(0.5f));
    REQUIRE(p.notInRangeValueVec3Optional.has_value());
    CHECK(p.notInRangeValueVec3Optional == glm::vec3(0.5f));
    CHECK(
        p.notInRangeValueVec3Vector ==
        std::vector{ glm::vec3(0.25f), glm::vec3(0.5f), glm::vec3(0.75f) }
    );

    CHECK(p.notInRangeValueVec4 == glm::vec4(0.5f));
    REQUIRE(p.notInRangeValueVec4Optional.has_value());
    CHECK(p.notInRangeValueVec4Optional == glm::vec4(0.5f));
    CHECK(
        p.notInRangeValueVec4Vector ==
        std::vector{ glm::vec4(0.25f), glm::vec4(0.5f), glm::vec4(0.75f) }
    );

    CHECK(p.notInRangeValueIVec2 == glm::ivec2(4));
    REQUIRE(p.notInRangeValueIVec2Optional.has_value());
    CHECK(p.notInRangeValueIVec2Optional == glm::ivec2(4));
    CHECK(
        p.notInRangeValueIVec2Vector ==
        std::vector{ glm::ivec2(4), glm::ivec2(5), glm::ivec2(6) }
    );

    CHECK(p.notInRangeValueIVec3 == glm::ivec3(4));
    REQUIRE(p.notInRangeValueIVec3Optional.has_value());
    CHECK(p.notInRangeValueIVec3Optional == glm::ivec3(4));
    CHECK(
        p.notInRangeValueIVec3Vector ==
        std::vector{ glm::ivec3(4), glm::ivec3(5), glm::ivec3(6) }
    );

    CHECK(p.notInRangeValueIVec4 == glm::ivec4(4));
    REQUIRE(p.notInRangeValueIVec4Optional.has_value());
    CHECK(p.notInRangeValueIVec4Optional == glm::ivec4(4));
    CHECK(
        p.notInRangeValueIVec4Vector ==
        std::vector{ glm::ivec4(4), glm::ivec4(5), glm::ivec4(6) }
    );


    CHECK(p.lessValueInt == 15);
    REQUIRE(p.lessValueIntOptional.has_value());
    CHECK(p.lessValueIntOptional == 16);
    CHECK(p.lessValueIntVector == std::vector{ 17, 18, 19 });

    CHECK(p.lessValueFloat == 20.1f);
    REQUIRE(p.lessValueFloatOptional.has_value());
    CHECK(p.lessValueFloatOptional == 20.2f);
    CHECK(p.lessValueFloatVector == std::vector{ 20.3f, 20.4f, 20.5f });

    CHECK(p.lessValueVec2 == glm::vec2(0.5f));
    REQUIRE(p.lessValueVec2Optional.has_value());
    CHECK(p.lessValueVec2Optional == glm::vec2(0.5f));
    CHECK(
        p.lessValueVec2Vector ==
        std::vector{ glm::vec2(0.25f), glm::vec2(0.5f), glm::vec2(0.75f) }
    );

    CHECK(p.lessValueVec3 == glm::vec3(0.5f));
    REQUIRE(p.lessValueVec3Optional.has_value());
    CHECK(p.lessValueVec3Optional == glm::vec3(0.5f));
    CHECK(
        p.lessValueVec3Vector ==
        std::vector{ glm::vec3(0.25f), glm::vec3(0.5f), glm::vec3(0.75f) }
    );

    CHECK(p.lessValueVec4 == glm::vec4(0.5f));
    REQUIRE(p.lessValueVec4Optional.has_value());
    CHECK(p.lessValueVec4Optional == glm::vec4(0.5f));
    CHECK(
        p.lessValueVec4Vector ==
        std::vector{ glm::vec4(0.25f), glm::vec4(0.5f), glm::vec4(0.75f) }
    );

    CHECK(p.lessValueIVec2 == glm::ivec2(0));
    REQUIRE(p.lessValueIVec2Optional.has_value());
    CHECK(p.lessValueIVec2Optional == glm::ivec2(0));
    CHECK(
        p.lessValueIVec2Vector ==
        std::vector{ glm::ivec2(0), glm::ivec2(-1), glm::ivec2(-2) }
    );

    CHECK(p.lessValueIVec3 == glm::ivec3(0));
    REQUIRE(p.lessValueIVec3Optional.has_value());
    CHECK(p.lessValueIVec3Optional == glm::ivec3(0));
    CHECK(
        p.lessValueIVec3Vector ==
        std::vector{ glm::ivec3(0), glm::ivec3(-1), glm::ivec3(-2) }
    );

    CHECK(p.lessValueIVec4 == glm::ivec4(0));
    REQUIRE(p.lessValueIVec4Optional.has_value());
    CHECK(p.lessValueIVec4Optional == glm::ivec4(0));
    CHECK(
        p.lessValueIVec4Vector ==
        std::vector{ glm::ivec4(0), glm::ivec4(-1), glm::ivec4(-2) }
    );

    CHECK(p.lessEqualValueInt == 21);
    REQUIRE(p.lessEqualValueIntOptional.has_value());
    CHECK(p.lessEqualValueIntOptional == 22);
    CHECK(p.lessEqualValueIntVector == std::vector{ 23, 24, 25 });

    CHECK(p.lessEqualValueFloat == 26.1f);
    REQUIRE(p.lessEqualValueFloatOptional.has_value());
    CHECK(p.lessEqualValueFloatOptional == 26.2f);
    CHECK(p.lessEqualValueFloatVector == std::vector{ 26.3f, 26.4f, 26.5f });

    CHECK(p.lessEqualValueVec2 == glm::vec2(0.5f));
    REQUIRE(p.lessEqualValueVec2Optional.has_value());
    CHECK(p.lessEqualValueVec2Optional == glm::vec2(0.5f));
    CHECK(
        p.lessEqualValueVec2Vector ==
        std::vector{ glm::vec2(0.25f), glm::vec2(0.5f), glm::vec2(0.75f) }
    );

    CHECK(p.lessEqualValueVec3 == glm::vec3(0.5f));
    REQUIRE(p.lessEqualValueVec3Optional.has_value());
    CHECK(p.lessEqualValueVec3Optional == glm::vec3(0.5f));
    CHECK(
        p.lessEqualValueVec3Vector ==
        std::vector{ glm::vec3(0.25f), glm::vec3(0.5f), glm::vec3(0.75f) }
    );

    CHECK(p.lessEqualValueVec4 == glm::vec4(0.5f));
    REQUIRE(p.lessEqualValueVec4Optional.has_value());
    CHECK(p.lessEqualValueVec4Optional == glm::vec4(0.5f));
    CHECK(
        p.lessEqualValueVec4Vector ==
        std::vector{ glm::vec4(0.25f), glm::vec4(0.5f), glm::vec4(0.75f) }
    );

    CHECK(p.lessEqualValueIVec2 == glm::ivec2(0));
    REQUIRE(p.lessEqualValueIVec2Optional.has_value());
    CHECK(p.lessEqualValueIVec2Optional == glm::ivec2(0));
    CHECK(
        p.lessEqualValueIVec2Vector ==
        std::vector{ glm::ivec2(0), glm::ivec2(-1), glm::ivec2(-2) }
    );

    CHECK(p.lessEqualValueIVec3 == glm::ivec3(0));
    REQUIRE(p.lessEqualValueIVec3Optional.has_value());
    CHECK(p.lessEqualValueIVec3Optional == glm::ivec3(0));
    CHECK(
        p.lessEqualValueIVec3Vector ==
        std::vector{ glm::ivec3(0), glm::ivec3(-1), glm::ivec3(-2) }
    );

    CHECK(p.lessEqualValueIVec4 == glm::ivec4(0));
    REQUIRE(p.lessEqualValueIVec4Optional.has_value());
    CHECK(p.lessEqualValueIVec4Optional == glm::ivec4(0));
    CHECK(
        p.lessEqualValueIVec4Vector ==
        std::vector{ glm::ivec4(0), glm::ivec4(-1), glm::ivec4(-2) }
    );

    CHECK(p.greaterValueInt == 27);
    REQUIRE(p.greaterValueIntOptional.has_value());
    CHECK(p.greaterValueIntOptional == 28);
    CHECK(p.greaterValueIntVector == std::vector{ 29, 30, 31 });

    CHECK(p.greaterValueFloat == 32.1f);
    REQUIRE(p.greaterValueFloatOptional.has_value());
    CHECK(p.greaterValueFloatOptional == 32.2f);
    CHECK(p.greaterValueFloatVector == std::vector{ 32.3f, 32.4f, 32.5f });

    CHECK(p.greaterValueVec2 == glm::vec2(1.5f));
    REQUIRE(p.greaterValueVec2Optional.has_value());
    CHECK(p.greaterValueVec2Optional == glm::vec2(1.5f));
    CHECK(
        p.greaterValueVec2Vector ==
        std::vector{ glm::vec2(1.25f), glm::vec2(1.5f), glm::vec2(1.75f) }
    );

    CHECK(p.greaterValueVec3 == glm::vec3(1.5f));
    REQUIRE(p.greaterValueVec3Optional.has_value());
    CHECK(p.greaterValueVec3Optional == glm::vec3(1.5f));
    CHECK(
        p.greaterValueVec3Vector ==
        std::vector{ glm::vec3(1.25f), glm::vec3(1.5f), glm::vec3(1.75f) }
    );

    CHECK(p.greaterValueVec4 == glm::vec4(1.5f));
    REQUIRE(p.greaterValueVec4Optional.has_value());
    CHECK(p.greaterValueVec4Optional == glm::vec4(1.5f));
    CHECK(
        p.greaterValueVec4Vector ==
        std::vector{ glm::vec4(1.25f), glm::vec4(1.5f), glm::vec4(1.75f) }
    );

    CHECK(p.greaterValueIVec2 == glm::ivec2(2));
    REQUIRE(p.greaterValueIVec2Optional.has_value());
    CHECK(p.greaterValueIVec2Optional == glm::ivec2(2));
    CHECK(
        p.greaterValueIVec2Vector ==
        std::vector{ glm::ivec2(2), glm::ivec2(3), glm::ivec2(4) }
    );

    CHECK(p.greaterValueIVec3 == glm::ivec3(2));
    REQUIRE(p.greaterValueIVec3Optional.has_value());
    CHECK(p.greaterValueIVec3Optional == glm::ivec3(2));
    CHECK(
        p.greaterValueIVec3Vector ==
        std::vector{ glm::ivec3(2), glm::ivec3(3), glm::ivec3(4) }
    );

    CHECK(p.greaterValueIVec4 == glm::ivec4(2));
    REQUIRE(p.greaterValueIVec4Optional.has_value());
    CHECK(p.greaterValueIVec4Optional == glm::ivec4(2));
    CHECK(
        p.greaterValueIVec4Vector ==
        std::vector{ glm::ivec4(2), glm::ivec4(3), glm::ivec4(4) }
    );

    CHECK(p.greaterEqualValueInt == 33);
    REQUIRE(p.greaterEqualValueIntOptional.has_value());
    CHECK(p.greaterEqualValueIntOptional == 34);
    CHECK(p.greaterEqualValueIntVector == std::vector{ 35, 36, 37 });

    CHECK(p.greaterEqualValueFloat == 38.1f);
    REQUIRE(p.greaterEqualValueFloatOptional.has_value());
    CHECK(p.greaterEqualValueFloatOptional == 38.2f);
    CHECK(p.greaterEqualValueFloatVector == std::vector{ 38.3f, 38.4f, 38.5f });

    CHECK(p.greaterEqualValueVec2 == glm::vec2(1.5f));
    REQUIRE(p.greaterEqualValueVec2Optional.has_value());
    CHECK(p.greaterEqualValueVec2Optional == glm::vec2(1.5f));
    CHECK(
        p.greaterEqualValueVec2Vector ==
        std::vector{ glm::vec2(1.25f), glm::vec2(1.5f), glm::vec2(1.75f) }
    );

    CHECK(p.greaterEqualValueVec3 == glm::vec3(1.5f));
    REQUIRE(p.greaterEqualValueVec3Optional.has_value());
    CHECK(p.greaterEqualValueVec3Optional == glm::vec3(1.5f));
    CHECK(
        p.greaterEqualValueVec3Vector ==
        std::vector{ glm::vec3(1.25f), glm::vec3(1.5f), glm::vec3(1.75f) }
    );

    CHECK(p.greaterEqualValueVec4 == glm::vec4(1.5f));
    REQUIRE(p.greaterEqualValueVec4Optional.has_value());
    CHECK(p.greaterEqualValueVec4Optional == glm::vec4(1.5f));
    CHECK(
        p.greaterEqualValueVec4Vector ==
        std::vector{ glm::vec4(1.25f), glm::vec4(1.5f), glm::vec4(1.75f) }
    );

    CHECK(p.greaterEqualValueIVec2 == glm::ivec2(2));
    REQUIRE(p.greaterEqualValueIVec2Optional.has_value());
    CHECK(p.greaterEqualValueIVec2Optional == glm::ivec2(2));
    CHECK(
        p.greaterEqualValueIVec2Vector ==
        std::vector{ glm::ivec2(2), glm::ivec2(3), glm::ivec2(4) }
    );

    CHECK(p.greaterEqualValueIVec3 == glm::ivec3(2));
    REQUIRE(p.greaterEqualValueIVec3Optional.has_value());
    CHECK(p.greaterEqualValueIVec3Optional == glm::ivec3(2));
    CHECK(
        p.greaterEqualValueIVec3Vector ==
        std::vector{ glm::ivec3(2), glm::ivec3(3), glm::ivec3(4) }
    );

    CHECK(p.greaterEqualValueIVec4 == glm::ivec4(2));
    REQUIRE(p.greaterEqualValueIVec4Optional.has_value());
    CHECK(p.greaterEqualValueIVec4Optional == glm::ivec4(2));
    CHECK(
        p.greaterEqualValueIVec4Vector ==
        std::vector{ glm::ivec4(2), glm::ivec4(3), glm::ivec4(4) }
    );

    CHECK(p.unequalValueInt == 39);
    REQUIRE(p.unequalValueIntOptional.has_value());
    CHECK(p.unequalValueIntOptional == 40);
    CHECK(p.unequalValueIntVector == std::vector{ 41, 42, 43 });

    CHECK(p.unequalValueFloat == 44.1f);
    REQUIRE(p.unequalValueFloatOptional.has_value());
    CHECK(p.unequalValueFloatOptional == 44.2f);
    CHECK(p.unequalValueFloatVector == std::vector{ 44.3f, 44.4f, 44.5f });

    CHECK(p.unequalValueVec2 == glm::vec2(0.5f));
    REQUIRE(p.unequalValueVec2Optional.has_value());
    CHECK(p.unequalValueVec2Optional == glm::vec2(0.5f));
    CHECK(
        p.unequalValueVec2Vector ==
        std::vector{ glm::vec2(0.25f), glm::vec2(0.5f), glm::vec2(0.75f) }
    );

    CHECK(p.unequalValueVec3 == glm::vec3(0.5f));
    REQUIRE(p.unequalValueVec3Optional.has_value());
    CHECK(p.unequalValueVec3Optional == glm::vec3(0.5f));
    CHECK(
        p.unequalValueVec3Vector ==
        std::vector{ glm::vec3(0.25f), glm::vec3(0.5f), glm::vec3(0.75f) }
    );

    CHECK(p.unequalValueVec4 == glm::vec4(0.5f));
    REQUIRE(p.unequalValueVec4Optional.has_value());
    CHECK(p.unequalValueVec4Optional == glm::vec4(0.5f));
    CHECK(
        p.unequalValueVec4Vector ==
        std::vector{ glm::vec4(0.25f), glm::vec4(0.5f), glm::vec4(0.75f) }
    );

    CHECK(p.unequalValueIVec2 == glm::ivec2(0));
    REQUIRE(p.unequalValueIVec2Optional.has_value());
    CHECK(p.unequalValueIVec2Optional == glm::ivec2(0));
    CHECK(
        p.unequalValueIVec2Vector ==
        std::vector{ glm::ivec2(0), glm::ivec2(-1), glm::ivec2(-2) }
    );

    CHECK(p.unequalValueIVec3 == glm::ivec3(0));
    REQUIRE(p.unequalValueIVec3Optional.has_value());
    CHECK(p.unequalValueIVec3Optional == glm::ivec3(0));
    CHECK(
        p.unequalValueIVec3Vector ==
        std::vector{ glm::ivec3(0), glm::ivec3(-1), glm::ivec3(-2) }
    );

    CHECK(p.unequalValueIVec4 == glm::ivec4(0));
    REQUIRE(p.unequalValueIVec4Optional.has_value());
    CHECK(p.unequalValueIVec4Optional == glm::ivec4(0));
    CHECK(
        p.unequalValueIVec4Vector ==
        std::vector{ glm::ivec4(0), glm::ivec4(-1), glm::ivec4(-2) }
    );

    CHECK(p.descValue == true);

    CHECK(p.inListValue1 == "A");
    REQUIRE(p.inListValue1Optional.has_value());
    CHECK(p.inListValue1Optional == "B");
    CHECK(p.inListValue1Vector == std::vector<std::string>{ "C", "D", "E" });

    CHECK(p.inListValue2 == "F");
    REQUIRE(p.inListValue2Optional.has_value());
    CHECK(p.inListValue2Optional == "G");
    CHECK(p.inListValue2Vector == std::vector<std::string>{ "H", "I", "J" });

    CHECK(p.unequalValueString == "zyxwv");
    REQUIRE(p.unequalValueStringOptional.has_value());
    CHECK(p.unequalValueStringOptional == "zyxwv");
    CHECK(
        p.unequalValueStringVector ==
        std::vector<std::string>{ "zyxwv1", "zyxwv2", "zyxwv3"}
    );
    CHECK(p.notEmptyStringVector.size() == 3);
    CHECK(p.notEmptyStringVector == std::vector<std::string>{ "jkl", "mno", "pqr" });
    REQUIRE(p.notEmptyStringOptionalVector.has_value());
    CHECK(p.notEmptyStringOptionalVector->size() == 3);
    CHECK(
        p.notEmptyStringOptionalVector == std::vector<std::string>{ "jkl", "mno", "pqr" }
    );

    CHECK(p.notEmptyString == "def");
    REQUIRE(p.notEmptyStringOptional.has_value());
    CHECK(p.notEmptyStringOptional == "def");


    CHECK(p.annotation == "annotation_abc");
    CHECK(p.annotationOptional == "annotation_def");
    CHECK(p.annotationVector.size() == 3);
    CHECK(
        p.annotationVector ==
        std::vector<std::string>{ "annotation_ghi", "annotation_jkl", "annotation_mno" }
    );

    CHECK(p.dcolor3Value == glm::dvec3(0.0, 0.1, 0.2));
    REQUIRE(p.optionalDcolor3Value.has_value());
    CHECK(*p.optionalDcolor3Value == glm::dvec3(0.3, 0.4, 0.5));
    REQUIRE(p.vectorDcolor3Value.size() == 3);
    CHECK(p.vectorDcolor3Value[0] == glm::dvec3(0.60, 0.61, 0.62));
    CHECK(p.vectorDcolor3Value[1] == glm::dvec3(0.63, 0.64, 0.65));
    CHECK(p.vectorDcolor3Value[2] == glm::dvec3(0.66, 0.67, 0.68));

    CHECK(p.color3Value == glm::vec3(0.70f, 0.71f, 0.72f));
    REQUIRE(p.optionalColor3Value.has_value());
    CHECK(*p.optionalColor3Value == glm::vec3(0.73f, 0.74f, 0.75f));
    REQUIRE(p.vectorColor3Value.size() == 3);
    CHECK(p.vectorColor3Value[0] == glm::vec3(0.80f, 0.81f, 0.82f));
    CHECK(p.vectorColor3Value[1] == glm::vec3(0.83f, 0.84f, 0.85f));
    CHECK(p.vectorColor3Value[2] == glm::vec3(0.86f, 0.87f, 0.88f));

    CHECK(p.dcolor4Value == glm::dvec4(0.0, 0.1, 0.2, 0.3));
    REQUIRE(p.optionalDcolor4Value.has_value());
    CHECK(*p.optionalDcolor4Value == glm::dvec4(0.4, 0.5, 0.6, 0.7));
    REQUIRE(p.vectorDcolor4Value.size() == 3);
    CHECK(p.vectorDcolor4Value[0] == glm::dvec4(0.60, 0.61, 0.62, 0.63));
    CHECK(p.vectorDcolor4Value[1] == glm::dvec4(0.64, 0.65, 0.66, 0.67));
    CHECK(p.vectorDcolor4Value[2] == glm::dvec4(0.68, 0.69, 0.70, 0.71));

    CHECK(p.color4Value == glm::vec4(0.80f, 0.81f, 0.82f, 0.83f));
    REQUIRE(p.optionalColor4Value.has_value());
    CHECK(*p.optionalColor4Value == glm::vec4(0.84f, 0.85f, 0.86f, 0.87f));
    REQUIRE(p.vectorColor4Value.size() == 3);
    CHECK(p.vectorColor4Value[0] == glm::vec4(0.90f, 0.91f, 0.92f, 0.93f));
    CHECK(p.vectorColor4Value[1] == glm::vec4(0.94f, 0.95f, 0.96f, 0.97f));
    CHECK(p.vectorColor4Value[2] == glm::vec4(0.98f, 0.99f, 0.991f, 0.992f));

    CHECK(p.dateTimeValue == "1990 03 11 12:40:53");
    REQUIRE(p.optionalDateTimeValue.has_value());
    CHECK(*p.optionalDateTimeValue == "1991 03 11 12:40:53");
    REQUIRE(p.vectorDateTimeValue.size() == 3);
    CHECK(p.vectorDateTimeValue[0] == "1992 03 11 12:40:53");
    CHECK(p.vectorDateTimeValue[1] == "1993 03 11 12:40:53");
    CHECK(p.vectorDateTimeValue[2] == "1994 03 11 12:40:53");
    REQUIRE(p.optionalVectorDateTimeValue.has_value());
    REQUIRE((*p.optionalVectorDateTimeValue).size() == 3);
    CHECK((*p.optionalVectorDateTimeValue)[0] == "1995 03 11 12:40:53");
    CHECK((*p.optionalVectorDateTimeValue)[1] == "1996 03 11 12:40:53");
    CHECK((*p.optionalVectorDateTimeValue)[2] == "1997 03 11 12:40:53");

    CHECK(p.privateValue == "abcdef"s);
    REQUIRE(p.optionalPrivateValue.has_value());
    CHECK(*p.optionalPrivateValue == "defghi"s);
    REQUIRE(p.vectorPrivateValue.size() == 3);
    CHECK(p.vectorPrivateValue[0] == "ghijkl");
    CHECK(p.vectorPrivateValue[1] == "jklomn");
    CHECK(p.vectorPrivateValue[2] == "omnpqr");
    REQUIRE(p.optionalVectorPrivateValue.has_value());
    REQUIRE(p.optionalVectorPrivateValue->size() == 3);
    CHECK((*p.optionalVectorPrivateValue)[0] == "pqrstu");
    CHECK((*p.optionalVectorPrivateValue)[1] == "stuvwx");
    CHECK((*p.optionalVectorPrivateValue)[2] == "vwxyzz");
}

// This test is split into two parts as both Visual Studio and GCC had issues with the
// stack size
TEST_CASE("Execution/Structs/Attributes:  Documentation 1/2", "[Execution][Structs]") {
    Documentation doc = codegen::doc<Parameters>("");

    REQUIRE(doc.entries.size() == 255);
    {
        const DocumentationEntry& e = doc.entries[0];
        CHECK(e.key == "KeyKey");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "keyValue documentation");
        CHECK(e.verifier->type() == "Double");
        CHECK(dynamic_cast<DoubleVerifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[1];
        CHECK(e.key == "KeyKeyOptional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "keyValueOptional documentation");
        CHECK(e.verifier->type() == "Double");
        CHECK(dynamic_cast<DoubleVerifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[2];
        CHECK(e.key == "KeyKeyVector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "keyValueVector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Double");
        CHECK(dynamic_cast<DoubleVerifier*>(v->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[3];
        CHECK(e.key == "InRangeValueInt");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueInt documentation");
        CHECK(e.verifier->type() == "Integer");
        InRangeVerifier<IntVerifier>* v =
            dynamic_cast<InRangeVerifier<IntVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == -2);
        CHECK(v->upper == 8);
    }
    {
        const DocumentationEntry& e = doc.entries[4];
        CHECK(e.key == "InRangeValueIntOptional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueIntOptional documentation");
        CHECK(e.verifier->type() == "Integer");
        InRangeVerifier<IntVerifier>* v =
            dynamic_cast<InRangeVerifier<IntVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == -2);
        CHECK(v->upper == 8);
    }
    {
        const DocumentationEntry& e = doc.entries[5];
        CHECK(e.key == "InRangeValueIntVector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueIntVector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Integer");
        InRangeVerifier<IntVerifier>* w = dynamic_cast<InRangeVerifier<IntVerifier>*>(
            v->documentations[0].verifier.get()
        );
        REQUIRE(w);
        CHECK(w->lower == -2);
        CHECK(w->upper == 8);
    }
    {
        const DocumentationEntry& e = doc.entries[6];
        CHECK(e.key == "InRangeValueFloat");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueFloat documentation");
        CHECK(e.verifier->type() == "Double");
        InRangeVerifier<DoubleVerifier>* v =
            dynamic_cast<InRangeVerifier<DoubleVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == 8.0);
        CHECK(v->upper == 9.0);
    }
    {
        const DocumentationEntry& e = doc.entries[7];
        CHECK(e.key == "InRangeValueFloatOptional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueFloatOptional documentation");
        CHECK(e.verifier->type() == "Double");
        InRangeVerifier<DoubleVerifier>* v =
            dynamic_cast<InRangeVerifier<DoubleVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == 8.0);
        CHECK(v->upper == 9.0);
    }
    {
        const DocumentationEntry& e = doc.entries[8];
        CHECK(e.key == "InRangeValueFloatVector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueFloatVector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Double");
        InRangeVerifier<DoubleVerifier>* w =
            dynamic_cast<InRangeVerifier<DoubleVerifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->lower == 8.0);
        CHECK(w->upper == 9.0);
    }
    {
        const DocumentationEntry& e = doc.entries[9];
        CHECK(e.key == "InRangeValueVec2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueVec2 documentation");
        CHECK(e.verifier->type() == "Vector2<double>");
        InRangeVerifier<DoubleVector2Verifier>* v =
            dynamic_cast<InRangeVerifier<DoubleVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::dvec2(1.f));
        CHECK(v->upper == glm::dvec2(2.f));
    }
    {
        const DocumentationEntry& e = doc.entries[10];
        CHECK(e.key == "InRangeValueVec2Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueVec2Optional documentation");
        CHECK(e.verifier->type() == "Vector2<double>");
        InRangeVerifier<DoubleVector2Verifier>* v =
            dynamic_cast<InRangeVerifier<DoubleVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::dvec2(1.0));
        CHECK(v->upper == glm::dvec2(2.0));
    }
    {
        const DocumentationEntry& e = doc.entries[11];
        CHECK(e.key == "InRangeValueVec2Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueVec2Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector2<double>");
        InRangeVerifier<DoubleVector2Verifier>* w =
            dynamic_cast<InRangeVerifier<DoubleVector2Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->lower == glm::dvec2(1.0));
        CHECK(w->upper == glm::dvec2(2.0));
    }
    {
        const DocumentationEntry& e = doc.entries[12];
        CHECK(e.key == "InRangeValueVec3");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueVec3 documentation");
        CHECK(e.verifier->type() == "Vector3<double>");
        InRangeVerifier<DoubleVector3Verifier>* v =
            dynamic_cast<InRangeVerifier<DoubleVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::dvec3(1.f));
        CHECK(v->upper == glm::dvec3(2.f));
    }
    {
        const DocumentationEntry& e = doc.entries[13];
        CHECK(e.key == "InRangeValueVec3Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueVec3Optional documentation");
        CHECK(e.verifier->type() == "Vector3<double>");
        InRangeVerifier<DoubleVector3Verifier>* v =
            dynamic_cast<InRangeVerifier<DoubleVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::dvec3(1.0));
        CHECK(v->upper == glm::dvec3(2.0));
    }
    {
        const DocumentationEntry& e = doc.entries[14];
        CHECK(e.key == "InRangeValueVec3Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueVec3Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector3<double>");
        InRangeVerifier<DoubleVector3Verifier>* w =
            dynamic_cast<InRangeVerifier<DoubleVector3Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->lower == glm::dvec3(1.0));
        CHECK(w->upper == glm::dvec3(2.0));
    }
    {
        const DocumentationEntry& e = doc.entries[15];
        CHECK(e.key == "InRangeValueVec4");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueVec4 documentation");
        CHECK(e.verifier->type() == "Vector4<double>");
        InRangeVerifier<DoubleVector4Verifier>* v =
            dynamic_cast<InRangeVerifier<DoubleVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::dvec4(1.f));
        CHECK(v->upper == glm::dvec4(2.f));
    }
    {
        const DocumentationEntry& e = doc.entries[16];
        CHECK(e.key == "InRangeValueVec4Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueVec4Optional documentation");
        CHECK(e.verifier->type() == "Vector4<double>");
        InRangeVerifier<DoubleVector4Verifier>* v =
            dynamic_cast<InRangeVerifier<DoubleVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::dvec4(1.0));
        CHECK(v->upper == glm::dvec4(2.0));
    }
    {
        const DocumentationEntry& e = doc.entries[17];
        CHECK(e.key == "InRangeValueVec4Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueVec4Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector4<double>");
        InRangeVerifier<DoubleVector4Verifier>* w =
            dynamic_cast<InRangeVerifier<DoubleVector4Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->lower == glm::dvec4(1.0));
        CHECK(w->upper == glm::dvec4(2.0));
    }
    {
        const DocumentationEntry& e = doc.entries[18];
        CHECK(e.key == "InRangeValueIVec2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueIVec2 documentation");
        CHECK(e.verifier->type() == "Vector2<int>");
        InRangeVerifier<IntVector2Verifier>* v =
            dynamic_cast<InRangeVerifier<IntVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::ivec2(1));
        CHECK(v->upper == glm::ivec2(3));
    }
    {
        const DocumentationEntry& e = doc.entries[19];
        CHECK(e.key == "InRangeValueIVec2Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueIVec2Optional documentation");
        CHECK(e.verifier->type() == "Vector2<int>");
        InRangeVerifier<IntVector2Verifier>* v =
            dynamic_cast<InRangeVerifier<IntVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::ivec2(1));
        CHECK(v->upper == glm::ivec2(3));
    }
    {
        const DocumentationEntry& e = doc.entries[20];
        CHECK(e.key == "InRangeValueIVec2Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueIVec2Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector2<int>");
        InRangeVerifier<IntVector2Verifier>* w =
            dynamic_cast<InRangeVerifier<IntVector2Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->lower == glm::ivec2(1));
        CHECK(w->upper == glm::ivec2(3));
    }
    {
        const DocumentationEntry& e = doc.entries[21];
        CHECK(e.key == "InRangeValueIVec3");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueIVec3 documentation");
        CHECK(e.verifier->type() == "Vector3<int>");
        InRangeVerifier<IntVector3Verifier>* v =
            dynamic_cast<InRangeVerifier<IntVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::ivec3(1));
        CHECK(v->upper == glm::ivec3(3));
    }
    {
        const DocumentationEntry& e = doc.entries[22];
        CHECK(e.key == "InRangeValueIVec3Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueIVec3Optional documentation");
        CHECK(e.verifier->type() == "Vector3<int>");
        InRangeVerifier<IntVector3Verifier>* v =
            dynamic_cast<InRangeVerifier<IntVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::ivec3(1));
        CHECK(v->upper == glm::ivec3(3));
    }
    {
        const DocumentationEntry& e = doc.entries[23];
        CHECK(e.key == "InRangeValueIVec3Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueIVec3Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector3<int>");
        InRangeVerifier<IntVector3Verifier>* w =
            dynamic_cast<InRangeVerifier<IntVector3Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->lower == glm::ivec3(1));
        CHECK(w->upper == glm::ivec3(3));
    }
    {
        const DocumentationEntry& e = doc.entries[24];
        CHECK(e.key == "InRangeValueIVec4");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueIVec4 documentation");
        CHECK(e.verifier->type() == "Vector4<int>");
        InRangeVerifier<IntVector4Verifier>* v =
            dynamic_cast<InRangeVerifier<IntVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::ivec4(1));
        CHECK(v->upper == glm::ivec4(3));
    }
    {
        const DocumentationEntry& e = doc.entries[25];
        CHECK(e.key == "InRangeValueIVec4Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueIVec4Optional documentation");
        CHECK(e.verifier->type() == "Vector4<int>");
        InRangeVerifier<IntVector4Verifier>* v =
            dynamic_cast<InRangeVerifier<IntVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::ivec4(1));
        CHECK(v->upper == glm::ivec4(3));
    }
    {
        const DocumentationEntry& e = doc.entries[26];
        CHECK(e.key == "InRangeValueIVec4Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inRangeValueIVec4Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector4<int>");
        InRangeVerifier<IntVector4Verifier>* w =
            dynamic_cast<InRangeVerifier<IntVector4Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->lower == glm::ivec4(1));
        CHECK(w->upper == glm::ivec4(3));
    }
    {
        const DocumentationEntry& e = doc.entries[27];
        CHECK(e.key == "NotInRangeValueInt");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueInt documentation");
        CHECK(e.verifier->type() == "Integer");
        NotInRangeVerifier<IntVerifier>* v =
            dynamic_cast<NotInRangeVerifier<IntVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == 5);
        CHECK(v->upper == 7);
    }
    {
        const DocumentationEntry& e = doc.entries[28];
        CHECK(e.key == "NotInRangeValueIntOptional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueIntOptional documentation");
        CHECK(e.verifier->type() == "Integer");
        NotInRangeVerifier<IntVerifier>* v =
            dynamic_cast<NotInRangeVerifier<IntVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == 5);
        CHECK(v->upper == 7);
    }
    {
        const DocumentationEntry& e = doc.entries[29];
        CHECK(e.key == "NotInRangeValueIntVector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueIntVector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Integer");
        NotInRangeVerifier<IntVerifier>* w =
            dynamic_cast<NotInRangeVerifier<IntVerifier>*>(
                v->documentations[0].verifier.get()
                );
        REQUIRE(w);
        CHECK(w->lower == 5);
        CHECK(w->upper == 7);
    }
    {
        const DocumentationEntry& e = doc.entries[30];
        CHECK(e.key == "NotInRangeValueFloat");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueFloat documentation");
        CHECK(e.verifier->type() == "Double");
        NotInRangeVerifier<DoubleVerifier>* v =
            dynamic_cast<NotInRangeVerifier<DoubleVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == -0.5);
        CHECK(v->upper == 0.5);
    }
    {
        const DocumentationEntry& e = doc.entries[31];
        CHECK(e.key == "NotInRangeValueFloatOptional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueFloatOptional documentation");
        CHECK(e.verifier->type() == "Double");
        NotInRangeVerifier<DoubleVerifier>* v =
            dynamic_cast<NotInRangeVerifier<DoubleVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == -0.5);
        CHECK(v->upper == 0.5);
    }
    {
        const DocumentationEntry& e = doc.entries[32];
        CHECK(e.key == "NotInRangeValueFloatVector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueFloatVector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Double");
        NotInRangeVerifier<DoubleVerifier>* w =
            dynamic_cast<NotInRangeVerifier<DoubleVerifier>*>(
                v->documentations[0].verifier.get()
                );
        REQUIRE(w);
        CHECK(w->lower == -0.5);
        CHECK(w->upper == 0.5);
    }
    {
        const DocumentationEntry& e = doc.entries[33];
        CHECK(e.key == "NotInRangeValueVec2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueVec2 documentation");
        CHECK(e.verifier->type() == "Vector2<double>");
        NotInRangeVerifier<DoubleVector2Verifier>* v =
            dynamic_cast<NotInRangeVerifier<DoubleVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::dvec2(1.f));
        CHECK(v->upper == glm::dvec2(2.f));
    }
    {
        const DocumentationEntry& e = doc.entries[34];
        CHECK(e.key == "NotInRangeValueVec2Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueVec2Optional documentation");
        CHECK(e.verifier->type() == "Vector2<double>");
        NotInRangeVerifier<DoubleVector2Verifier>* v =
            dynamic_cast<NotInRangeVerifier<DoubleVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::dvec2(1.0));
        CHECK(v->upper == glm::dvec2(2.0));
    }
    {
        const DocumentationEntry& e = doc.entries[35];
        CHECK(e.key == "NotInRangeValueVec2Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueVec2Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector2<double>");
        NotInRangeVerifier<DoubleVector2Verifier>* w =
            dynamic_cast<NotInRangeVerifier<DoubleVector2Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->lower == glm::dvec2(1.0));
        CHECK(w->upper == glm::dvec2(2.0));
    }
    {
        const DocumentationEntry& e = doc.entries[36];
        CHECK(e.key == "NotInRangeValueVec3");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueVec3 documentation");
        CHECK(e.verifier->type() == "Vector3<double>");
        NotInRangeVerifier<DoubleVector3Verifier>* v =
            dynamic_cast<NotInRangeVerifier<DoubleVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::dvec3(1.f));
        CHECK(v->upper == glm::dvec3(2.f));
    }
    {
        const DocumentationEntry& e = doc.entries[37];
        CHECK(e.key == "NotInRangeValueVec3Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueVec3Optional documentation");
        CHECK(e.verifier->type() == "Vector3<double>");
        NotInRangeVerifier<DoubleVector3Verifier>* v =
            dynamic_cast<NotInRangeVerifier<DoubleVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::dvec3(1.0));
        CHECK(v->upper == glm::dvec3(2.0));
    }
    {
        const DocumentationEntry& e = doc.entries[38];
        CHECK(e.key == "NotInRangeValueVec3Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueVec3Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector3<double>");
        NotInRangeVerifier<DoubleVector3Verifier>* w =
            dynamic_cast<NotInRangeVerifier<DoubleVector3Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->lower == glm::dvec3(1.0));
        CHECK(w->upper == glm::dvec3(2.0));
    }
    {
        const DocumentationEntry& e = doc.entries[39];
        CHECK(e.key == "NotInRangeValueVec4");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueVec4 documentation");
        CHECK(e.verifier->type() == "Vector4<double>");
        NotInRangeVerifier<DoubleVector4Verifier>* v =
            dynamic_cast<NotInRangeVerifier<DoubleVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::dvec4(1.f));
        CHECK(v->upper == glm::dvec4(2.f));
    }
    {
        const DocumentationEntry& e = doc.entries[40];
        CHECK(e.key == "NotInRangeValueVec4Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueVec4Optional documentation");
        CHECK(e.verifier->type() == "Vector4<double>");
        NotInRangeVerifier<DoubleVector4Verifier>* v =
            dynamic_cast<NotInRangeVerifier<DoubleVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::dvec4(1.0));
        CHECK(v->upper == glm::dvec4(2.0));
    }
    {
        const DocumentationEntry& e = doc.entries[41];
        CHECK(e.key == "NotInRangeValueVec4Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueVec4Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector4<double>");
        NotInRangeVerifier<DoubleVector4Verifier>* w =
            dynamic_cast<NotInRangeVerifier<DoubleVector4Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->lower == glm::dvec4(1.0));
        CHECK(w->upper == glm::dvec4(2.0));
    }
    {
        const DocumentationEntry& e = doc.entries[42];
        CHECK(e.key == "NotInRangeValueIVec2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueIVec2 documentation");
        CHECK(e.verifier->type() == "Vector2<int>");
        NotInRangeVerifier<IntVector2Verifier>* v =
            dynamic_cast<NotInRangeVerifier<IntVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::ivec2(1));
        CHECK(v->upper == glm::ivec2(3));
    }
    {
        const DocumentationEntry& e = doc.entries[43];
        CHECK(e.key == "NotInRangeValueIVec2Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueIVec2Optional documentation");
        CHECK(e.verifier->type() == "Vector2<int>");
        NotInRangeVerifier<IntVector2Verifier>* v =
            dynamic_cast<NotInRangeVerifier<IntVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::ivec2(1));
        CHECK(v->upper == glm::ivec2(3));
    }
    {
        const DocumentationEntry& e = doc.entries[44];
        CHECK(e.key == "NotInRangeValueIVec2Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueIVec2Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector2<int>");
        NotInRangeVerifier<IntVector2Verifier>* w =
            dynamic_cast<NotInRangeVerifier<IntVector2Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->lower == glm::ivec2(1));
        CHECK(w->upper == glm::ivec2(3));
    }
    {
        const DocumentationEntry& e = doc.entries[45];
        CHECK(e.key == "NotInRangeValueIVec3");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueIVec3 documentation");
        CHECK(e.verifier->type() == "Vector3<int>");
        NotInRangeVerifier<IntVector3Verifier>* v =
            dynamic_cast<NotInRangeVerifier<IntVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::ivec3(1));
        CHECK(v->upper == glm::ivec3(3));
    }
    {
        const DocumentationEntry& e = doc.entries[46];
        CHECK(e.key == "NotInRangeValueIVec3Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueIVec3Optional documentation");
        CHECK(e.verifier->type() == "Vector3<int>");
        NotInRangeVerifier<IntVector3Verifier>* v =
            dynamic_cast<NotInRangeVerifier<IntVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::ivec3(1));
        CHECK(v->upper == glm::ivec3(3));
    }
    {
        const DocumentationEntry& e = doc.entries[47];
        CHECK(e.key == "NotInRangeValueIVec3Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueIVec3Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector3<int>");
        NotInRangeVerifier<IntVector3Verifier>* w =
            dynamic_cast<NotInRangeVerifier<IntVector3Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->lower == glm::ivec3(1));
        CHECK(w->upper == glm::ivec3(3));
    }
    {
        const DocumentationEntry& e = doc.entries[48];
        CHECK(e.key == "NotInRangeValueIVec4");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueIVec4 documentation");
        CHECK(e.verifier->type() == "Vector4<int>");
        NotInRangeVerifier<IntVector4Verifier>* v =
            dynamic_cast<NotInRangeVerifier<IntVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::ivec4(1));
        CHECK(v->upper == glm::ivec4(3));
    }
    {
        const DocumentationEntry& e = doc.entries[49];
        CHECK(e.key == "NotInRangeValueIVec4Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueIVec4Optional documentation");
        CHECK(e.verifier->type() == "Vector4<int>");
        NotInRangeVerifier<IntVector4Verifier>* v =
            dynamic_cast<NotInRangeVerifier<IntVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->lower == glm::ivec4(1));
        CHECK(v->upper == glm::ivec4(3));
    }
    {
        const DocumentationEntry& e = doc.entries[50];
        CHECK(e.key == "NotInRangeValueIVec4Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInRangeValueIVec4Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector4<int>");
        NotInRangeVerifier<IntVector4Verifier>* w =
            dynamic_cast<NotInRangeVerifier<IntVector4Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->lower == glm::ivec4(1));
        CHECK(w->upper == glm::ivec4(3));
    }
    {
        const DocumentationEntry& e = doc.entries[51];
        CHECK(e.key == "LessValueInt");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueInt documentation");
        CHECK(e.verifier->type() == "Integer");
        LessVerifier<IntVerifier>* v =
            dynamic_cast<LessVerifier<IntVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == 20);
    }
    {
        const DocumentationEntry& e = doc.entries[52];
        CHECK(e.key == "LessValueIntOptional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueIntOptional documentation");
        CHECK(e.verifier->type() == "Integer");
        LessVerifier<IntVerifier>* v =
            dynamic_cast<LessVerifier<IntVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == 20);
    }
    {
        const DocumentationEntry& e = doc.entries[53];
        CHECK(e.key == "LessValueIntVector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueIntVector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Integer");
        LessVerifier<IntVerifier>* w = dynamic_cast<LessVerifier<IntVerifier>*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->value == 20);
    }
    {
        const DocumentationEntry& e = doc.entries[54];
        CHECK(e.key == "LessValueFloat");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueFloat documentation");
        CHECK(e.verifier->type() == "Double");
        LessVerifier<DoubleVerifier>* v =
            dynamic_cast<LessVerifier<DoubleVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == 21.0);
    }
    {
        const DocumentationEntry& e = doc.entries[55];
        CHECK(e.key == "LessValueFloatOptional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueFloatOptional documentation");
        CHECK(e.verifier->type() == "Double");
        LessVerifier<DoubleVerifier>* v =
            dynamic_cast<LessVerifier<DoubleVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == 22.0);
    }
    {
        const DocumentationEntry& e = doc.entries[56];
        CHECK(e.key == "LessValueFloatVector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueFloatVector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Double");
        LessVerifier<DoubleVerifier>* w = dynamic_cast<LessVerifier<DoubleVerifier>*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->value == 23.0);
    }
    {
        const DocumentationEntry& e = doc.entries[57];
        CHECK(e.key == "LessValueVec2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueVec2 documentation");
        CHECK(e.verifier->type() == "Vector2<double>");
        LessVerifier<DoubleVector2Verifier>* v =
            dynamic_cast<LessVerifier<DoubleVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec2(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[58];
        CHECK(e.key == "LessValueVec2Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueVec2Optional documentation");
        CHECK(e.verifier->type() == "Vector2<double>");
        LessVerifier<DoubleVector2Verifier>* v =
            dynamic_cast<LessVerifier<DoubleVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec2(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[59];
        CHECK(e.key == "LessValueVec2Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueVec2Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector2<double>");
        LessVerifier<DoubleVector2Verifier>* w =
            dynamic_cast<LessVerifier<DoubleVector2Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->value == glm::dvec2(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[60];
        CHECK(e.key == "LessValueVec3");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueVec3 documentation");
        CHECK(e.verifier->type() == "Vector3<double>");
        LessVerifier<DoubleVector3Verifier>* v =
            dynamic_cast<LessVerifier<DoubleVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec3(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[61];
        CHECK(e.key == "LessValueVec3Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueVec3Optional documentation");
        CHECK(e.verifier->type() == "Vector3<double>");
        LessVerifier<DoubleVector3Verifier>* v =
            dynamic_cast<LessVerifier<DoubleVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec3(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[62];
        CHECK(e.key == "LessValueVec3Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueVec3Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector3<double>");
        LessVerifier<DoubleVector3Verifier>* w =
            dynamic_cast<LessVerifier<DoubleVector3Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->value == glm::dvec3(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[63];
        CHECK(e.key == "LessValueVec4");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueVec4 documentation");
        CHECK(e.verifier->type() == "Vector4<double>");
        LessVerifier<DoubleVector4Verifier>* v =
            dynamic_cast<LessVerifier<DoubleVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec4(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[64];
        CHECK(e.key == "LessValueVec4Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueVec4Optional documentation");
        CHECK(e.verifier->type() == "Vector4<double>");
        LessVerifier<DoubleVector4Verifier>* v =
            dynamic_cast<LessVerifier<DoubleVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec4(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[65];
        CHECK(e.key == "LessValueVec4Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueVec4Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector4<double>");
        LessVerifier<DoubleVector4Verifier>* w =
            dynamic_cast<LessVerifier<DoubleVector4Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->value == glm::dvec4(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[66];
        CHECK(e.key == "LessValueIVec2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueIVec2 documentation");
        CHECK(e.verifier->type() == "Vector2<int>");
        LessVerifier<IntVector2Verifier>* v =
            dynamic_cast<LessVerifier<IntVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec2(1));
    }
    {
        const DocumentationEntry& e = doc.entries[67];
        CHECK(e.key == "LessValueIVec2Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueIVec2Optional documentation");
        CHECK(e.verifier->type() == "Vector2<int>");
        LessVerifier<IntVector2Verifier>* v =
            dynamic_cast<LessVerifier<IntVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec2(1));
    }
    {
        const DocumentationEntry& e = doc.entries[68];
        CHECK(e.key == "LessValueIVec2Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueIVec2Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector2<int>");
        LessVerifier<IntVector2Verifier>* w =
            dynamic_cast<LessVerifier<IntVector2Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->value == glm::ivec2(1));
    }
    {
        const DocumentationEntry& e = doc.entries[69];
        CHECK(e.key == "LessValueIVec3");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueIVec3 documentation");
        CHECK(e.verifier->type() == "Vector3<int>");
        LessVerifier<IntVector3Verifier>* v =
            dynamic_cast<LessVerifier<IntVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec3(1));
    }
    {
        const DocumentationEntry& e = doc.entries[70];
        CHECK(e.key == "LessValueIVec3Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueIVec3Optional documentation");
        CHECK(e.verifier->type() == "Vector3<int>");
        LessVerifier<IntVector3Verifier>* v =
            dynamic_cast<LessVerifier<IntVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec3(1));
    }
    {
        const DocumentationEntry& e = doc.entries[71];
        CHECK(e.key == "LessValueIVec3Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueIVec3Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector3<int>");
        LessVerifier<IntVector3Verifier>* w =
            dynamic_cast<LessVerifier<IntVector3Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->value == glm::ivec3(1));
    }
    {
        const DocumentationEntry& e = doc.entries[72];
        CHECK(e.key == "LessValueIVec4");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueIVec4 documentation");
        CHECK(e.verifier->type() == "Vector4<int>");
        LessVerifier<IntVector4Verifier>* v =
            dynamic_cast<LessVerifier<IntVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec4(1));
    }
    {
        const DocumentationEntry& e = doc.entries[73];
        CHECK(e.key == "LessValueIVec4Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueIVec4Optional documentation");
        CHECK(e.verifier->type() == "Vector4<int>");
        LessVerifier<IntVector4Verifier>* v =
            dynamic_cast<LessVerifier<IntVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec4(1));
    }
    {
        const DocumentationEntry& e = doc.entries[74];
        CHECK(e.key == "LessValueIVec4Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessValueIVec4Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector4<int>");
        LessVerifier<IntVector4Verifier>* w =
            dynamic_cast<LessVerifier<IntVector4Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->value == glm::ivec4(1));
    }
    {
        const DocumentationEntry& e = doc.entries[75];
        CHECK(e.key == "LessEqualValueInt");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueInt documentation");
        CHECK(e.verifier->type() == "Integer");
        LessEqualVerifier<IntVerifier>* v =
            dynamic_cast<LessEqualVerifier<IntVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == 25);
    }
    {
        const DocumentationEntry& e = doc.entries[76];
        CHECK(e.key == "LessEqualValueIntOptional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueIntOptional documentation");
        CHECK(e.verifier->type() == "Integer");
        LessEqualVerifier<IntVerifier>* v =
            dynamic_cast<LessEqualVerifier<IntVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == 25);
    }
    {
        const DocumentationEntry& e = doc.entries[77];
        CHECK(e.key == "LessEqualValueIntVector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueIntVector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Integer");
        LessEqualVerifier<IntVerifier>* w = dynamic_cast<LessEqualVerifier<IntVerifier>*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->value == 25);
    }
    {
        const DocumentationEntry& e = doc.entries[78];
        CHECK(e.key == "LessEqualValueFloat");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueFloat documentation");
        CHECK(e.verifier->type() == "Double");
        LessEqualVerifier<DoubleVerifier>* v =
            dynamic_cast<LessEqualVerifier<DoubleVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == 27.0);
    }
    {
        const DocumentationEntry& e = doc.entries[79];
        CHECK(e.key == "LessEqualValueFloatOptional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueFloatOptional documentation");
        CHECK(e.verifier->type() == "Double");
        LessEqualVerifier<DoubleVerifier>* v =
            dynamic_cast<LessEqualVerifier<DoubleVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == 27.0);
    }
    {
        const DocumentationEntry& e = doc.entries[80];
        CHECK(e.key == "LessEqualValueFloatVector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueFloatVector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Double");
        LessEqualVerifier<DoubleVerifier>* w =
            dynamic_cast<LessEqualVerifier<DoubleVerifier>*>(
                v->documentations[0].verifier.get()
                );
        REQUIRE(w);
        CHECK(w->value == 27.0);
    }
    {
        const DocumentationEntry& e = doc.entries[81];
        CHECK(e.key == "LessEqualValueVec2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueVec2 documentation");
        CHECK(e.verifier->type() == "Vector2<double>");
        LessEqualVerifier<DoubleVector2Verifier>* v =
            dynamic_cast<LessEqualVerifier<DoubleVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec2(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[82];
        CHECK(e.key == "LessEqualValueVec2Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueVec2Optional documentation");
        CHECK(e.verifier->type() == "Vector2<double>");
        LessEqualVerifier<DoubleVector2Verifier>* v =
            dynamic_cast<LessEqualVerifier<DoubleVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec2(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[83];
        CHECK(e.key == "LessEqualValueVec2Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueVec2Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector2<double>");
        LessEqualVerifier<DoubleVector2Verifier>* w =
            dynamic_cast<LessEqualVerifier<DoubleVector2Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->value == glm::dvec2(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[84];
        CHECK(e.key == "LessEqualValueVec3");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueVec3 documentation");
        CHECK(e.verifier->type() == "Vector3<double>");
        LessEqualVerifier<DoubleVector3Verifier>* v =
            dynamic_cast<LessEqualVerifier<DoubleVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec3(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[85];
        CHECK(e.key == "LessEqualValueVec3Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueVec3Optional documentation");
        CHECK(e.verifier->type() == "Vector3<double>");
        LessEqualVerifier<DoubleVector3Verifier>* v =
            dynamic_cast<LessEqualVerifier<DoubleVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec3(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[86];
        CHECK(e.key == "LessEqualValueVec3Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueVec3Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector3<double>");
        LessEqualVerifier<DoubleVector3Verifier>* w =
            dynamic_cast<LessEqualVerifier<DoubleVector3Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->value == glm::dvec3(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[87];
        CHECK(e.key == "LessEqualValueVec4");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueVec4 documentation");
        CHECK(e.verifier->type() == "Vector4<double>");
        LessEqualVerifier<DoubleVector4Verifier>* v =
            dynamic_cast<LessEqualVerifier<DoubleVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec4(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[88];
        CHECK(e.key == "LessEqualValueVec4Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueVec4Optional documentation");
        CHECK(e.verifier->type() == "Vector4<double>");
        LessEqualVerifier<DoubleVector4Verifier>* v =
            dynamic_cast<LessEqualVerifier<DoubleVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec4(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[89];
        CHECK(e.key == "LessEqualValueVec4Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueVec4Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector4<double>");
        LessEqualVerifier<DoubleVector4Verifier>* w =
            dynamic_cast<LessEqualVerifier<DoubleVector4Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->value == glm::dvec4(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[90];
        CHECK(e.key == "LessEqualValueIVec2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueIVec2 documentation");
        CHECK(e.verifier->type() == "Vector2<int>");
        LessEqualVerifier<IntVector2Verifier>* v =
            dynamic_cast<LessEqualVerifier<IntVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec2(1));
    }
    {
        const DocumentationEntry& e = doc.entries[91];
        CHECK(e.key == "LessEqualValueIVec2Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueIVec2Optional documentation");
        CHECK(e.verifier->type() == "Vector2<int>");
        LessEqualVerifier<IntVector2Verifier>* v =
            dynamic_cast<LessEqualVerifier<IntVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec2(1));
    }
    {
        const DocumentationEntry& e = doc.entries[92];
        CHECK(e.key == "LessEqualValueIVec2Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueIVec2Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector2<int>");
        LessEqualVerifier<IntVector2Verifier>* w =
            dynamic_cast<LessEqualVerifier<IntVector2Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->value == glm::ivec2(1));
    }
    {
        const DocumentationEntry& e = doc.entries[93];
        CHECK(e.key == "LessEqualValueIVec3");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueIVec3 documentation");
        CHECK(e.verifier->type() == "Vector3<int>");
        LessEqualVerifier<IntVector3Verifier>* v =
            dynamic_cast<LessEqualVerifier<IntVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec3(1));
    }
    {
        const DocumentationEntry& e = doc.entries[94];
        CHECK(e.key == "LessEqualValueIVec3Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueIVec3Optional documentation");
        CHECK(e.verifier->type() == "Vector3<int>");
        LessEqualVerifier<IntVector3Verifier>* v =
            dynamic_cast<LessEqualVerifier<IntVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec3(1));
    }
    {
        const DocumentationEntry& e = doc.entries[95];
        CHECK(e.key == "LessEqualValueIVec3Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueIVec3Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector3<int>");
        LessEqualVerifier<IntVector3Verifier>* w =
            dynamic_cast<LessEqualVerifier<IntVector3Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->value == glm::ivec3(1));
    }
    {
        const DocumentationEntry& e = doc.entries[96];
        CHECK(e.key == "LessEqualValueIVec4");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueIVec4 documentation");
        CHECK(e.verifier->type() == "Vector4<int>");
        LessEqualVerifier<IntVector4Verifier>* v =
            dynamic_cast<LessEqualVerifier<IntVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec4(1));
    }
    {
        const DocumentationEntry& e = doc.entries[97];
        CHECK(e.key == "LessEqualValueIVec4Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueIVec4Optional documentation");
        CHECK(e.verifier->type() == "Vector4<int>");
        LessEqualVerifier<IntVector4Verifier>* v =
            dynamic_cast<LessEqualVerifier<IntVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec4(1));
    }
    {
        const DocumentationEntry& e = doc.entries[98];
        CHECK(e.key == "LessEqualValueIVec4Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "lessEqualValueIVec4Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector4<int>");
        LessEqualVerifier<IntVector4Verifier>* w =
            dynamic_cast<LessEqualVerifier<IntVector4Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->value == glm::ivec4(1));
    }
    {
        const DocumentationEntry& e = doc.entries[99];
        REQUIRE(e.key == "GreaterValueInt");
        REQUIRE(!e.optional);
        CHECK(!e.isPrivate);
        REQUIRE(e.documentation == "greaterValueInt documentation");
        REQUIRE(e.verifier->type() == "Integer");
        GreaterVerifier<IntVerifier>* v =
            dynamic_cast<GreaterVerifier<IntVerifier>*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->value == 25);
    }
    {
        const DocumentationEntry& e = doc.entries[100];
        CHECK(e.key == "GreaterValueIntOptional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterValueIntOptional documentation");
        CHECK(e.verifier->type() == "Integer");
        GreaterVerifier<IntVerifier>* v =
            dynamic_cast<GreaterVerifier<IntVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == 25);
    }
    {
        const DocumentationEntry& e = doc.entries[101];
        CHECK(e.key == "GreaterValueIntVector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterValueIntVector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Integer");
        GreaterVerifier<IntVerifier>* w = dynamic_cast<GreaterVerifier<IntVerifier>*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->value == 25);
    }
    {
        const DocumentationEntry& e = doc.entries[102];
        CHECK(e.key == "GreaterValueFloat");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterValueFloat documentation");
        CHECK(e.verifier->type() == "Double");
        GreaterVerifier<DoubleVerifier>* v =
            dynamic_cast<GreaterVerifier<DoubleVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == -5.0);
    }
    {
        const DocumentationEntry& e = doc.entries[103];
        CHECK(e.key == "GreaterValueFloatOptional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterValueFloatOptional documentation");
        CHECK(e.verifier->type() == "Double");
        GreaterVerifier<DoubleVerifier>* v =
            dynamic_cast<GreaterVerifier<DoubleVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == -5.0);
    }
    {
        const DocumentationEntry& e = doc.entries[104];
        CHECK(e.key == "GreaterValueFloatVector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterValueFloatVector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Double");
        GreaterVerifier<DoubleVerifier>* w =
            dynamic_cast<GreaterVerifier<DoubleVerifier>*>(
                v->documentations[0].verifier.get()
                );
        REQUIRE(w);
        CHECK(w->value == -5.0);
    }
    {
        const DocumentationEntry& e = doc.entries[105];
        CHECK(e.key == "GreaterValueVec2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterValueVec2 documentation");
        CHECK(e.verifier->type() == "Vector2<double>");
        GreaterVerifier<DoubleVector2Verifier>* v =
            dynamic_cast<GreaterVerifier<DoubleVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec2(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[106];
        CHECK(e.key == "GreaterValueVec2Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterValueVec2Optional documentation");
        CHECK(e.verifier->type() == "Vector2<double>");
        GreaterVerifier<DoubleVector2Verifier>* v =
            dynamic_cast<GreaterVerifier<DoubleVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec2(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[107];
        CHECK(e.key == "GreaterValueVec2Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterValueVec2Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector2<double>");
        GreaterVerifier<DoubleVector2Verifier>* w =
            dynamic_cast<GreaterVerifier<DoubleVector2Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->value == glm::dvec2(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[108];
        CHECK(e.key == "GreaterValueVec3");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterValueVec3 documentation");
        CHECK(e.verifier->type() == "Vector3<double>");
        GreaterVerifier<DoubleVector3Verifier>* v =
            dynamic_cast<GreaterVerifier<DoubleVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec3(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[109];
        CHECK(e.key == "GreaterValueVec3Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterValueVec3Optional documentation");
        CHECK(e.verifier->type() == "Vector3<double>");
        GreaterVerifier<DoubleVector3Verifier>* v =
            dynamic_cast<GreaterVerifier<DoubleVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec3(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[110];
        CHECK(e.key == "GreaterValueVec3Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterValueVec3Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector3<double>");
        GreaterVerifier<DoubleVector3Verifier>* w =
            dynamic_cast<GreaterVerifier<DoubleVector3Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->value == glm::dvec3(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[111];
        CHECK(e.key == "GreaterValueVec4");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterValueVec4 documentation");
        CHECK(e.verifier->type() == "Vector4<double>");
        GreaterVerifier<DoubleVector4Verifier>* v =
            dynamic_cast<GreaterVerifier<DoubleVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec4(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[112];
        CHECK(e.key == "GreaterValueVec4Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterValueVec4Optional documentation");
        CHECK(e.verifier->type() == "Vector4<double>");
        GreaterVerifier<DoubleVector4Verifier>* v =
            dynamic_cast<GreaterVerifier<DoubleVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec4(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[113];
        CHECK(e.key == "GreaterValueVec4Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterValueVec4Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector4<double>");
        GreaterVerifier<DoubleVector4Verifier>* w =
            dynamic_cast<GreaterVerifier<DoubleVector4Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->value == glm::dvec4(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[114];
        CHECK(e.key == "GreaterValueIVec2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterValueIVec2 documentation");
        CHECK(e.verifier->type() == "Vector2<int>");
        GreaterVerifier<IntVector2Verifier>* v =
            dynamic_cast<GreaterVerifier<IntVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec2(1));
    }
    {
        const DocumentationEntry& e = doc.entries[115];
        CHECK(e.key == "GreaterValueIVec2Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterValueIVec2Optional documentation");
        CHECK(e.verifier->type() == "Vector2<int>");
        GreaterVerifier<IntVector2Verifier>* v =
            dynamic_cast<GreaterVerifier<IntVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec2(1));
    }
    {
        const DocumentationEntry& e = doc.entries[116];
        CHECK(e.key == "GreaterValueIVec2Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterValueIVec2Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector2<int>");
        GreaterVerifier<IntVector2Verifier>* w =
            dynamic_cast<GreaterVerifier<IntVector2Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->value == glm::ivec2(1));
    }
    {
        const DocumentationEntry& e = doc.entries[117];
        CHECK(e.key == "GreaterValueIVec3");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterValueIVec3 documentation");
        CHECK(e.verifier->type() == "Vector3<int>");
        GreaterVerifier<IntVector3Verifier>* v =
            dynamic_cast<GreaterVerifier<IntVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec3(1));
    }
    {
        const DocumentationEntry& e = doc.entries[118];
        CHECK(e.key == "GreaterValueIVec3Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterValueIVec3Optional documentation");
        CHECK(e.verifier->type() == "Vector3<int>");
        GreaterVerifier<IntVector3Verifier>* v =
            dynamic_cast<GreaterVerifier<IntVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec3(1));
    }
    {
        const DocumentationEntry& e = doc.entries[119];
        CHECK(e.key == "GreaterValueIVec3Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterValueIVec3Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector3<int>");
        GreaterVerifier<IntVector3Verifier>* w =
            dynamic_cast<GreaterVerifier<IntVector3Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->value == glm::ivec3(1));
    }
    {
        const DocumentationEntry& e = doc.entries[120];
        CHECK(e.key == "GreaterValueIVec4");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterValueIVec4 documentation");
        CHECK(e.verifier->type() == "Vector4<int>");
        GreaterVerifier<IntVector4Verifier>* v =
            dynamic_cast<GreaterVerifier<IntVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec4(1));
    }
    {
        const DocumentationEntry& e = doc.entries[121];
        CHECK(e.key == "GreaterValueIVec4Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterValueIVec4Optional documentation");
        CHECK(e.verifier->type() == "Vector4<int>");
        GreaterVerifier<IntVector4Verifier>* v =
            dynamic_cast<GreaterVerifier<IntVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec4(1));
    }
    {
        const DocumentationEntry& e = doc.entries[122];
        CHECK(e.key == "GreaterValueIVec4Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterValueIVec4Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector4<int>");
        GreaterVerifier<IntVector4Verifier>* w =
            dynamic_cast<GreaterVerifier<IntVector4Verifier>*>(
                v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->value == glm::ivec4(1));
    }
    {
        const DocumentationEntry& e = doc.entries[123];
        CHECK(e.key == "GreaterEqualValueInt");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueInt documentation");
        CHECK(e.verifier->type() == "Integer");
        GreaterEqualVerifier<IntVerifier>* v =
            dynamic_cast<GreaterEqualVerifier<IntVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == 33);
    }
    {
        const DocumentationEntry& e = doc.entries[124];
        CHECK(e.key == "GreaterEqualValueIntOptional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueIntOptional documentation");
        CHECK(e.verifier->type() == "Integer");
        GreaterEqualVerifier<IntVerifier>* v =
            dynamic_cast<GreaterEqualVerifier<IntVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == 33);
    }
    {
        const DocumentationEntry& e = doc.entries[125];
        CHECK(e.key == "GreaterEqualValueIntVector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueIntVector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Integer");
        GreaterEqualVerifier<IntVerifier>* w =
            dynamic_cast<GreaterEqualVerifier<IntVerifier>*>(
                v->documentations[0].verifier.get()
                );
        REQUIRE(w);
        CHECK(w->value == 33);
    }
    {
        const DocumentationEntry& e = doc.entries[126];
        CHECK(e.key == "GreaterEqualValueFloat");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueFloat documentation");
        CHECK(e.verifier->type() == "Double");
        GreaterEqualVerifier<DoubleVerifier>* v =
            dynamic_cast<GreaterEqualVerifier<DoubleVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == -25.0);
    }
    {
        const DocumentationEntry& e = doc.entries[127];
        CHECK(e.key == "GreaterEqualValueFloatOptional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueFloatOptional documentation");
        CHECK(e.verifier->type() == "Double");
        GreaterEqualVerifier<DoubleVerifier>* v =
            dynamic_cast<GreaterEqualVerifier<DoubleVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == -25.0);
    }
}

TEST_CASE("Execution/Structs/Attributes:  Documentation 2/2", "[Execution][Structs]") {
    Documentation doc = codegen::doc<Parameters>("");

    REQUIRE(doc.entries.size() == 255);
    {
        const DocumentationEntry& e = doc.entries[128];
        CHECK(e.key == "GreaterEqualValueFloatVector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueFloatVector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Double");
        GreaterEqualVerifier<DoubleVerifier>* w =
            dynamic_cast<GreaterEqualVerifier<DoubleVerifier>*>(
                v->documentations[0].verifier.get()
                );
        REQUIRE(w);
        CHECK(w->value == -25.0);
    }
    {
        const DocumentationEntry& e = doc.entries[129];
        CHECK(e.key == "GreaterEqualValueVec2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueVec2 documentation");
        CHECK(e.verifier->type() == "Vector2<double>");
        GreaterEqualVerifier<DoubleVector2Verifier>* v =
            dynamic_cast<GreaterEqualVerifier<DoubleVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec2(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[130];
        CHECK(e.key == "GreaterEqualValueVec2Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueVec2Optional documentation");
        CHECK(e.verifier->type() == "Vector2<double>");
        GreaterEqualVerifier<DoubleVector2Verifier>* v =
            dynamic_cast<GreaterEqualVerifier<DoubleVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec2(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[131];
        CHECK(e.key == "GreaterEqualValueVec2Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueVec2Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector2<double>");
        GreaterEqualVerifier<DoubleVector2Verifier>* w =
            dynamic_cast<GreaterEqualVerifier<DoubleVector2Verifier>*>(
                v->documentations[0].verifier.get()
                );
        REQUIRE(w);
        CHECK(w->value == glm::dvec2(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[132];
        CHECK(e.key == "GreaterEqualValueVec3");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueVec3 documentation");
        CHECK(e.verifier->type() == "Vector3<double>");
        GreaterEqualVerifier<DoubleVector3Verifier>* v =
            dynamic_cast<GreaterEqualVerifier<DoubleVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec3(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[133];
        CHECK(e.key == "GreaterEqualValueVec3Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueVec3Optional documentation");
        CHECK(e.verifier->type() == "Vector3<double>");
        GreaterEqualVerifier<DoubleVector3Verifier>* v =
            dynamic_cast<GreaterEqualVerifier<DoubleVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec3(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[134];
        CHECK(e.key == "GreaterEqualValueVec3Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueVec3Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector3<double>");
        GreaterEqualVerifier<DoubleVector3Verifier>* w =
            dynamic_cast<GreaterEqualVerifier<DoubleVector3Verifier>*>(
                v->documentations[0].verifier.get()
                );
        REQUIRE(w);
        CHECK(w->value == glm::dvec3(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[135];
        CHECK(e.key == "GreaterEqualValueVec4");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueVec4 documentation");
        CHECK(e.verifier->type() == "Vector4<double>");
        GreaterEqualVerifier<DoubleVector4Verifier>* v =
            dynamic_cast<GreaterEqualVerifier<DoubleVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec4(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[136];
        CHECK(e.key == "GreaterEqualValueVec4Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueVec4Optional documentation");
        CHECK(e.verifier->type() == "Vector4<double>");
        GreaterEqualVerifier<DoubleVector4Verifier>* v =
            dynamic_cast<GreaterEqualVerifier<DoubleVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec4(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[137];
        CHECK(e.key == "GreaterEqualValueVec4Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueVec4Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector4<double>");
        GreaterEqualVerifier<DoubleVector4Verifier>* w =
            dynamic_cast<GreaterEqualVerifier<DoubleVector4Verifier>*>(
                v->documentations[0].verifier.get()
                );
        REQUIRE(w);
        CHECK(w->value == glm::dvec4(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[138];
        CHECK(e.key == "GreaterEqualValueIVec2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueIVec2 documentation");
        CHECK(e.verifier->type() == "Vector2<int>");
        GreaterEqualVerifier<IntVector2Verifier>* v =
            dynamic_cast<GreaterEqualVerifier<IntVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec2(1));
    }
    {
        const DocumentationEntry& e = doc.entries[139];
        CHECK(e.key == "GreaterEqualValueIVec2Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueIVec2Optional documentation");
        CHECK(e.verifier->type() == "Vector2<int>");
        GreaterEqualVerifier<IntVector2Verifier>* v =
            dynamic_cast<GreaterEqualVerifier<IntVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec2(1));
    }
    {
        const DocumentationEntry& e = doc.entries[140];
        CHECK(e.key == "GreaterEqualValueIVec2Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueIVec2Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector2<int>");
        GreaterEqualVerifier<IntVector2Verifier>* w =
            dynamic_cast<GreaterEqualVerifier<IntVector2Verifier>*>(
                v->documentations[0].verifier.get()
                );
        REQUIRE(w);
        CHECK(w->value == glm::ivec2(1));
    }
    {
        const DocumentationEntry& e = doc.entries[141];
        CHECK(e.key == "GreaterEqualValueIVec3");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueIVec3 documentation");
        CHECK(e.verifier->type() == "Vector3<int>");
        GreaterEqualVerifier<IntVector3Verifier>* v =
            dynamic_cast<GreaterEqualVerifier<IntVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec3(1));
    }
    {
        const DocumentationEntry& e = doc.entries[142];
        CHECK(e.key == "GreaterEqualValueIVec3Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueIVec3Optional documentation");
        CHECK(e.verifier->type() == "Vector3<int>");
        GreaterEqualVerifier<IntVector3Verifier>* v =
            dynamic_cast<GreaterEqualVerifier<IntVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec3(1));
    }
    {
        const DocumentationEntry& e = doc.entries[143];
        CHECK(e.key == "GreaterEqualValueIVec3Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueIVec3Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector3<int>");
        GreaterEqualVerifier<IntVector3Verifier>* w =
            dynamic_cast<GreaterEqualVerifier<IntVector3Verifier>*>(
                v->documentations[0].verifier.get()
                );
        REQUIRE(w);
        CHECK(w->value == glm::ivec3(1));
    }
    {
        const DocumentationEntry& e = doc.entries[144];
        CHECK(e.key == "GreaterEqualValueIVec4");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueIVec4 documentation");
        CHECK(e.verifier->type() == "Vector4<int>");
        GreaterEqualVerifier<IntVector4Verifier>* v =
            dynamic_cast<GreaterEqualVerifier<IntVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec4(1));
    }
    {
        const DocumentationEntry& e = doc.entries[145];
        CHECK(e.key == "GreaterEqualValueIVec4Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueIVec4Optional documentation");
        CHECK(e.verifier->type() == "Vector4<int>");
        GreaterEqualVerifier<IntVector4Verifier>* v =
            dynamic_cast<GreaterEqualVerifier<IntVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec4(1));
    }
    {
        const DocumentationEntry& e = doc.entries[146];
        CHECK(e.key == "GreaterEqualValueIVec4Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "greaterEqualValueIVec4Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector4<int>");
        GreaterEqualVerifier<IntVector4Verifier>* w =
            dynamic_cast<GreaterEqualVerifier<IntVector4Verifier>*>(
                v->documentations[0].verifier.get()
                );
        REQUIRE(w);
        CHECK(w->value == glm::ivec4(1));
    }
    {
        const DocumentationEntry& e = doc.entries[147];
        CHECK(e.key == "UnequalValueInt");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueInt documentation");
        CHECK(e.verifier->type() == "Integer");
        UnequalVerifier<IntVerifier>* v =
            dynamic_cast<UnequalVerifier<IntVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == 1);
    }
    {
        const DocumentationEntry& e = doc.entries[148];
        CHECK(e.key == "UnequalValueIntOptional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueIntOptional documentation");
        CHECK(e.verifier->type() == "Integer");
        UnequalVerifier<IntVerifier>* v =
            dynamic_cast<UnequalVerifier<IntVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == 1);
    }
    {
        const DocumentationEntry& e = doc.entries[149];
        CHECK(e.key == "UnequalValueIntVector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueIntVector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Integer");
        UnequalVerifier<IntVerifier>* w = dynamic_cast<UnequalVerifier<IntVerifier>*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->value == 1);
    }
    {
        const DocumentationEntry& e = doc.entries[150];
        CHECK(e.key == "UnequalValueFloat");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueFloat documentation");
        CHECK(e.verifier->type() == "Double");
        UnequalVerifier<DoubleVerifier>* v =
            dynamic_cast<UnequalVerifier<DoubleVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == 123.0);
    }
    {
        const DocumentationEntry& e = doc.entries[151];
        CHECK(e.key == "UnequalValueFloatOptional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueFloatOptional documentation");
        CHECK(e.verifier->type() == "Double");
        UnequalVerifier<DoubleVerifier>* v =
            dynamic_cast<UnequalVerifier<DoubleVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == 123.0);
    }
    {
        const DocumentationEntry& e = doc.entries[152];
        CHECK(e.key == "UnequalValueFloatVector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueFloatVector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Double");
        UnequalVerifier<DoubleVerifier>* w =
            dynamic_cast<UnequalVerifier<DoubleVerifier>*>(
                v->documentations[0].verifier.get()
                );
        REQUIRE(w);
        CHECK(w->value == 123.0);
    }
    {
        const DocumentationEntry& e = doc.entries[153];
        CHECK(e.key == "UnequalValueVec2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueVec2 documentation");
        CHECK(e.verifier->type() == "Vector2<double>");
        UnequalVerifier<DoubleVector2Verifier>* v =
            dynamic_cast<UnequalVerifier<DoubleVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec2(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[154];
        CHECK(e.key == "UnequalValueVec2Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueVec2Optional documentation");
        CHECK(e.verifier->type() == "Vector2<double>");
        UnequalVerifier<DoubleVector2Verifier>* v =
            dynamic_cast<UnequalVerifier<DoubleVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec2(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[155];
        CHECK(e.key == "UnequalValueVec2Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueVec2Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector2<double>");
        UnequalVerifier<DoubleVector2Verifier>* w =
            dynamic_cast<UnequalVerifier<DoubleVector2Verifier>*>(
                v->documentations[0].verifier.get()
                );
        REQUIRE(w);
        CHECK(w->value == glm::dvec2(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[156];
        CHECK(e.key == "UnequalValueVec3");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueVec3 documentation");
        CHECK(e.verifier->type() == "Vector3<double>");
        UnequalVerifier<DoubleVector3Verifier>* v =
            dynamic_cast<UnequalVerifier<DoubleVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec3(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[157];
        CHECK(e.key == "UnequalValueVec3Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueVec3Optional documentation");
        CHECK(e.verifier->type() == "Vector3<double>");
        UnequalVerifier<DoubleVector3Verifier>* v =
            dynamic_cast<UnequalVerifier<DoubleVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec3(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[158];
        CHECK(e.key == "UnequalValueVec3Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueVec3Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector3<double>");
        UnequalVerifier<DoubleVector3Verifier>* w =
            dynamic_cast<UnequalVerifier<DoubleVector3Verifier>*>(
                v->documentations[0].verifier.get()
                );
        REQUIRE(w);
        CHECK(w->value == glm::dvec3(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[159];
        CHECK(e.key == "UnequalValueVec4");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueVec4 documentation");
        CHECK(e.verifier->type() == "Vector4<double>");
        UnequalVerifier<DoubleVector4Verifier>* v =
            dynamic_cast<UnequalVerifier<DoubleVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec4(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[160];
        CHECK(e.key == "UnequalValueVec4Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueVec4Optional documentation");
        CHECK(e.verifier->type() == "Vector4<double>");
        UnequalVerifier<DoubleVector4Verifier>* v =
            dynamic_cast<UnequalVerifier<DoubleVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::dvec4(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[161];
        CHECK(e.key == "UnequalValueVec4Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueVec4Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector4<double>");
        UnequalVerifier<DoubleVector4Verifier>* w =
            dynamic_cast<UnequalVerifier<DoubleVector4Verifier>*>(
                v->documentations[0].verifier.get()
                );
        REQUIRE(w);
        CHECK(w->value == glm::dvec4(1.f));
    }
    {
        const DocumentationEntry& e = doc.entries[162];
        CHECK(e.key == "UnequalValueIVec2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueIVec2 documentation");
        CHECK(e.verifier->type() == "Vector2<int>");
        UnequalVerifier<IntVector2Verifier>* v =
            dynamic_cast<UnequalVerifier<IntVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec2(1));
    }
    {
        const DocumentationEntry& e = doc.entries[163];
        CHECK(e.key == "UnequalValueIVec2Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueIVec2Optional documentation");
        CHECK(e.verifier->type() == "Vector2<int>");
        UnequalVerifier<IntVector2Verifier>* v =
            dynamic_cast<UnequalVerifier<IntVector2Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec2(1));
    }
    {
        const DocumentationEntry& e = doc.entries[164];
        CHECK(e.key == "UnequalValueIVec2Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueIVec2Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector2<int>");
        UnequalVerifier<IntVector2Verifier>* w =
            dynamic_cast<UnequalVerifier<IntVector2Verifier>*>(
                v->documentations[0].verifier.get()
                );
        REQUIRE(w);
        CHECK(w->value == glm::ivec2(1));
    }
    {
        const DocumentationEntry& e = doc.entries[165];
        CHECK(e.key == "UnequalValueIVec3");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueIVec3 documentation");
        CHECK(e.verifier->type() == "Vector3<int>");
        UnequalVerifier<IntVector3Verifier>* v =
            dynamic_cast<UnequalVerifier<IntVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec3(1));
    }
    {
        const DocumentationEntry& e = doc.entries[166];
        CHECK(e.key == "UnequalValueIVec3Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueIVec3Optional documentation");
        CHECK(e.verifier->type() == "Vector3<int>");
        UnequalVerifier<IntVector3Verifier>* v =
            dynamic_cast<UnequalVerifier<IntVector3Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec3(1));
    }
    {
        const DocumentationEntry& e = doc.entries[167];
        CHECK(e.key == "UnequalValueIVec3Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueIVec3Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector3<int>");
        UnequalVerifier<IntVector3Verifier>* w =
            dynamic_cast<UnequalVerifier<IntVector3Verifier>*>(
                v->documentations[0].verifier.get()
                );
        REQUIRE(w);
        CHECK(w->value == glm::ivec3(1));
    }
    {
        const DocumentationEntry& e = doc.entries[168];
        CHECK(e.key == "UnequalValueIVec4");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueIVec4 documentation");
        CHECK(e.verifier->type() == "Vector4<int>");
        UnequalVerifier<IntVector4Verifier>* v =
            dynamic_cast<UnequalVerifier<IntVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec4(1));
    }
    {
        const DocumentationEntry& e = doc.entries[169];
        CHECK(e.key == "UnequalValueIVec4Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueIVec4Optional documentation");
        CHECK(e.verifier->type() == "Vector4<int>");
        UnequalVerifier<IntVector4Verifier>* v =
            dynamic_cast<UnequalVerifier<IntVector4Verifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == glm::ivec4(1));
    }
    {
        const DocumentationEntry& e = doc.entries[170];
        CHECK(e.key == "UnequalValueIVec4Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueIVec4Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Vector4<int>");
        UnequalVerifier<IntVector4Verifier>* w =
            dynamic_cast<UnequalVerifier<IntVector4Verifier>*>(
                v->documentations[0].verifier.get()
                );
        REQUIRE(w);
        CHECK(w->value == glm::ivec4(1));
    }
    {
        const DocumentationEntry& e = doc.entries[171];
        CHECK(e.key == "DescValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == description);
        CHECK(e.verifier->type() == "Boolean");
        CHECK(dynamic_cast<BoolVerifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[172];
        CHECK(e.key == "InListValue1");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inListValue1 documentation");
        CHECK(e.verifier->type() == "String");
        InListVerifier<StringVerifier>* v =
            dynamic_cast<InListVerifier<StringVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->values == std::vector<std::string>{ "A", "B", "C", "D", "E" });
    }
    {
        const DocumentationEntry& e = doc.entries[173];
        CHECK(e.key == "InListValue1Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inListValue1Optional documentation");
        CHECK(e.verifier->type() == "String");
        InListVerifier<StringVerifier>* v =
            dynamic_cast<InListVerifier<StringVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->values == std::vector<std::string>{ "A", "B", "C", "D", "E" });
    }
    {
        const DocumentationEntry& e = doc.entries[174];
        CHECK(e.key == "InListValue1Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inListValue1Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "String");
        InListVerifier<StringVerifier>* w = dynamic_cast<InListVerifier<StringVerifier>*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->values == std::vector<std::string>{ "A", "B", "C", "D", "E" });
    }
    {
        const DocumentationEntry& e = doc.entries[175];
        CHECK(e.key == "InListValue2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inListValue2 documentation");
        CHECK(e.verifier->type() == "String");
        InListVerifier<StringVerifier>* v =
            dynamic_cast<InListVerifier<StringVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->values == std::vector<std::string>{ "F", "G", "H", "I", "J" });
    }
    {
        const DocumentationEntry& e = doc.entries[176];
        CHECK(e.key == "InListValue2Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inListValue2Optional documentation");
        CHECK(e.verifier->type() == "String");
        InListVerifier<StringVerifier>* v =
            dynamic_cast<InListVerifier<StringVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->values == std::vector<std::string>{ "F", "G", "H", "I", "J" });
    }
    {
        const DocumentationEntry& e = doc.entries[177];
        CHECK(e.key == "InListValue2Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "inListValue2Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "String");
        InListVerifier<StringVerifier>* w = dynamic_cast<InListVerifier<StringVerifier>*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->values == std::vector<std::string>{ "F", "G", "H", "I", "J" });
    }
    {
        const DocumentationEntry& e = doc.entries[178];
        CHECK(e.key == "NotInListValue1");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInListValue1 documentation");
        CHECK(e.verifier->type() == "String");
        NotInListVerifier<StringVerifier>* v =
            dynamic_cast<NotInListVerifier<StringVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->values == std::vector<std::string>{ "A", "B", "C", "D", "E" });
    }
    {
        const DocumentationEntry& e = doc.entries[179];
        CHECK(e.key == "NotInListValue1Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInListValue1Optional documentation");
        CHECK(e.verifier->type() == "String");
        NotInListVerifier<StringVerifier>* v =
            dynamic_cast<NotInListVerifier<StringVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->values == std::vector<std::string>{ "A", "B", "C", "D", "E" });
    }
    {
        const DocumentationEntry& e = doc.entries[180];
        CHECK(e.key == "NotInListValue1Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInListValue1Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "String");
        NotInListVerifier<StringVerifier>* w =
            dynamic_cast<NotInListVerifier<StringVerifier>*>(
                v->documentations[0].verifier.get()
                );
        REQUIRE(w);
        CHECK(w->values == std::vector<std::string>{ "A", "B", "C", "D", "E" });
    }
    {
        const DocumentationEntry& e = doc.entries[181];
        CHECK(e.key == "NotInListValue2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInListValue2 documentation");
        CHECK(e.verifier->type() == "String");
        NotInListVerifier<StringVerifier>* v =
            dynamic_cast<NotInListVerifier<StringVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->values == std::vector<std::string>{ "F", "G", "H", "I", "J" });
    }
    {
        const DocumentationEntry& e = doc.entries[182];
        CHECK(e.key == "NotInListValue2Optional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInListValue2Optional documentation");
        CHECK(e.verifier->type() == "String");
        NotInListVerifier<StringVerifier>* v =
            dynamic_cast<NotInListVerifier<StringVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->values == std::vector<std::string>{ "F", "G", "H", "I", "J" });
    }
    {
        const DocumentationEntry& e = doc.entries[183];
        CHECK(e.key == "NotInListValue2Vector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "notInListValue2Vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "String");
        NotInListVerifier<StringVerifier>* w =
            dynamic_cast<NotInListVerifier<StringVerifier>*>(
                v->documentations[0].verifier.get()
                );
        REQUIRE(w);
        CHECK(w->values == std::vector<std::string>{ "F", "G", "H", "I", "J" });
    }
    {
        const DocumentationEntry& e = doc.entries[184];
        CHECK(e.key == "UnequalValueString");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueString documentation");
        CHECK(e.verifier->type() == "String");
        UnequalVerifier<StringVerifier>* v =
            dynamic_cast<UnequalVerifier<StringVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == "abcdef");
    }
    {
        const DocumentationEntry& e = doc.entries[185];
        CHECK(e.key == "UnequalValueStringOptional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueStringOptional documentation");
        CHECK(e.verifier->type() == "String");
        UnequalVerifier<StringVerifier>* v =
            dynamic_cast<UnequalVerifier<StringVerifier>*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->value == "abcdef");
    }
    {
        const DocumentationEntry& e = doc.entries[186];
        CHECK(e.key == "UnequalValueStringVector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "unequalValueStringVector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "String");
        UnequalVerifier<StringVerifier>* w =
            dynamic_cast<UnequalVerifier<StringVerifier>*>(
                v->documentations[0].verifier.get()
                );
        REQUIRE(w);
        CHECK(w->value == "abcdef");
    }
    {
        const DocumentationEntry& e = doc.entries[187];
        CHECK(e.key == "NotEmptyString");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "string not empty value documentation");
        CHECK(e.verifier->type() == "String");
        StringVerifier* v = dynamic_cast<StringVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->mustBeNotEmpty() == true);
    }
    {
        const DocumentationEntry& e = doc.entries[188];
        CHECK(e.key == "NotEmptyStringOptional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "string not empty optional value documentation");
        CHECK(e.verifier->type() == "String");
        StringVerifier* v = dynamic_cast<StringVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->mustBeNotEmpty() == true);
    }
    {
        const DocumentationEntry& e = doc.entries[189];
        CHECK(e.key == "NotEmptyStringVector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "string not empty vector value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "String");

        StringVerifier* w =
            dynamic_cast<StringVerifier*>(v->documentations[0].verifier.get());
        REQUIRE(w);
        CHECK(w->mustBeNotEmpty() == true);
    }
    {
        const DocumentationEntry& e = doc.entries[190];
        CHECK(e.key == "NotEmptyStringOptionalVector");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "string not empty optional vector value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "String");

        StringVerifier* w =
            dynamic_cast<StringVerifier*>(v->documentations[0].verifier.get());
        REQUIRE(w);
        CHECK(w->mustBeNotEmpty() == true);
    }
    {
        const DocumentationEntry& e = doc.entries[191];
        CHECK(e.key == "ReferenceValueOptional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "referenceValueOptional documentation");
        CHECK(e.verifier->type() == "Table");
        ReferencingVerifier* v = dynamic_cast<ReferencingVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->identifier == "abc");
    }
    {
        const DocumentationEntry& e = doc.entries[192];
        CHECK(e.key == "ReferenceValueVector");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "referenceValueVector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Table");
        ReferencingVerifier* w = dynamic_cast<ReferencingVerifier*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->identifier == "abc");
    }
    {
        const DocumentationEntry& e = doc.entries[193];
        CHECK(e.key == "DictValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dictValue documentation");
        CHECK(e.verifier->type() == "Table");
        ReferencingVerifier* v = dynamic_cast<ReferencingVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->identifier == "abc");
    }
    {
        const DocumentationEntry& e = doc.entries[194];
        CHECK(e.key == "DictValueVector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dictValueVector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Table");
        ReferencingVerifier* w = dynamic_cast<ReferencingVerifier*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->identifier == "abc");
    }
    {
        const DocumentationEntry& e = doc.entries[195];
        CHECK(e.key == "DictValueOptional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dictValueOptional documentation");
        CHECK(e.verifier->type() == "Table");
        ReferencingVerifier* v = dynamic_cast<ReferencingVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->identifier == "abc");
    }
    {
        const DocumentationEntry& e = doc.entries[196];
        CHECK(e.key == "DictValueMap");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dictValueMap documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Table");
        ReferencingVerifier* w = dynamic_cast<ReferencingVerifier*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->identifier == "abc");
    }
    {
        const DocumentationEntry& e = doc.entries[197];
        CHECK(e.key == "VectorDictValueMap");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "vectorDictValueMap documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Table");
        TableVerifier* w = dynamic_cast<TableVerifier*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        REQUIRE(w->documentations.size() == 1);
        CHECK(w->documentations[0].key == "*");
        CHECK(w->documentations[0].verifier->type() == "Table");
        ReferencingVerifier* u = dynamic_cast<ReferencingVerifier*>(
            w->documentations[0].verifier.get()
            );
        REQUIRE(u);
        CHECK(u->identifier == "abc");
    }
    {
        const DocumentationEntry& e = doc.entries[198];
        CHECK(e.key == "OptionalDictValueMap");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optionalDictValueMap documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Table");
        ReferencingVerifier* w = dynamic_cast<ReferencingVerifier*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->identifier == "abc");
    }
    {
        const DocumentationEntry& e = doc.entries[199];
        CHECK(e.key == "OptionalVectorDictValueMap");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optionalVectorDictValueMap documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Table");
        TableVerifier* w = dynamic_cast<TableVerifier*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        REQUIRE(w->documentations.size() == 1);
        CHECK(w->documentations[0].key == "*");
        CHECK(w->documentations[0].verifier->type() == "Table");
        ReferencingVerifier* u = dynamic_cast<ReferencingVerifier*>(
            w->documentations[0].verifier.get()
            );
        REQUIRE(u);
        CHECK(u->identifier == "abc");
    }
    {
        const DocumentationEntry& e = doc.entries[200];
        CHECK(e.key == "DictValueVectorMap");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dictValueVectorMap documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Table");
        TableVerifier* w = dynamic_cast<TableVerifier*>(v->documentations[0].verifier.get());
        REQUIRE(w);
        REQUIRE(w->documentations.size() == 1);
        CHECK(w->documentations[0].key == "*");
        CHECK(w->documentations[0].verifier->type() == "Table");
        ReferencingVerifier* u = dynamic_cast<ReferencingVerifier*>(
            w->documentations[0].verifier.get()
            );
        REQUIRE(u);
        CHECK(u->identifier == "abc");
    }
    {
        const DocumentationEntry& e = doc.entries[201];
        CHECK(e.key == "VectorDictValueVectorMap");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "vectorDictValueVectorMap documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Table");
        TableVerifier* w = dynamic_cast<TableVerifier*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        REQUIRE(w->documentations.size() == 1);
        CHECK(w->documentations[0].key == "*");
        CHECK(w->documentations[0].verifier->type() == "Table");
        TableVerifier* u = dynamic_cast<TableVerifier*>(w->documentations[0].verifier.get());
        REQUIRE(u);
        REQUIRE(u->documentations.size() == 1);
        CHECK(u->documentations[0].key == "*");
        CHECK(u->documentations[0].verifier->type() == "Table");
        ReferencingVerifier* x = dynamic_cast<ReferencingVerifier*>(
            u->documentations[0].verifier.get()
            );
        REQUIRE(x);
        CHECK(x->identifier == "abc");
    }
    {
        const DocumentationEntry& e = doc.entries[202];
        CHECK(e.key == "OptionalDictValueVectorMap");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optionalDictValueVectorMap documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Table");
        TableVerifier* w = dynamic_cast<TableVerifier*>(v->documentations[0].verifier.get());
        REQUIRE(w);
        REQUIRE(w->documentations.size() == 1);
        CHECK(w->documentations[0].key == "*");
        CHECK(w->documentations[0].verifier->type() == "Table");
        ReferencingVerifier* u = dynamic_cast<ReferencingVerifier*>(
            w->documentations[0].verifier.get()
            );
        REQUIRE(u);
        CHECK(u->identifier == "abc");
    }
    {
        const DocumentationEntry& e = doc.entries[203];
        CHECK(e.key == "OptionalVectorDictValueVectorMap");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optionalVectorDictValueVectorMap documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Table");
        TableVerifier* w = dynamic_cast<TableVerifier*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        REQUIRE(w->documentations.size() == 1);
        CHECK(w->documentations[0].key == "*");
        CHECK(w->documentations[0].verifier->type() == "Table");
        TableVerifier* u = dynamic_cast<TableVerifier*>(w->documentations[0].verifier.get());
        REQUIRE(u);
        REQUIRE(u->documentations.size() == 1);
        CHECK(u->documentations[0].key == "*");
        CHECK(u->documentations[0].verifier->type() == "Table");
        ReferencingVerifier* x = dynamic_cast<ReferencingVerifier*>(
            u->documentations[0].verifier.get()
            );
        REQUIRE(x);
        CHECK(x->identifier == "abc");
    }

    {
        const DocumentationEntry& e = doc.entries[204];
        CHECK(e.key == "Annotation");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "annotation documentation");
        CHECK(e.verifier->type() == "String");
        StringAnnotationVerifier* v =
            dynamic_cast<StringAnnotationVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->annotation == "abc");
    }
    {
        const DocumentationEntry& e = doc.entries[205];
        CHECK(e.key == "AnnotationOptional");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "annotationOptional documentation");
        CHECK(e.verifier->type() == "String");
        StringAnnotationVerifier* v =
            dynamic_cast<StringAnnotationVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->annotation == "def");
    }
    {
        const DocumentationEntry& e = doc.entries[206];
        CHECK(e.key == "AnnotationVector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "annotationVector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].verifier->type() == "String");
        StringAnnotationVerifier* w =
            dynamic_cast<StringAnnotationVerifier*>(v->documentations[0].verifier.get());
        REQUIRE(w);
        CHECK(w->annotation == "ghi");
    }
    {
        const DocumentationEntry& e = doc.entries[207];
        CHECK(e.key == "Dcolor3Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dcolor3Value documentation");
        CHECK(e.verifier->type() == "Color3");
        CHECK(dynamic_cast<Color3Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[208];
        CHECK(e.key == "OptionalDcolor3Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optionalDcolor3Value documentation");
        CHECK(e.verifier->type() == "Color3");
        CHECK(dynamic_cast<Color3Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[209];
        CHECK(e.key == "VectorDcolor3Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "vectorDcolor3Value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].verifier->type() == "Color3");
        CHECK(dynamic_cast<Color3Verifier*>(v->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[210];
        CHECK(e.key == "Color3Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "color3Value documentation");
        CHECK(e.verifier->type() == "Color3");
        CHECK(dynamic_cast<Color3Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[211];
        CHECK(e.key == "OptionalColor3Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optionalColor3Value documentation");
        CHECK(e.verifier->type() == "Color3");
        CHECK(dynamic_cast<Color3Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[212];
        CHECK(e.key == "VectorColor3Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "vectorColor3Value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].verifier->type() == "Color3");
        CHECK(dynamic_cast<Color3Verifier*>(v->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[213];
        CHECK(e.key == "Dcolor4Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dcolor4Value documentation");
        CHECK(e.verifier->type() == "Color4");
        CHECK(dynamic_cast<Color4Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[214];
        CHECK(e.key == "OptionalDcolor4Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optionalDcolor4Value documentation");
        CHECK(e.verifier->type() == "Color4");
        CHECK(dynamic_cast<Color4Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[215];
        CHECK(e.key == "VectorDcolor4Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "vectorDcolor4Value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].verifier->type() == "Color4");
        CHECK(dynamic_cast<Color4Verifier*>(v->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[216];
        CHECK(e.key == "Color4Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "color4Value documentation");
        CHECK(e.verifier->type() == "Color4");
        CHECK(dynamic_cast<Color4Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[217];
        CHECK(e.key == "OptionalColor4Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optionalColor4Value documentation");
        CHECK(e.verifier->type() == "Color4");
        CHECK(dynamic_cast<Color4Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[218];
        CHECK(e.key == "VectorColor4Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "vectorColor4Value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].verifier->type() == "Color4");
        CHECK(dynamic_cast<Color4Verifier*>(v->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[219];
        CHECK(e.key == "DateTimeValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dateTime value documentation");
        CHECK(e.verifier->type() == "Date and time");
        CHECK(dynamic_cast<DateTimeVerifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[220];
        CHECK(e.key == "OptionalDateTimeValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optional dateTime value documentation");
        CHECK(e.verifier->type() == "Date and time");
        CHECK(dynamic_cast<DateTimeVerifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[221];
        CHECK(e.key == "VectorDateTimeValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "vector dateTime value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].verifier->type() == "Date and time");
        CHECK(dynamic_cast<DateTimeVerifier*>(v->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[222];
        CHECK(e.key == "OptionalVectorDateTimeValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optional vector dateTime value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Date and time");
        CHECK(dynamic_cast<DateTimeVerifier*>(v->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[223];
        CHECK(e.key == "IdentifierValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "identifier value documentation");
        CHECK(e.verifier->type() == "Identifier");
        CHECK(dynamic_cast<IdentifierVerifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[224];
        CHECK(e.key == "OptionalIdentifierValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optional identifier value documentation");
        CHECK(e.verifier->type() == "Identifier");
        CHECK(dynamic_cast<IdentifierVerifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[225];
        CHECK(e.key == "VectorIdentifierValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "vector identifier value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].verifier->type() == "Identifier");
        CHECK(dynamic_cast<IdentifierVerifier*>(v->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[226];
        CHECK(e.key == "OptionalVectorIdentifierValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optional vector identifier value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "Identifier");
        CHECK(dynamic_cast<IdentifierVerifier*>(v->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[227];
        CHECK(e.key == "PrivateValue");
        CHECK(!e.optional);
        CHECK(e.isPrivate);
        CHECK(e.documentation == "private value documentation");
        CHECK(e.verifier->type() == "String");
        CHECK(dynamic_cast<StringVerifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[228];
        CHECK(e.key == "OptionalPrivateValue");
        CHECK(e.optional);
        CHECK(e.isPrivate);
        CHECK(e.documentation == "optional private value documentation");
        CHECK(e.verifier->type() == "String");
        CHECK(dynamic_cast<StringVerifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[229];
        CHECK(e.key == "VectorPrivateValue");
        CHECK(!e.optional);
        CHECK(e.isPrivate);
        CHECK(e.documentation == "vector private value documentation");
        CHECK(e.verifier->type() == "List of strings");
        StringListVerifier* v = dynamic_cast<StringListVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].verifier->type() == "String");
        CHECK(dynamic_cast<StringVerifier*>(v->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[230];
        CHECK(e.key == "OptionalVectorPrivateValue");
        CHECK(e.optional);
        CHECK(e.isPrivate);
        CHECK(e.documentation == "optional vector private value documentation");
        CHECK(e.verifier->type() == "List of strings");
        StringListVerifier* v = dynamic_cast<StringListVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].verifier->type() == "String");
        CHECK(dynamic_cast<StringVerifier*>(v->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[231];
        CHECK(e.key == "DefaultMustExistValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "default mustexist documentation");
        CHECK(e.verifier->type() == "File");
        FileVerifier* v = dynamic_cast<FileVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->mustExist());
    }
    {
        const DocumentationEntry& e = doc.entries[232];
        CHECK(e.key == "DefaultOptionalMustExistValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "default optional mustexist documentation");
        CHECK(e.verifier->type() == "File");
        FileVerifier* v = dynamic_cast<FileVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->mustExist());
    }
    {
        const DocumentationEntry& e = doc.entries[233];
        CHECK(e.key == "DefaultVectorMustExistValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "default vector mustexist documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].verifier->type() == "File");
        FileVerifier* w = dynamic_cast<FileVerifier*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->mustExist());
    }
    {
        const DocumentationEntry& e = doc.entries[234];
        CHECK(e.key == "DefaultOptionalVectorMustExistValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "default optionalvector mustexist documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].verifier->type() == "File");
        FileVerifier* w = dynamic_cast<FileVerifier*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->mustExist());
    }
    {
        const DocumentationEntry& e = doc.entries[235];
        CHECK(e.key == "YesMustExistValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "yes mustexist documentation");
        CHECK(e.verifier->type() == "File");
        FileVerifier* v = dynamic_cast<FileVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->mustExist());
    }
    {
        const DocumentationEntry& e = doc.entries[236];
        CHECK(e.key == "YesOptionalMustExistValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "yes optional mustexist documentation");
        CHECK(e.verifier->type() == "File");
        FileVerifier* v = dynamic_cast<FileVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->mustExist());
    }
    {
        const DocumentationEntry& e = doc.entries[237];
        CHECK(e.key == "YesVectorMustExistValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "yes vector mustexist documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].verifier->type() == "File");
        FileVerifier* w = dynamic_cast<FileVerifier*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->mustExist());
    }
    {
        const DocumentationEntry& e = doc.entries[238];
        CHECK(e.key == "YesOptionalVectorMustExistValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "yes optionalvector mustexist documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].verifier->type() == "File");
        FileVerifier* w = dynamic_cast<FileVerifier*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->mustExist());
    }
    {
        const DocumentationEntry& e = doc.entries[239];
        CHECK(e.key == "NoMustExistValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "no mustexist documentation");
        CHECK(e.verifier->type() == "File");
        FileVerifier* v = dynamic_cast<FileVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(!v->mustExist());
    }
    {
        const DocumentationEntry& e = doc.entries[240];
        CHECK(e.key == "NoOptionalMustExistValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "no optional mustexist documentation");
        CHECK(e.verifier->type() == "File");
        FileVerifier* v = dynamic_cast<FileVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(!v->mustExist());
    }
    {
        const DocumentationEntry& e = doc.entries[241];
        CHECK(e.key == "NoVectorMustExistValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "no vector mustexist documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].verifier->type() == "File");
        FileVerifier* w = dynamic_cast<FileVerifier*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(!w->mustExist());
    }
    {
        const DocumentationEntry& e = doc.entries[242];
        CHECK(e.key == "NoOptionalVectorMustExistValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "no optionalvector mustexist documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].verifier->type() == "File");
        FileVerifier* w = dynamic_cast<FileVerifier*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(!w->mustExist());
    }
    {
        const DocumentationEntry& e = doc.entries[243];
        CHECK(e.key == "DefaultMustExistDirectoryValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "default mustexist directory documentation");
        CHECK(e.verifier->type() == "Directory");
        DirectoryVerifier* v = dynamic_cast<DirectoryVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->mustExist());
    }
    {
        const DocumentationEntry& e = doc.entries[244];
        CHECK(e.key == "DefaultOptionalMustExistDirectoryValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "default optional mustexist directory documentation");
        CHECK(e.verifier->type() == "Directory");
        DirectoryVerifier* v = dynamic_cast<DirectoryVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->mustExist());
    }
    {
        const DocumentationEntry& e = doc.entries[245];
        CHECK(e.key == "DefaultVectorMustExistDirectoryValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "default vector mustexist directory documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].verifier->type() == "Directory");
        DirectoryVerifier* w = dynamic_cast<DirectoryVerifier*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->mustExist());
    }
    {
        const DocumentationEntry& e = doc.entries[246];
        CHECK(e.key == "DefaultOptionalVectorMustExistDirectoryValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "default optionalvector mustexist directory documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].verifier->type() == "Directory");
        DirectoryVerifier* w = dynamic_cast<DirectoryVerifier*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->mustExist());
    }
    {
        const DocumentationEntry& e = doc.entries[247];
        CHECK(e.key == "YesMustExistDirectoryValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "yes mustexist directory documentation");
        CHECK(e.verifier->type() == "Directory");
        DirectoryVerifier* v = dynamic_cast<DirectoryVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->mustExist());
    }
    {
        const DocumentationEntry& e = doc.entries[248];
        CHECK(e.key == "YesOptionalMustExistDirectoryValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "yes optional mustexist directory documentation");
        CHECK(e.verifier->type() == "Directory");
        DirectoryVerifier* v = dynamic_cast<DirectoryVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->mustExist());
    }
    {
        const DocumentationEntry& e = doc.entries[249];
        CHECK(e.key == "YesVectorMustExistDirectoryValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "yes vector mustexist directory documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].verifier->type() == "Directory");
        DirectoryVerifier* w = dynamic_cast<DirectoryVerifier*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->mustExist());
    }
    {
        const DocumentationEntry& e = doc.entries[250];
        CHECK(e.key == "YesOptionalVectorMustExistDirectoryValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "yes optionalvector mustexist directory documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].verifier->type() == "Directory");
        DirectoryVerifier* w = dynamic_cast<DirectoryVerifier*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(w->mustExist());
    }
    {
        const DocumentationEntry& e = doc.entries[251];
        CHECK(e.key == "NoMustExistDirectoryValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "no mustexist directory documentation");
        CHECK(e.verifier->type() == "Directory");
        DirectoryVerifier* v = dynamic_cast<DirectoryVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(!v->mustExist());
    }
    {
        const DocumentationEntry& e = doc.entries[252];
        CHECK(e.key == "NoOptionalMustExistDirectoryValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "no optional mustexist directory documentation");
        CHECK(e.verifier->type() == "Directory");
        DirectoryVerifier* v = dynamic_cast<DirectoryVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(!v->mustExist());
    }
    {
        const DocumentationEntry& e = doc.entries[253];
        CHECK(e.key == "NoVectorMustExistDirectoryValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "no vector mustexist directory documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].verifier->type() == "Directory");
        DirectoryVerifier* w = dynamic_cast<DirectoryVerifier*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(!w->mustExist());
    }
    {
        const DocumentationEntry& e = doc.entries[254];
        CHECK(e.key == "NoOptionalVectorMustExistDirectoryValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "no optionalvector mustexist directory documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].verifier->type() == "Directory");
        DirectoryVerifier* w = dynamic_cast<DirectoryVerifier*>(
            v->documentations[0].verifier.get()
            );
        REQUIRE(w);
        CHECK(!w->mustExist());
    }
}
