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

#include "verifier.h"

#include "keywords.h"
#include "types.h"
#include <cassert>
#include <format>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>

namespace {
    void reportUnsupportedAttribute(BasicType::Type type,
                                    const Variable::Attributes& attributes)
    {
        auto report = [type](std::string_view attr, std::string_view name) {
            if (!attr.empty()) {
                std::string t = generateTypename(type);
                throw CodegenError(std::format(
                    "Type '{}' does not support attribute '{}'", t, name
                ));
            }
        };
        auto reportBool = [type](bool attr, std::string_view name) {
            if (attr) {
                std::string t = generateTypename(type);
                throw CodegenError(std::format(
                    "Type '{}' does not support attribute '{}'", t, name
                ));
            }
        };

        using Type = BasicType::Type;
        if (type == Type::Bool ||
            type == Type::Mat2x2 || type == Type::Mat2x3 || type == Type::Mat2x4 ||
            type == Type::Mat3x2 || type == Type::Mat3x3 || type == Type::Mat3x4 ||
            type == Type::Mat4x2 || type == Type::Mat4x3 || type == Type::Mat4x4 ||
            type == Type::DMat2x2 || type == Type::DMat2x3 || type == Type::DMat2x4 ||
            type == Type::DMat3x2 || type == Type::DMat3x3 || type == Type::DMat3x4 ||
            type == Type::DMat4x2 || type == Type::DMat4x3 || type == Type::DMat4x4)
        {
            report(attributes.annotation, keywords::Annotation);
            report(attributes.inlist, keywords::InList);
            report(attributes.inrange, keywords::InRange);
            report(attributes.less, keywords::Less);
            report(attributes.lessequal, keywords::LessEqual);
            report(attributes.greater, keywords::Greater);
            report(attributes.greaterequal, keywords::GreaterEqual);
            report(attributes.notinlist, keywords::NotInList);
            report(attributes.notinrange, keywords::NotInRange);
            report(attributes.reference, keywords::Reference);
            report(attributes.unequal, keywords::Unequal);
            reportBool(attributes.isColor, keywords::Color);
            reportBool(attributes.isDirectory, keywords::Directory);
            reportBool(attributes.isDateTime, keywords::DateTime);
            reportBool(attributes.isIdentifier, keywords::Identifier);
            reportBool(attributes.mustBeNotEmpty, keywords::MustBeNotEmpty);
        }
        else if (
            type == Type::Ivec2 || type == Type::Ivec3 || type == Type::Ivec4 ||
            type == Type::Dvec2 || type == Type::Vec2)
        {
            report(attributes.annotation, keywords::Annotation);
            report(attributes.inlist, keywords::InList);
            report(attributes.notinlist, keywords::NotInList);
            report(attributes.reference, keywords::Reference);
            reportBool(attributes.isColor, keywords::Color);
            reportBool(attributes.isDirectory, keywords::Directory);
            reportBool(attributes.isDateTime, keywords::DateTime);
            reportBool(attributes.isIdentifier, keywords::Identifier);
            reportBool(attributes.mustBeNotEmpty, keywords::MustBeNotEmpty);
        }
        else if (
            type == Type::Dvec3 || type == Type::Dvec4 ||
            type == Type::Vec3 || type == Type::Vec4)
        {
            report(attributes.annotation, keywords::Annotation);
            report(attributes.inlist, keywords::InList);
            report(attributes.notinlist, keywords::NotInList);
            report(attributes.reference, keywords::Reference);
            reportBool(attributes.isDirectory, keywords::Directory);
            reportBool(attributes.isDateTime, keywords::DateTime);
            reportBool(attributes.isIdentifier, keywords::Identifier);
            reportBool(attributes.mustBeNotEmpty, keywords::MustBeNotEmpty);
        }
        else if (type == Type::Int || type == Type::Double || type == Type::Float) {
            report(attributes.annotation, keywords::Annotation);
            report(attributes.inlist, keywords::InList);
            report(attributes.notinlist, keywords::NotInList);
            report(attributes.reference, keywords::Reference);
            reportBool(attributes.isColor, keywords::Color);
            reportBool(attributes.isDirectory, keywords::Directory);
            reportBool(attributes.isDateTime, keywords::DateTime);
            reportBool(attributes.isIdentifier, keywords::Identifier);
            reportBool(attributes.mustBeNotEmpty, keywords::MustBeNotEmpty);
        }
        else if (type == Type::String) {
            report(attributes.inrange, keywords::InRange);
            report(attributes.less, keywords::Less);
            report(attributes.lessequal, keywords::LessEqual);
            report(attributes.greater, keywords::Greater);
            report(attributes.greaterequal, keywords::GreaterEqual);
            report(attributes.notinrange, keywords::NotInRange);
            report(attributes.reference, keywords::Reference);
            reportBool(attributes.isColor, keywords::Color);
            reportBool(attributes.isDirectory, keywords::Directory);
        }
        else if (type == Type::Path) {
            report(attributes.annotation, keywords::Annotation);
            report(attributes.inlist, keywords::InList);
            report(attributes.inrange, keywords::InRange);
            report(attributes.less, keywords::Less);
            report(attributes.lessequal, keywords::LessEqual);
            report(attributes.greater, keywords::Greater);
            report(attributes.greaterequal, keywords::GreaterEqual);
            report(attributes.notinlist, keywords::NotInList);
            report(attributes.notinrange, keywords::NotInRange);
            report(attributes.unequal, keywords::Unequal);
            reportBool(attributes.isColor, keywords::Color);
            reportBool(attributes.isDateTime, keywords::DateTime);
            reportBool(attributes.isIdentifier, keywords::Identifier);
            reportBool(attributes.mustBeNotEmpty, keywords::MustBeNotEmpty);
        }
        else if (type == Type::Dictionary) {
            report(attributes.annotation, keywords::Annotation);
            report(attributes.inlist, keywords::InList);
            report(attributes.inrange, keywords::InRange);
            report(attributes.less, keywords::Less);
            report(attributes.lessequal, keywords::LessEqual);
            report(attributes.greater, keywords::Greater);
            report(attributes.greaterequal, keywords::GreaterEqual);
            report(attributes.notinlist, keywords::NotInList);
            report(attributes.notinrange, keywords::NotInRange);
            report(attributes.unequal, keywords::Unequal);
            reportBool(attributes.isColor, keywords::Color);
            reportBool(attributes.isDirectory, keywords::Directory);
            reportBool(attributes.isDateTime, keywords::DateTime);
            reportBool(attributes.isIdentifier, keywords::Identifier);
            reportBool(attributes.mustBeNotEmpty, keywords::MustBeNotEmpty);
        }
    }

    std::string addQualifier(std::string ver, std::string qual, std::string_view param) {
        assert(!ver.empty());
        assert(!qual.empty());
        return std::format("{}<{}>({})", std::move(qual), std::move(ver), param);
    }
} // namespace

std::string verifierForType(BasicType::Type type, const Variable::Attributes& attributes)
{
    using Type = BasicType::Type;

    // @TODO (abock, 2021-02-12) Really the check for unsupported attributes should be
    // done directly after parsing and not when we are trying to generate the verifiers
    reportUnsupportedAttribute(type, attributes);
    std::string res;
    switch (type) {
        case Type::Bool: return "BoolVerifier";
        case Type::Int:
            res = "IntVerifier";
            if (!attributes.inrange.empty()) {
                res = addQualifier(res, "InRangeVerifier", attributes.inrange);
            }
            if (!attributes.notinrange.empty()) {
                res = addQualifier(res, "NotInRangeVerifier", attributes.notinrange);
            }
            if (!attributes.less.empty()) {
                res = addQualifier(res, "LessVerifier", attributes.less);
            }
            if (!attributes.lessequal.empty()) {
                res = addQualifier(res, "LessEqualVerifier", attributes.lessequal);
            }
            if (!attributes.greater.empty()) {
                res = addQualifier(res, "GreaterVerifier", attributes.greater);
            }
            if (!attributes.greaterequal.empty()) {
                res = addQualifier(res, "GreaterEqualVerifier", attributes.greaterequal);
            }
            if (!attributes.unequal.empty()) {
                res = addQualifier(res, "UnequalVerifier", attributes.unequal);
            }
            return res;
        case Type::Double:
        case Type::Float:
            res = "DoubleVerifier";
            if (!attributes.inrange.empty()) {
                res = addQualifier(res, "InRangeVerifier", attributes.inrange);
            }
            if (!attributes.notinrange.empty()) {
                res = addQualifier(res, "NotInRangeVerifier", attributes.notinrange);
            }
            if (!attributes.less.empty()) {
                res = addQualifier(res, "LessVerifier", attributes.less);
            }
            if (!attributes.lessequal.empty()) {
                res = addQualifier(res, "LessEqualVerifier", attributes.lessequal);
            }
            if (!attributes.greater.empty()) {
                res = addQualifier(res, "GreaterVerifier", attributes.greater);
            }
            if (!attributes.greaterequal.empty()) {
                res = addQualifier(res, "GreaterEqualVerifier", attributes.greaterequal);
            }
            if (!attributes.unequal.empty()) {
                res = addQualifier(res, "UnequalVerifier", attributes.unequal);
            }
            return res;
        case Type::String:
            res = "StringVerifier";
            if (!attributes.inlist.empty()) {
                const std::string param = '{' + std::string(attributes.inlist) + '}';
                res = addQualifier(res, "InListVerifier", param);
            }
            if (!attributes.notinlist.empty()) {
                const std::string param = '{' + std::string(attributes.notinlist) + '}';
                res = addQualifier(res, "NotInListVerifier", param);
            }
            if (!attributes.unequal.empty()) {
                res = addQualifier(res, "UnequalVerifier", attributes.unequal);
            }
            if (attributes.mustBeNotEmpty) {
                if (!attributes.inlist.empty() || !attributes.unequal.empty() ||
                    !attributes.annotation.empty())
                {
                    throw CodegenError(std::format(
                        "With the notempty attribute, no other attribute can be used:\n{}",
                        generateTypename(type)
                    ));
                }

                res = "StringVerifier(true)";
            }
            if (!attributes.annotation.empty()) {
                if (!attributes.inlist.empty() || !attributes.unequal.empty() ||
                    attributes.mustBeNotEmpty)
                {
                    throw CodegenError(std::format(
                        "With the annotation attribute, no other attribute can be used:\n{}",
                        generateTypename(type)
                    ));
                }
                res = addQualifier(res, "AnnotationVerifier", attributes.annotation);
            }
            if (attributes.isDateTime) {
                return "DateTimeVerifier";
            }
            if (attributes.isIdentifier) {
                return "IdentifierVerifier";
            }
            return res;
        case Type::Path:
            if (attributes.isDirectory) {
                if (attributes.mustExist) {
                    return "DirectoryVerifier(true)";
                }
                else {
                    return "DirectoryVerifier(false)";
                }
            }
            else {
                if (attributes.mustExist) {
                    return "FileVerifier(true)";
                }
                else {
                    return "FileVerifier(false)";
                }
            }
        case Type::Ivec2:
            res = "IntVector2Verifier";
            if (!attributes.inrange.empty()) {
                res = addQualifier(res, "InRangeVerifier", attributes.inrange);
            }
            if (!attributes.notinrange.empty()) {
                res = addQualifier(res, "NotInRangeVerifier", attributes.notinrange);
            }
            if (!attributes.less.empty()) {
                res = addQualifier(res, "LessVerifier", attributes.less);
            }
            if (!attributes.lessequal.empty()) {
                res = addQualifier(res, "LessEqualVerifier", attributes.lessequal);
            }
            if (!attributes.greater.empty()) {
                res = addQualifier(res, "GreaterVerifier", attributes.greater);
            }
            if (!attributes.greaterequal.empty()) {
                res = addQualifier(res, "GreaterEqualVerifier", attributes.greaterequal);
            }
            if (!attributes.unequal.empty()) {
                res = addQualifier(res, "UnequalVerifier", attributes.unequal);
            }
            return res;
        case Type::Ivec3:
            res = "IntVector3Verifier";
            if (!attributes.inrange.empty()) {
                res = addQualifier(res, "InRangeVerifier", attributes.inrange);
            }
            if (!attributes.notinrange.empty()) {
                res = addQualifier(res, "NotInRangeVerifier", attributes.notinrange);
            }
            if (!attributes.less.empty()) {
                res = addQualifier(res, "LessVerifier", attributes.less);
            }
            if (!attributes.lessequal.empty()) {
                res = addQualifier(res, "LessEqualVerifier", attributes.lessequal);
            }
            if (!attributes.greater.empty()) {
                res = addQualifier(res, "GreaterVerifier", attributes.greater);
            }
            if (!attributes.greaterequal.empty()) {
                res = addQualifier(res, "GreaterEqualVerifier", attributes.greaterequal);
            }
            if (!attributes.unequal.empty()) {
                res = addQualifier(res, "UnequalVerifier", attributes.unequal);
            }
            return res;
        case Type::Ivec4:
            res = "IntVector4Verifier";
            if (!attributes.inrange.empty()) {
                res = addQualifier(res, "InRangeVerifier", attributes.inrange);
            }
            if (!attributes.notinrange.empty()) {
                res = addQualifier(res, "NotInRangeVerifier", attributes.notinrange);
            }
            if (!attributes.less.empty()) {
                res = addQualifier(res, "LessVerifier", attributes.less);
            }
            if (!attributes.lessequal.empty()) {
                res = addQualifier(res, "LessEqualVerifier", attributes.lessequal);
            }
            if (!attributes.greater.empty()) {
                res = addQualifier(res, "GreaterVerifier", attributes.greater);
            }
            if (!attributes.greaterequal.empty()) {
                res = addQualifier(res, "GreaterEqualVerifier", attributes.greaterequal);
            }
            if (!attributes.unequal.empty()) {
                res = addQualifier(res, "UnequalVerifier", attributes.unequal);
            }
            return res;
        case Type::Dvec2:
        case Type::Vec2:
            res = "DoubleVector2Verifier";
            if (!attributes.inrange.empty()) {
                res = addQualifier(res, "InRangeVerifier", attributes.inrange);
            }
            if (!attributes.notinrange.empty()) {
                res = addQualifier(res, "NotInRangeVerifier", attributes.notinrange);
            }
            if (!attributes.less.empty()) {
                res = addQualifier(res, "LessVerifier", attributes.less);
            }
            if (!attributes.lessequal.empty()) {
                res = addQualifier(res, "LessEqualVerifier", attributes.lessequal);
            }
            if (!attributes.greater.empty()) {
                res = addQualifier(res, "GreaterVerifier", attributes.greater);
            }
            if (!attributes.greaterequal.empty()) {
                res = addQualifier(res, "GreaterEqualVerifier", attributes.greaterequal);
            }
            if (!attributes.unequal.empty()) {
                res = addQualifier(res, "UnequalVerifier", attributes.unequal);
            }
            return res;
        case Type::Dvec3:
        case Type::Vec3:
            if (attributes.isColor) {
                res = "Color3Verifier";
            }
            else {
                res = "DoubleVector3Verifier";
            }

            if (!attributes.inrange.empty()) {
                res = addQualifier(res, "InRangeVerifier", attributes.inrange);
            }
            if (!attributes.notinrange.empty()) {
                res = addQualifier(res, "NotInRangeVerifier", attributes.notinrange);
            }
            if (!attributes.less.empty()) {
                res = addQualifier(res, "LessVerifier", attributes.less);
            }
            if (!attributes.lessequal.empty()) {
                res = addQualifier(res, "LessEqualVerifier", attributes.lessequal);
            }
            if (!attributes.greater.empty()) {
                res = addQualifier(res, "GreaterVerifier", attributes.greater);
            }
            if (!attributes.greaterequal.empty()) {
                res = addQualifier(res, "GreaterEqualVerifier", attributes.greaterequal);
            }
            if (!attributes.unequal.empty()) {
                res = addQualifier(res, "UnequalVerifier", attributes.unequal);
            }
            return res;
        case Type::Dvec4:
        case Type::Vec4:
            if (attributes.isColor) {
                res = "Color4Verifier";
            }
            else {
                res = "DoubleVector4Verifier";
            }

            if (!attributes.inrange.empty()) {
                res = addQualifier(res, "InRangeVerifier", attributes.inrange);
            }
            if (!attributes.notinrange.empty()) {
                res = addQualifier(res, "NotInRangeVerifier", attributes.notinrange);
            }
            if (!attributes.less.empty()) {
                res = addQualifier(res, "LessVerifier", attributes.less);
            }
            if (!attributes.lessequal.empty()) {
                res = addQualifier(res, "LessEqualVerifier", attributes.lessequal);
            }
            if (!attributes.greater.empty()) {
                res = addQualifier(res, "GreaterVerifier", attributes.greater);
            }
            if (!attributes.greaterequal.empty()) {
                res = addQualifier(res, "GreaterEqualVerifier", attributes.greaterequal);
            }
            if (!attributes.unequal.empty()) {
                res = addQualifier(res, "UnequalVerifier", attributes.unequal);
            }
            return res;
        case Type::DMat2x2: return "DoubleMatrix2x2Verifier";
        case Type::DMat2x3: return "DoubleMatrix2x3Verifier";
        case Type::DMat2x4: return "DoubleMatrix2x4Verifier";
        case Type::DMat3x2: return "DoubleMatrix3x2Verifier";
        case Type::DMat3x3: return "DoubleMatrix3x3Verifier";
        case Type::DMat3x4: return "DoubleMatrix3x4Verifier";
        case Type::DMat4x2: return "DoubleMatrix4x2Verifier";
        case Type::DMat4x3: return "DoubleMatrix4x3Verifier";
        case Type::DMat4x4: return "DoubleMatrix4x4Verifier";
        case Type::Mat2x2: return "DoubleMatrix2x2Verifier";
        case Type::Mat2x3: return "DoubleMatrix2x3Verifier";
        case Type::Mat2x4: return "DoubleMatrix2x4Verifier";
        case Type::Mat3x2: return "DoubleMatrix3x2Verifier";
        case Type::Mat3x3: return "DoubleMatrix3x3Verifier";
        case Type::Mat3x4: return "DoubleMatrix3x4Verifier";
        case Type::Mat4x2: return "DoubleMatrix4x2Verifier";
        case Type::Mat4x3: return "DoubleMatrix4x3Verifier";
        case Type::Mat4x4: return "DoubleMatrix4x4Verifier";
        case Type::Dictionary:
            if (attributes.reference.empty()) {
                return "TableVerifier";
            }
            else {
                return std::format("ReferencingVerifier({})", attributes.reference);
            }
        default: throw std::logic_error("Missing case label");
    }
}
