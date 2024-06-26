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

#include <modules/base/rotation/fixedrotation.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/query/query.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/profiling.h>
#include <optional>
#include <string>
#include <variant>

namespace {
    constexpr openspace::properties::Property::PropertyInfo EnableInfo = {
        "Enable",
        "Enabled",
        "If this value is 'true', all the machinery of this rotation is used, of it is "
        "'false', it provides the ability to change its attributes without risking some "
        "undefined behavior.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo XAxisTypeInfo = {
        "xAxisType",
        "xAxis: Specification Type",
        "This value specifies how this axis is being specified, that is whether it is "
        "referencing another object, specifying an absolute vector, or whether it is "
        "using the right handed coordinate system completion based off the other two "
        "vectors.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo YAxisTypeInfo = {
        "yAxisType",
        "yAxis: Specification Type",
        "This value specifies how this axis is being specified, that is whether it is "
        "referencing another object, specifying an absolute vector, or whether it is "
        "using the right handed coordinate system completion based off the other two "
        "vectors.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ZAxisTypeInfo = {
        "zAxisType",
        "zAxis: Specification Type",
        "This value specifies how this axis is being specified, that is whether it is "
        "referencing another object, specifying an absolute vector, or whether it is "
        "using the right handed coordinate system completion based off the other two "
        "vectors.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo XAxisObjectInfo = {
        "xAxisObject",
        "xAxis: Focus Object",
        "This is the object that the axis will focus on. This object must name an "
        "existing scene graph node in the currently loaded scene and the rotation will "
        "stay fixed to the current position of that object.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo YAxisObjectInfo = {
        "yAxisObject",
        "yAxis: Focus Object",
        "This is the object that the axis will focus on. This object must name an "
        "existing scene graph node in the currently loaded scene and the rotation will "
        "stay fixed to the current position of that object.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ZAxisObjectInfo = {
        "zAxisObject",
        "zAxis: Focus Object",
        "This is the object that the axis will focus on. This object must name an "
        "existing scene graph node in the currently loaded scene and the rotation will "
        "stay fixed to the current position of that object.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo XAxisInvertObjectInfo = {
        "xAxisInvertObject",
        "xAxis: Invert Object Point Direction",
        "If this value is set to 'true', and the type is set to 'Object', the inverse of "
        "the pointing direction is used, causing the object to point away from the "
        "referenced object.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo YAxisInvertObjectInfo = {
        "yAxisInvertObject",
        "yAxis: Invert Object Point Direction",
        "If this value is set to 'true', and the type is set to 'Object', the inverse of "
        "the pointing direction is used, causing the object to point away from the "
        "referenced object.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ZAxisInvertObjectInfo = {
        "zAxisInvertObject",
        "zAxis: Invert Object Point Direction",
        "If this value is set to 'true', and the type is set to 'Object', the inverse of "
        "the pointing direction is used, causing the object to point away from the "
        "referenced object.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo XAxisVectorInfo = {
        "xAxisVector",
        "xAxis: Direction vector",
        "This value specifies a static direction vector that is used for a fixed "
        "rotation.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo YAxisVectorInfo = {
        "yAxisVector",
        "yAxis: Direction vector",
        "This value specifies a static direction vector that is used for a fixed "
        "rotation.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ZAxisVectorInfo = {
        "zAxisVector",
        "zAxis: Direction vector",
        "This value specifies a static direction vector that is used for a fixed "
        "rotation.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo XAxisOrthogonalVectorInfo = {
        "xAxisOrthogonal",
        "xAxis: Vector is orthogonal",
        "This value determines whether the vector specified is used directly, or whether "
        "it is used together with another non-coordinate system completion vector to "
        "construct an orthogonal vector instead.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo YAxisOrthogonalVectorInfo = {
        "yAxisOrthogonal",
        "yAxis: Vector is orthogonal",
        "This value determines whether the vector specified is used directly, or whether "
        "it is used together with another non-coordinate system completion vector to "
        "construct an orthogonal vector instead.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ZAxisOrthogonalVectorInfo = {
        "zAxisOrthogonal",
        "zAxis: Vector is orthogonal",
        "This value determines whether the vector specified is used directly, or whether "
        "it is used together with another non-coordinate system completion vector to "
        "construct an orthogonal vector instead.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AttachedInfo = {
        "Attached",
        "Attached Node",
        "This is the name of the node that this rotation is attached to, this value is "
        "only needed if any of the three axis uses the Object type. In this case, the "
        "location of the attached node is required to compute the relative direction.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(FixedRotation)]] Parameters {
        // This value specifies the direction of the new X axis. If this value is not
        // specified, it will be computed by completing a right handed coordinate system
        // from the Y and Z axis, which must be specified instead. If this value is a
        // string, it is interpreted as the identifier of another scenegraph node. If this
        // value is a 3-vector, it is interpreted as a direction vector
        std::optional<std::variant<std::string, glm::vec3>> xAxis;

        // [[codegen::verbatim(XAxisOrthogonalVectorInfo.description)]]
        std::optional<bool> xAxisOrthogonal;

        // [[codegen::verbatim(XAxisInvertObjectInfo.description)]]
        std::optional<bool> xAxisInvert;

        // This value specifies the direction of the new Y axis. If this value is not
        // specified, it will be computed by completing a right handed coordinate system
        // from the X and Z axis, which must be specified instead. If this value is a
        // string, it is interpreted as the identifier of another scenegraph node. If this
        // value is a 3-vector, it is interpreted as a direction vector
        std::optional<std::variant<std::string, glm::vec3>> yAxis;

        // [[codegen::verbatim(YAxisOrthogonalVectorInfo.description)]]
        std::optional<bool> yAxisOrthogonal;

        // [[codegen::verbatim(YAxisInvertObjectInfo.description)]]
        std::optional<bool> yAxisInvert;

        // This value specifies the direction of the new Z axis. If this value is not
        // specified, it will be computed by completing a right handed coordinate system
        // from the X and Y axis, which must be specified instead. If this value is a
        // string, it is interpreted as the identifier of another scenegraph node. If this
        // value is a 3-vector, it is interpreted as a direction vector
        std::optional<std::variant<std::string, glm::vec3>> zAxis;

        // [[codegen::verbatim(ZAxisOrthogonalVectorInfo.description)]]
        std::optional<bool> zAxisOrthogonal;

        // [[codegen::verbatim(ZAxisInvertObjectInfo.description)]]
        std::optional<bool> zAxisInvert;

        // [[codegen::verbatim(AttachedInfo.description)]]
        std::optional<std::string> attached;
    };
#include "fixedrotation_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation FixedRotation::Documentation() {
    return codegen::doc<Parameters>("base_transform_rotation_fixed");
}

FixedRotation::FixedRotation(const ghoul::Dictionary& dictionary)
    : _enabled(EnableInfo, true)
    , _xAxis{
        properties::OptionProperty(
            XAxisTypeInfo,
            properties::OptionProperty::DisplayType::Dropdown
        ),
        properties::StringProperty(XAxisObjectInfo, ""),
        properties::BoolProperty(XAxisInvertObjectInfo, false),
        properties::Vec3Property(
            XAxisVectorInfo,
            glm::vec3(1.f, 0.f, 0.f),
            glm::vec3(-1.f),
            glm::vec3(1.f)
        ),
        properties::BoolProperty(XAxisOrthogonalVectorInfo, false),
        nullptr
    }
    , _yAxis{
        properties::OptionProperty(
            YAxisTypeInfo,
            properties::OptionProperty::DisplayType::Dropdown
        ),
        properties::StringProperty(YAxisObjectInfo, ""),
        properties::BoolProperty(YAxisInvertObjectInfo, false),
        properties::Vec3Property(
            YAxisVectorInfo,
            glm::vec3(0.f, 1.f, 0.f),
            glm::vec3(-1.f),
            glm::vec3(1.f)
        ),
        properties::BoolProperty(YAxisOrthogonalVectorInfo, false),
        nullptr
    }
    , _zAxis{
        properties::OptionProperty(
            ZAxisTypeInfo,
            properties::OptionProperty::DisplayType::Dropdown
        ),
        properties::StringProperty(ZAxisObjectInfo, ""),
        properties::BoolProperty(ZAxisInvertObjectInfo, false),
        properties::Vec3Property(
            ZAxisVectorInfo,
            glm::vec3(0.f, 0.f, 1.f),
            glm::vec3(-1.f),
            glm::vec3(1.f)
        ),
        properties::BoolProperty(ZAxisOrthogonalVectorInfo, false),
        nullptr
    }
    , _attachedObject(AttachedInfo, "")
    , _constructorDictionary(dictionary)
{
    // We check the Dictionary here in order to detect the errors early
    codegen::bake<Parameters>(dictionary);

    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "FixedRotation"
    );

    setPropertyGroupName("global", "Global");
    setPropertyGroupName("xAxis", "X Axis");
    setPropertyGroupName("yAxis", "Y Axis");
    setPropertyGroupName("zAxis", "Z Axis");

    _enabled.setGroupIdentifier("global");
    addProperty(_enabled);

    _attachedObject.setGroupIdentifier("global");
    addProperty(_attachedObject);
    _attachedObject.onChange([this](){
        _attachedNode = sceneGraphNode(_attachedObject);
    });

    auto setPropertyVisibility = [](Axis& axis) {
        using Visibility = properties::Property::Visibility;
        switch (axis.type) {
            case Axis::Type::Object:
                axis.object.setVisibility(Visibility::User);
                axis.invertObject.setVisibility(Visibility::User);
                axis.vector.setVisibility(Visibility::Hidden);
                break;
            case Axis::Type::Vector:
            case Axis::Type::OrthogonalVector:
                axis.object.setVisibility(Visibility::Hidden);
                axis.invertObject.setVisibility(Visibility::Hidden);
                axis.vector.setVisibility(Visibility::User);
                break;
            case Axis::Type::CoordinateSystemCompletion:
                axis.object.setVisibility(Visibility::Hidden);
                axis.invertObject.setVisibility(Visibility::Hidden);
                axis.vector.setVisibility(Visibility::Hidden);
                break;
            }
    };

    _xAxis.type.addOptions({
        { Axis::Type::Object, "Object" },
        { Axis::Type::Vector, "Vector" },
        { Axis::Type::OrthogonalVector, "Orthogonal Vector" },
        { Axis::Type::CoordinateSystemCompletion, "Coordinate System Completion" }
    });
    _xAxis.type.setGroupIdentifier("xAxis");
    _xAxis.type.onChange(
        [this, setPropertyVisibility]() { setPropertyVisibility(_xAxis); }
    );
    addProperty(_xAxis.type);

    _xAxis.object.setGroupIdentifier("xAxis");
    addProperty(_xAxis.object);
    _xAxis.object.onChange([this](){
        _xAxis.node = sceneGraphNode(_xAxis.object);
    });

    _xAxis.invertObject.setGroupIdentifier("xAxis");
    addProperty(_xAxis.invertObject);

    _xAxis.vector.setGroupIdentifier("xAxis");
    addProperty(_xAxis.vector);


    _yAxis.type.addOptions({
        { Axis::Type::Object, "Object" },
        { Axis::Type::Vector, "Vector" },
        { Axis::Type::OrthogonalVector, "Orthogonal Vector" },
        { Axis::Type::CoordinateSystemCompletion, "Coordinate System Completion" }
    });
    _yAxis.type.setGroupIdentifier("yAxis");
    _yAxis.type.onChange(
        [this, setPropertyVisibility]() { setPropertyVisibility(_yAxis); }
    );
    addProperty(_yAxis.type);

    _yAxis.object.setGroupIdentifier("yAxis");
    addProperty(_yAxis.object);
    _yAxis.object.onChange([this]() { _yAxis.node = sceneGraphNode(_yAxis.object); });

    _yAxis.invertObject.setGroupIdentifier("yAxis");
    addProperty(_yAxis.invertObject);

    _yAxis.vector.setGroupIdentifier("yAxis");
    addProperty(_yAxis.vector);

    _zAxis.type.addOptions({
        { Axis::Type::Object, "Object" },
        { Axis::Type::Vector, "Vector" },
        { Axis::Type::OrthogonalVector, "Orthogonal Vector" },
        { Axis::Type::CoordinateSystemCompletion, "Coordinate System Completion" }
    });
    _zAxis.type.setGroupIdentifier("zAxis");
    _zAxis.type.onChange(
        [this, setPropertyVisibility]() { setPropertyVisibility(_zAxis); }
    );
    addProperty(_zAxis.type);

    _zAxis.object.setGroupIdentifier("zAxis");
    addProperty(_zAxis.object);
    _zAxis.object.onChange([this]() { _zAxis.node = sceneGraphNode(_zAxis.object); });

    _zAxis.invertObject.setGroupIdentifier("zAxis");
    addProperty(_zAxis.invertObject);

    _zAxis.vector.setGroupIdentifier("zAxis");
    addProperty(_zAxis.vector);

    setPropertyVisibility(_xAxis);
    setPropertyVisibility(_yAxis);
    setPropertyVisibility(_zAxis);
}

bool FixedRotation::initialize() {
    ZoneScoped;

    // We have already checked this before, but still
    const Parameters p = codegen::bake<Parameters>(_constructorDictionary);

    // We need to do this in the initialize and not the constructor as the scene graph
    // nodes referenced in the dictionary might not exist yet at construction time. At
    // initialization time, however, we know that they already have been created

    const bool res = Rotation::initialize();

    _attachedObject = p.attached.value_or(_attachedObject);

    if (p.xAxis.has_value()) {
        if (std::holds_alternative<std::string>(*p.xAxis)) {
            _xAxis.type = Axis::Type::Object;
            _xAxis.object = std::get<std::string>(*p.xAxis);
        }
        else {
            ghoul_assert(std::holds_alternative<glm::vec3>(*p.xAxis), "");
            _xAxis.type = Axis::Type::Vector;
            _xAxis.vector = std::get<glm::vec3>(*p.xAxis);
        }
    }
    _xAxis.isOrthogonal = p.xAxisOrthogonal.value_or(_xAxis.isOrthogonal);
    if (_xAxis.isOrthogonal) {
        _xAxis.type = Axis::Type::OrthogonalVector;
    }
    _xAxis.invertObject = p.xAxisInvert.value_or(_xAxis.invertObject);

    if (p.yAxis.has_value()) {
        if (std::holds_alternative<std::string>(*p.yAxis)) {
            _yAxis.type = Axis::Type::Object;
            _yAxis.object = std::get<std::string>(*p.yAxis);
        }
        else {
            ghoul_assert(std::holds_alternative<glm::vec3>(*p.yAxis), "");
            _yAxis.type = Axis::Type::Vector;
            _yAxis.vector = std::get<glm::vec3>(*p.yAxis);
        }
    }
    _yAxis.isOrthogonal = p.yAxisOrthogonal.value_or(_yAxis.isOrthogonal);
    if (_yAxis.isOrthogonal) {
        _yAxis.type = Axis::Type::OrthogonalVector;
    }
    _yAxis.invertObject = p.yAxisInvert.value_or(_yAxis.invertObject);

    if (p.zAxis.has_value()) {
        if (std::holds_alternative<std::string>(*p.zAxis)) {
            _zAxis.type = Axis::Type::Object;
            _zAxis.object = std::get<std::string>(*p.zAxis);
        }
        else {
            ghoul_assert(std::holds_alternative<glm::vec3>(*p.zAxis), "");
            _zAxis.type = Axis::Type::Vector;
            _zAxis.vector = std::get<glm::vec3>(*p.zAxis);
        }
    }
    _zAxis.isOrthogonal = p.zAxisOrthogonal.value_or(_zAxis.isOrthogonal);
    if (_zAxis.isOrthogonal) {
        _zAxis.type = Axis::Type::OrthogonalVector;
    }
    _zAxis.invertObject = p.zAxisInvert.value_or(_zAxis.invertObject);



    if (!p.xAxis.has_value() && p.yAxis.has_value() && p.zAxis.has_value()) {
        _xAxis.type = Axis::Type::CoordinateSystemCompletion;
    }

    if (p.xAxis.has_value() && !p.yAxis.has_value() && p.zAxis.has_value()) {
        _yAxis.type = Axis::Type::CoordinateSystemCompletion;
    }

    if (p.xAxis.has_value() && p.yAxis.has_value() && !p.zAxis.has_value()) {
        _zAxis.type = Axis::Type::CoordinateSystemCompletion;
    }

    // No need to hold on to the data
    _constructorDictionary = ghoul::Dictionary();
    return res;
}

void FixedRotation::update(const UpdateData& data) {
    const bool anyAxisIsObjectType = (
        _xAxis.type == Axis::Type::Object ||
        _yAxis.type == Axis::Type::Object ||
        _zAxis.type == Axis::Type::Object
    );

    // @TODO (2024-06-15, abock) None of the following four checks should be necessary
    //       as the nodes are retrieved whenever the property value is changed, but we
    //       had an initialization issue and this was the fastest way to fix the bug
    if (!_attachedNode) {
        _attachedNode = sceneGraphNode(_attachedObject);
    }
    if (_xAxis.type == Axis::Type::Object && !_xAxis.node) {
        _xAxis.node = sceneGraphNode(_xAxis.object);
    }
    if (_yAxis.type == Axis::Type::Object && !_yAxis.node) {
        _yAxis.node = sceneGraphNode(_yAxis.object);
    }
    if (_zAxis.type == Axis::Type::Object && !_zAxis.node) {
        _zAxis.node = sceneGraphNode(_zAxis.object);
    }

    if (_attachedNode || anyAxisIsObjectType) {
        requireUpdate();
    }

    Rotation::update(data);
}

glm::dmat3 FixedRotation::matrix(const UpdateData&) const {
    if (!_enabled) {
        return glm::dmat3();
    }

    const glm::vec3 x = xAxis();
    const glm::vec3 y = yAxis();
    const glm::vec3 z = zAxis();

    constexpr float Epsilon = 1e-3f;

    if (glm::dot(x, y) > 1.f - Epsilon ||
        glm::dot(y, z) > 1.f - Epsilon ||
        glm::dot(x, z) > 1.f - Epsilon)
    {
        LWARNINGC(
            "FixedRotation",
            std::format(
                "Near-collinear vectors detected: "
                "x ({}, {}, {}) y ({}, {}, {}) z ({}, {}, {})",
                x.x, x.y, x.z, y.x, y.y, y.z, z.x, z.y, z.z
            )
        );
        return glm::dmat3();
    }
    else {
        return {
            x.x, x.y, x.z,
            y.x, y.y, y.z,
            z.x, z.y, z.z
        };
    }
}

glm::vec3 FixedRotation::xAxis() const {
    switch (_xAxis.type) {
        case Axis::Type::Unspecified:
            LWARNINGC("FixedRotation", "Unspecified axis type for X axis");
            return glm::vec3(1.f, 0.f, 0.f);
        case Axis::Type::Object:
            if (_xAxis.node && _attachedNode) {
                glm::dvec3 dir = _xAxis.node->worldPosition() -
                    _attachedNode->worldPosition();

                if (dir == glm::dvec3(0.0)) {
                    dir = glm::dvec3(1.0, 0.0, 0.0);
                }
                const glm::vec3 dirNorm = glm::vec3(glm::normalize(dir));
                return _xAxis.invertObject ? -dirNorm : dirNorm;
            }
            else {
                if (_xAxis.node) {
                    LWARNINGC("FixedRotation", "Missing attachment node");
                    return glm::vec3(1.f, 0.f, 0.f);
                }
                else {
                    LWARNINGC("FixedRotation", "Missing node for X axis");
                    return glm::vec3(1.f, 0.f, 0.f);
                }
            }
        case Axis::Type::Vector:
            if (_xAxis.vector.value() == glm::vec3(0.f)) {
                LWARNINGC("FixedRotation", "Zero vector detected for X Axis");
                return glm::vec3(1.f, 0.f, 0.f);
            }
            else {
                return glm::normalize(_xAxis.vector.value());
            }
        case Axis::Type::OrthogonalVector:
            if (_xAxis.vector.value() == glm::vec3(0.f)) {
                LWARNINGC("FixedRotation", "Zero vector detected for X Axis");
                return glm::vec3(1.f, 0.f, 0.f);
            }
            else {
                if (_yAxis.type != Axis::Type::CoordinateSystemCompletion) {
                    return glm::normalize(
                        glm::cross(_xAxis.vector.value(), yAxis())
                    );
                }
                else {
                    return glm::normalize(
                        glm::cross(_xAxis.vector.value(), zAxis())
                    );
                }
            }
        case Axis::Type::CoordinateSystemCompletion:
            return glm::normalize(-glm::cross(yAxis(), zAxis()));
        default:
            throw ghoul::MissingCaseException();
    }
}

glm::vec3 FixedRotation::yAxis() const {
    switch (_yAxis.type) {
        case Axis::Type::Unspecified:
            LWARNINGC("FixedRotation", "Unspecified axis type for Y axis");
            return glm::vec3(0.f, 1.f, 0.f);
        case Axis::Type::Object:
            if (_yAxis.node && _attachedNode) {
                const glm::vec3 dir = glm::vec3(glm::normalize(
                    // @TODO(abock): This should be changed to be in the coordinate system
                    // of the attached node // same with xAxis and zAxis ofc
                    _yAxis.node->worldPosition() - _attachedNode->worldPosition()
                ));
                return _yAxis.invertObject ? -dir : dir;
            }
            else {
                if (_yAxis.node) {
                    LWARNINGC("FixedRotation", "Missing attachment node");
                    return glm::vec3(0.f, 1.f, 0.f);
                }
                else {
                    LWARNINGC("FixedRotation", "Missing node for Y axis");
                    return glm::vec3(0.f, 1.f, 0.f);
                }
            }
        case Axis::Type::Vector:
            if (_yAxis.vector.value() == glm::vec3(0.f)) {
                LWARNINGC("FixedRotation", "Zero vector detected for Y Axis");
                return glm::vec3(0.f, 1.f, 0.f);
            }
            else {
                return glm::normalize(_yAxis.vector.value());
            }
        case Axis::Type::OrthogonalVector:
            if (_yAxis.vector.value() == glm::vec3(0.f)) {
                LWARNINGC("FixedRotation", "Zero vector detected for Y Axis");
                return glm::vec3(0.f, 1.f, 0.f);
            }
            else {
                if (_zAxis.type != Axis::Type::CoordinateSystemCompletion) {
                    return glm::normalize(
                        glm::cross(_yAxis.vector.value(), zAxis())
                    );
                }
                else {
                    return glm::normalize(
                        glm::cross(_yAxis.vector.value(), xAxis())
                    );
                }
            }
        case Axis::Type::CoordinateSystemCompletion:
            return glm::normalize(glm::cross(xAxis(), -zAxis()));
        default:
            throw ghoul::MissingCaseException();
    }
}

glm::vec3 FixedRotation::zAxis() const {
    switch (_zAxis.type) {
        case Axis::Type::Unspecified:
            LWARNINGC("FixedRotation", "Unspecified axis type for Z axis");
            return glm::vec3(0.f, 0.f, 1.f);
        case Axis::Type::Object:
            if (_zAxis.node && _attachedNode) {
                const glm::vec3 dir = glm::vec3(glm::normalize(
                    _zAxis.node->worldPosition() - _attachedNode->worldPosition()
                ));
                return _zAxis.invertObject ? -dir : dir;
            }
            else {
                if (_zAxis.node) {
                    LWARNINGC("FixedRotation", "Missing attachment node");
                    return glm::vec3(0.f, 0.f, 1.f);
                }
                else {
                    LWARNINGC("FixedRotation", "Missing node for Z axis");
                    return glm::vec3(0.f, 0.f, 1.f);
                }
            }
        case Axis::Type::Vector:
            if (_zAxis.vector.value() == glm::vec3(0.f)) {
                LWARNINGC("FixedRotation", "Zero vector detected for Z Axis");
                return glm::vec3(0.f, 0.f, 1.f);
            }
            else {
                return glm::normalize(_zAxis.vector.value());
            }
        case Axis::Type::OrthogonalVector:
            if (_zAxis.vector.value() == glm::vec3(0.f)) {
                LWARNINGC("FixedRotation", "Zero vector detected for Z Axis");
                return glm::vec3(0.f, 0.f, 1.f);
            }
            else {
                if (_xAxis.type != Axis::Type::CoordinateSystemCompletion) {
                    return glm::normalize(
                        glm::cross(_zAxis.vector.value(), xAxis())
                    );
                }
                else {
                    return glm::normalize(
                        glm::cross(_zAxis.vector.value(), yAxis())
                    );
                }
            }
        case Axis::Type::CoordinateSystemCompletion:
            return glm::normalize(glm::cross(xAxis(), yAxis()));
        default:
            throw ghoul::MissingCaseException();
    }
}

} // namespace openspace
