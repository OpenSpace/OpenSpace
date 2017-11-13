/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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
#include <openspace/scene/scenegraphnode.h>
#include <openspace/query/query.h>

#include <ghoul/misc/assert.h>

namespace {
    const char* KeyXAxis = "XAxis";
    const char* KeyYAxis = "YAxis";
    const char* KeyZAxis = "ZAxis";

    static const openspace::properties::Property::PropertyInfo EnableInfo = {
        "Enable",
        "Enabled",
        "If this value is 'true', all the machinery of this rotation is used, of it is "
        "'false', it provides the ability to change its attributes without risking some "
        "undefined behavior."
    };

    static const openspace::properties::Property::PropertyInfo TypeInfo = {
        "Type",
        "Specification Type",
        "This value specifies how this axis is being specified, that is whether it is "
        "referencing another object, specifying an absolute vector, or whether it is "
        "using the right handed coordinate system completion based off the other two "
        "vectors."
    };

    static const openspace::properties::Property::PropertyInfo ObjectInfo = {
        "Object",
        "Focus Object",
        "This is the object that the axis will focus on. This object must name an "
        "existing scene graph node in the currently loaded scene and the rotation will "
        "stay fixed to the current position of that object."
    };

    static const openspace::properties::Property::PropertyInfo InvertObjectInfo = {
        "InvertObject",
        "Invert Object Point Direction",
        "If this value is set to 'true', and the type is set to 'Object', the inverse of "
        "the pointing direction is used, causing the object to point away from the "
        "referenced object."
    };

    static const openspace::properties::Property::PropertyInfo VectorInfo = {
        "Vector",
        "Direction vector",
        "This value specifies a static direction vector that is used for a fixed "
        "rotation."
    };

    static const openspace::properties::Property::PropertyInfo OrthogonalVectorInfo = {
        "Orthogonal",
        "Vector is orthogonal",
        "This value determines whether the vector specified is used directly, or whether "
        "it is used together with another non-coordinate system completion vector to "
        "construct an orthogonal vector instead."
    };

    static const openspace::properties::Property::PropertyInfo AttachedInfo = {
        "Attached",
        "Attached Node",
        "This is the name of the node that this rotation is attached to, this value is "
        "only needed if any of the three axis uses the Object type. In this case, the "
        "location of the attached node is required to compute the relative direction."
    };
} // namespace

namespace openspace {

documentation::Documentation FixedRotation::Documentation() {
    using namespace openspace::documentation;
    return {
        "Fixed Rotation",
        "base_transform_rotation_fixed",
        {
            {
                "Type",
                new StringEqualVerifier("FixedRotation"),
                Optional::No
            },
            {
                KeyXAxis,
                new OrVerifier(
                    new StringVerifier,
                    new DoubleVector3Verifier
                ),
                Optional::Yes,
                "This value specifies the direction of the new X axis. If this value is "
                "not specified, it will be computed by completing a right handed "
                "coordinate system from the Y and Z axis, which must be specified "
                "instead."
            },
            {
                KeyXAxis + OrthogonalVectorInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                OrthogonalVectorInfo.description
            },
            {
                KeyYAxis,
                new OrVerifier(
                    new StringVerifier,
                    new DoubleVector3Verifier
                ),
                Optional::Yes,
                "This value specifies the direction of the new Y axis. If this value is "
                "not specified, it will be computed by completing a right handed "
                "coordinate system from the X and Z axis, which must be specified "
                "instead."
            },
            {
                KeyYAxis + OrthogonalVectorInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                OrthogonalVectorInfo.description
            },
            {
                KeyZAxis,
                new OrVerifier(
                    new StringVerifier,
                    new DoubleVector3Verifier
                ),
                Optional::Yes,
                "This value specifies the direction of the new Z axis. If this value is "
                "not specified, it will be computed by completing a right handed "
                "coordinate system from the X and Y axis, which must be specified "
                "instead."
            },
            {
                KeyZAxis + OrthogonalVectorInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                OrthogonalVectorInfo.description
            },
            {
                AttachedInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                AttachedInfo.description
            }
        }
    };
}

FixedRotation::FixedRotation(const ghoul::Dictionary& dictionary)
    : _enabled(EnableInfo, true)
    , _xAxis{
        properties::OptionProperty(
            {
                "xAxis-" + TypeInfo.identifier,
                "xAxis:" + TypeInfo.guiName,
                TypeInfo.description
            },
            properties::OptionProperty::DisplayType::Dropdown
        ),
        properties::StringProperty(
            {
                "xAxis-" + ObjectInfo.identifier,
                "xAxis:" + ObjectInfo.guiName,
                ObjectInfo.description
            },
            ""
        ),
        properties::BoolProperty(
            {
                "xAxis-" + InvertObjectInfo.identifier,
                "xAxis:" + InvertObjectInfo.guiName,
                InvertObjectInfo.description
            },
            false
        ),
        properties::Vec3Property(
            {
                "xAxis-" + VectorInfo.identifier,
                "xAxis:" + VectorInfo.guiName,
                VectorInfo.description
            },
            glm::vec3(1.f, 0.f, 0.f),
            glm::vec3(0.f),
            glm::vec3(1.f)
        ),
        properties::BoolProperty(
            {
                "xAxis-" + OrthogonalVectorInfo.identifier,
                "xAxis:" + OrthogonalVectorInfo.guiName,
                OrthogonalVectorInfo.description
            },
            false
        ),
        nullptr
    }
    , _yAxis{
        properties::OptionProperty(
            {
                "yAxis-" + TypeInfo.identifier,
                "yAxis:" + TypeInfo.guiName,
                "yAxis:" + TypeInfo.description
            },
            properties::OptionProperty::DisplayType::Dropdown
        ),
        properties::StringProperty(
            {
                "yAxis-" + ObjectInfo.identifier,
                "yAxis:" + ObjectInfo.guiName,
                "yAxis:" + ObjectInfo.description
            },
            ""
        ),
        properties::BoolProperty(
            {
                "yAxis-" + InvertObjectInfo.identifier,
                "yAxis:" + InvertObjectInfo.guiName,
                InvertObjectInfo.description
            },
            false
        ),
        properties::Vec3Property(
            {
                "yAxis-" + VectorInfo.identifier,
                "yAxis:" + VectorInfo.guiName,
                "yAxis:" + VectorInfo.description
            },
            glm::vec3(0.f, 1.f, 0.f),
            glm::vec3(0.f),
            glm::vec3(1.f)
        ),
        properties::BoolProperty(
            {
                "yAxis-" + OrthogonalVectorInfo.identifier,
                "yAxis:" + OrthogonalVectorInfo.guiName,
                OrthogonalVectorInfo.description
            },
            false
        ),
        nullptr
    }
    , _zAxis{
        properties::OptionProperty(
            {
                "zAxis-" + TypeInfo.identifier,
                "zAxis:" + TypeInfo.guiName,
                "zAxis:" + TypeInfo.description
            },
            properties::OptionProperty::DisplayType::Dropdown
        ),
        properties::StringProperty(
            {
                "zAxis-" + ObjectInfo.identifier,
                "zAxis:" + ObjectInfo.guiName,
                "zAxis:" + ObjectInfo.description
            },
            ""
        ),
        properties::BoolProperty(
                {
                "zAxis-" + InvertObjectInfo.identifier,
                "zAxis:" + InvertObjectInfo.guiName,
                InvertObjectInfo.description
            },
            false
        ),
        properties::Vec3Property(
            {
                "zAxis-" + VectorInfo.identifier,
                "zAxis:" + VectorInfo.guiName,
                "zAxis:" + VectorInfo.description
            },
            glm::vec3(0.f, 0.f, 1.f),
            glm::vec3(0.f),
            glm::vec3(1.f)
        ),
        properties::BoolProperty(
            {
                "zAxis-" + OrthogonalVectorInfo.identifier,
                "zAxis:" + OrthogonalVectorInfo.guiName,
                OrthogonalVectorInfo.description
            },
            false
        ),
        nullptr
    }
    , _attachedObject(AttachedInfo, "")
    , _attachedNode(nullptr)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "FixedRotation"
    );

    _constructorDictionary = dictionary;

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

    auto setPropertyVisibility = [this](Axis& axis) {
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
    _xAxis.type.onChange([&]() {
        setPropertyVisibility(_xAxis);
    });
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
    _yAxis.type.onChange([&]() {
        setPropertyVisibility(_yAxis);
    });
    addProperty(_yAxis.type);

    _yAxis.object.setGroupIdentifier("yAxis");
    addProperty(_yAxis.object);
    _yAxis.object.onChange([this](){
        _yAxis.node = sceneGraphNode(_yAxis.object);
    });

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
    _zAxis.type.onChange([&]() {
        setPropertyVisibility(_zAxis);
    });
    addProperty(_zAxis.type);

    _zAxis.object.setGroupIdentifier("zAxis");
    addProperty(_zAxis.object);
    _zAxis.object.onChange([this](){
        _zAxis.node = sceneGraphNode(_zAxis.object);
    });

    _zAxis.invertObject.setGroupIdentifier("zAxis");
    addProperty(_zAxis.invertObject);

    _zAxis.vector.setGroupIdentifier("zAxis");
    addProperty(_zAxis.vector);

    setPropertyVisibility(_xAxis);
    setPropertyVisibility(_yAxis);
    setPropertyVisibility(_zAxis);
}

bool FixedRotation::initialize() {
    // We need to do this in the initialize and not the constructor as the scene graph
    // nodes referenced in the dictionary might not exist yet at construction time. At
    // initialization time, however, we know that they already have been created

    bool res = Rotation::initialize();

    if (_constructorDictionary.hasKey(AttachedInfo.identifier)) {
        _attachedObject = _constructorDictionary.value<std::string>(
            AttachedInfo.identifier
        );
    }

    bool hasXAxis = _constructorDictionary.hasKey(KeyXAxis);
    if (hasXAxis) {
        if (_constructorDictionary.hasKeyAndValue<std::string>(KeyXAxis)) {
            _xAxis.type = Axis::Type::Object;
            _xAxis.object = _constructorDictionary.value<std::string>(KeyXAxis);
        }
        else {
            // We know it has to be a vector now
            _xAxis.type = Axis::Type::Vector;
            _xAxis.vector = _constructorDictionary.value<glm::dvec3>(KeyXAxis);
        }
    }

    if (_constructorDictionary.hasKey(KeyXAxis + OrthogonalVectorInfo.identifier)) {
        _xAxis.isOrthogonal = _constructorDictionary.value<bool>(
            KeyXAxis + OrthogonalVectorInfo.identifier
        );
    }
    if (_xAxis.isOrthogonal) {
        _xAxis.type = Axis::Type::OrthogonalVector;
    }

    bool hasYAxis = _constructorDictionary.hasKey(KeyYAxis);
    if (hasYAxis) {
        if (_constructorDictionary.hasKeyAndValue<std::string>(KeyYAxis)) {
            _yAxis.type = Axis::Type::Object;
            _yAxis.object = _constructorDictionary.value<std::string>(KeyYAxis);
        }
        else {
            // We know it has to be a vector now
            _yAxis.type = Axis::Type::Vector;
            _yAxis.vector = _constructorDictionary.value<glm::dvec3>(KeyYAxis);
        }
    }

    if (_constructorDictionary.hasKey(KeyYAxis + OrthogonalVectorInfo.identifier)) {
        _yAxis.isOrthogonal = _constructorDictionary.value<bool>(
            KeyYAxis + OrthogonalVectorInfo.identifier
        );
    }
    if (_yAxis.isOrthogonal) {
        _yAxis.type = Axis::Type::OrthogonalVector;
    }

    bool hasZAxis = _constructorDictionary.hasKey(KeyZAxis);
    if (hasZAxis) {
        if (_constructorDictionary.hasKeyAndValue<std::string>(KeyZAxis)) {
            _zAxis.type = Axis::Type::Object;
            _zAxis.object = _constructorDictionary.value<std::string>(KeyZAxis);
        }
        else {
            // We know it has to be a vector now
            _zAxis.type = Axis::Type::Vector;
            _zAxis.vector = _constructorDictionary.value<glm::dvec3>(KeyZAxis);
        }
    }

    if (_constructorDictionary.hasKey(KeyZAxis + OrthogonalVectorInfo.identifier)) {
        _zAxis.isOrthogonal = _constructorDictionary.value<bool>(
            KeyZAxis + OrthogonalVectorInfo.identifier
        );
    }
    if (_zAxis.isOrthogonal) {
        _zAxis.type = Axis::Type::OrthogonalVector;
    }



    if (!hasXAxis && hasYAxis && hasZAxis) {
        _xAxis.type = Axis::Type::CoordinateSystemCompletion;
    }

    if (hasXAxis && !hasYAxis && hasZAxis) {
        _yAxis.type = Axis::Type::CoordinateSystemCompletion;
    }

    if (hasXAxis && hasYAxis && !hasZAxis) {
        _zAxis.type = Axis::Type::CoordinateSystemCompletion;
    }

    // No need to hold on to the data
    _constructorDictionary = {};
    return res;
}

void FixedRotation::update(const UpdateData&) {
    if (!_enabled) {
        _matrix = glm::dmat3();
        return;
    }

    glm::vec3 x = xAxis();
    glm::vec3 y = yAxis();
    glm::vec3 z = zAxis();

    static const float Epsilon = 1e-3;

    if (glm::dot(x, y) > 1.f - Epsilon ||
        glm::dot(y, z) > 1.f - Epsilon ||
        glm::dot(x, z) > 1.f - Epsilon)
    {
        LWARNINGC(
            "FixedRotation",
            "Dangerously collinear vectors detected: " <<
            "x: " << x << "  y: " << y << "  z: " << z
        );
        _matrix = glm::dmat3();
    }
    else {
        _matrix = {
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
                glm::vec3 dir = glm::vec3(glm::normalize(
                    glm::dvec3(_xAxis.node->worldPosition()) -
                    glm::dvec3(_attachedNode->worldPosition())
                ));
                return _xAxis.invertObject ? -dir : dir;
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
                glm::vec3 dir = glm::vec3(glm::normalize(
                    glm::dvec3(_yAxis.node->worldPosition()) -
                    glm::dvec3(_attachedNode->worldPosition())
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
                glm::vec3 dir = glm::vec3(glm::normalize(
                    glm::dvec3(_zAxis.node->worldPosition()) -
                    glm::dvec3(_attachedNode->worldPosition())
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
