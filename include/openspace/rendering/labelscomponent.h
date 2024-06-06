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

#ifndef __OPENSPACE_CORE___LABELSCOMPONENT___H__
#define __OPENSPACE_CORE___LABELSCOMPONENT___H__

#include <openspace/properties/propertyowner.h>
#include <openspace/rendering/fadeable.h>

#include <openspace/data/dataloader.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/ivec2property.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/util/distanceconversion.h>
#include <ghoul/glm.h>
#include <filesystem>

namespace ghoul::fontrendering { class Font; }

namespace openspace {
struct RenderData;

namespace documentation { struct Documentation; }

class LabelsComponent : public properties::PropertyOwner, public Fadeable {
public:
    explicit LabelsComponent(const ghoul::Dictionary& dictionary);

    ~LabelsComponent() override = default;

    dataloader::Labelset& labelSet();
    const dataloader::Labelset& labelSet() const;

    void initialize();

    /**
     * Create the labels from an already loaded dataset. That dataset should have a comment
     * per point to be used for the labels.
     *
     * This function should be called before the labels are initialized
     *
     * \param dataset The dataset to create the labelset from, including xyz position and
     *        a string to be used for the text.
     * \param unit The unit to use when interpreting the point information in the dataset
     */
    void loadLabelsFromDataset(const dataloader::Dataset& dataset, DistanceUnit unit);

    void loadLabels();

    bool isReady() const;
    bool enabled() const;

    void render(const RenderData& data, const glm::dmat4& modelViewProjectionMatrix,
        const glm::vec3& orthoRight, const glm::vec3& orthoUp,
        float fadeInVariable = 1.f);

    static documentation::Documentation Documentation();

private:
    std::filesystem::path _labelFile;
    DistanceUnit _unit = DistanceUnit::Parsec;
    dataloader::Labelset _labelset;

    bool _useCache = true;

    std::shared_ptr<ghoul::fontrendering::Font> _font = nullptr;

    glm::dmat4 _transformationMatrix = glm::dmat4(1.0);

    bool _createdFromDataset = false;

    properties::BoolProperty _enabled;
    properties::Vec3Property _color;
    properties::FloatProperty _size;
    properties::FloatProperty _fontSize;
    properties::IVec2Property _minMaxSize;
    properties::BoolProperty _faceCamera;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___LABELSCOMPONENT___H__
