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

#ifndef __OPENSPACE_MODULE_ISWA___DATACYGNET___H__
#define __OPENSPACE_MODULE_ISWA___DATACYGNET___H__

#include <modules/iswa/rendering/iswacygnet.h>

#include <openspace/properties/selectionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <glm/gtx/std_based_type.hpp>

namespace openspace {

class DataProcessor;

/**
 * This class abstracts away the the loading of data and creation of textures for all data
 * cygnets. It specifies the interface that needs to be implemented for all concrete
 * subclasses
 */
class DataCygnet : public IswaCygnet {
public:
    DataCygnet(const ghoul::Dictionary& dictionary);
    ~DataCygnet();

protected:
    bool updateTexture() override;
    void fillOptions(const std::string& source);

    /**
     * Loads the transferfunctions specified in tfPath into _transferFunctions list.
     *
     * \param tfPath Path to transfer function file
     */
    void readTransferFunctions(std::string tfPath);

    /**
     * This function binds and sets all textures that should go to the shader program,
     * this includes both the data and transferfunctions.
     */
    void setTextureUniforms();

    /**
     * Sets and defines the on-change handlers for the gui properties.
     */
    void setPropertyCallbacks();

    /**
     * Subscribes to the group events that are shared by all DataCygnets
     */
    void subscribeToGroup();

    /**
     * Optional interface method. this has an implementation in datacygnet.cpp, but needs
     * to be overriden for kameleonplane
     */
    virtual bool updateTextureResource() override;

    virtual std::vector<float*> textureData() = 0;

    properties::SelectionProperty _dataOptions;
    properties::StringProperty _transferFunctionsFile;
    properties::Vec2Property _backgroundValues;
    properties::Vec2Property _normValues;
    properties::BoolProperty _useLog;
    properties::BoolProperty _useHistogram;
    properties::BoolProperty _autoFilter;

    std::shared_ptr<DataProcessor> _dataProcessor;
    std::string _dataBuffer;
    glm::size3_t _textureDimensions;

private:
    bool readyToRender() const override;
    bool downloadTextureResource(double timestamp) override;
};

} //namespace openspace

#endif // __OPENSPACE_MODULE_ISWA___DATACYGNET___H__
