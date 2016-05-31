/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2016                                                               *
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

#ifndef __DATAPLANE_H__
#define __DATAPLANE_H__

#include <modules/iswa/rendering/datacygnet.h>
#include <openspace/properties/vectorproperty.h>
#include <openspace/properties/selectionproperty.h>

namespace openspace{
class IswaGroup;

/**
 * DataPlane is a concrete IswaCygnet with data files as its input source. 
 * The class handles creation, destruction and rendering of a plane geometry.
 * It also specifies what uniforms to use and what GUI properties it needs.
 */
class DataPlane : public DataCygnet {
friend class IswaGroup;
public:
     DataPlane(const ghoul::Dictionary& dictionary);
     ~DataPlane();

     bool initialize() override;

protected:

    /**
     * Creates a plane geometry
     */
    bool createGeometry() override;
    bool destroyGeometry() override;
    void renderGeometry() const override;
    void setUniforms() override;

private:

    void subscribeToGroup();

    properties::StringProperty _transferFunctionsFile;
    properties::Vec2Property _backgroundValues;
    properties::Vec2Property _normValues;
    properties::BoolProperty _useLog;
    properties::BoolProperty _useHistogram;
    properties::BoolProperty _autoFilter;

    GLuint _quad;
    GLuint _vertexPositionBuffer;
};

} // namespace openspace

#endif //__DATAPLANE_H__