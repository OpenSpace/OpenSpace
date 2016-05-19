// ****************************************************************************************
// *                                                                                       *
// * OpenSpace                                                                             *
// *                                                                                       *
// * Copyright (c) 2014-2015                                                               *
// *                                                                                       *
// * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
// * software and associated documentation files (the "Software"), to deal in the Software *
// * without restriction, including without limitation the rights to use, copy, modify,    *
// * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
// * permit persons to whom the Software is furnished to do so, subject to the following   *
// * conditions:                                                                           *
// *                                                                                       *
// * The above copyright notice and this permission notice shall be included in all copies *
// * or substantial portions of the Software.                                              *
// *                                                                                       *
// * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
// * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
// * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
// * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
// * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
// * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
// ***************************************************************************************

#ifndef __KAMELEONPLANE_H__
#define __KAMELEONPLANE_H__

#include <modules/iswa/rendering/cygnetplane.h>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <modules/iswa/util/dataprocessor.h>
#include <openspace/properties/vectorproperty.h>
#include <openspace/properties/selectionproperty.h>

 namespace openspace{
 
 class KameleonPlane : public CygnetPlane {
 public:
 	KameleonPlane(const ghoul::Dictionary& dictionary);
 	~KameleonPlane();

 private:
 	virtual bool loadTexture() override;
 	virtual bool updateTexture() override;

 	virtual bool readyToRender() override;
    virtual void setUniformAndTextures() override;
    virtual bool createShader() override;

    void setTransferFunctions(std::string tfPath);
    void fillOptions();

	static int id();

    properties::SelectionProperty _dataOptions;
    properties::StringProperty _transferFunctionsFile;
    properties::Vec2Property _backgroundValues;

    properties::Vec2Property _normValues;
    properties::BoolProperty _useLog;
    properties::BoolProperty _useHistogram;

	std::shared_ptr<KameleonWrapper> _kw;
	std::string _kwPath;

    glm::size3_t _dimensions;
	glm::size3_t _textureDimensions;
	float* _dataSlice;
	std::string _var;

	std::vector<float*> _dataSlices;
    std::shared_ptr<DataProcessor> _dataProcessor; 

 };
 
 } // namespace openspace

#endif //__KAMELEONPLANE_H__