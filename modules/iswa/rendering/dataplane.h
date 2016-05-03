/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2015                                                               *
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

#include <modules/iswa/rendering/cygnetplane.h>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <openspace/properties/vectorproperty.h>
#include <modules/iswa/rendering/colorbar.h>
#include <openspace/properties/selectionproperty.h>

namespace openspace{
class ISWAGroup;

class DataPlane : public CygnetPlane {
friend class ISWAGroup;
 public:
     DataPlane(const ghoul::Dictionary& dictionary);
     ~DataPlane();

    virtual bool initialize() override;
    virtual bool deinitialize() override;
    // virtual void render(const RenderData& data) override; //moved to cygnetPlane
    // virtual void update(const UpdateData& data) override; //moved to cygnetPlane

 protected:
    void useLog(bool useLog){ _useLog.setValue(useLog); };
    void normValues(glm::vec2 normValues){  _normValues.setValue(normValues); };
    void useHistogram(bool useHistogram){ _useHistogram.setValue(useHistogram); };
    void dataOptions(std::vector<int> options){ _dataOptions.setValue(options); };
    void transferFunctionsFile(std::string tfPath){ _transferFunctionsFile.setValue(tfPath); };
    void backgroundValues(glm::vec2 backgroundValues){ _backgroundValues.setValue(backgroundValues); };

    // const std::vector<openspace::properties::SelectionProperty::Option>& dataOptions() const {return _dataOptions.options(); };

 private:
    virtual bool loadTexture() override;
    virtual bool updateTexture() override;
    virtual void setUniforms() override;
    virtual bool textureReady() override;

    void readHeader();
    std::vector<float*> readData();
    void processData(
        float* outputData, // Where you want your processed data to go 
        std::vector<float>& inputData, //data that needs processing 
        float min, // min value of the input data
        float max, // max valye of the input data
        float sum // sum of the input data 
    );

    float normalizeWithStandardScore(float value, float mean, float sd);
    float normalizeWithLogarithm(float value, int logMean);

    void setTransferFunctions(std::string tfPath);

    properties::SelectionProperty _dataOptions;
    properties::StringProperty _transferFunctionsFile;
    properties::Vec2Property _normValues;
    properties::Vec2Property _backgroundValues;
    properties::BoolProperty _useLog;
    properties::BoolProperty _useHistogram;
    
    glm::size3_t _dimensions;
    // std::shared_ptr<ColorBar> _colorbar;
    
    //FOR TESTING
    // double _avgBenchmarkTime=0.0;
    // int _numOfBenchmarks = 0;
    //===========
 };
 
 } // namespace openspace

#endif //__DATAPLANE_H__