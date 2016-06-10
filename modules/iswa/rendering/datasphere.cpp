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

#include <modules/iswa/rendering/datasphere.h>
#include <openspace/util/powerscaledsphere.h>
#include <modules/iswa/util/dataprocessorjson.h>

namespace {
    const std::string _loggerCat = "DataSphere";
}

namespace openspace {

DataSphere::DataSphere(const ghoul::Dictionary& dictionary)
    :DataCygnet(dictionary)
    ,_sphere(nullptr)
{
    float radius;
    dictionary.getValue("Radius", radius);
    _radius = radius;

    _programName = "DataSphereProgram";
    _vsPath = "${MODULE_ISWA}/shaders/datasphere_vs.glsl";
    _fsPath = "${MODULE_ISWA}/shaders/datasphere_fs.glsl";
}

DataSphere::~DataSphere(){}

bool DataSphere::initialize(){
    IswaCygnet::initialize();

    if(_group){
        _dataProcessor = _group->dataProcessor();
        subscribeToGroup();
    }else{
        OsEng.gui()._iswa.registerProperty(&_useLog);
        OsEng.gui()._iswa.registerProperty(&_useHistogram);
        OsEng.gui()._iswa.registerProperty(&_autoFilter);
        OsEng.gui()._iswa.registerProperty(&_backgroundValues);
        OsEng.gui()._iswa.registerProperty(&_normValues);
        OsEng.gui()._iswa.registerProperty(&_transferFunctionsFile);
        OsEng.gui()._iswa.registerProperty(&_dataOptions);

        _dataProcessor = std::make_shared<DataProcessorJson>();
        //If autofiler is on, background values property should be hidden
        _autoFilter.onChange([this](){
            // If autofiler is selected, use _dataProcessor to set backgroundValues 
            // and unregister backgroundvalues property.
            if(_autoFilter.value()){
                _backgroundValues.setValue(_dataProcessor->filterValues());
            // else if autofilter is turned off, register backgroundValues 
            } else {
                OsEng.gui()._iswa.registerProperty(&_backgroundValues, &_autoFilter);            
            }
        });
    }

    readTransferFunctions(_transferFunctionsFile.value());

    setPropertyCallbacks();
    _useHistogram.setValue(true);
    _autoFilter.setValue(true);

    return true;
}

bool DataSphere::createGeometry(){
    PowerScaledScalar radius =  PowerScaledScalar(6.371f*_radius, 6.0);
    int segments = 100;
    _sphere = std::make_shared<PowerScaledSphere>(radius, segments);
    _sphere->initialize();
    return true;
}

bool DataSphere::destroyGeometry(){
    _sphere = nullptr;
    return true;
}

void DataSphere::renderGeometry() const {
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    _sphere->render();
}

std::vector<float*> DataSphere::textureData(){
    // if the buffer in the datafile is empty, do not proceed
    if(_dataBuffer.empty())
        return std::vector<float*>();

    if(!_dataOptions.options().size()){ // load options for value selection
        fillOptions(_dataBuffer);
        _dataProcessor->addDataValues(_dataBuffer, _dataOptions);

        // if this datacygnet has added new values then reload texture
        // for the whole group, including this datacygnet, and return after.
        if(_group){
            _group->updateGroup();
            return std::vector<float*>();
        }
    }
    // _textureDimensions = _dataProcessor->dimensions();
    return _dataProcessor->processData(_dataBuffer, _dataOptions, _textureDimensions);
}

void DataSphere::setUniforms(){
    // set both data texture and transfer function texture
    setTextureUniforms();
    _shader->setUniform("backgroundValues", _backgroundValues.value());
    _shader->setUniform("transparency", _alpha.value());
}
} //namespace openspace