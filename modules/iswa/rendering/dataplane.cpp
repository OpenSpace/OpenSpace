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
#include <modules/iswa/rendering/dataplane.h>
#include <modules/iswa/util/dataprocessortext.h>

namespace {
    const std::string _loggerCat = "DataPlane";
}

namespace openspace {

DataPlane::DataPlane(const ghoul::Dictionary& dictionary)
    :DataCygnet(dictionary)
    ,_useLog("useLog","Use Logarithm", false)
    ,_useHistogram("useHistogram", "Auto Contrast", false)
    ,_autoFilter("autoFilter", "Auto Filter", false)
    ,_normValues("normValues", "Normalize Values", glm::vec2(1.0,1.0), glm::vec2(0), glm::vec2(5.0))
    ,_backgroundValues("backgroundValues", "Background Values", glm::vec2(0.0), glm::vec2(0), glm::vec2(1.0))
    ,_transferFunctionsFile("transferfunctions", "Transfer Functions", "${SCENE}/iswa/tfs/hot.tf")
{     

    addProperty(_useLog);
    addProperty(_useHistogram);
    addProperty(_autoFilter);
    addProperty(_normValues);
    addProperty(_backgroundValues);
    addProperty(_transferFunctionsFile);

    _programName = "DataPlaneProgram";
    _vsPath = "${MODULE_ISWA}/shaders/dataplane_vs.glsl";
    _fsPath = "${MODULE_ISWA}/shaders/dataplane_fs.glsl";
}

DataPlane::~DataPlane(){}

bool DataPlane::initialize(){
    IswaCygnet::initialize();

    if(_group){
        _dataProcessor = _group->dataProcessor();
        subscribeToGroup();
    }else{
        OsEng.gui()._iswa.registerProperty(&_useLog);
        OsEng.gui()._iswa.registerProperty(&_useHistogram);
        OsEng.gui()._iswa.registerProperty(&_autoFilter);
        OsEng.gui()._iswa.registerProperty(&_normValues);
        OsEng.gui()._iswa.registerProperty(&_backgroundValues);
        OsEng.gui()._iswa.registerProperty(&_transferFunctionsFile);
        OsEng.gui()._iswa.registerProperty(&_dataOptions);
        _dataProcessor = std::make_shared<DataProcessorText>();

        //If autofiler is on, background values property should be hidden
        _autoFilter.onChange([this](){
            // If autofiler is selected, use _dataProcessor to set backgroundValues 
            // and unregister backgroundvalues property.
            if(_autoFilter.value()){
                _backgroundValues.setValue(_dataProcessor->filterValues());
                OsEng.gui()._iswa.unregisterProperty(&_backgroundValues); 
            // else if autofilter is turned off, register backgroundValues 
            } else {
                OsEng.gui()._iswa.registerProperty(&_backgroundValues, &_autoFilter);            
            }
        });
    }

    readTransferFunctions(_transferFunctionsFile.value());

    _normValues.onChange([this](){
        _dataProcessor->normValues(_normValues.value());
        loadTexture();
    });
    
    _useLog.onChange([this](){
        _dataProcessor->useLog(_useLog.value());
        loadTexture();
    });

    _useHistogram.onChange([this](){
        _dataProcessor->useHistogram(_useHistogram.value());
        loadTexture();
        if(_autoFilter.value())
            _backgroundValues.setValue(_dataProcessor->filterValues());
    });

    _dataOptions.onChange([this](){ 
        if(_dataOptions.value().size() > MAX_TEXTURES)
            LWARNING("Too many options chosen, max is " + std::to_string(MAX_TEXTURES));
        loadTexture();
    });

    _transferFunctionsFile.onChange([this](){
        readTransferFunctions(_transferFunctionsFile.value());
    });

    _autoFilter.setValue(true);

    return true;
}

bool DataPlane::createGeometry() {
    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
    
    // ============================
    //         GEOMETRY (quad)
    // ============================
    // GLfloat x,y, z;
    float s = _data->spatialScale.x;
    const GLfloat x = s*_data->scale.x/2.0;
    const GLfloat y = s*_data->scale.y/2.0;
    const GLfloat z = s*_data->scale.z/2.0;
    const GLfloat w = _data->spatialScale.w;

    const GLfloat vertex_data[] = { // square of two triangles (sigh)
    //   x   y               z   w  s  t
        -x, -y,             -z,  w, 0, 1,
         x,  y,              z,  w, 1, 0,
        -x,  ((x>1)?y:-y),   z,  w, 0, 0,
        -x, -y,             -z,  w, 0, 1,
         x,  ((x>1)?-y:y),  -z,  w, 1, 1,
         x,  y,              z,  w, 1, 0,
    };

    glBindVertexArray(_quad); // bind array
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer); // bind buffer
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(sizeof(GLfloat) * 4));

    return true;
}

bool DataPlane::destroyGeometry(){
    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    return true;
}

void DataPlane::renderGeometry() const {
    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
}

void DataPlane::setUniforms(){
    // set both data texture and transfer function texture
    setTextureUniforms();
    _shader->setUniform("backgroundValues", _backgroundValues.value());
    _shader->setUniform("transparency", _alpha.value());
}

void DataPlane::subscribeToGroup(){
    auto groupEvent = _group->groupEvent();
    groupEvent->subscribe(name(), "useLogChanged", [&](const ghoul::Dictionary& dict){
        LDEBUG(name() + " Event useLogChanged");
        _useLog.setValue(dict.value<bool>("useLog"));
    });

    groupEvent->subscribe(name(), "normValuesChanged", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event normValuesChanged");
        std::shared_ptr<glm::vec2> values;
        bool success = dict.getValue("normValues", values);
        if(success){
            _normValues.setValue(*values);            
        }
    });

    groupEvent->subscribe(name(), "useHistogramChanged", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event useHistogramChanged");
        _useHistogram.setValue(dict.value<bool>("useHistogram"));
    });

    groupEvent->subscribe(name(), "dataOptionsChanged", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event dataOptionsChanged");
        std::shared_ptr<std::vector<int> > values;
        bool success = dict.getValue("dataOptions", values);
        if(success){
            _dataOptions.setValue(*values);            
        }
    });

    groupEvent->subscribe(name(), "transferFunctionsChanged", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event transferFunctionsChanged");
        _transferFunctionsFile.setValue(dict.value<std::string>("transferFunctions"));
    });

    groupEvent->subscribe(name(), "backgroundValuesChanged", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event backgroundValuesChanged");
        std::shared_ptr<glm::vec2> values;
        bool success = dict.getValue("backgroundValues", values);
        if(success){
            _backgroundValues.setValue(*values);            
        }
    });

    groupEvent->subscribe(name(), "autoFilterChanged", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event autoFilterChanged");
        _autoFilter.setValue(dict.value<bool>("autoFilter"));
    });

    groupEvent->subscribe(name(), "updateGroup", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event updateGroup");
        loadTexture();
    });
}

}// namespace openspace