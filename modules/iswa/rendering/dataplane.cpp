// /*****************************************************************************************
//  *                                                                                       *
//  * OpenSpace                                                                             *
//  *                                                                                       *
//  * Copyright (c) 2014-2016                                                               *
//  *                                                                                       *
//  * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
//  * software and associated documentation files (the "Software"), to deal in the Software *
//  * without restriction, including without limitation the rights to use, copy, modify,    *
//  * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
//  * permit persons to whom the Software is furnished to do so, subject to the following   *
//  * conditions:                                                                           *
//  *                                                                                       *
//  * The above copyright notice and this permission notice shall be included in all copies *
//  * or substantial portions of the Software.                                              *
//  *                                                                                       *
//  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
//  * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
//  * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
//  * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
//  * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
//  * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
//  ****************************************************************************************/

#include <modules/iswa/rendering/dataplane.h>
//#include <ghoul/filesystem/filesystem>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>

namespace {
    const std::string _loggerCat = "DataPlane";
}

namespace openspace {

DataPlane::DataPlane(const ghoul::Dictionary& dictionary)
    :CygnetPlane(dictionary)
    ,_dataOptions("dataOptions", "Data Options")
    ,_midLevel("midLevel","Middle level", 0.5, 0.0, 1.0)
    ,_topColor("topColor", "Top Color", glm::vec4(1,0,0,1), glm::vec4(0), glm::vec4(1))
    ,_midColor("midColor", "Mid Color", glm::vec4(0,0,0,0), glm::vec4(0), glm::vec4(1))
    ,_botColor("botColor", "Bot Color", glm::vec4(0,0,1,1), glm::vec4(0), glm::vec4(1))
    ,_tfValues("tfValues", "TF Values", glm::vec2(0.5,0.1), glm::vec2(0), glm::vec2(1))
    ,_normValues("zScoreValues", "Z Score Values", glm::vec2(0.1,0.2), glm::vec2(0), glm::vec2(0.5))
    ,_colorbar(nullptr)
    ,_futureData(nullptr)
    ,_dataSlice(nullptr)
{   
    _id = id();
    
    std::string name;
    dictionary.getValue("Name", name);
    setName(name);

    addProperty(_dataOptions);
    //addProperty(_midLevel);
    // addProperty(_topColor);
    // addProperty(_midColor);
    // addProperty(_botColor);
    //addProperty(_tfValues);
    addProperty(_normValues);

    registerProperties();
    OsEng.gui()._iSWAproperty.registerProperty(&_dataOptions);
    //OsEng.gui()._iSWAproperty.registerProperty(&_midLevel);
    // OsEng.gui()._iSWAproperty.registerProperty(&_topColor);
    // OsEng.gui()._iSWAproperty.registerProperty(&_midColor);
    // OsEng.gui()._iSWAproperty.registerProperty(&_botColor);
   // OsEng.gui()._iSWAproperty.registerProperty(&_tfValues);
    OsEng.gui()._iSWAproperty.registerProperty(&_normValues);
    

    // dictionary.getValue("kwPath", _kwPath);
    _normValues.onChange([this](){loadTexture();});
   // _midLevel.onChange([this](){loadTexture();});
    _dataOptions.onChange([this](){loadTexture();});
}

DataPlane::~DataPlane(){}


bool DataPlane::initialize(){
    // std::string kwPath;
    // _kw = std::make_shared<KameleonWrapper>(_kwPath);
    // dictionary.getValue("KW", _kw);

    // KameleonWrapper::Model model = _kw->model();
    // if( model == KameleonWrapper::Model::BATSRUS)
    //     _var = "p";
    // else
    //     _var = "rho";


    createPlane();
    
    if (_shader == nullptr) {
    // DatePlane Program
    RenderEngine& renderEngine = OsEng.renderEngine();
    _shader = renderEngine.buildRenderProgram("PlaneProgram",
        "${MODULE_ISWA}/shaders/dataplane_vs.glsl",
        "${MODULE_ISWA}/shaders/dataplane_fs.glsl"
        );
    if (!_shader)
        return false;
    }

    // _dimensions = glm::size3_t(500,500,1);
    // float zSlice = 0.5f;
    // _dataSlice = _kw->getUniformSliceValues(std::string(_var), _dimensions, zSlice);

    // loadTexture();
    updateTexture();


    // std::cout << "Creating Colorbar" << std::endl;
    // _colorbar = std::make_shared<ColorBar>();
    // if(_colorbar){
    //     _colorbar->initialize(); 
    // }

    return isReady();
}

bool DataPlane::deinitialize(){
    unregisterProperties();
    destroyPlane();
    destroyShader();
    
    // _kw = nullptr;
    _texture = nullptr;
    _memorybuffer = "";
    
    // _colorbar->deinitialize();
    // _colorbar = nullptr;

    return true;
}



void DataPlane::render(const RenderData& data){
    
    if(!_texture) return;
    
    psc position = data.position;
    glm::mat4 transform = glm::mat4(1.0);

    glm::mat4 rotx = glm::rotate(transform, static_cast<float>(M_PI_2), glm::vec3(1, 0, 0));
    glm::mat4 roty = glm::rotate(transform, static_cast<float>(M_PI_2), glm::vec3(0, -1, 0));
    glm::mat4 rotz = glm::rotate(transform, static_cast<float>(M_PI_2), glm::vec3(0, 0, 1));

    glm::mat4 rot = glm::mat4(1.0);
    for (int i = 0; i < 3; i++){
        for (int j = 0; j < 3; j++){
            transform[i][j] = static_cast<float>(_stateMatrix[i][j]);
        }
    }

    transform = transform * rotz * roty; //BATSRUS

    if(_data->frame == "GSM"){
        glm::vec4 v(1,0,0,1);
        glm::vec3 xVec = glm::vec3(transform*v);
        xVec = glm::normalize(xVec);

        double  lt;
        glm::vec3 sunVec =
        SpiceManager::ref().targetPosition("SUN", "Earth", "GALACTIC", {}, _time, lt);
        sunVec = glm::normalize(sunVec);

        float angle = acos(glm::dot(xVec, sunVec));
        glm::vec3 ref =  glm::cross(xVec, sunVec);

        glm::mat4 rotation = glm::rotate(glm::mat4(1.0f), angle, ref); 
        transform = rotation * transform;
    }

    position += transform*glm::vec4(_data->spatialScale.x*_data->offset, _data->spatialScale.y);
    

    // Activate shader
    _shader->activate();
    glEnable(GL_ALPHA_TEST);
    glDisable(GL_CULL_FACE);

    _shader->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    _shader->setUniform("ModelTransform", transform);

    // _shader->setUniform("top", _topColor.value());
    // _shader->setUniform("mid", _midColor.value());
    // _shader->setUniform("bot", _botColor.value());
    // _shader->setUniform("tfValues", _tfValues.value());

    setPscUniforms(*_shader.get(), data.camera, position);

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();
    _shader->setUniform("texture1", unit);

    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glEnable(GL_CULL_FACE);
    _shader->deactivate();

    // position += transform*(glm::vec4(0.5f*_data->scale.x+100.0f ,-0.5f*_data->scale.y, 0.0f, _data->scale.w));
    // // RenderData data = { *_camera, psc(), doPerformanceMeasurements };
    // ColorBarData cbdata = { data.camera, 
    //                         position,
    //                         transform,
    //                         _topColor.value(),
    //                         _midColor.value(),
    //                         _botColor.value(),
    //                         _tfValues.value()
    //                         // transform
    //                       };    
    // _colorbar->render(cbdata);
}

void DataPlane::update(const UpdateData& data){
    if(_planeIsDirty)
        createPlane();

    // if(abs(Time::ref().currentTime - _time) > 2*Time::ref().deltaTime())

    _time = Time::ref().currentTime();
    _stateMatrix = SpiceManager::ref().positionTransformMatrix("GALACTIC", _data->frame, _time);

    float openSpaceUpdateInterval = abs(Time::ref().deltaTime()*_updateInterval);
    if(openSpaceUpdateInterval){
        if(abs(_time-_lastUpdateTime) >= openSpaceUpdateInterval){
            updateTexture();
            _lastUpdateTime = _time;
        }
    }

    if(_futureData && _futureData->isFinished && _memorybuffer != ""){
        if(!_dataOptions.options().size()){
            readHeader();
        }

        if(loadTexture())
            _futureData = nullptr;
    }
}

void DataPlane::readHeader(){
    if(!_memorybuffer.empty()){
        std::stringstream memorystream(_memorybuffer);
        std::string line;

        int numOptions = 0;
        while(getline(memorystream,line)){
            if(line.find("#") == 0){
                if(line.find("# Output data:") == 0){

                    line = line.substr(26);
                    std::stringstream ss(line);

                    std::string token;
                    getline(ss, token, 'x');
                    int x = std::stoi(token);

                    getline(ss, token, '=');
                    int y = std::stoi(token);

                    _dimensions = glm::size3_t(x, y, 1);

                    getline(memorystream, line);
                    line = line.substr(1);

                    ss = std::stringstream(line);
                    std::string option;
                    while(ss >> option){
                        if(option != "x" && option != "y" && option != "z"){
                            _dataOptions.addOption({numDataOptions, option});
                            numDataOptions++;
                        }
                    }

                    std::vector<int> v(1,0);
                    _dataOptions.setValue(v);
                }
            }else{
                break;
            }
        }
    }else{
        LWARNING("Noting in memory buffer, are you connected to the information super highway?");
    }
}

float* DataPlane::readData(){
    if(!_memorybuffer.empty()){
        std::stringstream memorystream(_memorybuffer);
        std::string line;

        std::vector<int> selectedOptions = _dataOptions.value();
        float refProcent = _midLevel.value();

        std::vector<float> min; 
        std::vector<float> max;
        std::vector<float> sum;
        std::vector<float> mean;
        std::vector<float> standardDeviation;
        std::vector<std::vector<float>> optionValues;

        for(int i=0; i < selectedOptions.size(); i++){
            min.push_back(std::numeric_limits<float>::max());
            max.push_back(std::numeric_limits<float>::min());
            sum.push_back(0);
            std::vector<float> v;
            optionValues.push_back(v);
        }

        float* combinedValues = new float[_dimensions.x*_dimensions.y];

        int numValues = 0;
        while(getline(memorystream, line)){
            if(line.find("#") == 0){
                //part of the header
                continue;
            }

            std::stringstream ss(line); 
            std::vector<float> value;
            float v;
            while(ss >> v){
                value.push_back(v);
            }

            if(value.size()){
                for(int i=0; i<optionValues.size(); i++){
                    optionValues[i].push_back(value[selectedOptions[i]+3]); //+3 because "options" x, y and z.

                    min[i] = std::min(min[i], optionValues[i][numValues]);
                    max[i] = std::max(max[i], optionValues[i][numValues]);
                    sum[i] += optionValues[i][numValues];

                }
                numValues++;
            }
        }

        if(numValues != _dimensions.x*_dimensions.y){
            LWARNING("Number of values read and expected are not the same");
            return nullptr;
        }

        // std::vector<int> refValue;
        // for(int i=0; j<optionValues.size(); i++){
        //     refValue.push_back(optionValues[i][0]);
        // }
    
        for(int i=0; i<optionValues.size(); i++){    
                
            //Calculate the mean
            mean.push_back((1.0 / numValues) * sum[i]);
            //Calculate the Standard Deviation
            standardDeviation.push_back(sqrt (((pow(sum[i], 2.0)) - ((1.0/numValues) * (pow(sum[i],2.0)))) / (numValues - 1.0)));           
        }
        
        for(int i=0; i< numValues; i++){
            combinedValues[i] = 0;
            for(int j=0; j<optionValues.size(); j++){
                float v = optionValues[j][i];

                combinedValues[i] = normalizeWithStandardScore(v, mean[j], standardDeviation[j]);
            }
            combinedValues[i] /= selectedOptions.size();
        }
        return combinedValues;
    
    } else {
        LWARNING("Noting in memory buffer, are you connected to the information super highway?");
    }
} 

float DataPlane::normalizeWithStandardScore(float value, float mean, float sd){
    
    float zScoreMin = _normValues.value().x;
    float zScoreMax = _normValues.value().y;
    float standardScore = ( value - mean ) / sd;
    // Clamp intresting values
    standardScore = glm::clamp(standardScore, -zScoreMin, zScoreMax);
    //return and normalize
    return ( standardScore + zScoreMin )/(zScoreMin + zScoreMax );  
}

bool DataPlane::loadTexture() {
    float* values = readData();
    if(!values){
        return false;
    }

    if (!_texture) {
        std::unique_ptr<ghoul::opengl::Texture> texture =  std::make_unique<ghoul::opengl::Texture>(
                                                                values, 
                                                                _dimensions,
                                                                ghoul::opengl::Texture::Format::Red,
                                                                GL_RED, 
                                                                GL_FLOAT,
                                                                ghoul::opengl::Texture::FilterMode::Linear,
                                                                ghoul::opengl::Texture::WrappingMode::ClampToEdge
                                                            );

        if(texture){
            texture->uploadTexture();
            texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
            _texture = std::move(texture);
        }
    }else{
        _texture->setPixelData(values);
        _texture->uploadTexture();
    }
    return true;
}

bool DataPlane::updateTexture(){
    _memorybuffer = "";
    std::shared_ptr<DownloadManager::FileFuture> future = ISWAManager::ref().downloadDataToMemory(_data->id, _memorybuffer);

    if(future){
        _futureData = future;
        return (_memorybuffer != "");
    }

    return false;
}

int DataPlane::id(){
    static int id = 0;
    return id++;
}
}// namespace openspace