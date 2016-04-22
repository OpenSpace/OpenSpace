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

namespace {
    const std::string _loggerCat = "DataPlane";
}

namespace openspace {

DataPlane::DataPlane(const ghoul::Dictionary& dictionary)
    :CygnetPlane(dictionary)
    ,_dataOptions("dataOptions", "Data Options")
    ,_normValues("normValues", "Normalize Values", glm::vec2(0.1,0.2), glm::vec2(0), glm::vec2(0.5))
    ,_useLog("useLog","Use Logarithm Norm", false)
    ,_useRGB("useRGB","Use RGB Channels", false)
    // ,_topColor("topColor", "Top Color", glm::vec4(1,0,0,1), glm::vec4(0), glm::vec4(1))
    // ,_midColor("midColor", "Mid Color", glm::vec4(0,0,0,0), glm::vec4(0), glm::vec4(1))
    // ,_botColor("botColor", "Bot Color", glm::vec4(0,0,1,1), glm::vec4(0), glm::vec4(1))
    // ,_tfValues("tfValues", "TF Values", glm::vec2(0.5,0.1), glm::vec2(0), glm::vec2(1))
    // ,_colorbar(nullptr)
{   
    _id = id();
    
    std::string name;
    dictionary.getValue("Name", name);
    setName(name);

    addProperty(_useLog);
    addProperty(_useRGB);
    addProperty(_normValues);
    addProperty(_dataOptions);
    //addProperty(_midLevel);
    // addProperty(_topColor);
    // addProperty(_midColor);
    // addProperty(_botColor);
    //addProperty(_tfValues);

    registerProperties();

    OsEng.gui()._iSWAproperty.registerProperty(&_useLog);
    OsEng.gui()._iSWAproperty.registerProperty(&_useRGB);
    OsEng.gui()._iSWAproperty.registerProperty(&_normValues);
    OsEng.gui()._iSWAproperty.registerProperty(&_dataOptions);
    // OsEng.gui()._iSWAproperty.registerProperty(&_topColor);
    // OsEng.gui()._iSWAproperty.registerProperty(&_midColor);
    // OsEng.gui()._iSWAproperty.registerProperty(&_botColor);
    // OsEng.gui()._iSWAproperty.registerProperty(&_tfValues);
    


    _normValues.onChange([this](){loadTexture();});
    _useLog.onChange([this](){loadTexture();});
    _dataOptions.onChange([this](){
        if( _useRGB.value() && (_dataOptions.value().size() > 3)){
            LWARNING("More than 3 values, using only the red channel.");
        }
        loadTexture();
    });

    _useRGB.onChange([this](){
        if( _useRGB.value() && (_dataOptions.value().size() > 3)){
            LWARNING("More than 3 values, using only the red channel.");
        }
            loadTexture();
    });
}

DataPlane::~DataPlane(){}


bool DataPlane::initialize(){
    initializeTime();

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

    _texture = nullptr;
    _memorybuffer = "";
    
    // _colorbar->deinitialize();
    // _colorbar = nullptr;

    return true;
}

// void DataPlane::render(const RenderData& data){} //moved to CygnetPlane
// void DataPLane::update(const UpdateData& data){} //moved to CygnetPlane

bool DataPlane::loadTexture() {
    float* values = readData();
    if(!values)
        return false;

    if (!_texture) {
        std::unique_ptr<ghoul::opengl::Texture> texture =  std::make_unique<ghoul::opengl::Texture>(
                                                                values, 
                                                                _dimensions,
                                                                ghoul::opengl::Texture::Format::RGB,
                                                                GL_RGB, 
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
    if(_futureObject)
        return false;

    _memorybuffer = "";
    std::shared_ptr<DownloadManager::FileFuture> future = ISWAManager::ref().downloadDataToMemory(_data->id, _memorybuffer);

    if(future){
        _futureObject = future;
        return true;
    }

    return false;
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
                            _dataOptions.addOption({numOptions, name()+"_"+option});
                            numOptions++;
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
        if(!_dataOptions.options().size()) // load options for value selection
            readHeader();

        std::stringstream memorystream(_memorybuffer);
        std::string line;

        std::vector<int> selectedOptions = _dataOptions.value();

        std::vector<float> min; 
        std::vector<float> max;

        std::vector<int> logmean;

        std::vector<float> sum;
        std::vector<float> mean;
        std::vector<float> standardDeviation;

        std::vector<std::vector<float>> optionValues;

        for(int i=0; i < selectedOptions.size(); i++){
            min.push_back(std::numeric_limits<float>::max());
            max.push_back(std::numeric_limits<float>::min());

            sum.push_back(0);
            logmean.push_back(0);

            std::vector<float> v;
            optionValues.push_back(v);
        }

        float* combinedValues = new float[3*_dimensions.x*_dimensions.y];

        int numValues = 0;
        while(getline(memorystream, line)){
            if(line.find("#") == 0){ //part of the header
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

                    float v = value[selectedOptions[i]+3]; //+3 because "options" x, y and z.
                    optionValues[i].push_back(v); 

                    min[i] = std::min(min[i], v);
                    max[i] = std::max(max[i], v);

                    sum[i] += v;
                    logmean[i] += (v != 0) ? ceil(log10(fabs(v))) : 0.0f;
                }
                numValues++;
            }
        }

        if(numValues != _dimensions.x*_dimensions.y){
            LWARNING("Number of values read and expected are not the same");
            return nullptr;
        }
    
        for(int i=0; i<optionValues.size(); i++){    
            //Calculate the mean
            mean.push_back((1.0 / numValues) * sum[i]);
            //Calculate the Standard Deviation
            standardDeviation.push_back(sqrt (((pow(sum[i], 2.0)) - ((1.0/numValues) * (pow(sum[i],2.0)))) / (numValues - 1.0)));

            //calulate log mean
            logmean[i] /= numValues;        
        }
        
        for(int i=0; i< numValues; i++){
            if(_useRGB.value() && (optionValues.size() <= 3)){
                for(int j=0; j<optionValues.size(); j++){

                    float v = optionValues[j][i];

                    if(_useLog.value()){
                        combinedValues[3*i+j] += normalizeWithLogarithm(v, logmean[j]);
                    }else{
                        combinedValues[3*i+j] += normalizeWithStandardScore(v, mean[j], standardDeviation[j]);
                    }
                }
            }else{
                combinedValues[3*i+0] = 0;
                combinedValues[3*i+1] = 0;
                combinedValues[3*i+2] = 0;

                for(int j=0; j<optionValues.size(); j++){

                    float v = optionValues[j][i];

                    if(_useLog.value()){
                        combinedValues[3*i+0] += normalizeWithLogarithm(v, logmean[j]);
                    }else{
                        combinedValues[3*i+0] += normalizeWithStandardScore(v, mean[j], standardDeviation[j]);
                    }
                }
                combinedValues[3*i+0] /= selectedOptions.size();
            }
        }
        return combinedValues;
    
    } else {
        LWARNING("Noting in memory buffer, are you connected to the information super highway?");
        return nullptr;
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

float DataPlane::normalizeWithLogarithm(float value, int logMean){
    float logMin = 10*_normValues.value().x;
    float logMax = 10*_normValues.value().y;

    float logNormalized = ((value/pow(10,logMean)+logMin))/(logMin+logMax);
    return glm::clamp(logNormalized,0.0f, 1.0f);
}

int DataPlane::id(){
    static int id = 0;
    return id++;
}
}// namespace openspace