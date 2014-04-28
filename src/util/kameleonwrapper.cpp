/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

#include <ghoul/logging/logmanager.h>
#include <openspace/util/kameleonwrapper.h>

#include <ccmc/Model.h>
#include <ccmc/Interpolator.h>
#include <ccmc/BATSRUS.h>
#include <ccmc/ENLIL.h>

#include <iomanip>

namespace openspace {

std::string _loggerCat = "KameleonWrapper";

KameleonWrapper::KameleonWrapper(const std::string& filename, Model model): _type(model) {
	switch (_type) {
        case Model::BATSRUS:
            _model = new ccmc::BATSRUS();
            if(!_model) LERROR("BATSRUS:Failed to create model instance");
            if (_model->open(filename) != ccmc::FileReader::OK)
				LERROR("BATSRUS:Failed to open "+filename);
            _interpolator = _model->createNewInterpolator();
            if (!_interpolator) LERROR("BATSRUS:Failed to create interpolator");
            break;
        case Model::ENLIL:
            _model = new ccmc::ENLIL();
            if(!_model) LERROR("Failed to create model instance");
            if (_model->open(filename) != ccmc::FileReader::OK)
				LERROR("Failed to open "+filename);
            _interpolator = _model->createNewInterpolator();
            if (!_interpolator) LERROR("Failed to create interpolator");
            break;
        default:
            LERROR("No valid model type provided!");
	}
}

KameleonWrapper::~KameleonWrapper() {
	delete _model;
	delete _interpolator;
}

float* KameleonWrapper::getUniformSampledValues(const std::string& var, glm::size3_t outDimensions) {
	assert(_model && _interpolator);
	assert(outDimensions.x > 0 && outDimensions.y > 0 && outDimensions.z > 0);
    assert(_type == Model::ENLIL || _type == Model::BATSRUS);
    LINFO("Loading CDF data");

	int size = outDimensions.x*outDimensions.y*outDimensions.z;
	float* data = new float[size];
    
    // get the grid system string
    std::string gridSystem = _model->getGlobalAttribute("grid_system_1").getAttributeString();
    
    // remove leading and trailing brackets
    gridSystem = gridSystem.substr(1,gridSystem.length()-2);
    
    // remove all whitespaces
    gridSystem.erase(remove_if(gridSystem.begin(), gridSystem.end(), isspace), gridSystem.end());
    
    // replace all comma signs with whitespaces
    std::replace( gridSystem.begin(), gridSystem.end(), ',', ' ');
    
    // tokenize
    std::istringstream iss(gridSystem);
    std::vector<std::string> tokens{std::istream_iterator<std::string>{iss},std::istream_iterator<std::string>{}};
    
    // validate
    if (tokens.size() != 3) {
        LERROR("Something went wrong");
        delete[] data;
        return 0;
    }

    std::string v_x = tokens.at(0), v_y = tokens.at(1), v_z = tokens.at(2);
    /*
    for(auto t: tokens)
        LDEBUG("t: " << t);
    */
    /*
    LERROR("getVariableAttributeNames");
    std::vector<std::string> attributeNames = _model->getVariableAttributeNames();
    for(auto name : attributeNames)
        LDEBUG(name);
    */
        //_model->getVa
    
    //auto fan = std::find(attributeNames.begin(), attributeNames.end(), "");
    
    
    //KameleonWrapper (Debug)	grid_system_1
    //KameleonWrapper (Debug)	grid_1_type
    
    LDEBUG("Using coordinate system: " << v_x << ", " << v_y << ", " << v_z);
    
	float xMin =    _model->getVariableAttribute(v_x, "actual_min").getAttributeFloat();
	float xMax =    _model->getVariableAttribute(v_x, "actual_max").getAttributeFloat();
	float yMin =    _model->getVariableAttribute(v_y, "actual_min").getAttributeFloat();
	float yMax =    _model->getVariableAttribute(v_y, "actual_max").getAttributeFloat();
	float zMin =    _model->getVariableAttribute(v_z, "actual_min").getAttributeFloat();
	float zMax =    _model->getVariableAttribute(v_z, "actual_max").getAttributeFloat();
	float varMin =  _model->getVariableAttribute(var, "actual_min").getAttributeFloat();
	float varMax =  _model->getVariableAttribute(var, "actual_max").getAttributeFloat();

	float stepX = (xMax-xMin)/(static_cast<float>(outDimensions.x));
	float stepY = (yMax-yMin)/(static_cast<float>(outDimensions.y));
	float stepZ = (zMax-zMin)/(static_cast<float>(outDimensions.z));
    
    
    LDEBUG(v_x << "Min: " << xMin);
    LDEBUG(v_x << "Max: " << xMax);
    LDEBUG(v_y << "Min: " << yMin);
    LDEBUG(v_y << "Max: " << yMax);
    LDEBUG(v_z << "Min: " << zMin);
    LDEBUG(v_z << "Max: " << zMax);
    LDEBUG(var << "Min: " << varMin);
    LDEBUG(var << "Max: " << varMax);
    
    int barWidth = 70;
    int lastiProgress = -1;
    for (int x = 0; x < outDimensions.x; ++x) {
        float progress = static_cast<float>(x) / static_cast<float>(outDimensions.x-1);
        int iprogress = static_cast<int>(progress*100.0f);
		if (iprogress != lastiProgress) {
            
            int pos = barWidth * progress;
            int eqWidth = pos+1;
            int spWidth = barWidth - pos + 2;
            std::cout   << "[" << std::setfill('=') << std::setw(eqWidth)
                        << ">" << std::setfill(' ') << std::setw(spWidth)
                        << "] " << iprogress << " %  \r" << std::flush;
		}
        lastiProgress = iprogress;
        
		for (int y = 0; y < outDimensions.y; ++y) {
			for (int z = 0; z < outDimensions.z; ++z) {
                
                int index = x + y*outDimensions.x + z*outDimensions.x*outDimensions.y;
                
                if(_type == Model::BATSRUS) {
                    float xPos = xMin + stepX*x;
                    float yPos = yMin + stepY*y;
                    float zPos = zMin + stepZ*z;
                    
                    // get interpolated data value for (xPos, yPos, zPos)
                    float value = _interpolator->interpolate(var, xPos, yPos, zPos);
                    
                    // scale to [0,1]
                    data[index] = (value-varMin)/(varMax-varMin);
                } else if (_type == Model::ENLIL) {
                    //LDEBUG("data: " << theval);
                    
                    // Calculate array index
                    //unsigned int index = r + theta*xDim_ + phi*xDim_*yDim_;
                    
                    // Put r in the [0..sqrt(3)] range
                    float rNorm = sqrt(3.0)*(float)x/(float)(outDimensions.x-1);
                    
                    // Put theta in the [0..PI] range
                    float thetaNorm = M_PI*(float)y/(float)(outDimensions.y-1);
                    
                    // Put phi in the [0..2PI] range
                    float phiNorm = 2.0*M_PI*(float)z/(float)(outDimensions.z-1);
                    
                    // Go to physical coordinates before sampling
                    float rPh = xMin + rNorm*(xMax-xMin);
                    float thetaPh = thetaNorm;
                    //phi range needs to be mapped to the slightly different
                    // model range to avoid gaps in the data
                    // Subtract a small term to avoid rounding errors when comparing
                    // to phiMax.
                    float phiPh = zMin + phiNorm/(2.0*M_PI)*(zMax-zMin-0.000001);
                    
                    // Hardcoded variables (rho or rho - rho_back)
                    // TODO Don't hardcode, make more flexible
                    float varValue = 0.f;//, rho_back = 0.f, diff = 0.f;
                    // See if sample point is inside domain
                    if (rPh < xMin || rPh > xMax || thetaPh < yMin ||
                        thetaPh > yMax || phiPh < zMin || phiPh > zMax) {
                        if (phiPh > zMax) {
                            std::cout << "Warning: There might be a gap in the data\n";
                        }
                        // Leave values at zero if outside domain
                    } else { // if inside
                        
                        // ENLIL CDF specific hacks!
                        // Convert from meters to AU for interpolator
                        rPh /= ccmc::constants::AU_in_meters;
                        // Convert from colatitude [0, pi] rad to latitude [-90, 90] degrees
                        thetaPh = -thetaPh*180.f/M_PI+90.f;
                        // Convert from [0, 2pi] rad to [0, 360] degrees
                        phiPh = phiPh*180.f/M_PI;
                        // Sample
                        varValue = _interpolator->interpolate(var, rPh, thetaPh, phiPh);
                        //rho_back = _interpolator->interpolate("rho-back",rPh,thetaPh,phiPh);
                        
                        // Calculate difference (or just rho)
                        //diff = rho;
                        //diff = rho - rho_back;
                        
                        // Clamp to 0
                        //if (diff < 0.f) diff = 0.f;
                    }
                    //if(var < 0.0f) var = 0.0f;
                    //data[index] = var;
                    data[index] = (varValue-varMin)/(varMax-varMin);
                    //LDEBUG("varValue:" << varValue);
                    //LDEBUG("data[index]:" << data[index]);
                    //data[index] = var;
                    //data[index] = diff;
                }
			}
		}
	}
    std::cout << std::endl;
    LINFO("Done!");

	return data;
}

} // namespace openspace

