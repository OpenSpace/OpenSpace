/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <openspace/rendering/renderable.h>

#include <modules/fieldlinessequence/util/fieldlinesstate.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec4property.h>
#include <openspace/rendering/transferfunction.h>
#include <atomic>

namespace { enum class SourceFileType; }

namespace openspace {
    
class RenderableStreamNodes : public Renderable {
public :
     RenderableStreamNodes(const ghoul::Dictionary& dictionary);

     void initializeGL() override;
     void deinitializeGL() override;

     bool isReady() const override;

     void render(const RenderData& data, RendererTasks& rendererTask) override;
     void update(const UpdateData& data) override;
    
private: 
    // ------------------------------------- ENUMS -------------------------------------//
   // Used to determine if lines should be colored UNIFORMLY or by an extraQuantity
    enum class ColorMethod : int {
        Uniform = 0,
        ByQuantity

    };
 
    // ------------------------------------ STRINGS ------------------------------------//
        std::string _identifier;    /// Name of the Nod

    // ------------------------------------- FLAGS -------------------------------------//
    // False => states are stored in RAM (using 'in-RAM-states'), True => states are
    // loaded from disk during runtime (using 'runtime-states')
    bool _loadingStatesDynamically  = false;

    // ---------------------------------- Properties ---------------------------------- //
        properties::Vec4Property _pStreamColor;
        // Toggle flow [ON/OFF]
        properties::BoolProperty _pStreamsEnabled;
        // Group to hold the flow/particle properties
        properties::PropertyOwner _pStreamGroup;
        // Size of simulated flow particles


    // initialization
    std::vector<std::string> _sourceFiles;

    // ----------------------------------- POINTERS ------------------------------------//
    // The Lua-Modfile-Dictionary used during initialization
    std::unique_ptr<ghoul::Dictionary> _dictionary;

    std::unique_ptr<ghoul::opengl::ProgramObject> _shaderProgram;

    // --------------------- FUNCTIONS USED DURING INITIALIZATION --------------------- //    bool extractJsonInfoFromDictionary(fls::Model& model);

    bool extractMandatoryInfoFromDictionary(SourceFileType& sourceFileType);   
    void extractOptionalInfoFromDictionary(std::string& outputFolderPath); 
    bool loadJsonStatesIntoRAM(const std::string& outputFolder);
    bool extractJsonInfoFromDictionary(fls::Model& model);
    //std::vector<std::string> LoadJsonfile(const std::string& filename);
    std::vector<std::string> LoadJsonfile();
    void setupProperties();


    // ------------------------- FUNCTIONS USED DURING RUNTIME ------------------------ //
    ;
};




}
