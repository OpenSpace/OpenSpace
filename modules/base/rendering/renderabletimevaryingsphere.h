/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLETIMEVARYINGESPHERE___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLETIMEVARYINGESPHERE___H__

#include <modules/base/rendering/renderablesphere.h>
#include <modules/fieldlinessequence/util/webfieldlinesmanager.h>

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

class PowerScaledSphere;
struct RenderData;
struct UpdateData;

namespace documentation { struct Documentation; }

class RenderableTimeVaryingSphere : public Renderable {
public:
    RenderableTimeVaryingSphere(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    
    // --------------------- Web Download Manager ----------------------------------- //
    // Web download manager
    WebFieldlinesManager _webFieldlinesManager;
    
    // ------------------------------------ STRINGS ------------------------------------//
    // Name of the RenderableSphere
    std::string _identifier;
    // Web content endpoint
    std::string _webContentUrl;
    
    // ------------------------------------- FLAGS -------------------------------------//
    // True when loading a new state from disk on another thread.
    std::atomic_bool _isLoadingTextureFromDisk = false;
    // True if new texture must be loaded from disk.
    // False => the previous frame's state should still be shown
    bool _mustLoadNewTextureFromDisk  = false;
    // True when finished loading a new texture from disk on another thread.
    std::atomic_bool _newTextureIsReady = false;
    // Stated weather the asset will get textures from the web
    bool _webContent = false;
    // URL to the dynamic web content.
    std::string _dynWebContentUrl = "";
    
    // --------------------------------- NUMERICALS ----------------------------------- //
    // Active index of _states. If(==-1)=>no state available for current time.
    int _activeTriggerTimeIndex = -1;
    // Number of textures in the sequence
    size_t _nTextures = 0;
    // Estimated end of sequence.
    double _sequenceEndTime;
    
    // ----------------------------------- POINTERS ------------------------------------//
    // The Lua-Modfile-Dictionary used during initialization
    std::unique_ptr<ghoul::Dictionary> _dictionary;
    // The actual RenderableSphere object who's texture is to be modified
    std::unique_ptr<RenderableSphere> _renderableSphere;
    // Current texture
    std::unique_ptr<ghoul::opengl::Texture> _currentTexture;
    // Used when switching out current texture to a new texture
    std::unique_ptr<ghoul::opengl::Texture> _newTexture;
    
    // ------------------------------------ VECTORS ----------------------------------- //
    // Stores the provided or downloaded source file paths
    std::vector<std::string> _sourceFiles;
    // Contains the _triggerTimes for all Textures in the sequence
    std::vector<double> _startTimes;
    
    // ---------------------------------- Properties ---------------------------------- //
    // The default texture to display when no time varying data is loaded,
    // set in the lua asset file
    properties::StringProperty _defaultTexturePath;
    
    // --------------------- FUNCTIONS USED DURING INITIALIZATION --------------------- //
    void createRenderableSphere();
    void computeSequenceEndTime();
    bool prepareForTimeVaryingTexture();
    void initializeWebManager();
    bool extractMandatoryInfoFromDictionary();
    void removeOtherFiles(std::string fileExtension)
    
    // ------------------------- FUNCTIONS USED DURING RUNTIME ------------------------ //
    void readNewTexture(const std::string& filePath);
    // Read fits-file and turn into texture
    void processFitsData(std::string filename, std::vector<float> *imagedata);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLETIMEVARYINGSPHERE___H__
