/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEFIELDLINESSEQUENCENEW___H__
#define __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEFIELDLINESSEQUENCENEW___H__

#include <openspace/rendering/renderable.h>

#include <modules/fieldlinessequence/util/fieldlinesstate.h>
#include <modules/fieldlinessequence/util/dynamicdownloadermanager.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec4property.h>

#include <openspace/rendering/transferfunction.h>

#include <filesystem>
#include <vector>


namespace openspace {

class RenderableFieldlinesSequenceNew : public Renderable {
public:
    RenderableFieldlinesSequenceNew(const ghoul::Dictionary& dictionary);
    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();


private:
    void definePropertyCallbackFunctions();

    //0: static loading and static downloading
    //1: dynamic loading but static downloading
    //2: dynamic loading and dynamic downloading
    enum class LoadingType {
        StaticLoading = 0,
        DynamicLoading = 1,
        DynamicDownloading = 2
    };

    enum class SourceFileType {
        Cdf = 0,
        Json = 1,
        Osfls = 2
    };

    // Used to determine if lines should be colored UNIFORMLY or by an extraQuantity
    enum class ColorMethod {
        Uniform = 0,
        ByQuantity = 1
    };

    struct File {
        enum class FileStatus {
            Available = 0,
            Downloaded = 1,
            Loaded = 2
        };
        FileStatus status;
        std::filesystem::path path;
        double timestamp;

    };

    std::vector<File> _files;
    size_t activeTriggerTimeIndex = -1;

    // Transfer function used to color lines when _pColorMethod is set to BY_QUANTITY
    std::unique_ptr<TransferFunction> _transferFunction;



    ///////////////////////////////////////////////
    //                PROPERTIES                 //
    ///////////////////////////////////////////////

    // Group to hold the color properties
    properties::PropertyOwner _colorGroup;
    // Uniform/transfer function/topology?
    properties::OptionProperty _colorMethod;
    // Index of the extra quantity to color lines by.
    //TODO: Change to options instead of index
    properties::OptionProperty _colorQuantity;
    // Color table/transfer function min and max range
    properties::Vec2Property _colorQuantityMinMax;
    // Uniform Field Line Color
    properties::Vec4Property _colorUniform;



    ///////////////other.//////////////////////////

    // Paths to color tables. One for each 'extraQuantity'
    std::vector<std::string> _colorTablePaths;



};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEFIELDLINESSEQUENCENEW___H__
