/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#ifndef __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEMOVINGFIELDLINES___H__
#define __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEMOVINGFIELDLINES___H__

#include <openspace/rendering/renderable.h>

#include <modules/fieldlinessequence/util/fieldlinesstate.h>
#include <modules/fieldlinessequence/util/kameleonfieldlinehelper.h>



namespace openspace {

namespace documentation { struct Documentation; }

class RenderableMovingFieldlines : public Renderable {
public:
    RenderableMovingFieldlines(const ghoul::Dictionary& dictionary);
    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendertask) override;
    void update(const UpdateData& data) override;
    static documentation::Documentation Documentation();

private:
    bool getStatesFromCdfFiles();

    double _manualTimeOffset = 0.0;
    std::filesystem::path _sourceFolder;
    std::filesystem::path _seedFilePath;
    std::vector<glm::vec3> _seedPoints;
    // Extra variables such as rho, p or t
    std::vector<std::string> _extraVars;
    // which tracing vaiable to trace. 'b' for fieldline is default
    std::string _tracingVariable = "b";
};

}

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEMOVINGFIELDLINES___H__
