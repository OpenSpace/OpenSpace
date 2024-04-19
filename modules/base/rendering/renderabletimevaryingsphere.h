/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLETIMEVARYINGSPHERE___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLETIMEVARYINGSPHERE___H__

#include <modules/base/rendering/renderablesphere.h>

#include <filesystem>

namespace ghoul::opengl { class Texture; }

namespace openspace {

struct RenderData;
struct UpdateData;

namespace documentation { struct Documentation; }

class RenderableTimeVaryingSphere : public RenderableSphere {
public:
    RenderableTimeVaryingSphere(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

protected:
    void bindTexture() override;

private:
    struct FileData {
        std::filesystem::path path;
        double time;
        std::unique_ptr<ghoul::opengl::Texture> texture;
    };
    void loadTexture();
    void extractMandatoryInfoFromSourceFolder();
    void updateActiveTriggerTimeIndex(double currenttime);
    void computeSequenceEndTime();

    // If there's just one state it should never disappear!
    double _sequenceEndTime = std::numeric_limits<double>::max();
    std::vector<FileData> _files;
    int _activeTriggerTimeIndex = 0;

    properties::StringProperty _textureSourcePath;
    ghoul::opengl::Texture* _texture = nullptr;
    bool _textureIsDirty = false;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLETIMEVARYINGSPHERE___H__
