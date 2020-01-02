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

#ifndef __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___SPACECRAFTINSTRUMENTS_MODULE___H__
#define __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___SPACECRAFTINSTRUMENTS_MODULE___H__

#include <openspace/util/openspacemodule.h>

#include <ghoul/opengl/programobjectmanager.h>

namespace openspace {

class SpacecraftInstrumentsModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "SpacecraftInstruments";

    SpacecraftInstrumentsModule();

    std::vector<documentation::Documentation> documentations() const override;

    static ghoul::opengl::ProgramObjectManager ProgramObjectManager;

    bool addFrame(std::string body, std::string frame);
    std::string frameFromBody(const std::string& body);

protected:
    void internalInitialize(const ghoul::Dictionary&) override;
    void internalDeinitialize() override;
    void internalDeinitializeGL() override;

private:
    std::vector<std::pair<std::string, std::string>> _frameByBody;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___SPACECRAFTINSTRUMENTS_MODULE___H__
