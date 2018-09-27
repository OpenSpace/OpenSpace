/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_DSN___COMMUNICATIONLINES___H__
#define __OPENSPACE_MODULE_DSN___COMMUNICATIONLINES___H__

#include <openspace/rendering/renderable.h>
#include <modules/dsn/dsnmodule.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ext/xml/rapidxml.hpp>
#include <ext/xml/rapidxml_utils.hpp>
#include <ext/xml/rapidxml_iterators.hpp>
#include <ext/xml/rapidxml_print.hpp>

#include <fstream>

namespace openspace {

    class CommunicationLines : public Renderable {
    public:

        CommunicationLines(const ghoul::Dictionary& dictionary);
        void logSomething();
        bool extractMandatoryInfoFromDictionary();
        void xmlParser(std::string filename, std::ofstream &logfile);

        void initializeGL() override;
        void deinitializeGL() override;

        bool isReady() const override;

        void render(const RenderData& data, RendererTasks& rendererTask) override;
        void update(const UpdateData& data) override;
    private:
        std::unique_ptr<ghoul::Dictionary> _dictionary;
       // std::string _identifier;
        std::vector<std::string> _dataFiles;
    };

}
#endif 
