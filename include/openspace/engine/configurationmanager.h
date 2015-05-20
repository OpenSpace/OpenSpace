/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#ifndef __CONFIGURATIONMANAGER_H__
#define __CONFIGURATIONMANAGER_H__

#include <ghoul/misc/dictionary.h>

namespace openspace {

class ConfigurationManager : public ghoul::Dictionary {
public:
    static const std::string KeyPaths;
    static const std::string KeyCache;
    static const std::string KeyCachePath;
    static const std::string KeyFonts;
    static const std::string KeyConfigSgct;
    static const std::string KeyLuaDocumentationType;
    static const std::string KeyLuaDocumentationFile;
    static const std::string KeyPropertyDocumentationType;
    static const std::string KeyPropertyDocumentationFile;
    static const std::string KeyConfigScene;
    static const std::string KeyEnableGui;
    static const std::string KeyStartupScript;
    static const std::string KeySettingsScript;
    static const std::string KeySpiceTimeKernel;
    static const std::string KeySpiceLeapsecondKernel;
    static const std::string KeyLogLevel;
    static const std::string KeyLogImmediateFlush;
    static const std::string KeyLogs;
    static const std::string KeyDisableMasterRendering;

	bool loadFromFile(const std::string& filename);

private:
	bool checkCompleteness() const;
};

} // namespace openspace

#endif  // __CONFIGURATIONMANAGER_H__
