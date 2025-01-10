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

#ifndef __OPENSPACE_MODULE_ISWA___ISWAKAMELEONGROUP___H__
#define __OPENSPACE_MODULE_ISWA___ISWAKAMELEONGROUP___H__

#include <modules/iswa/rendering/iswadatagroup.h>

namespace openspace {

class IswaKameleonGroup : public IswaDataGroup {
public:
    IswaKameleonGroup(std::string name, std::string type);
    virtual ~IswaKameleonGroup();

    virtual void clearGroup() override;

    std::set<std::string> fieldlineValue() const;
    void setFieldlineInfo(std::filesystem::path fieldlineIndexFile,
        std::filesystem::path kameleonPath);
    void changeCdf(std::string path);

protected:
    void registerProperties();

    void readFieldlinePaths(const std::filesystem::path& indexFile);
    void updateFieldlineSeeds();
    void clearFieldlines();

    properties::FloatProperty _resolution;
    properties::SelectionProperty _fieldlines;

    std::filesystem::path _fieldlineIndexFile;
    std::filesystem::path _kameleonPath;
    std::map<int, std::tuple<std::string, std::string, bool>> _fieldlineState;
};

} //namespace openspace

#endif // __OPENSPACE_MODULE_ISWA___ISWAKAMELEONGROUP___H__
