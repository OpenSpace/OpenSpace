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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___SYSTEMVIEW___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___SYSTEMVIEW___H__

#include <modules/exoplanetsexperttool/datastructures.h>

namespace openspace::exoplanets {

class DataViewer;

class SystemViewer {
public:
    SystemViewer(DataViewer& dataViewer);

    void renderAllSystemViews();
    void renderSystemViewQuickControls(const std::string& host);

    const std::list<std::string>& showSystemViews() const;
    void showSystemView(const std::string& host);

    bool systemCanBeAdded(const std::string& host) const;

    void addExoplanetSystem(const std::string& host) const;
    void addOrTargetPlanet(const ExoplanetItem& item) const;
    void flyToStar(std::string_view hostIdentifier) const;

private:
    void renderSystemViewContent(const std::string& host);

    void renderVisualsTabContent(const std::string& host,
        const std::vector<size_t>& planetIndices);
    void renderOverviewTabContent(const std::string& host,
        const std::vector<size_t>& planetIndices);

    DataViewer& _dataViewer;

    std::list<std::string> _shownPlanetSystemWindows;

    bool _colorOrbits = false;
    bool _highlightDefaultOrbits = false;

};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___SYSTEMVIEW___H__
