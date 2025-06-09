/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_MODULE_SKYBROWSER___WWTCOMMUNICATOR___H__
#define __OPENSPACE_MODULE_SKYBROWSER___WWTCOMMUNICATOR___H__

#include <modules/webbrowser/include/browserinstance.h>
#include <openspace/properties/scalar/doubleproperty.h>

#include <deque>

namespace openspace {
using SelectedImageDeque = std::deque<std::pair<std::string, double>>;

class WwtCommunicator {
public:
    explicit WwtCommunicator(BrowserInstance* browserInstance);
    ~WwtCommunicator() = default;

    // WorldWide Telescope communication
    void selectImage(const std::string& imageUrl);
    void addImageLayerToWwt(const std::string& imageUrl);
    void removeSelectedImage(const std::string& imageUrl);
    void setImageOrder(const std::string& imageUrl, int order);
    void loadImageCollection(const std::string& collection);
    void setImageOpacity(const std::string& imageUrl, float opacity);
    void hideChromeInterface() const;

    bool isImageCollectionLoaded() const;

    std::vector<std::string> selectedImages() const;
    std::vector<double> opacities() const;

    void setImageCollectionIsLoaded(bool isLoaded);
    void setAim(glm::dvec2 equatorialAim, double vFov, double roll);
    void setBorderColor(glm::ivec3 color);
    void setBorderRadius(double radius);

    void setIdInBrowser(const std::string& id) const;
    SelectedImageDeque::iterator findSelectedImage(const std::string& imageUrl);

private:
    void executeJavascript(const std::string& script) const;
    void sendMessageToWwt(const ghoul::Dictionary& msg) const;

    bool _isImageCollectionLoaded = false;
    SelectedImageDeque _selectedImages;


    BrowserInstance* _browserInstance = nullptr;
};
} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___WWTCOMMUNICATOR___H__
