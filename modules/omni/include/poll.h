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

#ifndef __OPENSPACE_MODULE_OMNI___POLL___H__
#define __OPENSPACE_MODULE_OMNI___POLL___H__

#include <modules/omni/include/scene.h>



#include <openspace/json.h>
#include <ghoul/misc/dictionary.h>


namespace openspace::documentation { struct Documentation; }

namespace openspace::omni {


class Poll : public Scene {
public:
    explicit Poll(const ghoul::Dictionary& dictionary);
    virtual ~Poll() = default;


    static documentation::Documentation Documentation();

    nlohmann::json getAssetInformation();

    void enableScene();
    void disableScene();

private:
    void onEnable();
    void onDisable();


    const std::string _identifier;

    ghoul::Dictionary _dictionary;

};

} // namespace openspace::omni


#endif // !__OPENSPACE_MODULE_OMNI___POLL___H__
