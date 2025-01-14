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

#ifndef __OPENSPACE_MODULE_SERVER___ERRORLOGTOPIC____H__
#define __OPENSPACE_MODULE_SERVER___ERRORLOGTOPIC____H__

#include <modules/server/include/topics/topic.h>
#include <ghoul/logging/log.h>

namespace openspace {

class ErrorLogTopic : public Topic {
public:
    ErrorLogTopic() = default;
    ~ErrorLogTopic() override;

    void handleJson(const nlohmann::json& json) override;
    bool isDone() const override;

private:
    bool _isSubscribedTo = false;
    // Non owning but we remove the log from LogManager on destruction
    ghoul::logging::Log* _log = nullptr;
};

} // namespace openspace

#endif // !__OPENSPACE_MODULE_SERVER___ERRORLOGTOPIC____H__
