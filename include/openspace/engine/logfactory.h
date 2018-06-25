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

#ifndef __OPENSPACE_CORE___LOGFACTORY___H__
#define __OPENSPACE_CORE___LOGFACTORY___H__

#include <memory>

namespace ghoul { class Dictionary; }
namespace ghoul::logging { class Log; }

namespace openspace {

namespace documentation { struct Documentation; }

/**
 * This function provides the capabilities to create a new ghoul::logging::Log from the
 * provided ghoul::Dictionary%. The Dictionary must at least contain a <code>Type</code>
 * value that determines the type of the created Log. Currently the types
 * <code>HTML</code> and <code>Text</code> are supported which create a
 * ghoul::logging::TextLog%, and ghoul::logging::HTMLLog respectively with both also
 * require the <code>FileName</code> value for the location at which the logfile should be
 * created . Both logs can be customized using the <code>Append</code>,
 * <code>TimeStamping</code>, <code>DateStamping</code>, <code>CategoryStamping</code>,
 * and <code>LogLevelStamping</code> values.
 *
 * \param  dictionary The dictionary from which the ghoul::logging::Log should be created
 * \return The created ghoul::logging::Log
 * \post   The return value will not be <code>nullptr</code>
 * \throw  ghoul::RuntimeError If there was an error creating the ghoul::logging::Log
 * \sa     ghoul::logging::TextLog
 * \sa     ghoul::logging::HTMLLog
 */
std::unique_ptr<ghoul::logging::Log> createLog(const ghoul::Dictionary& dictionary);

/**
 * Returns the Documentation that describes a Dictionary used to create a log by using the
 * function createLog.
 *
 * \return The Documentation that describes an acceptable Dictionary for the method
 *         createLog
 */
documentation::Documentation LogFactoryDocumentation();

} // namespace openspace

#endif // __OPENSPACE_CORE___LOGFACTORY___H__
