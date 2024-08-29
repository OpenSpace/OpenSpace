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

#ifndef __OPENSPACE_CORE___ACTION___H__
#define __OPENSPACE_CORE___ACTION___H__

#include <ghoul/misc/boolean.h>
#include <string>

namespace openspace::interaction {

struct Action {
    BooleanType(IsLocal);

    /// Unique identifier that identifies this action. There is no special naming scheme
    /// that we enforce, we are trying to stick to the same . separated structure that
    /// hopefully provides some protection against accidentally reusing identifiers
    std::string identifier;

    /// The Lua script that gets executed whenever this action is triggered. Optional
    /// parameters can be passed to actions which are accessible through an `args`
    /// variable that contains all of the arguments passed into the action. This means
    /// that the provided script must not use this variable name itself or the script will
    /// not successfully execute
    std::string command;

    /// The human-readable name of this action. This name must not be unique, but it is
    /// recommended that the combination of GuiPath + name should be unique to prevent
    /// user confusion
    std::string name;

    /// A user-facing description of what the action does when it gets triggered. If the
    /// action uses optional arguments, they should be described in here, too
    std::string documentation;

    /// This variable defines a subdivision of where this action is placed in a user
    /// interface. The individual path components are separated by '/' with a leading '/'
    /// for the root path
    std::string guiPath = "/";

    /// If this value is set to `Yes`, the execution of this action is restricted to the
    /// current OpenSpace instance. If it is `No`, it is synchronized to other OpenSpace
    /// instances, for example other nodes in a cluster environment, or to other OpenSpace
    /// instances using a parallel connection
    IsLocal isLocal = IsLocal::No;
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___ACTION___H__
