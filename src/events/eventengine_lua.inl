/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

namespace openspace::luascriptfunctions {

int registerEventAction(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 2, 3 }, "lua::registerEventAction");
    auto [event, action, filter] =
        ghoul::lua::values<std::string, std::string, std::optional<ghoul::Dictionary>>(L);

    events::Event::Type type = events::fromString(event);

    global::eventEngine->registerEventAction(type, std::move(action), std::move(filter));
    return 0;
}

int unregisterEventAction(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 2, 3 }, "lua::unregisterEventAction");
    auto [event, action, filter] =
        ghoul::lua::values<std::string, std::string, std::optional<ghoul::Dictionary>>(L);

    events::Event::Type type = events::fromString(event);

    global::eventEngine->unregisterEventAction(type, action, filter);
    return 0;
}

} // namespace openspace::luascriptfunctions
