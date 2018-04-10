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

#ifndef __OPENSPACE_CORE___TRIGGERPROPERTY___H__
#define __OPENSPACE_CORE___TRIGGERPROPERTY___H__

#include <openspace/properties/property.h>

namespace openspace::properties {

/**
 * TriggerProperty that can be used to fire events into your code using the callback
 * method #onChange.
 */
class TriggerProperty : public Property {
public:
    /**
     * Initializes the TriggerProperty by delegating the <code>identifier</code> and
     * <code>guiName</code> to the Property constructor.
     * \param info The PropertyInfo structure that contains all the required static
     * information for initializing this Property.
     * \pre \p info.identifier must not be empty
     * \pre \p info.guiName must not be empty
     */
    TriggerProperty(PropertyInfo info);

    /**
     * Returns the class name <code>TriggerProperty</code>.
     * \return The class name <code>TriggerProperty</code>
     */
    std::string className() const override;

    /**
     * Accepts only the <code>LUA_TNIL</code> type and will notify all the listeners
     * that the event has been triggered.
     * \param state The unused Lua state
     * \return Returns always <code>true</code>
     */
    bool setLuaValue(lua_State* state) override;

    /**
     * Silently ignores any value that is passed into this function and will trigger the
     * listeners regardless of the value
     * \param value The ignored value
     */
    void set(ghoul::any value) override;

    std::string toJson() const override;

    std::string jsonValue() const override;
};

} // namespace openspace::properties

#endif // __OPENSPACE_CORE___TRIGGERPROPERTY___H__
