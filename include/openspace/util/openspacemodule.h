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

#ifndef __OPENSPACE_CORE___OPENSPACEMODULE___H__
#define __OPENSPACE_CORE___OPENSPACEMODULE___H__

#include <openspace/properties/propertyowner.h>

#include <ghoul/systemcapabilities/version.h>
#include <string>
#include <vector>

namespace ghoul { class Dictionary; }

namespace openspace {

namespace documentation {  struct Documentation; }
namespace scripting { struct LuaLibrary; }

class ModuleEngine;

/**
 * This class is the base class for an OpenSpace module. A module groups functionality
 * into a useful granularity to be mostly used self-sufficiently. Each OpenSpaceModule
 * needs a unique, nonempty <code>name</code>.
 */
class OpenSpaceModule : public properties::PropertyOwner {
public:
    /**
     * Constructs the OpenSpaceModule with a specific \p name. The uniqueness of the
     * \p name will be checked at a later stage.
     *
     * \param name The name of this OpenSpace module
     *
     * \pre \p name must not be empty
     */
    OpenSpaceModule(std::string name);

    /// Default destructor
    virtual ~OpenSpaceModule() = default;

    /**
     * Initialization method that will register a token of the form
     * <code>${MODULE_\<\<NAME\>\>}</code> for a specific <code>\<\<NAME\>\></code> that
     * is set in the OpenSpaceModule constructor. This method will call the
     * internalInitialize method for further customization for each subclass.
     */
    void initialize(const ModuleEngine* moduleEngine,
        const ghoul::Dictionary& configuration);

    /**
     * This method calls the internalInitializeGL method for further customization for
     * each subclass.
     */
    void initializeGL();

    /**
     * Empty deinitialization method that will call the internalDeinitialize method for
     * module-specific customization.
     */
    void deinitialize();

    /**
     * This method will call the internalDeinitializeGL method for each subclass.
     */
    void deinitializeGL();

    /**
     * Returns a list of Documentation classes that are valid for this OpenSpaceModule.
     *
     * \return A list of Documentation classes that are valid for this OpenSapceModule
     */
    virtual std::vector<documentation::Documentation> documentations() const;

    /**
     * Returns the Lua library with functions defined by this OpenSpaceModule. The default
     * implementation returns an empty library.
     *
     * \return The Lua library with functions defined by this OpenSpaceModule
     */
    virtual scripting::LuaLibrary luaLibrary() const;

    /**
     * Returns the Lua libraries that are defined by objects contained in this
     * OpenSpaceModule. Note that the #luaLibrary library is *not* contained in this list
     * as this is solely the list of libraries as defined by, for example Renderables,
     * defined in the OpenSpaceModule.
     *
     * \return A list of libraries defined by items contained in this OpenSpaceModule
     */
    virtual std::vector<scripting::LuaLibrary> luaLibraries() const;

    /**
     * Returns the minimum required OpenGL version of this OpenSpaceModule. Unless
     * overwritten, it returns an OpenGL version of <code>3.3</code>.
     *
     * \return The minimum required OpenGL version of this OpenSpaceModule
     */
    virtual ghoul::systemcapabilities::Version requiredOpenGLVersion() const;

    /**
     * Returns the list of required OpenGL extensions for this OpenSpaceModule. Unless
     * overwritten, this function returns an empty list.
     *
     * \return The list of required OpenGL extensions necessary to use this
     *         OpenSpaceModule
     */
    virtual std::vector<std::string> requiredOpenGLExtensions() const;

protected:
    /**
     * Customization point for each derived class. The internalInitialize method is called
     * by the initialize method.
     *
     * \param configuration The configuration options that were read from the
     *        configuration file
     */
    virtual void internalInitialize(const ghoul::Dictionary& configuration);

    /**
     * Customization point for each derived class. The internalInitializeGL method is
     * called by the initializeGL method at a time when a valid OpenGL state is
     * guaranteed.
     */
    virtual void internalInitializeGL();

    /**
     * Customization point for each derived class. The internalDeinitialize method is
     * called by the deinitialize method.
     */
    virtual void internalDeinitialize();

    /**
     * Customization point for each derived class. The internalDeinitializeGL method is
     * called by the deinitializeGL method at a time when a valid OpenGL state is
     * guaranteed.
     */
    virtual void internalDeinitializeGL();

    /**
     * Returns the path for this module, possibly containing ghoul::filesystem::FileSystem
     * path tokens.
     */
    std::string modulePath() const;

    /**
     * Returns a const pointer to the module engine
     */
    const ModuleEngine* moduleEngine() const;

private:
    const ModuleEngine* _moduleEngine = nullptr;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___OPENSPACEMODULE___H__
