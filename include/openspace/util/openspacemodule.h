/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifndef __OPENSPACEMODULE_H__
#define __OPENSPACEMODULE_H__

#include <string>

namespace openspace {

/**
 * This class is the base class for an OpenSpace module. A module groups functionality
 * into a useful granularity to be mostly used self-sufficiently. Each OpenSpaceModule
 * needs a unique, nonempty <code>name</code>.
 */
class OpenSpaceModule {
public:
    /**
     * Constructs the OpenSpaceModule with a specific \p name. The uniqueness of the 
     * \p name will be checked at a later stage.
     * \param name The name of this OpenSpace module
     * \pre \p name must not be empty
     */
    OpenSpaceModule(std::string name);
    
    /// Default destructor
    virtual ~OpenSpaceModule() = default;

    /**
     * Initialization method that will register a token of the form
     * <code>${MODULE_<<NAME>>}</code> for a specific <code><<NAME>></code> that is set in
     * the OpenSpaceModule constructor. This method will call the internalInitialize
     * method for further customization for each subclass.
     */
    void initialize();
    
    /**
     * Empty deinitialization method that will call the internalDeinitialize method for
     * module-specific customization.
     */
    void deinitialize();

    /**
     * Returns the name for this OpenSpaceModule.
     * \return THe name for this OpenSpaceModule
     */
    std::string name() const;

protected:
    /**
     * Customization point for each derived class. The internalInitialize method is called
     * by the initiailze method.
     */
    virtual void internalInitialize();
    
    /**
     * Customization point for each derived class. The internalDeinitialize method is
     * called by the deinitialize method.
     */
    virtual void internalDeinitialize();
    
    /**
     * Returns the path for this module, possibly containing ghoul::filesystem::FileSystem
     * path tokens.
     */
    std::string modulePath() const;

    /// The name of this OpenSpaceModule
    const std::string _name;
};

} // namespace openspace

#endif // __OPENSPACEMODULE_H__
