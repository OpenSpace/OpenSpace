/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#ifndef __FACTORYMANAGER_H__
#define __FACTORYMANAGER_H__

#include <ghoul/misc/templatefactory.h>

namespace openspace {

/**
 * Singleton factory class that handles a variety of ghoul::TemplateFactory%s and makes
 * them available through the #addFactory and factory #methods. Each
 * ghoul::TemplateFactory can only be added once and can be accessed by its type.
 */
class FactoryManager {
public:
    /**
     * Static initializer that initializes the static member. This needs to be done before
     * the FactoryManager can be used.
     * \pre The FactoryManager must not have been initialized before
     */
    static void initialize();
    
    /**
     * Deinitializes the static member and all the registered ghoul::TemplateFactory%s.
     * \pre The FactoryManager must have been initialized before
     */
    static void deinitialize();

    /**
     * Returns <code>true</code> if the static FactoryManager has already been
     * initiailzed, <code>false</code> otherwise.
     * \return The initialization status of the static FactoryManager
     */
    static bool isInitialized();
    
    /**
     * This method returns a reference to the initialized FactoryManager.
     * \return An initialized reference to the singleton manager
     * \pre The FactoryManager must have been initialized before
     */
    static FactoryManager& ref();

    void addFactory(ghoul::TemplateFactoryBase* factory);

    template <class T>
    ghoul::TemplateFactory<T>* factory() const;

private:
    FactoryManager() = default;
    ~FactoryManager();

    FactoryManager(const FactoryManager& c) = delete;

    static FactoryManager* _manager;  ///< Singleton member

    std::vector<ghoul::TemplateFactoryBase*> _factories;
};

} // namespace openspace

#include <openspace/util/factorymanager.inl>

#endif // __FACTORYMANAGER_H__
