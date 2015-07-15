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

// ghoul includes
#include <ghoul/misc/templatefactory.h>

namespace openspace {

class FactoryManager {
public:
    /**
     * Static initializer that initializes the static member. This needs to be done before
     * the FactoryManager can be used. If the manager has been already initialized, an
     * assertion will be triggered.
     */
    static void initialize();
    static void deinitialize();

    /**
     * This method returns a reference to the initialized FactoryManager. If the manager
     * has not been initialized, an assertion will be triggered.
     * \return An initialized reference to the singleton manager
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
