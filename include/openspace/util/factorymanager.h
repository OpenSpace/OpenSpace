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

#ifndef __OPENSPACE_CORE___FACTORYMANAGER___H__
#define __OPENSPACE_CORE___FACTORYMANAGER___H__

#include <ghoul/misc/exception.h>
#include <ghoul/misc/templatefactory.h>
#include <memory>

namespace openspace {

/**
 * Singleton factory class that handles a variety of ghoul::TemplateFactory%s and makes
 * them available through the #addFactory and #factory methods. Each
 * ghoul::TemplateFactory can only be added once and can be accessed by its type.
 */
class FactoryManager {
public:
    struct FactoryInfo {
        std::unique_ptr<ghoul::TemplateFactoryBase> factory;
        std::string name;
    };

    /**
     * This exception is thrown if the ghoul::TemplateFactory could not be found in the
     * #factory method.
     */
    struct FactoryNotFoundError : public ghoul::RuntimeError {
        /**
         * Constructor for FactoryNotFoundError, the \p type is a human-readable (-ish)
         * type descriptor for the type `T` for the TemplateFactory that could
         * not be found.
         *
         * \param t The type `T` for the `TemplateFactory<T>` that could not be found
         * \pre \p t must not be empty
         */
        explicit FactoryNotFoundError(std::string t);

        /// The type describing the ghoul::TemplateFactory that could not be found
        std::string type;
    };

    FactoryManager();

    /**
     * Static initializer that initializes the static member. This needs to be done before
     * the FactoryManager can be used.
     *
     * \pre The FactoryManager must not have been initialized before
     */
    static void initialize();

    /**
     * Deinitializes the static member and all the registered ghoul::TemplateFactory%s.
     *
     * \pre The FactoryManager must have been initialized before
     */
    static void deinitialize();

    /**
     * Returns `true` if the static FactoryManager has already been initialized, `false`
     * otherwise.
     *
     * \return The initialization status of the static FactoryManager
     */
    static bool isInitialized();

    /**
     * This method returns a reference to the initialized FactoryManager.
     *
     * \return An initialized reference to the singleton manager
     *
     * \pre The FactoryManager must have been initialized before
     */
    static FactoryManager& ref();

    /**
     * Adds the passed \p factory to the FactoryManager. Factories may only be added once.
     *
     * \tparam Factory The type for which a factory should be created and added
     * \param name A user-readable name for the registered factory
     *
     * \pre \p name must not be empty
     */
    template <typename T>
    void addFactory(std::string name);

    /**
     * This method provides access to all registered ghoul::TemplateFactory%s through
     * their type. The method will always return a proper ghoul::TemplateFactory or throw
     * an error if the appropriate ghoul::TemplateFactory was not registered.
     *
     * \tparam T The type that the requested ghoul::TemplateFactory should create
     * \return The ghoul::TemplateFactory that will create the pass type `T`
     *
     * \throw FactoryNotFoundError If the requested ghoul::TemplateFactory could not be
     *        found
     */
    template <class T>
    ghoul::TemplateFactory<T>* factory() const;

    const std::vector<FactoryInfo>& factories() const;

private:
    /// Singleton member for the Factory Manager
    static FactoryManager* _manager;

    std::vector<FactoryInfo> _factories;
};

} // namespace openspace

#include <openspace/util/factorymanager.inl>

#endif // __OPENSPACE_CORE___FACTORYMANAGER___H__
