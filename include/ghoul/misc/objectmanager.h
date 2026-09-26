/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#ifndef __GHOUL___OBJECTMANAGER___H__
#define __GHOUL___OBJECTMANAGER___H__

#include <ghoul/misc/boolean.h>
#include <functional>
#include <map>
#include <memory>
#include <string>

namespace ghoul {

/**
 * We need this intermediate class for the `using enum` declaration in the `BooleanType`
 * to not be a dependent type.
 */
class Manager {
public:
    BooleanType(Warnings);
};

/**
 * An ObjectManager can be used to cache multiple templated objects based on a unique
 * name. Each object can be requested using the #request method. If an object with the
 * specified name has already been created, a pointer to it is returned. Otherwise it will
 * be created using the passed creationFunction and stored internally. Clients can release
 * an object using the #release method. The stored objects are only deleted when the last
 * client that requested the object has released it. At the time of the ObjectManager
 * destruction, all stored objects will automatically be released.
 */
template <typename T>
class ObjectManager : public Manager {
public:
    using CreationCallback = std::function<std::unique_ptr<T>()>;
    using DestructionCallback = std::function<void(T*)>;

    /**
     * Default constructor that takes the specific name of the type of object, that is to
     * be created with this manager.
     *
     * \param name The specific name of this manager, for example TextureObjectManager or
     *        ProgramObjectManager
     */
    ObjectManager(std::string name);

    /**
     * Checks whether all objects have been released and asserts if the application was
     * built in Debug mode. If asserts are disabled, this method does nothing except free
     * all held objects.
     */
    ~ObjectManager();

    /**
     * This method can be called to blanket-release all remaining held objects. If
     * \p emitWarnings is `Yes`, each remaining object is logged, also mentioning the
     * remaining reference counter before destruction. If everything went well in
     * shutdown, this method should not do anything and should not emit any warnings.
     *
     * \param emitWarnings If `Yes` each remaining object will emit a warning including
     *        information about the remaining reference counter at destruction
     */
    void releaseAll(Warnings emitWarnings = Warnings::Yes);

    /**
     * Requests a new object with a unique \p name and, if it has not been created
     * previously, calls the \p creationFunction whose responsibility it is to return a
     * newly created object, a pointer to which is returned by this function call and all
     * subsequent function calls with the same \p name. If an object existed at the time
     * of the call, the \p creationFunction is not called. This method only returns
     * `nullptr` if \p creationFunction returned a `nullptr`, which will cause *all*
     * following calls with the same name to return a `nullptr` as well. The
     * \p creationFunction will be called exactly once for each \p name regardless of its
     * return value.
     *
     * \param name The name of the object that is to be generated
     * \param creationFunction If this is the first call with the provided \p name, this
     *        function is executed to create a new object. Regardless of its return value,
     *        this function is only ever going to be called exactly once
     */
    T* request(const std::string& name, const CreationCallback& creationFunction);

    /**
     * Releases the object with the provided \p name. If the object has been requested
     * `i` number of times, and this is the `i`th call for this \p name, the
     * \p destructionFunction is called with the object to take care of any additional
     * destruction. OBS: The regular destructor of the object will be automatically called
     * after the \p destructionFunction returns, so it is **not** advised for the client
     * to call `delete` on the object as well.
     *
     * \param name The unique name of the object that should be released
     * \param destructionFunction The function that can handle additional destruction
     *        events required by the client. Please not that the regular destructor will
     *        be automatically called after this method returns program control back to
     *        the ObjectManager
     */
    void release(const std::string& name,
        const DestructionCallback& destructionFunction = [](T*) {});

    /**
     * Releases the provided \p object. If the object has been requested `i` number of
     * times, and this is the `i`th call for this \p object, the \p destructionFunction is
     * called with the object to take care of any additional destruction. OBS: The regular
     * destructor of the object will be automatically called after the
     * \p destructionFunction returns, so it is **not** advised for the client to call
     * `delete` on the object as well.
     *
     * \param object The object that should be released or `nullptr`
     * \param destructionFunction The function that can handle additional destruction
     *        events required by the client. Please not that the regular destructor will
     *        be automatically called after this method returns program control back to
     *        the ObjectManager
     */
    void release(T* object, const DestructionCallback& destructionFunction = [](T*) {});

private:
    struct Info {
        std::unique_ptr<T> object = nullptr;
        int refCount = 0;
    };
    std::map<std::string, Info> _objects;

    const std::string _loggerCat;
};

} // namespace ghoul

#include "objectmanager.inl"

#endif // __GHOUL___OBJECTMANAGER___H__
