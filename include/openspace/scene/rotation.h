/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_CORE___ROTATION___H__
#define __OPENSPACE_CORE___ROTATION___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/scene/timeframe.h>
#include <ghoul/glm.h>
#include <ghoul/misc/managedmemoryuniqueptr.h>

namespace ghoul { class Dictionary; }

namespace openspace {

struct UpdateData;

namespace documentation { struct Documentation; }

/**
 * This class represents a configurable rotation which may or may not be time-dependent.
 * Generally, classes of this type should be created through the #createFromDictionary
 * function, which takes a dictionary that describes the type of Rotation to create and
 * the parameters for the specific type that should be created.
 *
 * For general use-case by the wider system, the Rotation class should get at most one
 * call to the #update method, which call calculate the Rotation using the provided data
 * and cache the results, making subsequent calls to matrix() very efficient. For more
 * advanced use cases, the matrix(const UpdateDate&) function will calulcate the rotation
 * every time it is called.
 *
 * Generally, when implementing a new type of this class, only the
 * matrix(const UpdateDate&) verison needs to be implemented as this base class will
 * handle the caching.
 */
class Rotation : public properties::PropertyOwner {
public:
    static ghoul::mm_unique_ptr<Rotation> createFromDictionary(
        const ghoul::Dictionary& dictionary);


    explicit Rotation(const ghoul::Dictionary& dictionary);
    virtual ~Rotation() override = default;

    virtual bool initialize();

    virtual void update(const UpdateData& data);
    const glm::dmat3& matrix() const;
    virtual glm::dmat3 matrix(const UpdateData& time) const = 0;

    static documentation::Documentation Documentation();

protected:
    void requireUpdate();

private:
    bool _needsUpdate = true;
    ghoul::mm_unique_ptr<TimeFrame> _timeFrame;
    double _cachedTime = -std::numeric_limits<double>::max();
    glm::dmat3 _cachedMatrix = glm::dmat3(1.0);
};

}  // namespace openspace

#endif // __OPENSPACE_CORE___ROTATION___H__
