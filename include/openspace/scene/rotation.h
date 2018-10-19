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

#ifndef __OPENSPACE_CORE___ROTATION___H__
#define __OPENSPACE_CORE___ROTATION___H__

#include <openspace/properties/propertyowner.h>

#include <ghoul/glm.h>
#include <memory>

namespace ghoul { class Dictionary; }

namespace openspace {

struct UpdateData;

namespace documentation { struct Documentation; }

class Rotation : public properties::PropertyOwner {
public:
    static std::unique_ptr<Rotation> createFromDictionary(
        const ghoul::Dictionary& dictionary);

    Rotation(const ghoul::Dictionary& dictionary);
    virtual ~Rotation() = default;

    virtual bool initialize();

    const glm::dmat3& matrix() const;
    virtual glm::dmat3 matrix(const UpdateData& time) const = 0;
    void update(const UpdateData& data);

    static documentation::Documentation Documentation();

protected:
    Rotation();
    void requireUpdate();

private:
    bool _needsUpdate = true;
    double _cachedTime = -std::numeric_limits<double>::max();
    glm::dmat3 _cachedMatrix;
};

}  // namespace openspace

#endif // __OPENSPACE_CORE___ROTATION___H__
