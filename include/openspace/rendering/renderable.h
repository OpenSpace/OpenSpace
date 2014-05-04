/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014                                                                    *
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

#ifndef __RENDERABLE_H__
#define __RENDERABLE_H__

// open space includes
#include <openspace/util/psc.h>
#include <openspace/util/pss.h>
#include <openspace/util/camera.h>
#include <ghoul/misc/dictionary.h>
#include <openspace/properties/propertyowner.h>

namespace openspace {

class Renderable : public properties::PropertyOwner {
public:
    // constructors & destructor
    Renderable(const ghoul::Dictionary& dictionary);
    virtual ~Renderable();

    void setName(std::string name);
    const std::string& name() const override;

    virtual bool initialize() = 0;
    virtual bool deinitialize() = 0;

    void setBoundingSphere(const pss& boundingSphere);
    const pss& getBoundingSphere();

    virtual void render(const Camera* camera, const psc& thisPosition) = 0;
    virtual void update();

protected:
    // Renderable();
private:
    std::string _name;

    pss boundingSphere_;
};

}  // namespace openspace

#endif  // __RENDERABLE_H__
