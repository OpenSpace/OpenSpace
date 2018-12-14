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

#ifndef __OPENSPACE_MODULE_DSN___RENDERABLELABEL___H__
#define __OPENSPACE_MODULE_DSN___RENDERABLELABEL___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/vector/vec4property.h>

namespace ghoul::fontrendering { class Font; }

namespace openspace {

    namespace documentation { struct Documentation; }

    class RenderableLabel: public Renderable {
    public:
        explicit RenderableLabel(const ghoul::Dictionary& dictionary);
        ~RenderableLabel() = default;

        void initialize() override;
        void initializeGL() override;

        bool isReady() const override;

        void render(const RenderData& data, RendererTasks& rendererTask) override;
       // void update(const UpdateData& data) override;

        static documentation::Documentation Documentation();

    private:
        //enum Unit {
        //    Meter = 0,
        //    Kilometer = 1,
        //    Parsec = 2,
        //    Kiloparsec = 3,
        //    Megaparsec = 4,
        //    Gigaparsec = 5,
        //    GigalightYears = 6
        //};

        void renderLabels(const RenderData& data, const glm::dmat4& modelViewProjectionMatrix,
            const glm::dvec3& orthoRight, const glm::dvec3& orthoUp, float fadeInVariable);

        bool loadData();
        bool loadLabelDataFromId();

        bool _dataIsDirty = true;
        bool _textColorIsDirty = true;
        bool _hasLabel = false;
        bool _hasLabelIdMap = false;

        properties::FloatProperty _scaleFactor;
        properties::Vec4Property _textColor;
        properties::FloatProperty _textSize;
        properties::FloatProperty _textMinSize;
        properties::FloatProperty _textMaxSize;
        properties::BoolProperty _drawLabels;
        properties::Vec2Property _fadeInDistance;
        properties::BoolProperty _disableFadeInDistance;

        // DEBUG:
        properties::OptionProperty _renderOption;

        std::shared_ptr<ghoul::fontrendering::Font> _font;
        ghoul::Dictionary _labelIdMap;
        std::vector<std::pair<glm::vec3, std::string>> _labelData;
        glm::dmat4 _transformationMatrix = glm::dmat4(1.0);
    };

} // namespace openspace

#endif // __OPENSPACE_MODULE_DSN___RENDERABLELABEL___H__s
