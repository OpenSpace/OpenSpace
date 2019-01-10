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

#ifndef __OPENSPACE_MODULE_DSN___RENDERABLEDSNLABELS___H__
#define __OPENSPACE_MODULE_DSN___RENDERABLEDSNLABELS___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/vector/vec4property.h>
#include <openspace/util/updatestructures.h>

namespace ghoul::fontrendering { class Font; }

namespace openspace {

    namespace documentation { struct Documentation; }

    class RenderableDsnLabels: public Renderable {
    public:
        explicit RenderableDsnLabels(const ghoul::Dictionary& dictionary);
        ~RenderableDsnLabels() = default;

        void initialize() override;
        void initializeGL() override;

        bool isReady() const override;

        void render(const RenderData& data, RendererTasks& rendererTask) override;

        static documentation::Documentation Documentation();
    private:

        double _fadeInDistanceUnit = 1E10;
        double _fadeOutDistanceUnit = 2E3;
        double _sizeDistanceUnit = 1.0;

        void renderLabels(const RenderData& data, const glm::dmat4& modelViewProjectionMatrix,
            const glm::dvec3& orthoRight, const glm::dvec3& orthoUp);
        void updateTextColor();

        bool loadData(const Time& time);
        bool loadLabelDataFromId(const Time& time);

        bool _hasStaticLabelSize = false;
        bool _dataIsDirty = true;
        bool _textColorIsDirty = true;
        bool _hasLabel = false;
        bool _hasLabelIdMap = false;
      

        double maxMinNormalize(double value, glm::dvec2 newRange, glm::dvec2 oldRange);

        properties::FloatProperty _scaleFactor;
        properties::Vec4Property _textColorProperty;
        properties::FloatProperty _labelSize;
        properties::Vec2Property _labelSizeRange;
        properties::Vec2Property _sizeDistanceRange;
        properties::BoolProperty _drawLabels;
        properties::Vec2Property _fadeInDistance;
        properties::Vec2Property _fadeOutDistance;
        properties::BoolProperty _disableFadeDistances;

        // DEBUG:
        properties::OptionProperty _renderOption;

        std::shared_ptr<ghoul::fontrendering::Font> _font;

        struct LabelInfo {
            std::string text;
            glm::vec4 textColor;
            std::string attachedId;
            double startTime; 
            double endTime;
            bool hasKeyTimeFrame = false;
            bool hasIndividualColor = false;
        };

        /* Contains the necessary info about all labels, set from the asset file */
        std::vector<LabelInfo> labelDataInfo;
        /* The actual data needed for the rendering, 
         * this is updated each render step with the
         * help of labelDataInfo */
        std::vector< std::tuple<glm::dvec3, std::string, glm::vec4> > _labelData;

        //Make label render first character right at scene graph node
        glm::vec2 _offset = { -20.f, -5.f };
        glm::dmat4 _transformationMatrix = glm::dmat4(1.0);

        double getUnitFactor(std::string unitString);

        /*The minimal size(in pixels) of the text for the labels 
        for the astronomical objects being rendered.*/
        double _textMinSize = 1.0;
        /*The maximal size(in pixels) of the text for the labels
        for the astronomical objects being rendered.*/
        double _textMaxSize = 100.0;

        glm::vec4 _defaultTextColor = { 0.4, 0.4, 0.4, 1.0 };

    };

} // namespace openspace

#endif // __OPENSPACE_MODULE_DSN___RENDERABLEDSNLABELS___H__s
