#ifndef __OPENSPACE_MODULE_WEBGUI___STORYHANDLER___H__
#define __OPENSPACE_MODULE_WEBGUI___STORYHANDLER___H__

#include <openspace/properties/propertyowner.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/stringlistproperty.h>
#include <openspace/properties/scalar/boolproperty.h>

namespace openspace::webgui {

    class StoryHandler : public properties::PropertyOwner {
    public:
        StoryHandler();

        void removeTags();
        void addTags();
        float overviewLimit();
        float zoomInLimit();
        bool storyStyleActive();

    private:
        properties::FloatProperty _overviewLimit;
        properties::FloatProperty _zoomInLimit;
        properties::StringProperty _storyIdentifier;
        properties::TriggerProperty _applyAddTag;
        properties::TriggerProperty _applyRemoveTag;
        properties::StringProperty _focusNodesList;
        properties::BoolProperty _storyStyleActive;

    }; // namespace openspace webgui
};
#endif //__OPENSPACE_MODULE_WEBGUI___STORYHANDLER___H__
