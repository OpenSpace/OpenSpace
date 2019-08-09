#ifndef __OPENSPACE_MODULE_WEBGUI___STORYHANDLER___H__
#define __OPENSPACE_MODULE_WEBGUI___STORYHANDLER___H__

#include <openspace/properties/propertyowner.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/stringlistproperty.h>

namespace openspace::webgui {

    class StoryHandler : public properties::PropertyOwner {
    public:
        StoryHandler();

        float overviewLimit();
        float zoomInLimit();

    private:
        properties::FloatProperty _overviewLimit;
        properties::FloatProperty _zoomInLimit;
        properties::StringProperty _storyIdentifier;
        properties::StringProperty _focusNodesList;

    }; // namespace openspace webgui
};
#endif //__OPENSPACE_MODULE_WEBGUI___STORYHANDLER___H__
