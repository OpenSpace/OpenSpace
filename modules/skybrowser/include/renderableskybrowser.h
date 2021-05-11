#ifndef __OPENSPACE_MODULE_SKYBROWSER___RENDERABLESKYBROWSER___H__
#define __OPENSPACE_MODULE_SKYBROWSER___RENDERABLESKYBROWSER___H__

#include <modules/base/rendering/renderableplane.h>
#include <openspace/documentation/documentation.h>

namespace openspace::documentation { struct Documentation; }

namespace openspace {

    class RenderableSkyBrowser : public RenderablePlane
    {
    public:
        RenderableSkyBrowser(const ghoul::Dictionary& dictionary);
        virtual ~RenderableSkyBrowser() = default;

    private:

    };
}

#endif // __OPENSPACE_MODULE_SKYBROWSER___RENDERABLESKYBROWSER___H__

