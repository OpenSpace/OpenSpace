#ifndef __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER___H__
#define __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER___H__

#include <modules/webbrowser/include/screenspacebrowser.h>

namespace openspace {

    class ScreenSpaceSkyBrowser : public ScreenSpaceBrowser
    {
    public:
        ScreenSpaceSkyBrowser(const ghoul::Dictionary& dictionary);
        virtual ~ScreenSpaceSkyBrowser() = default;
        void executeJavascript(std::string& script) const;
        void translate(glm::vec3 translation);
    private:


    };
}

#endif // __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACESKYBROWSER___H__

