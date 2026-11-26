#ifndef __OPENSPACE_MODULE_GAUSSIANSPLATTING___GAUSSIANSPLATTINGMODULE___H__
#define __OPENSPACE_MODULE_GAUSSIANSPLATTING___GAUSSIANSPLATTINGMODULE___H__

#include <openspace/util/openspacemodule.h>

namespace openspace {

class GaussianSplattingModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "GaussianSplatting";

    GaussianSplattingModule();
    ~GaussianSplattingModule() override = default;

protected:
    void internalInitialize(const Dictionary& dictionary) override;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_GAUSSIANSPLATTING___GAUSSIANSPLATTINGMODULE___H__
