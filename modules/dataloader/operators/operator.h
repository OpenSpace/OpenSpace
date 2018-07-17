
#ifndef __OPENSPACE_MODULE_DATALOADER___OPERATOR___H__
#define __OPENSPACE_MODULE_DATALOADER___OPERATOR___H__

#include <string>

namespace openspace {

class DataLoaderModule;
class Scene;
class Time;
class RenderEngine;

namespace dataloader {

class Operator {
    public:
        virtual ~Operator() {};

    protected:
        DataLoaderModule* module();
        Scene* scene();
        void setTime(std::string value);
};

} // namespace dataloader
} // namespace openspace

#endif // __OPENSPACE_MODULE_DATALOADER___OPERATOR___H__
