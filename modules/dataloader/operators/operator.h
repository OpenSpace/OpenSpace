
#ifndef __OPENSPACE_MODULE_DATALOADER___OPERATOR___H__
#define __OPENSPACE_MODULE_DATALOADER___OPERATOR___H__

namespace openspace {

class DataLoaderModule;
class Scene;
class Time;

namespace dataloader {

class Operator {
    public:
        virtual ~Operator() {};

    protected:
        DataLoaderModule* module();
        Scene* scene();
        Time& time();
};

} // namespace dataloader
} // namespace openspace

#endif // __OPENSPACE_MODULE_DATALOADER___OPERATOR___H__