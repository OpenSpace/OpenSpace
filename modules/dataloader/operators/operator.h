
#ifndef __OPENSPACE_MODULE_DATALOADER___OPERATOR___H__
#define __OPENSPACE_MODULE_DATALOADER___OPERATOR___H__

namespace openspace {

class DataLoaderModule;

namespace dataloader {

class Operator {
    public:
        virtual ~Operator() {};

    protected:
        DataLoaderModule* getModule();
};

}
}

#endif // __OPENSPACE_MODULE_DATALOADER___OPERATOR___H__