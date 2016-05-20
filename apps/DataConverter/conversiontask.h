#ifndef __CONVERSIONTASK_H__
#define __CONVERSIONTASK_H__

#include <functional>

namespace openspace {
namespace dataconverter {

class ConversionTask {
public:
    virtual void perform(const std::function<void(float)>& onProgress) = 0;
};

}
}

#endif
