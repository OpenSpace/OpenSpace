#ifndef __CONVERSIONTASK_H__
#define __CONVERSIONTASK_H__

#include <functional>
#include <ghoul/misc/dictionary.h>
#include <openspace/documentation/documentation.h>

namespace openspace {

class Task {
public:
    using ProgressCallback = std::function<void(float)>;

    virtual ~Task() = default;
    virtual void perform(const ProgressCallback& onProgress) = 0;
    virtual std::string description() = 0;
    static std::unique_ptr<Task> createFromDictionary(const ghoul::Dictionary& dictionary);
    static openspace::Documentation documentation();
};

}

#endif
