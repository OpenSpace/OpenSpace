#ifndef __TASKRUNNER_H__
#define __TASKRUNNER_H__

#include <string>
#include <ghoul/misc/dictionary.h>
#include <openspace/util/task.h>

namespace openspace {
class TaskLoader {
public:
    std::vector<std::unique_ptr<Task>> tasksFromDictionary(const ghoul::Dictionary& dictionary);
    std::vector<std::unique_ptr<Task>> tasksFromFile(const std::string& path);
};

}

#endif
