#ifndef __OPENSPACE_MODULE_FIELDLINESSEQUENCE___HELPERFUNCTIONS___H__
#define __OPENSPACE_MODULE_FIELDLINESSEQUENCE___HELPERFUNCTIONS___H__

#include <vector>

namespace openspace {
namespace vector_extraction {

template <typename T>
inline void keepEveryNthElement(const unsigned int& n, std::vector<T>& vec) {
    if (n < 2) { return; }
    std::vector<T> tmpVec;
    const size_t size = vec.size();
    for(size_t i = 0 ; i < size; i += n) { tmpVec.push_back(vec[i]); }
    vec = std::move(tmpVec);
}

template <typename T>
inline void removeFirstNElements(const unsigned int& n, std::vector<T>& vec) {
    const size_t size = vec.size();
    if (n < 1 || n >= size) { return; }
    vec.erase(vec.begin(), vec.begin() + n);
}

template <typename T>
inline void removeLastNElements(const unsigned int& n, std::vector<T>& vec) {
    const size_t size = vec.size();
    if (n < 1 || n >= size) { return; }
    vec.erase(vec.end() - n, vec.end());
}

template <typename T>
inline void keepMaxNElements(const unsigned int& n, std::vector<T>& vec) {
    const size_t size = vec.size();
    if (n < 1 || n >= size) { return; }
    vec.resize(n);
}

template <typename T>
inline void extractChosenElements(const unsigned int& startOffset,
                           const unsigned int& step,
                           const unsigned int& maxElements,
                           std::vector<T>& vec) {
    removeFirstNElements(startOffset, vec);
    keepEveryNthElement(step, vec);
    keepMaxNElements(maxElements, vec);
}

} // namespace vec_extraction
} // namespace openspace

// #include "helperfunctions.inl"

#endif __OPENSPACE_MODULE_FIELDLINESSEQUENCE___HELPERFUNCTIONS___H__