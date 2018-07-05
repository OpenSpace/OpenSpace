/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#ifndef __OPENSPACE_CORE___HISTOGRAM___H__
#define __OPENSPACE_CORE___HISTOGRAM___H__

#include <vector>

namespace openspace {

class Histogram {
public:
    Histogram() = default;
    Histogram(float minValue, float maxValue, int numBins, float* data = nullptr);
    Histogram(Histogram&& other) = default;
    ~Histogram();

    Histogram& operator=(Histogram&& other) = default;

    int numBins() const;
    float minValue() const;
    float maxValue() const;
    bool isValid() const;

    /**
     * Enter value into the histogram. The add method takes the given
     * value, works out which bin this corresponds to, and increments
     * this bin by 'repeat'.
     *
     * @param value The Value to insert into the histogram
     * @param repeat How many times you want to insert it
     *
     * @return Returns true if succesful insertion, otherwise return false
     */
    bool add(float value, float repeat = 1.0f);
    bool add(const Histogram& histogram);
    bool addRectangle(float lowBin, float highBin, float value);

    float interpolate(float bin) const;
    float sample(int binIndex) const;
    const float* data() const;
    std::vector<std::pair<float,float>> getDecimated(int numBins) const;

    void normalize();
    void print() const;
    void generateEqualizer();
    Histogram equalize();
    float equalize(float) const;
    float entropy();
    std::vector<char> getBinaryData() const;

    float highestBinValue(bool equalized, int overBins=0);
    float binWidth();

    void changeRange(float minValue, float maxValue);

private:
    int _numBins = -1;
    float _minValue = 0.f;
    float _maxValue = 0.f;


    float* _data = nullptr;
    std::vector<float> _equalizer;
    int _numValues = 0;

};

}  // namespace openspace

#endif // __OPENSPACE_CORE___HISTOGRAM___H__
