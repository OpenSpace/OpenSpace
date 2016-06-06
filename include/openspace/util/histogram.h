/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2015                                                                    *
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

#ifndef __HISTOGRAM_H__
#define __HISTOGRAM_H__

#include <vector>
#include <iostream>

namespace openspace {
class Histogram {

public:
    Histogram();
    Histogram(float minValue, float maxValue, int numBins);
    Histogram(float minValue, float maxValue, int numBins, float *data);
    Histogram(Histogram&& other);
    ~Histogram();

    Histogram& operator=(Histogram&& other);

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
    float equalize (float);
    float entropy();

    float highestBinValue(bool equalized);
    float binWidth();

    void changeRange(float minValue, float maxValue);

private:
    int _numBins;
    float _minValue;
    float _maxValue;


    float* _data;
    std::vector<float> _equalizer;
    int _numValues;

}; // class Histogram
}  // namespace openspace

#endif //__HISTOGRAM_H__
