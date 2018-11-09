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

#include <openspace/util/histogram.h>

#include <ghoul/logging/logmanager.h>
#include <cmath>

namespace {
    constexpr const char* _loggerCat = "Histogram";
} // namespace

namespace openspace {

Histogram::Histogram(float minValue, float maxValue, int numBins, float* data)
    : _numBins(numBins)
    , _minValue(minValue)
    , _maxValue(maxValue)
    , _data(data)
{
    if (!data) {
        _data = new float[numBins];
        for (int i = 0; i < numBins; ++i) {
            _data[i] = 0.0;
        }
    }
}

Histogram::~Histogram() {
    delete[] _data;
}

int Histogram::numBins() const {
    return _numBins;
}

float Histogram::minValue() const {
    return _minValue;
}

float Histogram::maxValue() const {
    return _maxValue;
}

bool Histogram::isValid() const {
    return _numBins != -1;
}

bool Histogram::add(float value, float repeat) {
    if (value < _minValue || value > _maxValue) {
        // Out of range
        return false;
    }

    const float normalizedValue = (value - _minValue) / (_maxValue - _minValue);
    const int binIndex = static_cast<int>(std::min(
        static_cast<float>(floor(normalizedValue * _numBins)),
        _numBins - 1.f
    )); // [0, _numBins - 1]

    _data[binIndex] += repeat;
    _numValues = static_cast<int>(_numValues + repeat);

    return true;
}

void Histogram::changeRange(float minValue, float maxValue){
    if (minValue > _minValue && maxValue < _maxValue) {
        return;
    }

    float* oldData = _data;
    float oldMin = _minValue;
    float oldMax = _maxValue;

    float* newData = new float[_numBins]{0.0};
    for(int i=0; i<_numBins; i++){
        float unNormalizedValue = i*(oldMax-oldMin)+oldMin;
        float normalizedValue = (unNormalizedValue - _minValue) /
                                (_maxValue - _minValue);      // [0.0, 1.0]
        int binIndex = static_cast<int>(std::min(
            static_cast<float>(floor(normalizedValue * _numBins)),
            _numBins - 1.f
        )); // [0, _numBins - 1]

        newData[binIndex] = oldData[i];
    }

    _data = newData;
    delete oldData;

    _minValue = minValue;
    _maxValue = maxValue;

}

bool Histogram::add(const Histogram& histogram) {
    if (_minValue == histogram.minValue() &&
        _maxValue == histogram.maxValue() &&
        _numBins == histogram.numBins())
    {
        const float* data = histogram.data();
        for (int i = 0; i < _numBins; i++) {
            _data[i] += data[i];
        }
        _numValues += histogram._numValues;
        return true;
    } else {
        LERROR("Dimension mismatch");
        return false;
    }
}

bool Histogram::addRectangle(float lowBin, float highBin, float value) {
    if (lowBin == highBin) {
        return true;
    }

    if (lowBin > highBin) {
        std::swap(lowBin, highBin);
    }
    if (lowBin < _minValue || highBin > _maxValue) {
        // Out of range
        return false;
    }

    const float normalizedLowBin = (lowBin - _minValue) / (_maxValue - _minValue);
    const float normalizedHighBin = (highBin - _minValue) / (_maxValue - _minValue);

    const float lowBinIndex = normalizedLowBin * _numBins;
    const float highBinIndex = normalizedHighBin * _numBins;

    const int fillLow = static_cast<int>(floor(lowBinIndex));
    const int fillHigh = static_cast<int>(ceil(highBinIndex));

    for (int i = fillLow; i < fillHigh; i++) {
        _data[i] += value;
    }

    if (lowBinIndex > fillLow) {
        const float diff = lowBinIndex - fillLow;
        _data[fillLow] -= diff * value;
    }
    if (highBinIndex < fillHigh) {
        const float diff = -highBinIndex + fillHigh;
        _data[fillHigh - 1] -= diff * value;
    }

    return true;
}

float Histogram::interpolate(float bin) const {
    const float normalizedBin = (bin - _minValue) / (_maxValue - _minValue);
    const float binIndex = normalizedBin * _numBins - 0.5f; // Center

    const float interpolator = binIndex - floor(binIndex);
    int binLow = static_cast<int>(floor(binIndex));
    int binHigh = static_cast<int>(ceil(binIndex));

    // Clamp bins
    if (binLow < 0) {
        binLow = 0;
    }
    if (binHigh >= _numBins) {
        binHigh = _numBins - 1;
    }

    return (1.f - interpolator) * _data[binLow] + interpolator * _data[binHigh];
}

float Histogram::sample(int binIndex) const {
    ghoul_assert(binIndex >= 0 && binIndex < _numBins, "binIndex out of range");

    return _data[binIndex];
}

const float* Histogram::data() const {
    return _data;
}

std::vector<std::pair<float,float>> Histogram::getDecimated(int) const {
    // Return a copy of _data decimated as in Ljung 2004
    return std::vector<std::pair<float,float>>();
}

void Histogram::normalize() {
    float sum = 0.0;
    for (int i = 0; i < _numBins; i++) {
        sum += _data[i];
    }
    for (int i = 0; i < _numBins; i++) {
        _data[i] /= sum;
    }
}

/*
 * Will create an internal array for histogram equalization.
 * Old histogram value is the index of the array, and the new equalized
 * value will be the value at the index.
 */
void Histogram::generateEqualizer() {
    float previousCdf = 0.0f;
    _equalizer = std::vector<float>(_numBins, 0.0f);
    for (int i = 0; i < _numBins; i++) {
        const float probability = _data[i] / static_cast<float>(_numValues);
        const float cdf = std::min(1.0f, previousCdf + probability);
        _equalizer[i] = cdf * (_numBins-1);
        previousCdf = cdf;
    }
}

/*
 * Will return a equalized histogram
 */
Histogram Histogram::equalize() {
    Histogram equalizedHistogram(_minValue, _maxValue, _numBins);

    for (int i = 0; i < _numBins; i++) {
        equalizedHistogram._data[static_cast<int>(_equalizer[i])] += _data[i];
    }
    equalizedHistogram._numValues = _numValues;
    return equalizedHistogram;
}

/*
 * Given a value within the domain of this histogram (_minValue < value < maxValue),
 * this method will use its equalizer to return a histogram equalized result.
 */
float Histogram::equalize(float value) const {
    // if (value < _minValue || value > _maxValue) {
    //     LWARNING(
    //         "Equalized value is is not within domain of histogram. min: " +
    //         std::to_string(_minValue) + " max: " + std::to_string(_maxValue) +
    //         " val: " + std::to_string(value)
    //     );
    // }
    float normalizedValue = (value-_minValue)/(_maxValue-_minValue);
    int bin = static_cast<int>(floor(normalizedValue * _numBins));
    // If value == _maxValues then bin == _numBins, which is a invalid index.
    bin = std::min(_numBins-1, bin);
    bin = std::max(0, bin);

    return _equalizer[bin];
}

float Histogram::entropy() {
    float entropy = 0.f;
    for (int i = 0; i < _numBins; ++i) {
        if (_data[i] != 0) {
            entropy -= (_data[i] / static_cast<float>(_numValues)) *
                        (log2(_data[i]) / static_cast<float>(_numValues));
        }
    }
    return entropy;
}

std::vector<char> Histogram::getBinaryData() const {
    std::vector<char> dataVector(
        reinterpret_cast<char*>(_data),
        reinterpret_cast<char*>(_data + _numValues)
    );
    return dataVector;
}

void Histogram::print() const {
    //for (int i = 0; i < _numBins; i++) {
    //    //float low = _minValue + float(i) / _numBins * (_maxValue - _minValue);
    //    //float high = low + (_maxValue - _minValue) / float(_numBins);
    //    // std::cout << i << " [" << low << ", " << high << "]"
    //    //           << "   " << _data[i] << std::endl;
    //    std::cout << _data[i]/static_cast<float>(_numValues) << ", ";
    //}
    //std::cout << std::endl;
    //// std::cout << std::endl << std::endl << std::endl<< "==============" << std::endl;
}

float Histogram::highestBinValue(bool equalized, int overBins){
    int highestBin = 0;
    float highestValue = 0;

    for (int i = 0; i < _numBins; i++) {
        float value = 0;
        int num = 0;
        for (int j=0; j<overBins; j++) {
            if (i - j > 0) {
                value += _data[i-j];
                num++;
            }
            if (i + j < _numBins) {
                value += _data[i+j];
                num++;
            }
        }

        value += _data[i];
        value /= static_cast<float>(++num);

        if (value > highestValue) {
            highestBin = i;
            highestValue = value;
        }
    }


    if (!equalized) {
        float low = _minValue + static_cast<float>(highestBin) /
                    _numBins * (_maxValue - _minValue);
        float high = low + (_maxValue - _minValue) / static_cast<float>(_numBins);
        return (high+low) / 2.f;
    } else {
        return highestBin / static_cast<float>(_numBins);
        // return equalize((high+low)/2.0);
    }
}

float Histogram::binWidth() {
    return (_maxValue - _minValue) / _numBins;
}

} // namespace openspace
