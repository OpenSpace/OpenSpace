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

#include <modules/iswa/util/dataprocessor.h>

#include <openspace/util/histogram.h>
#include <algorithm>
#include <fstream>
#include <numeric>

namespace openspace {

void DataProcessor::useLog(bool useLog) {
    _useLog = useLog;
}

void DataProcessor::useHistogram(bool useHistogram) {
    _useHistogram = useHistogram;
}

void DataProcessor::normValues(glm::vec2 normValues) {
    _normValues = normValues;
}

glm::size3_t DataProcessor::dimensions() const {
    return _dimensions;
}

glm::vec2 DataProcessor::filterValues() const {
    return _filterValues;
}

void DataProcessor::clear() {
    _min.clear();
    _max.clear();
    _sum.clear();
    _standardDeviation.clear();
    _histograms.clear();
    _numValues.clear();
}

float DataProcessor::processDataPoint(float value, int option) {
    if (_numValues.empty()) {
        return 0.f;
    }
    const Histogram& histogram = *_histograms[option];
    const float mean = (1.f / _numValues[option]) * _sum[option];
    const float sd = _standardDeviation[option];

    if (_useHistogram) {
        return histogram.equalize(
            normalizeWithStandardScore(value, mean, sd, _histNormValues)
        ) / 512.f;
    } else {
        return normalizeWithStandardScore(value, mean, sd, _normValues);
    }
}

float DataProcessor::normalizeWithStandardScore(float value, float mean, float sd,
                                                const glm::vec2& normalizationValues)
{
    float zScoreMin = normalizationValues.x;
    float zScoreMax = normalizationValues.y;
    float standardScore = ( value - mean ) / sd;
    // Clamp intresting values
    standardScore = glm::clamp(standardScore, -zScoreMin, zScoreMax);
    //return and normalize
    return ( standardScore + zScoreMin ) / (zScoreMin + zScoreMax );
}

float DataProcessor::unnormalizeWithStandardScore(float standardScore, float mean,
                                                  float sd,
                                                  const glm::vec2& normalizationValues)
{
    float zScoreMin = normalizationValues.x;
    float zScoreMax = normalizationValues.y;

    float value = standardScore * (zScoreMax + zScoreMin) - zScoreMin;
    value = value * sd + mean;
    return value;
}

void DataProcessor::initializeVectors(int numOptions) {
    if (_min.empty()) {
        _min = std::vector<float>(numOptions, std::numeric_limits<float>::max());
    }
    if (_max.empty()) {
        _max = std::vector<float>(numOptions, std::numeric_limits<float>::min());
    }
    if (_sum.empty()) {
        _sum = std::vector<float>(numOptions, 0.0f);
    }
    if (_standardDeviation.empty()) {
        _standardDeviation = std::vector<float>(numOptions, 0.0f);
    }
    if (_numValues.empty()) {
        _numValues = std::vector<float>(numOptions, 0.0f);
    }
    if (_histograms.empty()) {
        _histograms.clear();
        _histograms.reserve(numOptions);
    }
}

void DataProcessor::calculateFilterValues(const std::vector<int>& selectedOptions) {
    _filterValues = glm::vec2(0.f);
    if (selectedOptions.empty()) {
        return;
    }

    if (!_histograms.empty()) {
        for (int option : selectedOptions) {
            float filterMid;
            float filterWidth;
            if (!_useHistogram) {
                float mean = (1.f / _numValues[option]) * _sum[option];
                float standardDeviation = _standardDeviation[option];
                Histogram& histogram = *_histograms[option];

                filterMid = histogram.highestBinValue(_useHistogram);
                filterWidth = mean + histogram.binWidth();

                filterMid = normalizeWithStandardScore(
                    filterMid,
                    mean,
                    standardDeviation,
                    _normValues
                );
                filterWidth = fabs(0.5f - normalizeWithStandardScore(
                    filterWidth,
                    mean,
                    standardDeviation,
                    _normValues)
                );
            } else {
                Histogram hist = _histograms[option]->equalize();
                filterMid = hist.highestBinValue(true);
                filterWidth = 1.f / 512.f;
            }

             _filterValues += glm::vec2(filterMid, filterWidth);
        }
        const int numSelected = static_cast<int>(selectedOptions.size());
        _filterValues /= numSelected;
    }
}

void DataProcessor::add(const std::vector<std::vector<float>>& optionValues,
                        const std::vector<float>& sum)
{
    const int numOptions = static_cast<int>(optionValues.size());

    for (int i = 0; i < numOptions; ++i) {
        const std::vector<float>& values = optionValues[i];
        const int numValues = static_cast<int>(values.size());

        const float mean = sum[i] / numValues;
        const float variance = std::accumulate(
            values.begin(),
            values.end(),
            0.f,
            [mean](float l, float r) { return l + pow(r - mean, 2); }
        );
        const float standardDeviation = sqrt(variance / numValues);

        const float oldStandardDeviation = _standardDeviation[i];
        const float oldMean = (1.f / _numValues[i]) * _sum[i];

        _sum[i] += sum[i];
        _standardDeviation[i] = sqrt(pow(standardDeviation, 2) +
                                pow(_standardDeviation[i], 2));
        _numValues[i] += numValues;

        const float min = normalizeWithStandardScore(
            _min[i],
            _sum[i] / _numValues[i],
            _standardDeviation[i],
            _histNormValues
        );
        const float max = normalizeWithStandardScore(
            _max[i],
            _sum[i] / _numValues[i],
            _standardDeviation[i],
            _histNormValues
        );

        if (!_histograms[i]) {
             _histograms[i] = std::make_unique<Histogram>(min, max, 512);
        }
        else {
            const float* histData = _histograms[i]->data();
            const float histMin = _histograms[i]->minValue();
            const float histMax = _histograms[i]->maxValue();
            const int numBins = _histograms[i]->numBins();

            const float unNormHistMin = unnormalizeWithStandardScore(
                histMin,
                oldMean,
                oldStandardDeviation,
                _histNormValues
            );
            const float unNormHistMax = unnormalizeWithStandardScore(
                histMax,
                oldMean,
                oldStandardDeviation,
                _histNormValues
            );
            //unnormalize histMin, histMax
            std::unique_ptr<Histogram> newHist = std::make_unique<Histogram>(
                std::min(min, normalizeWithStandardScore(
                    unNormHistMin,
                    mean,
                    _standardDeviation[i],
                    _histNormValues
                )),
                std::min(max, normalizeWithStandardScore(
                    unNormHistMax,
                    mean,
                    _standardDeviation[i],
                    _histNormValues
                )),
                numBins
            );

            for (int j = 0; j < numBins; ++j) {
                float value = unnormalizeWithStandardScore(
                    j * (histMax - histMin) + histMin,
                    oldMean,
                    oldStandardDeviation,
                    _histNormValues
                );
                _histograms[i]->add(
                    normalizeWithStandardScore(
                        value,
                        mean,
                        _standardDeviation[i],
                        _histNormValues
                    ),
                    histData[j]
                );
            }
            // _histograms[i]->changeRange(min, max);
            _histograms[i] = std::move(newHist);
        }

        for (int j = 0; j < numValues; ++j) {
            _histograms[i]->add(
                normalizeWithStandardScore(
                    values[j],
                    mean,
                    _standardDeviation[i],
                    _histNormValues
                ),
                1
            );
        }

        _histograms[i]->generateEqualizer();
    }
}

} // namespace
