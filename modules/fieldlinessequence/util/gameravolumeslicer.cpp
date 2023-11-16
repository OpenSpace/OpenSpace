///*****************************************************************************************
// *                                                                                       *
// * OpenSpace                                                                             *
// *                                                                                       *
// * Copyright (c) 2014-2023                                                               *
// *                                                                                       *
// * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
// * software and associated documentation files (the "Software"), to deal in the Software *
// * without restriction, including without limitation the rights to use, copy, modify,    *
// * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
// * permit persons to whom the Software is furnished to do so, subject to the following   *
// * conditions:                                                                           *
// *                                                                                       *
// * The above copyright notice and this permission notice shall be included in all copies *
// * or substantial portions of the Software.                                              *
// *                                                                                       *
// * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
// * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
// * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
// * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
// * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
// * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
// ****************************************************************************************/

#include <modules/fieldlinessequence/util/gameravolumeslicer.h>
#include <HighFive/H5File.hpp>
#include <HighFive/bits/H5Slice_traits.hpp>
#include <highfive/H5Easy.hpp>
#include <highfive/H5DataSet.hpp>
#include <highfive/H5Object.hpp>
#include <HighFive/H5Group.hpp>
#include <cstring>
#include <iostream>
#include <cmath>

#include <fstream>
#include <optional>
#include <ostream>
#include <algorithm>
#include <cmath>

#include <ghoul/logging/logmanager.h>

namespace  openspace{
    constexpr std::string_view _loggerCat = "GameraVolumeSlicer";
//************************ INTERPOLATOR ***********************//
// Interpolation algorithm : v = (t-1)*d1 + t*d2
//************************************************************//
void GameraVolumeSlicer::interpolator(float value, std::vector<std::vector<std::vector<float>>> slicedDataBDP, 
                                      std::vector<std::vector<std::vector<float>>> slicedDataADP,
                                      std::vector<std::vector<std::vector<float>>>& data) 
{

    int size_1 = slicedDataADP[0].size();
    int size_2 = slicedDataADP[0][0].size();
    
    if (slicedDataBDP.size() != slicedDataADP.size() ||
        slicedDataBDP[0].size() != slicedDataADP[0].size() ||
        slicedDataBDP[0][0].size() != slicedDataADP[0][0].size()) {
    }
    
    if (value < 0.0 || value > 1.0) {
        std::cout << "VALUE utanfor range" << std::endl;
    }


    double integerPart;
    float value_shifted = value + 0.5; // Caused by the shift 0.5 from grid to data (cell centered data)
    float decimalPart = modf(value_shifted, &integerPart);

    std::cout << "value: " << value << ", shifted value: " << value_shifted << ", decimal: " << decimalPart << "\n";

    if (decimalPart == 1) { // Sliced right on datapoint
        data = slicedDataBDP;
    }
    else {
        for (size_t q = 0; q < slicedDataBDP.size(); q++) {
            std::vector<std::vector<float>> temp(size_1, std::vector<float>(size_2));

            for (size_t i = 0; i < size_1; i++) {
                for (size_t j = 0; j < size_2; j++) {
                    float d1 = slicedDataBDP[q][i][j];
                    float d2 = slicedDataADP[q][i][j];

                    float v = abs((decimalPart - 1)) * d1 + decimalPart * d2;

                    temp[i][j] = v;
                }
            }
            data.push_back(temp);
        }
    }
    
}

//************************ SLICER ***********************//
// "index" is calculated as: start value + desired cutplane value
// "offset" decides where the selection should start
// "count" decides how many values from the offset value
// that should be included
//******************************************************//
std::vector<std::vector<std::vector<float>>> GameraVolumeSlicer::slicer(std::string axis, float value, HighFive::Group timeStep, HighFive::File file) {

    std::transform(axis.begin(), axis.end(), axis.begin(),
                      [](unsigned char a) { return std::tolower(a); });

    std::vector<std::vector<std::vector<float>>> slicedDataBDP; // Slice fo data BEFORE position of slice, these can be function variables
    std::vector<std::vector<std::vector<float>>> slicedDataADP; // Slice fo data AFTER position of slice, these can be function variables
    std::vector<std::vector<std::vector<float>>> data; // To store the final slices
    
    const HighFive::DataSet dsX = file.getDataSet("/X");
    const HighFive::DataSet dsY = file.getDataSet("/Y");
    const HighFive::DataSet dsZ = file.getDataSet("/Z");
    
    auto dataset_dimensionsX = dsX.getSpace().getDimensions();
    auto dataset_dimensionsY = dsY.getSpace().getDimensions();
    auto dataset_dimensionsZ = dsZ.getSpace().getDimensions();
 

    for (int q = 0; q < _dataPropertyNames.size(); q++) {
        std::string dataPropertyName = _dataPropertyNames[q];
        const HighFive::DataSet dsProperty = timeStep.getDataSet(dataPropertyName);

        // Get the dataset dimensions
        auto dataset_dimensions = dsProperty.getSpace().getDimensions();

        // Define the selection parameters
        std::vector<size_t> offsetBDP(dataset_dimensions.size(), 0);
        std::vector<size_t> offsetADP(dataset_dimensions.size(), 0);
        std::vector<size_t> count(dataset_dimensions.size(), 1);

        if (axis == "x") {
            if (value <= _volumeDimensions[0][0] || value >= _volumeDimensions[0][1]) {
                LERROR("Invalid value, unable to slice");
            }
            auto stepSizeFactor =  dataset_dimensionsX[0] / (abs(_volumeDimensions[0][0]) + abs(_volumeDimensions[0][1]));
            int index = abs(_volumeDimensions[0][0]) * stepSizeFactor  + std::round(value);
            offsetBDP[2] = index - 1;
            offsetADP[2] = index;
            count[0] = dataset_dimensions[0];
            count[1] = dataset_dimensions[1];
        }
        else if (axis == "y") {
            if (value <= _volumeDimensions[1][0] || value >= _volumeDimensions[1][1]) {
                LERROR("Invalid value, unable to slice");
            }
            auto stepSizeFactor =  dataset_dimensionsX[1] / (abs(_volumeDimensions[1][0]) + abs(_volumeDimensions[1][1]));
            int index = abs(_volumeDimensions[1][0]) * stepSizeFactor + std::round(value);
            offsetBDP[1] = index - 1;
            offsetADP[1] = index;
            count[0] = dataset_dimensions[0];
            count[2] = dataset_dimensions[2];
        }
        else if (axis == "z") {
            if (value <= _volumeDimensions[2][0] || value >= _volumeDimensions[2][1]) {
                LERROR("Invalid value, unable to slice");
            }
            auto stepSizeFactor =  dataset_dimensionsX[2] / (abs(_volumeDimensions[2][0]) + abs(_volumeDimensions[2][1]));
            int index = abs(_volumeDimensions[2][0]) * stepSizeFactor + std::round(value);
            offsetBDP[0] = index - 1;
            offsetADP[0] = index;
            count[1] = dataset_dimensions[1];
            count[2] = dataset_dimensions[2];
        }
        else {
            std::cout << "Invalid axis input";
            return data;
        }

        HighFive::Selection sliceBDP = dsProperty.select(offsetBDP, count);
        HighFive::Selection sliceADP = dsProperty.select(offsetADP, count);

        // Create a 3D vector to store the slice
        std::vector<std::vector<std::vector<float>>> dataBDP(count[0], std::vector<std::vector<float>>(count[1], std::vector<float>(count[2])));
        std::vector<std::vector<std::vector<float>>> dataADP(count[0], std::vector<std::vector<float>>(count[1], std::vector<float>(count[2])));

        // Read the slice data into the 3D vector
        sliceBDP.read(dataBDP);
        sliceADP.read(dataADP);

        std::vector<std::vector<float>> tempBDP;
        std::vector<std::vector<float>> tempADP;

        if (axis == "x") { // Find another solution for formating the different dimensions in the same way.
            // Flatten the 3D vector into a 2D vector
            for (const auto& innerVec : dataBDP) {
                std::vector<float> temp;
                for (const auto& vec : innerVec) {
                    for (const auto& num : vec) {
                        temp.push_back(num);
                    }
                }
                tempBDP.push_back(temp);
            }

            // Flatten the 3D vector into a 2D vector
            for (const auto& innerVec : dataADP) {
                std::vector<float> temp;
                for (const auto& vec : innerVec) {
                    for (const auto& num : vec) {
                        temp.push_back(num);
                    }
                }
                tempADP.push_back(temp);
            }
        }
        else {
            // Flatten the 3D vector into a 2D vector
            for (const auto& innerVec : dataBDP) {
                for (const auto& vec : innerVec) {
                    tempBDP.push_back(vec);
                }
            }

            // Flatten the 3D vector into a 2D vector
            for (const auto& innerVec : dataADP) {
                for (const auto& vec : innerVec) {
                    tempADP.push_back(vec);
                }
            }
        }

        slicedDataBDP.push_back(tempBDP);
        slicedDataADP.push_back(tempADP);

    }

    interpolator(value, slicedDataBDP, slicedDataADP, data);
    return data;
}

//****************** EXTRACT VOLUME DIMENSIONS *****************//
//*************************************************************//
std::vector<std::vector<float>> GameraVolumeSlicer::getVolumeDimensions(HighFive::File file) {
    // Open the datasets
    const HighFive::DataSet dsX = file.getDataSet("/X");
    const HighFive::DataSet dsY = file.getDataSet("/Y");
    const HighFive::DataSet dsZ = file.getDataSet("/Z");

    // Get the dataset dimensions
    auto dataset_dimensions = dsX.getSpace().getDimensions();

    // Define the selection parameters
    std::vector<size_t> offsetMin(dataset_dimensions.size(), 0);
    std::vector<size_t> offsetMax(dataset_dimensions.size(), 0);
    std::vector<size_t> count(dataset_dimensions.size(), 1);

    // Set the starting offset for the dimension to the size for 
    // that dimension minus 1 to get the last slot in the dimension
    for (size_t i = 0; i < dataset_dimensions.size(); i++) {
        offsetMax[i] = dataset_dimensions[i] - 1;
    }

    std::vector<std::vector<float>> volumeDimensions;
    // For each dimension, get the max and min position
    for (size_t i = 0; i < dataset_dimensions.size(); i++) {

        float valMin;
        float valMax;
        std::vector<float> interval;

        if (i == 0) {
            HighFive::Selection selection_min = dsX.select(offsetMin, count);
            // Read the selected data
            selection_min.read(valMin);
            interval.push_back(valMin);

            HighFive::Selection selection_max = dsX.select(offsetMax, count);
            // Read the selected data
            selection_max.read(valMax);
            interval.push_back(valMax);

        }
        else if (i == 1) {
            HighFive::Selection selection_min = dsY.select(offsetMin, count);
            // Read the selected data
            selection_min.read(valMin);
            interval.push_back(valMin);

            HighFive::Selection selection_max = dsY.select(offsetMax, count);
            // Read the selected data
            selection_max.read(valMax);
            interval.push_back(valMax);
        }
        else if (i == 2) {
            HighFive::Selection selection_min = dsZ.select(offsetMin, count);
            // Read the selected data
            selection_min.read(valMin);
            interval.push_back(valMin);

            HighFive::Selection selection_max = dsZ.select(offsetMax, count);
            // Read the selected data
            selection_max.read(valMax);
            interval.push_back(valMax);
        }
        volumeDimensions.push_back(interval);
    }

    return volumeDimensions;
}

//************************ RETURN _QUANTITIESNAMES ***********************//
//*************************************************************//
std::vector<std::string> GameraVolumeSlicer::dataPropertyNames() {
    return _dataPropertyNames;
}

//************************ RETURN _VOLUMEDIMENSIONS ***********************//
//*************************************************************//
std::vector<std::vector<float>> GameraVolumeSlicer::volumeDimensions() {
    return _volumeDimensions;
}

//************************ RETURN _DATA ***********************//
//*************************************************************//
std::vector<std::vector<std::vector<float>>> GameraVolumeSlicer::data() {
    return _data;
}


//************************ MAIN FUNCTION ***********************//
//*************************************************************//
void GameraVolumeSlicer::getSlice(std::string pathToHdf5File, std::string axis, int value) {

    // Open the HDF5 file
    HighFive::File file(pathToHdf5File, HighFive::File::ReadOnly);

    // Open the group for desired timestep
    const HighFive::Group timeStep = file.getGroup("/Step#0");

    // Get all the names of datasets stored in group 'timeStep'
    _dataPropertyNames = timeStep.listObjectNames();
    _volumeDimensions = getVolumeDimensions(file);
    _data = slicer(axis, value, timeStep, file);
 
}
} // namespace openspace

