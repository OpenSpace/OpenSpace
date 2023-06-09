#include "test.h"
#include <HighFive/H5File.hpp>
//#include <HighFive/H5Selection.hpp>
#include <HighFive/bits/H5Slice_traits.hpp>
#include <highfive/H5Easy.hpp>
#include <highfive/H5DataSet.hpp>
#include <highfive/H5Object.hpp>
#include <HighFive/H5Group.hpp>
#include "cstring";
#include "iostream";
#include <cmath>;



std::string myFile = "../openSpace.eb3.h5";
std::vector<std::vector<std::vector<std::vector<float>>>> _volumeCoordinates;

std::vector<std::string> _extraQuantatiesNames;
std::vector<std::vector<std::vector<std::vector<float>>>> _extraQuantaties;

std::vector<std::vector<std::vector<float>>> _slicedDataBP; // Slice fo data BEFORE position of slice
std::vector<std::vector<std::vector<float>>> _slicedDataAP; // Slice fo data AFTER position of slice

std::vector<std::vector<std::vector<float>>> _interpolatedData;

void readHdfFile(std::string pathToHdf5File) {
	HighFive::File file(pathToHdf5File, HighFive::File::ReadOnly);

	std::vector<std::vector<std::vector<float>>> vCoordDataX;
	std::vector<std::vector<std::vector<float>>> vCoordDataY;
	std::vector<std::vector<std::vector<float>>> vCoordDataZ;

	std::vector<std::vector<std::vector<float>>> extraData;

	const HighFive::DataSet dsX = file.getDataSet("/X");
	const HighFive::DataSet dsY = file.getDataSet("/Y");
	const HighFive::DataSet dsZ = file.getDataSet("/Z");

	dsX.read(vCoordDataX);
	dsY.read(vCoordDataY);
	dsZ.read(vCoordDataZ);

	int all_z = vCoordDataZ.size(); // number of matrices
	int all_y = vCoordDataY[0].size(); // number of cols for each matrix
	int all_x = vCoordDataX[0][0].size(); // number of rows for each 

	//TODO gör alla temp_coord to glm::vec3
	_volumeCoordinates.resize(all_z, std::vector<std::vector<std::vector<float>>>(all_y, std::vector<std::vector<float>>(all_x)));

	for (int z = 0; z < all_z; z++) { //z ---- Every z value that exists
		for (int y = 0; y < all_y; y++) { //y ---- Every y value that exists on (z)
			for (int x = 0; x < all_x; x++) { //x ---- Every x value that exists on (z, y)

				std::vector<float> temp_coord;

				temp_coord.push_back(vCoordDataX[0][0][x]); //First matrix, first column, all rows
				temp_coord.push_back(vCoordDataY[0][y][0]); //First matrix, all columns, first rows
				temp_coord.push_back(vCoordDataZ[z][0][0]); //All matrices, first column, all rows

				_volumeCoordinates[z][y][x] = temp_coord;
			}
		}
	}

	const HighFive::Group timeStep = file.getGroup("/Step#1");
	_extraQuantatiesNames = timeStep.listObjectNames();

	for (int q = 0; q < _extraQuantatiesNames.size(); q++) {
		std::vector<float> temp;
		std::string quantityName = _extraQuantatiesNames[q];

		const HighFive::DataSet dsExtraQ = timeStep.getDataSet(quantityName);
		dsExtraQ.read(extraData);

		_extraQuantaties.push_back(extraData);
	}

}

void slicer(char axis, float value) {

	int all_z = _volumeCoordinates.size(); // number of matrices
	int all_y = _volumeCoordinates[0].size(); // number of cols for each matrix
	int all_x = _volumeCoordinates[0][0].size(); // number of rows for each

	int all_extraQ = _extraQuantaties.size(); // number of datasets
	int all_zq = _extraQuantaties[0].size(); // number of matrices
	int all_yq = _extraQuantaties[0][0].size(); // number of cols for each matrix
	int all_xq = _extraQuantaties[0][0][0].size(); // number of rows for each 

	char axisInput = char(tolower(axis));

	std::vector<float> firstPoint = _volumeCoordinates.front().front().front();
	std::vector<float> lastPoint = _volumeCoordinates.back().back().back();

	 These should probably be class members
	float firstX = firstPoint[0];
	float lastX = lastPoint[0];
	float firstY = firstPoint[1];
	float lastY = lastPoint[1];
	float firstZ = firstPoint[2];
	float lastZ = lastPoint[2];

	if (axisInput == 'x') {
		int index = abs(firstX) + std::round(value);; //start value + desired cutplane value

		for (size_t q = 0; q < all_extraQ; q++) {
			std::vector<std::vector<float>> temp1(all_zq, std::vector<float>(all_yq)); //make vec2
			std::vector<std::vector<float>> temp2(all_zq, std::vector<float>(all_yq)); //make vec2

			for (size_t z = 0; z < all_zq; z++) {
				for (size_t y = 0; y < all_yq; y++) {
					temp1[z][y] = _extraQuantaties[q][z][y][index - 1]; // Gives the cutplane for all d1
					temp2[z][y] = _extraQuantaties[q][z][y][index]; // Gives the cutplane for all d2
				}
			}
			_slicedDataBP.push_back(temp1);
			_slicedDataAP.push_back(temp2);
		}
	}
	else if (axisInput == 'y') {
		int index = abs(firstY) + std::round(value);; //start value + desired cutplane value

		for (size_t q = 0; q < all_extraQ; q++) {
			std::vector<std::vector<float>> temp1(all_zq, std::vector<float>(all_xq)); //make vec2
			std::vector<std::vector<float>> temp2(all_zq, std::vector<float>(all_xq)); //make vec2

			for (size_t z = 0; z < all_zq; z++) {
				for (size_t x = 0; x < all_xq; x++) {
					temp1[z][x] = _extraQuantaties[q][z][index - 1][x]; // Gives the cutplane for all d1
					temp2[z][x] = _extraQuantaties[q][z][index][x]; // Gives the cutplane for all d2
				}
			}
			_slicedDataBP.push_back(temp1);
			_slicedDataAP.push_back(temp2);
		}
	}
	else if (axisInput == 'z') {
		int index = abs(firstZ) + std::round(value); //start value + desired cutplane value

		for (size_t q = 0; q < all_extraQ; q++) {
			std::vector<std::vector<float>> temp1(all_yq, std::vector<float>(all_xq)); //make vec2
			std::vector<std::vector<float>> temp2(all_yq, std::vector<float>(all_xq)); //make vec2
			for (size_t y = 0; y < all_yq; y++) {
				for (size_t x = 0; x < all_xq; x++) {
					temp1[y][x] = _extraQuantaties[q][index - 1][y][x]; // Gives the cutplane for all d1
					temp2[y][x] = _extraQuantaties[q][index][y][x]; // Gives the cutplane for all d2
				}
			}
			_slicedDataBP.push_back(temp1);
			_slicedDataAP.push_back(temp2);
		}

	}
	else {
		std::cout << "Invalid axis input";
		return;
	}
}

void interpolator(float value) {

	int size_1 = _slicedDataAP[0].size();
	int size_2 = _slicedDataAP[0][0].size();

	double integerPart;
	float value_shifted = value + 0.5; // Caused by the shift 0.5 from grid to data (cell centered data)
	float decimalPart = modf(value_shifted, &integerPart);

	std::cout << "value: " << value << ", shifted value: " << value_shifted << ", decimal: " << decimalPart << "\n";

	if (decimalPart == 1) { // Sliced right on datapoint
		_interpolatedData = _slicedDataBP;
	}
	else {
		for (size_t q = 0; q < _extraQuantaties.size(); q++) {
			std::vector<std::vector<float>> temp(size_1, std::vector<float>(size_2)); //make vec2

			for (size_t i = 0; i < size_1; i++) {
				for (size_t j = 0; j < size_2; j++) {
					float d1 = _slicedDataBP[q][i][j];
					float d2 = _slicedDataAP[q][i][j];

					float v = (decimalPart - 1) * d1 + decimalPart * d2; // Interpolation algorithm : v = (t-1)*d1 + t*d2

					temp[i][j] = v;
				}
			}
			_interpolatedData.push_back(temp);
		}
	}
}

int main() {
	char axis = 'x';
	float value = 3.3;
	readHdfFile(myFile);
	slicer(axis, value);
	interpolator(value);

//for (int a = 0; a < 10; a++)
//{
//    for (int b = 0; b < 10; b++)
//    {
//        std::cout << _interpolatedData[0][a][b] << "  ";
//    }
//    std::cout << "\n";
//}
//
//for (auto i : _volumeCoordinates[0][0]) {
//        std::cout << "x: " << i[0];
//        std::cout << " y: " << i[1];
//        std::cout << " z: " << i[2]
//            << "\n";
//
//}
}







