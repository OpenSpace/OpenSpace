/*****************************************************************************************
*                                                                                       *
* GHOUL                                                                                 *
* General Helpful Open Utility Library                                                  *
*                                                                                       *
* Copyright (c) 2012-2017                                                               *
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

#include <modules/marsrover/surfaceprojection/mslterrain.h>

//#include <modules/globebrowsing/globebrowsingmodule.h>

#include <ghoul/misc/assert.h>

#include <modules/globebrowsing/globes/renderableglobe.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/glm.h>

#include <vector>
#include <iterator>
#include <algorithm>

namespace {
//const std::string _loggerCat = "MslTerrain";
}

namespace openspace {
	
	MslTerrain::MslTerrain(const ghoul::Dictionary& dictionary)
	 : ProjectionProvider(dictionary) {
	}

	//MslTerrain::MslTerrain() 
	//{
	//	height_value = 0;
	//	//min_value = 0;
	//	//max_value = 0;
	//}

	//bool MslTerrain::Initialize (std::string heightFile) {
	//}
	
	double MslTerrain::getHeightValue(glm::dvec3 position) {

	}

	struct Node* MslTerrain::newNode(double arr[])
	{
		struct Node* temp = new Node;
		for (int i = 0; i < 3; i++) 
			temp->point[i] = arr[i];
			
		temp->left = temp->right = NULL;

		return temp;
	}

	Node* MslTerrain::insertPoint(Node *root, double point[], int depth)
	{
		//tree empty
		if (root == NULL)
			return newNode(point);

		//calculate current dimension
		int cd = depth % 3;

		if (point[cd] < (root->point[cd]))
			root->left = insertPoint(root->left, point depth + 1);
		else
			root->right = insertPoint(root->left, point depth + 1);

		return root;
	}

	bool MslTerrain::searchForPoint(Node* root, double point[], int depth) 
	{
		//base cases
		if (root == NULL)
			return false;

		//more code
	}

	void MslTerrain::createHeightMap() 
	{
		//First call for readHeightmap
		//readHeightmapFile(std::string("HeightmapTesting.jpg"));

		//testing testing
		struct Node *root = NULL;
		double points[][3] = { {3, 6}, {17, 15}, {13, 15}, {6, 12}, {9, 1}, {2, 7}, {10, 19}}

		int n = sizeof(points)/sizeof(points[0]);

		for (int i = 0; i < n; i++)
			root = insert(root, points[i]);


	}


	void MslTerrain::loadHeightMap() {
		std::string texturePath = "Heightmap.jpg";

		if (texturePath.value() != "") {
			using TR = ghoul::io::TextureReader;
			std::unique_ptr<ghoul::opengl::Texture> texture = TR::ref().loadTexture(texturePath);

			if (texture) {
				//LDEBUG(
				//	"MarsHeightMap",
				//	fmt::format("Loaded texture from '{}'", absPath(texturePath))
				//);

				//not to use?
				//texture->uploadTexture();
				//not to use?
				//texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);

				_texture = std::move(texture);
			}
		}
	}

	

	
	std::vector<std::vector<float>> MslTerrain::readHeightMapFile(std::string filepath) {
		
		usigned char bytes[4];

		FILE *fileID = fopen(filepath.c_str(), "rb");
		
		bool firstIsFound = false;
		float f;
		std::std::vector<float> xyz;

		//iterate over all bands
		for (int band = 0; band < pci._cols; ++band) {

			std::vctor<float> lines;
			xyz.push_back(lines);

			for (int j = 0; j < pci._cols; j++ ) {
				for (int k = 0; k < pci._lines; ++k ) {

					float cf;
					char *floatToConvert = (char*)& f;
					char *floatConverted = (char*)& cf;

					//floatConverted[0] = floatToConvert[3];
					//floatConverted[1] = floatToConvert[2];
					//floatConverted[2] = floatToConvert[1];
					//floatConverted[3] = floatToConvert[0];

					xyz.at(band).push_back(cf);
				}				
			}			
		}
		fclose(fileID);
		return xyz;
	}

	/*
	std::vector<std::vector<float>> MslTerrain::readHeightMapFile(std::string filepath) {
		//Define file stream object
		std::ifstream file("HeightmapTesting.jpg", ios::binary);
		//Prepare iterator pairs to iterate the file content
		std::istream_iterator<unisigned char>begin(file), end;
		//reading the file content using the iterator
		std::vector<unisigned char> buffer(begin, end);
	}*/
	

	void MslTerrain::initialize() {
		return;
	}

}

