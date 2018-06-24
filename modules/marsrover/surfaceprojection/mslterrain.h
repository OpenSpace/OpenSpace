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

#ifndef __OPENSPACE_MODULE_MARSROVER___MSLTERRAIN___H__
#define __OPENSPACE_MODULE_MARSROVER___MSLTERRAIN___H__

#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/glm.h>

#include <modules/marsrover/surfaceprojection/projectionprovider.h>

namespace openspace {

class MslTerrain : public ProjectionProvider {
	public:
		//MslTerrain();
		MslTerrain(const ghoul::Dictionary& dictionary);

		//bool Initialize (std::string heightFile);
		
		//double getHeightValue(glm::dvec3 position);
		//struct Node* newNode(double arr[]);
		//Node* insertPoint(Node *root, double point[], int depth);
		//bool searchForPoint(Node* root, double point[], int depth);
		void createHeightMap();  

		void initialize();

	//private:
		//int nr_vertices;
		//int nr_indices;
		//double height_value;
		////double min_value;
		////double max_value;

		//double heightOffset; //value for scaling to recieve correct height with respect to OS

		//int cols;
		//int rows;

		//struct Node {
		//	double point[3];
		//	Node *left, *right;
		//};



};

}

#endif //__OPENSPACE_MODULE_MARSROVER___MARSPROJECTION___H__
