/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2017                                                               *
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

#include <modules/globebrowsing/tasks/meshgeneration.h>

#include <modules/globebrowsing/tasks/imgreader.h>
#include <modules/globebrowsing/tasks/meshwriter.h>

#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>

#include <pcl/point_types.h>
#include <pcl/io/obj_io.h>
#include <pcl/filters/voxel_grid.h>
#include <pcl/features/normal_3d.h>
#include <pcl/common/transforms.h>
#include <pcl/surface/gp3.h>
#include <pcl/surface/vtk_smoothing/vtk_mesh_subdivision.h>
#include <glm/gtx/quaternion.hpp>

#include <fstream>

namespace {
	const std::string _loggerCat = "MeshGeneration";
}

namespace openspace {
namespace globebrowsing {
	void MeshGeneration::generateMeshFromBinary(ghoul::Dictionary dictionary) {
		//const std::string binary_path, std::string output_path
		std::string binary_path;
		dictionary.getValue("BinaryPath", binary_path);

		std::string output_path;
		dictionary.getValue("OutputPath", output_path);

		ghoul::Dictionary extensions; 
		dictionary.getValue("Extension", extensions);

		std::string texture_extension;
		extensions.getValue("TextureExtension", texture_extension);

		ghoul_assert(!binary_path.empty(), "Filename must not be empty");

		// Create filename without path and extension
		std::string file_name_no_extension = binary_path.substr(0, binary_path.find_last_of("."));
		// Strip path
		std::string file_name_stripped = file_name_no_extension;
		file_name_stripped.erase(0, file_name_no_extension.find_last_of('/') + 1);
		
		const clock_t begin_time = clock();

		output_path = correctPath(file_name_stripped, output_path);
		
		if (!FileSys.directoryExists(output_path)) {
			ghoul::Boolean k(true);
			FileSys.createDirectory(output_path, k);
		}

		ImgReader::PointCloudInfo mInfo = ImgReader::readBinaryHeader(binary_path);

		LINFO("POINTS BEFORE DECIMATION: ");
		std::vector<std::vector<float>> xyz;

		ImgReader::readBinaryData(binary_path, xyz, mInfo);
		
		int uvTeller = 0;

		pcl::PointCloud<pcl::PointXYZ>::Ptr cloud(new pcl::PointCloud<pcl::PointXYZ>);
		pcl::PointCloud<pcl::PointNormal>::Ptr uvCloud(new pcl::PointCloud<pcl::PointNormal>);

		// TODO: Once again, try and create custom pcl::PointUV and see if we can speed up this process
		// Reconstruct the data format, and add cloud containing uv-coordinates
		for (int i = 0; i < mInfo._cols; ++i) {
			for (int k = 0; k < mInfo._lines; ++k) {

				// Extract points, the coordinate system for the binary file is the same as rover
				// Invert so that we get z up, too keep right handed coordinate system swap(x, y)
				float x = xyz.at(0).at(uvTeller);
				float y = xyz.at(1).at(uvTeller);
				float z = xyz.at(2).at(uvTeller);

				pcl::PointXYZ depthPoint;
				depthPoint.x = x;
				depthPoint.y = y;
				depthPoint.z = z;

				// Create uv coordinates
				pcl::PointNormal uvPoint;
				if (i < mInfo._cols - 1 && k < mInfo._lines - 1) {
					uvPoint.x = x;
					uvPoint.y = y;
					uvPoint.z = z;

					glm::fvec2 uv = glm::fvec2(i, k);
					uvPoint.normal_x = uv.x;
					uvPoint.normal_y = uv.y;
				}

				// We avoid adding origo, since the binary files contains zero-vectors for NULL data 
				if (x != 0.0 || y != 0.0 || z != 0.0) {
					if (i < mInfo._cols - 1 && k < mInfo._lines - 1) {
						uvCloud->push_back(uvPoint);
					}

					cloud->push_back(depthPoint);
				}

				uvTeller++;
			}
		}

		LINFO("POINTS BEFORE DECIMATION: " << cloud->points.size());

		if (cloud->points.size() == 0) return;

		writeTxtFile(file_name_stripped, output_path);

		// Create a VoxelGrid for the model
		// Used to simplify the model
		pcl::VoxelGrid<pcl::PointXYZ> voxel_grid;
		voxel_grid.setDownsampleAllData(false);
		voxel_grid.setInputCloud(cloud);
		voxel_grid.setLeafSize(0.18, 0.18, 0.18);
		voxel_grid.filter(*cloud);
		
		LINFO("POINTS AFTER DECIMATION: " << cloud->points.size());

		// Normal estimation
		pcl::NormalEstimation<pcl::PointXYZ, pcl::Normal> n;
		pcl::PointCloud<pcl::Normal>::Ptr normals(new pcl::PointCloud<pcl::Normal>);
		pcl::search::KdTree<pcl::PointXYZ>::Ptr tree(new pcl::search::KdTree<pcl::PointXYZ>);
		tree->setInputCloud(cloud);
		n.setInputCloud(cloud);
		n.setSearchMethod(tree);
		n.setKSearch(20);
		n.compute(*normals);

		// Concatenate the XYZ and normal fields
		pcl::PointCloud<pcl::PointNormal>::Ptr cloud_with_normals(new pcl::PointCloud<pcl::PointNormal>);
		pcl::concatenateFields(*cloud, *normals, *cloud_with_normals);

		LINFO("DONE CALCULATING THE NORMALS");
		
		// Create search tree
		pcl::search::KdTree<pcl::PointNormal>::Ptr tree2(new pcl::search::KdTree<pcl::PointNormal>);
		tree2->setInputCloud(cloud_with_normals);

		Eigen::Quaternionf q(mInfo._roverQuat.at(0), mInfo._roverQuat.at(1), mInfo._roverQuat.at(2), mInfo._roverQuat.at(3));

		Eigen::Matrix4f rotation = Eigen::Matrix4f::Identity();

		rotation.block(0, 0, 3, 3) = q.toRotationMatrix();

		typename pcl::PointCloud<pcl::PointNormal>::Ptr originalCloudTransformed(new pcl::PointCloud<pcl::PointNormal>);
		typename pcl::PointCloud<pcl::PointNormal>::Ptr originalCloudTransformed2(new pcl::PointCloud<pcl::PointNormal>);

		pcl::transformPointCloud(*cloud_with_normals, *originalCloudTransformed2, rotation);

		Eigen::AngleAxisf rollAngle(-M_PI / 2.0, Eigen::Vector3f::UnitZ());
		Eigen::AngleAxisf yawAngle(M_PI, Eigen::Vector3f::UnitY());
		Eigen::AngleAxisf pitchAngle(0, Eigen::Vector3f::UnitX());

		Eigen::Quaternionf q2 = pitchAngle * yawAngle * rollAngle;

		Eigen::Matrix4f rotationMatrix = Eigen::Matrix4f::Identity();
		rotationMatrix.block(0, 0, 3, 3) = q2.toRotationMatrix();

		pcl::transformPointCloud(*originalCloudTransformed2, *originalCloudTransformed, rotationMatrix);

		// Greedy projection triangulation algorithm
		pcl::GreedyProjectionTriangulation<pcl::PointNormal> gp3;
		pcl::PolygonMesh triangles;
		LINFO("RECONSTRUCTING MESH");
		// Set the maximum distance between connected points (maximum edge length)
		gp3.setSearchRadius(1.0);
		gp3.setMu(2.5);
		gp3.setMaximumNearestNeighbors(250);
		gp3.setMaximumSurfaceAngle(M_PI); // 45 degrees
		gp3.setMinimumAngle(0); // 10 degrees
		gp3.setMaximumAngle(M_PI); // 120 degrees
		gp3.setNormalConsistency(true);

		gp3.setInputCloud(originalCloudTransformed);
		gp3.setSearchMethod(tree2);
		gp3.reconstruct(triangles);

		LINFO("RECONSTRUCTION IS DONE");
		/*
		pcl::PolygonMesh::Ptr triangles = pcl::PolygonMesh::Ptr(new pcl::PolygonMesh);
		triangles->cloud = triangles2.cloud;
		triangles->header = triangles2.header;
		for (size_t i = 0; i < triangles2.polygons.size(); ++i) {
			triangles->polygons.push_back(triangles2.polygons.at(i));
		}

		pcl::PolygonMeshConstPtr constMeshPtr = pcl::PolygonMeshConstPtr(triangles);

		pcl::MeshSubdivisionVTK k;
		pcl::PolygonMesh pm;

		k.setInputMesh(triangles);
		k.setFilterType(pcl::MeshSubdivisionVTK::LINEAR);
		k.process(pm);
		*/
		// Create mesh from point cloud
		// TODO: Make this into TextureMeshPtr
		pcl::TextureMesh texMesh;

		std::vector<pcl::Vertices> polygons;

		texMesh.header = triangles.header;
		texMesh.cloud = triangles.cloud;

		// Move all the polygons from the pointcloud to mesh
		for (size_t i = 0; i < triangles.polygons.size(); ++i) {
			pcl::Vertices v1 = triangles.polygons.at(i);

			pcl::PointNormal pt = originalCloudTransformed->points[v1.vertices.at(0)];
			pcl::PointNormal pt2 = originalCloudTransformed->points[v1.vertices.at(1)];
			pcl::PointNormal pt3 = originalCloudTransformed->points[v1.vertices.at(2)];

			// Edges on triangle
			glm::fvec3 one = glm::normalize(glm::fvec3(pt.x, pt.y, pt.z) - glm::fvec3(pt3.x, pt3.y, pt3.z));
			glm::fvec3 two = glm::normalize(glm::fvec3(pt2.x, pt2.y, pt2.z) - glm::fvec3(pt3.x, pt3.y, pt3.z));

			// Normal for triangle
			glm::fvec3 norm = glm::cross(one, two);

			if (norm.z < 0) {
				//If the normal is pointing down, swap vertices
				int temp = v1.vertices.at(0);
				v1.vertices.at(0) = v1.vertices.at(1);
				v1.vertices.at(1) = temp;
			}

			polygons.push_back(v1);
		}

		// Add the polygons to 
		texMesh.tex_polygons.push_back(polygons);

		if (texMesh.cloud.data.empty()) {
			LERROR("Input point cloud has no data!");
		}
		///////////////////////////////////UV///////////////////////////////////////////

		typename pcl::PointCloud<pcl::PointXYZ>::Ptr originalCloud(new pcl::PointCloud<pcl::PointXYZ>);

		// Convert mesh's cloud to pcl format for easier access
		pcl::fromPCLPointCloud2(texMesh.cloud, *originalCloud);
		// Texture coordinates
		std::vector<std::vector<Eigen::Vector2f, Eigen::aligned_allocator<Eigen::Vector2f> > > texture_map;
		int nrOfOutside = 0;

		// Vector of texture coordinates for each face
		std::vector<Eigen::Vector2f, Eigen::aligned_allocator<Eigen::Vector2f> > texture_map_tmp;

		int nrOfFound = 0;
		int nrOfClosest = 0;

		pcl::PointNormal pt;
		size_t idx;
		float maxMinDist = 0.0f;

		typename pcl::PointCloud<pcl::PointNormal>::Ptr uvCloudTransformed(new pcl::PointCloud<pcl::PointNormal>);
		typename pcl::PointCloud<pcl::PointNormal>::Ptr uvCloudTransformed2(new pcl::PointCloud<pcl::PointNormal>);

		pcl::transformPointCloud(*uvCloud, *uvCloudTransformed2, rotation);
		pcl::transformPointCloud(*uvCloudTransformed2, *uvCloudTransformed, rotationMatrix);
		
		pcl::search::KdTree<pcl::PointNormal>::Ptr tree5(new pcl::search::KdTree<pcl::PointNormal>);
		tree5->setInputCloud(uvCloudTransformed);
		int K = 1;
		std::vector<int> pointIdxNKNSearch(K);
		std::vector<float> pointNKNSquaredDistance(K);

		float minDist = 10000.0f;
		float maxDist = 0.0f;
		LINFO("BEFORE UV POINT POSITIONING");
		
		glm::quat q3(mInfo._roverQuat.at(0), mInfo._roverQuat.at(1), mInfo._roverQuat.at(2), mInfo._roverQuat.at(3));
		
		glm::dmat4 rot = glm::dmat4(glm::toMat4(q3));

		glm::dvec3 angle = glm::dvec3(0, M_PI, -M_PI / 2.0);

		glm::dmat4 unitMat4(1);

		glm::dmat4 rotX = glm::rotate(unitMat4, angle.x, glm::dvec3(1, 0, 0));
		glm::dmat4 rotY = glm::rotate(unitMat4, angle.y, glm::dvec3(0, 1, 0));
		glm::dmat4 rotZ = glm::rotate(unitMat4, angle.z, glm::dvec3(0, 0, 1));

		glm::dmat4 debugModelRotation = rotX * rotY * rotZ;

		glm::dvec3 center = debugModelRotation * rot * glm::dvec4(mInfo._cameraCenter, 1);
		glm::dvec3 axis = debugModelRotation * rot * glm::dvec4(mInfo._cameraAxis, 1);
		glm::dvec3 horizontal = debugModelRotation * rot * glm::dvec4(mInfo._cameraHorizontal, 1);
		glm::dvec3 vector = debugModelRotation * rot * glm::dvec4(mInfo._cameraVector, 1);
		
		LERROR("ROTATIONMATRIX TO ROVERSPACE : ");
		glm::dmat4 totRot = debugModelRotation * rot;
		//LERROR(totRot[0][0] << ", " << totRot[1][0] << ", " << totRot[2][0], << ", " << totRot[3][0]);
		//LERROR(totRot[0][1] << ", " << totRot[1][1] << ", " << totRot[2][1], << ", " << totRot[3][1]);
		//LERROR(totRot[0][2] << ", " << totRot[1][2] << ", " << totRot[2][2], << ", " << totRot[3][2]);
		//LERROR(totRot[0][3] << ", " << totRot[1][3] << ", " << totRot[2][3], << ", " << totRot[3][3]);
		
		//LERROR("ROTATED: ");
		//LERROR(mInfo._cameraCenter.x << ", " << mInfo._cameraCenter.y << ", " << mInfo._cameraCenter.z);
		//LERROR(mInfo._cameraAxis.x << ", " << mInfo._cameraAxis.y << ", " << mInfo._cameraAxis.z);
		//LERROR(mInfo._cameraHorizontal.x << ", " << mInfo._cameraHorizontal.y << ", " << mInfo._cameraHorizontal.z);
		//LERROR(mInfo._cameraVector.x << ", " << mInfo._cameraVector.y << ", " << mInfo._cameraVector.z);


		glm::dvec3 center2 = debugModelRotation * rot * glm::dvec4(0.9151621, 0.6070837, -1.969376, 1);
		glm::dvec3 axis2 = debugModelRotation * rot * glm::dvec4(0.4535986, -0.7205702, 0.5244301, 1);
		glm::dvec3 horizontal2 = debugModelRotation * rot * glm::dvec4(11893.36, 6845.262, 402.5473, 1);
		glm::dvec3 vector2 = debugModelRotation * rot * glm::dvec4(-3605.031, 5617.956, 11992.82, 1);

		//LERROR(center2.x << ", " << center2.y << ", " << center2.z);
		//LERROR(axis2.x << ", " << axis2.y << ", " << axis2.z);
		//LERROR(horizontal2.x << ", " << horizontal2.y << ", " << horizontal2.z);
		//LERROR(vector2.x << ", " << vector2.y << ", " << vector2.z);


		//With x = y, y = x, z = -z 
		//glm::dvec3 center = glm::dvec3(0.324755, 0.80963, 1.8358);
		//glm::dvec3 axis = glm::dvec3(-0.239133, 0.562503, -0.791447);
		//glm::dvec3 horizontal = glm::dvec3(1006.26, 763.497, -401.87);
		//glm::dvec3 vector = glm::dvec3(258.049, -609.992, -1146.34);

		for (size_t i = 0; i < texMesh.tex_polygons[0].size(); ++i) {
			Eigen::Vector2f tmp_VT;
			// For each point of this face
			for (size_t j = 0; j < texMesh.tex_polygons[0][i].vertices.size(); ++j) {

				// Get point in scene
				idx = texMesh.tex_polygons[0][i].vertices[j];
				pt = originalCloudTransformed->points[idx];

				glm::dvec3 meshPoint;
				meshPoint.x = pt.x;
				meshPoint.y = pt.y;
				meshPoint.z = pt.z;

				double floor = glm::dot((meshPoint - center), axis);

				double xRoof = glm::dot((meshPoint - center), horizontal);
				double yRoof = glm::dot((meshPoint - center), vector);

				int x = xRoof / floor;
				int y = yRoof / floor;

				glm::dvec2 tc1 = glm::dvec2((1.0 / mInfo._lines) * x, 1.0 - (1.0 / mInfo._cols) * y);
				
				tmp_VT[0] = tc1.x;
				tmp_VT[1] = tc1.y;

				texture_map_tmp.push_back(tmp_VT);
			}
		}

		/*
		for (size_t i = 0; i < texMesh.tex_polygons[0].size(); ++i)
		{
			Eigen::Vector2f tmp_VT;
			// For each point of this face
			for (size_t j = 0; j < texMesh.tex_polygons[0][i].vertices.size(); ++j)
			{
				// Get point
				idx = texMesh.tex_polygons[0][i].vertices[j];
				pt = originalCloudTransformed->points[idx];

				// Point in mesh cloud
				glm::fvec3 meshPoint;
				meshPoint.x = pt.x;
				meshPoint.y = pt.y;
				meshPoint.z = pt.z;
				int index = 0;
				bool found = false;
				int minIndex = 0;

				pcl::PointNormal test;
				test.x = pt.x;
				test.y = pt.y;
				test.z = pt.z;

				
				if (tree5->nearestKSearch(test, K, pointIdxNKNSearch, pointNKNSquaredDistance) > 0) {
					nrOfFound++;

					// Get the pixel index from the uvCloud
					float indexi = uvCloudTransformed->points[pointIdxNKNSearch[0]].normal_x;
					float indexk = uvCloudTransformed->points[pointIdxNKNSearch[0]].normal_y;
					if (minDist > pointNKNSquaredDistance[0]) {
						minDist = pointNKNSquaredDistance[0];
					}
					if (maxDist < pointNKNSquaredDistance[0]) {
						maxDist = pointNKNSquaredDistance[0];
					}
					
					// Caculate uv for the coordinates
					glm::fvec2 tc1 = glm::fvec2((1.0 / mInfo._lines) * indexk, 1.0 - (1.0 / mInfo._cols) * indexi);
					glm::fvec2 tc2 = glm::fvec2((1.0 / mInfo._lines) * (indexk + 1), 1.0 - (1.0 / mInfo._cols) * indexi);
					glm::fvec2 tc3 = glm::fvec2((1.0 / mInfo._lines) * (indexk + 1), 1.0 - (1.0 / mInfo._cols) * (indexi + 1));
					glm::fvec2 tc4 = glm::fvec2((1.0 / mInfo._lines) * indexk, 1.0 - (1.0 / mInfo._cols) * (indexi + 1));

					// Magical number...
					// If we don't use this there will be an offset in the texture,
					// I believe that this comes from the offset between rover and camera
					// Since it's static and doesn't change from subsite to subsite
					float tempu = .33f + tc1.x;

					// Openspace cannot handle uv values > 1
					tempu = tempu <= 1 ? tempu : tempu - 1;

					tmp_VT[0] = tempu;
					tmp_VT[1] = tc1.y;

					texture_map_tmp.push_back(tmp_VT);

					//tmp_VT[0] = tc2.x;
					//tmp_VT[1] = tc2.y;

					//texture_map_tmp.push_back(tmp_VT);

					//tmp_VT[0] = tc3.x;
					//tmp_VT[1] = tc3.y;

					//texture_map_tmp.push_back(tmp_VT);

					//tmp_VT[0] = tc4.x;
					//tmp_VT[1] = tc4.y;

					//texture_map_tmp.push_back(tmp_VT);

					/*if (nrOfFound < 5) {
						for (size_t i = 0; i < pointIdxNKNSearch.size(); ++i)
							std::cout << "    " << uvCloud->points[pointIdxNKNSearch[i]].x
							<< " " << uvCloud->points[pointIdxNKNSearch[i]].y
							<< " " << uvCloud->points[pointIdxNKNSearch[i]].z
							<< " " << uvCloud->points[pointIdxNKNSearch[i]].normal_x
							<< " " << uvCloud->points[pointIdxNKNSearch[i]].normal_y
							<< " (squared distance: " << pointNKNSquaredDistance[i] << ")" << std::endl;
					}*/
					
				//}
				/* Slow way of finding nearest neighbour
				// Iterate through uv cloud to find the best match
				while (!found && index < uvCloud->size()) {
					glm::fvec3 uvPoint;
					uvPoint.x = uvCloud->at(index).x;
					uvPoint.y = uvCloud->at(index).y;
					uvPoint.z = uvCloud->at(index).z;

					// Calculate the distance between the mesh point and uv point
					float dist = glm::distance(meshPoint, uvPoint);

					// To keep track of the smallest distance found
					// Used if there is exact match
					if (dist < minDist) {
						minDist = dist;
						minIndex = index;
					}

					// If an exact match was found
					// The best case
					if (dist == 0.0) {
						found = true;
						nrOfFound++;

						// Get the pixel index from the uvCloud
						float indexi = uvCloud->at(index).normal_x;
						float indexk = uvCloud->at(index).normal_y;

						// Caculate uv for the coordinates
						glm::fvec2 tc1 = glm::fvec2((1.0 / mInfo._lines) * indexk, 1.0 - (1.0 / mInfo._cols) * indexi);
						glm::fvec2 tc2 = glm::fvec2((1.0 / mInfo._lines) * (indexk + 1), 1.0 - (1.0 / mInfo._cols) * indexi);
						glm::fvec2 tc3 = glm::fvec2((1.0 / mInfo._lines) * (indexk + 1), 1.0 - (1.0 / mInfo._cols) * (indexi + 1));
						glm::fvec2 tc4 = glm::fvec2((1.0 / mInfo._lines) * indexk, 1.0 - (1.0 / mInfo._cols) * (indexi + 1));

						// Magical number...
						float tempu = .33f + tc1.x;

						// Openspace cannot handle uv values > 1
						tempu = tempu <= 1 ? tempu : tempu - 1;

						tmp_VT[0] = tempu;
						tmp_VT[1] = tc1.y;

						texture_map_tmp.push_back(tmp_VT);

						//tmp_VT[0] = tc2.x;
						//tmp_VT[1] = tc2.y;

						//texture_map_tmp.push_back(tmp_VT);

						//tmp_VT[0] = tc3.x;
						//tmp_VT[1] = tc3.y;

						//texture_map_tmp.push_back(tmp_VT);

						//tmp_VT[0] = tc4.x;
						//tmp_VT[1] = tc4.y;

						//texture_map_tmp.push_back(tmp_VT);
					}
					// If there was no exact match found, use the smallest distance found
					if (index == uvCloud->size() - 1 && !found) {
						found = true;
						nrOfClosest++;

						// Get the pixel index from the uvCloud
						float indexi = uvCloud->at(minIndex).normal_x;
						float indexk = uvCloud->at(minIndex).normal_y;

						// To keep track of the maximum error distance
						if (maxMinDist < minDist) {
							maxMinDist = minDist;
						}

						// Caculate uv for the coordinates
						glm::fvec2 tc1 = glm::fvec2((1.0 / mInfo._lines) * indexk, 1.0 - (1.0 / mInfo._cols) * indexi);
						glm::fvec2 tc2 = glm::fvec2((1.0 / mInfo._lines) * (indexk + 1), 1.0 - (1.0 / mInfo._cols) * indexi);
						glm::fvec2 tc3 = glm::fvec2((1.0 / mInfo._lines) * (indexk + 1), 1.0 - (1.0 / mInfo._cols) * (indexi + 1));
						glm::fvec2 tc4 = glm::fvec2((1.0 / mInfo._lines) * indexk, 1.0 - (1.0 / mInfo._cols) * (indexi + 1));

						// Magical number...
						float tempu = .33f + tc1.x;

						// Openspace cannot handle uv values > 1
						tempu = tempu <= 1 ? tempu : tempu - 1;

						tmp_VT[0] = tempu;
						tmp_VT[1] = tc1.y;

						texture_map_tmp.push_back(tmp_VT);

						//tmp_VT[0] = tc2.x;
						//tmp_VT[1] = tc2.y;

						//texture_map_tmp.push_back(tmp_VT);

						//tmp_VT[0] = tc3.x;
						//tmp_VT[1] = tc3.y;

						//texture_map_tmp.push_back(tmp_VT);

						//tmp_VT[0] = tc4.x;
						//tmp_VT[1] = tc4.y;

						//texture_map_tmp.push_back(tmp_VT);

					}

					index++;
				}*/
		
		//	}
		//}
		std::size_t pos = file_name_stripped.find("XYR");
		std::string texture_filename = file_name_stripped.substr(0, pos) + "RAS" + file_name_stripped.substr(pos + 3);

		pcl::TexMaterial tx1;
		tx1.tex_name = "1";
		// texture materials
		std::stringstream tex_name;
		tex_name << "material_" << 0;
		tex_name >> tx1.tex_name;
		tx1.tex_file = correctPath(texture_filename, "D:/textures/") + texture_filename + texture_extension;
		
		tx1.tex_Ka.r = 0.0f;
		tx1.tex_Ka.g = 0.0f;
		tx1.tex_Ka.b = 0.0f;

		tx1.tex_Kd.r = 0.0f;
		tx1.tex_Kd.g = 0.0f;
		tx1.tex_Kd.b = 0.0f;

		tx1.tex_Ks.r = 1.0f;
		tx1.tex_Ks.g = 1.0f;
		tx1.tex_Ks.b = 1.0f;
		tx1.tex_d = 1.0f;
		tx1.tex_Ns = 0.0f;
		tx1.tex_illum = 1;
		texMesh.tex_materials.push_back(tx1);

		// texture coordinates
		texMesh.tex_coordinates.push_back(texture_map_tmp);
				
		MeshWriter::writeObjFile(file_name_stripped, output_path, texMesh);
		
		//writeObjFile(file_name_stripped, output_path, texMesh);
		
		MeshWriter::writeMtlFile(file_name_stripped, output_path, texMesh);

		//writeMtlFile(file_name_stripped, output_path, texMesh);

		//LINFO("FINISHED IN : " << float(clock() - begin_time) / CLOCKS_PER_SEC);

	}

	void MeshGeneration::writeTxtFile(const std::string filename, std::string output_path) {
		std::string txt_path = output_path + "filenames.txt";

		if (FileSys.fileExists(txt_path)) {
			std::ifstream txtFile;
			txtFile.open(txt_path.c_str());
			std::string line;
			while (std::getline(txtFile, line)) {
				if (filename == line) {
					return;
				}
			}
		}

		// Open file and add entry
		std::ofstream fs;
		fs.open(txt_path.c_str(), std::ios_base::app);

		fs << filename << "\n";
		fs.close();
	}

	std::string MeshGeneration::correctPath(const std::string filename, std::string output_path) {
		std::string txt_filename;
		// Create filename without path and extension
		std::string file_name_stripped = filename.substr(filename.find_last_of("_F") + 1, 7);

		std::string site_number_string = "site" + file_name_stripped.substr(0, 3) + "/";
		std::string drive_number_string = "drive" + file_name_stripped.substr(3, 7) + "/";

		return output_path + site_number_string + drive_number_string;

	}
}
}
