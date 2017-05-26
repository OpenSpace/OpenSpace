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

#include <modules/globebrowsing/tasks/collapsedmeshgeneration.h>
#include <modules/globebrowsing/tasks/meshgeneration.h>

#include <modules/globebrowsing/tasks/imgreader.h>
#include <modules/globebrowsing/tasks/meshwriter.h>

#include <ghoul/glm.h>
#include <glm/gtx/quaternion.hpp>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>

#include <pcl/point_types.h>
#include <pcl/io/obj_io.h>
#include <pcl/filters/voxel_grid.h>
#include <pcl/features/normal_3d.h>
#include <pcl/common/transforms.h>
#include <pcl/surface/gp3.h>
#include <pcl/surface/poisson.h>
#include <pcl/surface/mls.h>
#include <pcl/surface/simplification_remove_unused_vertices.h>

#include <fstream>

namespace {
	const std::string _loggerCat = "MeshGeneration";
}

namespace openspace {
	namespace globebrowsing {
		void CollapsedMeshGeneration::generateMeshFromBinary(ghoul::Dictionary dictionary) {
			//const std::string binary_path, std::string output_path

			std::string file_name_stripped2;
			std::string file_name_stripped_obj2;
			std::string output_path2;
			ImgReader::PointCloudInfo mInfo2;

			pcl::PointCloud<pcl::PointXYZ>::Ptr fullSite(new pcl::PointCloud<pcl::PointXYZ>);

			std::vector<pcl::PointCloud<pcl::PointXYZ>::Ptr> tempVec;
			
			for (size_t i = 0; i < dictionary.size(); ++i) {
				ghoul::Dictionary pointCloudsDic;

				dictionary.getValue("PointCloud" + std::to_string(i), pointCloudsDic);

				std::string binary_path;
				pointCloudsDic.getValue("BinaryPath", binary_path);

				std::string output_path;
				pointCloudsDic.getValue("OutputPath", output_path);

				ghoul::Dictionary extensions;
				pointCloudsDic.getValue("Extension", extensions);

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

				file_name_stripped2 = file_name_stripped;
				output_path2 = output_path;

				file_name_stripped_obj2 = "OBJ";


				ImgReader::PointCloudInfo mInfo = ImgReader::readBinaryHeader(binary_path);
				mInfo2 = mInfo;
				std::vector<std::vector<float>> xyz;

				ImgReader::readBinaryData(binary_path, xyz, mInfo);

				pcl::PointCloud<pcl::PointXYZ>::Ptr fullCloud(new pcl::PointCloud<pcl::PointXYZ>);

				MeshGeneration::extractCoordinatesFromArray(fullCloud, xyz, mInfo);
			
				tempVec.push_back(fullCloud);
				
				*fullSite += *fullCloud;

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


				mInfo._cameraCenter = center;
				mInfo._cameraAxis = axis;
				mInfo._cameraHorizontal = horizontal;
				mInfo._cameraVector = vector;

				MeshGeneration::writeTxtFile(file_name_stripped, output_path, mInfo);
			}
			
			std::vector<int> vec;
			vec.clear();

			pcl::PointCloud<pcl::PointXYZ>::Ptr filteredCloudRotatedNoNan(new pcl::PointCloud<pcl::PointXYZ>);

			pcl::removeNaNFromPointCloud(*fullSite, *filteredCloudRotatedNoNan, vec);
			
			// Output has the PointNormal type in order to store the normals calculated by MLS
			pcl::PointCloud<pcl::PointNormal>::Ptr normalCloud(new pcl::PointCloud<pcl::PointNormal>);
			// Init object (second point type is for the normals, even if unused)
			pcl::MovingLeastSquares<pcl::PointXYZ, pcl::PointNormal> mls;
			pcl::search::KdTree<pcl::PointXYZ>::Ptr tree(new pcl::search::KdTree<pcl::PointXYZ>);
			tree->setInputCloud(fullSite);
			// Set parameters
			mls.setInputCloud(fullSite);
			mls.setPolynomialFit(true);
			mls.setComputeNormals(true);
			mls.setSearchMethod(tree);
			mls.setSearchRadius(0.1);

			mls.setUpsamplingMethod(mls.SAMPLE_LOCAL_PLANE);
			mls.setUpsamplingRadius(0.1);
			mls.setUpsamplingStepSize(0.05);

			//mls.setUpsamplingMethod(mls.VOXEL_GRID_DILATION);
			//mls.setDilationIterations(5);
			//mls.setDilationVoxelSize(0.01f);
			// Reconstruct
			LINFO("before normalcloud size : " << fullSite->points.size());
			mls.process(*normalCloud);

			LINFO("POINTS AFTER NORMAL ESTIMATION: " << normalCloud->points.size());

			Eigen::Quaternionf q(mInfo2._roverQuat.at(0), mInfo2._roverQuat.at(1), mInfo2._roverQuat.at(2), mInfo2._roverQuat.at(3));

			Eigen::Matrix4f rotation = Eigen::Matrix4f::Identity();

			rotation.block(0, 0, 3, 3) = q.toRotationMatrix();

			typename pcl::PointCloud<pcl::PointNormal>::Ptr normalCloudTransformed(new pcl::PointCloud<pcl::PointNormal>);
			typename pcl::PointCloud<pcl::PointNormal>::Ptr normalCloudRotated(new pcl::PointCloud<pcl::PointNormal>);

			pcl::transformPointCloud(*normalCloud, *normalCloudTransformed, rotation);

			Eigen::AngleAxisf rollAngle(-M_PI / 2.0, Eigen::Vector3f::UnitZ());
			Eigen::AngleAxisf yawAngle(M_PI, Eigen::Vector3f::UnitY());
			Eigen::AngleAxisf pitchAngle(0, Eigen::Vector3f::UnitX());

			Eigen::Quaternionf q2 = pitchAngle * yawAngle * rollAngle;

			Eigen::Matrix4f rotationMatrix = Eigen::Matrix4f::Identity();
			rotationMatrix.block(0, 0, 3, 3) = q2.toRotationMatrix();

			pcl::transformPointCloud(*normalCloudTransformed, *normalCloudRotated, rotationMatrix);

			pcl::PointCloud<pcl::PointNormal>::Ptr voxelGriddedPCLCloud(new pcl::PointCloud<pcl::PointNormal>());

			pcl::VoxelGrid<pcl::PointNormal> voxelGridded;

			voxelGridded.setDownsampleAllData(true);
			voxelGridded.setInputCloud(normalCloudRotated);
			voxelGridded.setLeafSize(0.09, 0.09, 0.01);
			voxelGridded.filter(*voxelGriddedPCLCloud);

			pcl::search::KdTree<pcl::PointNormal>::Ptr kdTree(new pcl::search::KdTree<pcl::PointNormal>);
			kdTree->setInputCloud(voxelGriddedPCLCloud);

			// Greedy projection triangulation algorithm
			pcl::GreedyProjectionTriangulation<pcl::PointNormal> gp3;
			pcl::PolygonMesh triangles;
			LINFO("RECONSTRUCTING MESH " << voxelGriddedPCLCloud->size());
			// Set the maximum distance between connected points (maximum edge length)
			gp3.setSearchRadius(0.35);
			gp3.setMu(2.5);
			gp3.setNormalConsistency(true);
			gp3.setMaximumNearestNeighbors(750);
			gp3.setMaximumSurfaceAngle(M_PI + M_PI_2); // 270 degrees
			gp3.setMinimumAngle(0); // 0 degrees
			gp3.setMaximumAngle(M_PI + M_PI_2); // 270 degrees

			//gp3.setMaximumSurfaceAngle(M_PI / 4); // 45 degrees
			//gp3.setMinimumAngle(M_PI / 18); // 10 degrees
			//gp3.setMaximumAngle(2 * M_PI / 3); // 120 degrees

			gp3.setNormalConsistency(true);

			gp3.setInputCloud(voxelGriddedPCLCloud);
			gp3.setSearchMethod(kdTree);
			gp3.reconstruct(triangles);

			LERROR("GP3, nr of triangles: " << triangles.polygons.size());

			LERROR("TRIANGLES AFTER: " << triangles.polygons.size());

			pcl::PolygonMesh trianglesDecimated;

			pcl::surface::SimplificationRemoveUnusedVertices sruv;
			sruv.simplify(triangles, trianglesDecimated);

			LERROR("REMOVE UNUSED: " << trianglesDecimated.polygons.size());

			MeshWriter::writeObjFileNoTex(file_name_stripped_obj2, output_path2, trianglesDecimated);
		}

		void CollapsedMeshGeneration::extractCoordinatesFromArray(pcl::PointCloud<pcl::PointXYZ>::Ptr inputCloud, std::vector<std::vector<float>> xyz, ImgReader::PointCloudInfo mInfo) {
			
			int uvTeller = 0;
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
						inputCloud->push_back(depthPoint);
					}

					uvTeller++;
				}
			}

		}

		void CollapsedMeshGeneration::writeTxtFile(const std::string filename, std::string output_path) {
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

		std::string CollapsedMeshGeneration::correctPath(const std::string filename, std::string output_path) {
			std::string txt_filename;
			// Create filename without path and extension
			std::string file_name_stripped = filename.substr(filename.find_last_of("_F") + 1, 7);

			std::string site_number_string = "site" + file_name_stripped.substr(0, 3) + "/";
			std::string drive_number_string = "drive" + file_name_stripped.substr(3, 7) + "/";

			return output_path + "level1/" + site_number_string + drive_number_string;
		}
	}
}
