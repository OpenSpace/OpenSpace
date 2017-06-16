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
#include <modules/globebrowsing/tasks/pointcloudfilter.h>
#include <modules/globebrowsing/tasks/randompointcloudfilter.h>
#include <modules/globebrowsing/tasks/levelofdetail.h>

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
#include <pcl/filters/statistical_outlier_removal.h>
#include <pcl/features/normal_3d_omp.h>
#include <pcl/filters/passthrough.h>
#include <pcl/filters/extract_indices.h>

#include <fstream>

namespace {
	const std::string _loggerCat = "MeshGeneration";
}

namespace openspace {
	namespace globebrowsing {
		void CollapsedMeshGeneration::generateMeshFromBinary(ghoul::Dictionary dictionary) {
			//const std::string pathToBinaryFile, std::string output_path

			std::string outputFile;
			dictionary.getValue("OutputFile", outputFile);
			outputFile = "obj";
			std::string pathToModelsFolder;
			dictionary.getValue("OutputPath", pathToModelsFolder);
			ImgReader::PointCloudInfo mInfo2;
			std::string pathToBinaryFolder;
			dictionary.getValue("BinaryPath", pathToBinaryFolder);
			std::string level;
			dictionary.getValue("Level", level);

			std::unordered_map<std::string, LevelOfDetail>::const_iterator levelOfDetailVars = _levelOfDetailMap.find(level);

			LevelOfDetail levelOfDetail;

			if (levelOfDetailVars == _levelOfDetailMap.end()) {
				LINFO("USING LOWEST LEVEL OF DETAIL SINCE NOT SPECIFIED, OR NO EXISTING PREDEFINED FOUND");
				level = "level1";
				std::unordered_map<std::string, LevelOfDetail>::const_iterator levelOfDetail1 = _levelOfDetailMap.find(level);
				levelOfDetail = levelOfDetail1->second;
			}
			else {
				levelOfDetail = levelOfDetailVars->second;
			}

			ghoul::Dictionary filters;
			dictionary.getValue("Filters.Filters", filters);

			std::vector<std::unique_ptr<PointCloudFilter>> upSamplingFilters;
			LINFO("FILTERS USED: ");
			for (size_t i = 0; i < filters.size(); ++i) {
				std::string dictKey = std::to_string(i + 1);
				ghoul::Dictionary filterDic = filters.value<ghoul::Dictionary>(dictKey);
				std::string filtersUsed;
				filterDic.getValue("Type", filtersUsed);
				LINFO(filtersUsed);

				upSamplingFilters.push_back(std::move(PointCloudFilter::createFromDictionary(filterDic)));
			}
			
			std::string pathToDriveFolder;
			pcl::PointCloud<pcl::PointXYZ>::Ptr fullSite(new pcl::PointCloud<pcl::PointXYZ>);

			std::vector<pcl::PointCloud<pcl::PointXYZ>::Ptr> tempVec;
			glm::dmat4 outputMatrix;
			
			for (size_t i = 0; i < dictionary.size(); ++i) {
				std::string pathToBinaryFile;

				dictionary.getValue("PointCloud" + std::to_string(i), pathToBinaryFile);
				if (pathToBinaryFile == "") continue;
				pathToBinaryFile += ".IMG";
				ghoul_assert(!pathToBinaryFile.empty(), "Filename must not be empty");

				// Create filename without path and extension
				std::string file_name_no_extension = pathToBinaryFile.substr(0, pathToBinaryFile.find_last_of("."));
				// Strip path
				std::string file_name_stripped = file_name_no_extension;
				file_name_stripped.erase(0, file_name_no_extension.find_last_of('/') + 1);

				const clock_t begin_time = clock();
				pathToDriveFolder = correctPath(file_name_stripped, pathToModelsFolder + level + "/");
				//outputFile = file_name_stripped;

				if (!FileSys.directoryExists(pathToDriveFolder)) {
					ghoul::Boolean k(true);
					FileSys.createDirectory(pathToDriveFolder, k);
				}
				else if (FileSys.fileExists(pathToDriveFolder + outputFile + ".obj")) {
					
					LERROR("FILE ALREADY EXISTS " << pathToDriveFolder + outputFile + ".obj");
					return;
				}
				ImgReader::PointCloudInfo mInfo = ImgReader::readBinaryHeader(pathToBinaryFolder + pathToBinaryFile);
				mInfo2 = mInfo;
				std::vector<std::vector<float>> xyz;

				ImgReader::readBinaryData(pathToBinaryFolder + pathToBinaryFile, xyz, mInfo);

				pcl::PointCloud<pcl::PointXYZ>::Ptr fullCloudFiltered(new pcl::PointCloud<pcl::PointXYZ>);

				pcl::PointCloud<pcl::PointXYZ>::Ptr fullCloud(new pcl::PointCloud<pcl::PointXYZ>);

				MeshGeneration::extractCoordinatesFromArray(fullCloud, xyz, mInfo);
			
				tempVec.push_back(fullCloud);

				pcl::PointCloud<pcl::PointXYZ>::Ptr prevCloud(new pcl::PointCloud<pcl::PointXYZ>());
				prevCloud = fullCloud;
				
				for (size_t i = 0; i < upSamplingFilters.size(); ++i) {
					pcl::PointCloud<pcl::PointXYZ>::Ptr outputCloud(new pcl::PointCloud<pcl::PointXYZ>());
					upSamplingFilters.at(i)->setInputCloud(prevCloud);
					upSamplingFilters.at(i)->filter(outputCloud);
					prevCloud = nullptr;
					prevCloud = outputCloud;
				}

				*fullSite += *prevCloud;

				glm::quat q3(mInfo._roverQuat.at(0), mInfo._roverQuat.at(1), mInfo._roverQuat.at(2), mInfo._roverQuat.at(3));

				glm::dmat4 rot = glm::dmat4(glm::toMat4(q3));

				glm::dvec3 angle = glm::dvec3(0, M_PI, M_PI / 2.0);

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

				outputMatrix = debugModelRotation * rot;

				MeshGeneration::writeTxtFile(file_name_stripped, pathToDriveFolder, mInfo);
			}

			writeMatrixFile(pathToDriveFolder, outputMatrix);
			
			std::vector<int> vec;
			vec.clear();

			pcl::PointCloud<pcl::PointXYZ>::Ptr filteredCloudRotatedNoNan(new pcl::PointCloud<pcl::PointXYZ>);

			pcl::removeNaNFromPointCloud(*fullSite, *filteredCloudRotatedNoNan, vec);
			
			// Output has the PointNormal type in order to store the normals calculated by MLS
			pcl::PointCloud<pcl::PointXYZ>::Ptr movingLeastSquaresCloud(new pcl::PointCloud<pcl::PointXYZ>);
			// Init object (second point type is for the normals, even if unused)
			pcl::MovingLeastSquares<pcl::PointXYZ, pcl::PointXYZ> mls;
			pcl::search::KdTree<pcl::PointXYZ>::Ptr tree(new pcl::search::KdTree<pcl::PointXYZ>);
			tree->setInputCloud(fullSite);
			// Set parameters
			mls.setInputCloud(fullSite);
			mls.setPolynomialFit(true);
			mls.setPolynomialOrder(2);
			mls.setSearchMethod(tree);
			mls.setComputeNormals(false);

			//These values creates 'the best upsampling' should not be touched
			mls.setSearchRadius(0.1);
			mls.setUpsamplingMethod(mls.SAMPLE_LOCAL_PLANE);
			mls.setUpsamplingRadius(0.1);
			mls.setUpsamplingStepSize(0.05);
			
			// Reconstruct
			LINFO("Points before moving least squares size : " << fullSite->points.size());
			//mls.process(*movingLeastSquaresCloud);
			LINFO("Points after moving least squares: " << movingLeastSquaresCloud->points.size());
						
			Eigen::Quaternionf q(mInfo2._roverQuat.at(0), mInfo2._roverQuat.at(1), mInfo2._roverQuat.at(2), mInfo2._roverQuat.at(3));

			Eigen::Matrix4f rotation = Eigen::Matrix4f::Identity();

			rotation.block(0, 0, 3, 3) = q.toRotationMatrix();

			typename pcl::PointCloud<pcl::PointXYZ>::Ptr normalCloudTransformed(new pcl::PointCloud<pcl::PointXYZ>);
			typename pcl::PointCloud<pcl::PointXYZ>::Ptr normalCloudRotated(new pcl::PointCloud<pcl::PointXYZ>);

			pcl::transformPointCloud(*fullSite, *normalCloudTransformed, rotation);

			Eigen::AngleAxisf rollAngle(-M_PI / 2.0, Eigen::Vector3f::UnitZ());
			Eigen::AngleAxisf yawAngle(M_PI, Eigen::Vector3f::UnitY());
			Eigen::AngleAxisf pitchAngle(0, Eigen::Vector3f::UnitX());

			Eigen::Quaternionf q2 = pitchAngle * yawAngle * rollAngle;

			Eigen::Matrix4f rotationMatrix = Eigen::Matrix4f::Identity();
			rotationMatrix.block(0, 0, 3, 3) = q2.toRotationMatrix();

			pcl::transformPointCloud(*normalCloudTransformed, *normalCloudRotated, rotationMatrix);


			////////////////////////////////////////////////////////////////////////////
			
			pcl::PointCloud<pcl::PointXYZ>::Ptr xf_cloud(new pcl::PointCloud<pcl::PointXYZ>);
			pcl::PointCloud<pcl::PointXYZ>::Ptr yf_cloud(new pcl::PointCloud<pcl::PointXYZ>);
			pcl::PointCloud<pcl::PointXYZ>::Ptr zf_cloud(new pcl::PointCloud<pcl::PointXYZ>);
			pcl::PassThrough<pcl::PointXYZ> passX;
			passX.setInputCloud(normalCloudRotated);
			passX.setFilterFieldName("x");
			passX.setFilterLimits(-1.5,1.5);
			passX.setNegative(true);
			passX.filter(*xf_cloud);

			pcl::PassThrough<pcl::PointXYZ> passY;
			passY.setInputCloud(normalCloudRotated);
			passY.setFilterFieldName("y");
			passY.setFilterLimits(-1.5, 1.5);
			passY.setNegative(true);
			passY.filter(*yf_cloud);

			*zf_cloud = *yf_cloud + *xf_cloud;

			pcl::search::KdTree<pcl::PointXYZ>::Ptr kdTree2(new pcl::search::KdTree<pcl::PointXYZ>);
			kdTree2->setInputCloud(zf_cloud);
			pcl::PointXYZ camera;
			camera.x = -0.7;
			camera.y = 0;
			camera.z = 0;
			std::vector<int> indicies;
			std::vector<float> indiciesDist;
			kdTree2->radiusSearch(camera,5.0,indicies,indiciesDist);
			boost::shared_ptr<std::vector<int>> indicesptr(new std::vector<int>(indicies));
			LERROR("HAS THIS SIZE CHANGED?" << zf_cloud->size());
			typename pcl::PointCloud<pcl::PointXYZ>::Ptr normalCloudRotated2(new pcl::PointCloud<pcl::PointXYZ>);
			//The smaller one was with 10 i radius
			pcl::copyPointCloud(*zf_cloud, *normalCloudRotated2);

			typename pcl::PointCloud<pcl::PointXYZ>::Ptr normalCloudRotated3(new pcl::PointCloud<pcl::PointXYZ>);
			//The smaller one was with 10 i radius
			pcl::copyPointCloud(*zf_cloud, *normalCloudRotated3);
			
			pcl::PointCloud<pcl::PointXYZ>::Ptr zf_cloud3(new pcl::PointCloud<pcl::PointXYZ>);
			pcl::ExtractIndices<pcl::PointXYZ> eifilter(true); // Initializing with true will allow us to extract the removed indices
			eifilter.setInputCloud(zf_cloud);
			eifilter.setIndices(indicesptr);
			eifilter.filter(*zf_cloud3);
			
			pcl::PointCloud<pcl::PointXYZ>::Ptr voxelGriddedPCLCloud(new pcl::PointCloud<pcl::PointXYZ>());

			pcl::VoxelGrid<pcl::PointXYZ> voxelGridded;

			voxelGridded.setDownsampleAllData(true);
			voxelGridded.setInputCloud(zf_cloud3);

			voxelGridded.setLeafSize(levelOfDetail.voxelGridFilterLeafSize.x, levelOfDetail.voxelGridFilterLeafSize.y, levelOfDetail.voxelGridFilterLeafSize.z);
			voxelGridded.filter(*voxelGriddedPCLCloud);

			pcl::PointCloud<pcl::PointXYZ>::Ptr zf_cloud4(new pcl::PointCloud<pcl::PointXYZ>);
			pcl::IndicesConstPtr test = eifilter.getRemovedIndices();

			LERROR("NUMBER OF REMOVED: " << test->size());
			LERROR("NUMBER KEPT: " << indicesptr->size());
			
			pcl::search::KdTree<pcl::PointXYZ>::Ptr kdTree3(new pcl::search::KdTree<pcl::PointXYZ>);
			kdTree3->setInputCloud(normalCloudRotated3);

			std::vector<int> indicies2;
			std::vector<float> indiciesDist2;
			kdTree3->radiusSearch(camera, 7.0, indicies2, indiciesDist2);
			boost::shared_ptr<std::vector<int>> indicesptr2(new std::vector<int>(indicies2));
			boost::shared_ptr<std::vector<int>> indicesptr3(new std::vector<int>());

			//indicesptr2->insert(indicesptr2->begin(), indicesptr->begin(), indicesptr->end());

			//std::sort(indicesptr2->begin(), indicesptr2->end());
			//indicesptr2->erase(std::unique(indicesptr2->begin(), indicesptr2->end()), indicesptr2->end());
			
			
			std::sort(indicesptr2->begin(), indicesptr2->end());
			std::sort(indicesptr->begin(), indicesptr->end());

			auto pred = [&indicesptr](const int& key) ->bool
			{
				return std::find(indicesptr->begin(), indicesptr->end(), key) != indicesptr->end();
			};

			indicesptr2->erase(std::remove_if(indicesptr2->begin(), indicesptr2->end(), pred), indicesptr2->end());
			

			LERROR("THIS SIZE: " << indicesptr2->size());
			/*for (int k = 0; k < indicesptr2->size(); ++k) {
				
				if (std::find(indicesptr->begin(), indicesptr->end(), indicesptr2->at(k)) == indicesptr->end()) {
					indicesptr3->push_back(indicesptr2->at(k));
				}
			}
			LERROR("THIS SIZE: " << indicesptr3->size());*/
			pcl::ExtractIndices<pcl::PointXYZ> eifilter3(true); // Initializing with true will allow us to extract the removed indices
			eifilter3.setInputCloud(normalCloudRotated3);

			eifilter3.setIndices(indicesptr2);
			eifilter3.filter(*zf_cloud4);

			pcl::PointCloud<pcl::PointXYZ>::Ptr voxelGriddedPCLCloud3(new pcl::PointCloud<pcl::PointXYZ>());

			pcl::VoxelGrid<pcl::PointXYZ> voxelGridded3;

			voxelGridded3.setDownsampleAllData(true);
			voxelGridded3.setInputCloud(zf_cloud4);
			double voxelGriddedLeafSize2 = 0.07;
			voxelGridded3.setLeafSize(voxelGriddedLeafSize2, voxelGriddedLeafSize2, voxelGriddedLeafSize2);
			voxelGridded3.filter(*voxelGriddedPCLCloud3);

			pcl::PointCloud<pcl::PointXYZ>::Ptr zf_cloud9(new pcl::PointCloud<pcl::PointXYZ>);

			pcl::IndicesConstPtr test2 = eifilter3.getRemovedIndices();

			/*boost::shared_ptr<std::vector<int>> indicesptr4(new std::vector<int>());
			pcl::IndicesPtr testare = pcl::IndicesPtr();// = test2 + test;

			testare->insert(testare->begin(), test2->begin(), test2->end());
			testare->insert(testare->begin(), test->begin(), test->end());
			
			std::sort(testare->begin(), testare->end());
			testare->erase(unique(testare->begin(), testare->end()), testare->end());
			
			LERROR("THEIR SIZE: " << test2->size());
			LERROR("THEIR SIZE: " << test->size());
			*/
			/*for (int k = 0; k < test2->size(); ++k) {
				indicesptr4->push_back(test2->at(k));
			}
			for (int k = 0; k < test->size(); ++k) {

				if (std::find(indicesptr4->begin(), indicesptr4->end(), test->at(k)) == indicesptr4->end()) {
					indicesptr4->push_back(test->at(k));
				}
			}*/


			pcl::ExtractIndices<pcl::PointXYZ> eifilter2(true); // Initializing with true will allow us to extract the removed indices
			eifilter2.setInputCloud(normalCloudRotated2);

			LERROR("HAS THIS SIZE CHANGED?" << normalCloudRotated2->size());

			eifilter2.setIndices(test);
			eifilter2.filter(*zf_cloud9);

			pcl::PointCloud<pcl::PointXYZ>::Ptr voxelGriddedPCLCloud2(new pcl::PointCloud<pcl::PointXYZ>());

			pcl::VoxelGrid<pcl::PointXYZ> voxelGridded2;

			voxelGridded2.setDownsampleAllData(true);
			voxelGridded2.setInputCloud(zf_cloud9);
			double voxelGriddedLeafSize = 0.15;
			voxelGridded2.setLeafSize(voxelGriddedLeafSize,voxelGriddedLeafSize,voxelGriddedLeafSize);
			voxelGridded2.filter(*voxelGriddedPCLCloud2);

			LERROR("ARE THEY THE SAME SIZE? : " << zf_cloud3->size());
			LERROR("ARE THEY THE SAME SIZE? : " << zf_cloud4->size());


			pcl::PointCloud<pcl::PointXYZ>::Ptr zf_cloud5(new pcl::PointCloud<pcl::PointXYZ>);
			LERROR("EACH ONE'S SIZE: ");
			*zf_cloud5 = *voxelGriddedPCLCloud;// +*voxelGriddedPCLCloud2;
			*zf_cloud5 += *voxelGriddedPCLCloud3;
			*zf_cloud5 += *voxelGriddedPCLCloud2;
			LERROR(zf_cloud5->size());

			pcl::PointCloud<pcl::PointXYZ>::Ptr zf_cloud6(new pcl::PointCloud<pcl::PointXYZ>);
			LERROR("SIZE: ");
			for (size_t i = 0; i < upSamplingFilters.size(); ++i) {

				LERROR(zf_cloud5->size());
				pcl::PointCloud<pcl::PointXYZ>::Ptr outputCloud(new pcl::PointCloud<pcl::PointXYZ>());
				//upSamplingFilters.at(i)->setInputCloud(zf_cloud5);
				//upSamplingFilters.at(i)->filter(outputCloud);
				//zf_cloud5 = nullptr;
				LERROR(outputCloud->size());
				//zf_cloud5 = outputCloud;

			}

			pcl::RandomSample<pcl::PointXYZ> _rFilter;

			_rFilter.setInputCloud(zf_cloud5);
			_rFilter.setSeed(10000);
			_rFilter.filter(*zf_cloud6);
			

			/*
			pcl::PointCloud<pcl::PointXYZ>::Ptr voxelGriddedPCLCloud(new pcl::PointCloud<pcl::PointXYZ>());

			pcl::VoxelGrid<pcl::PointXYZ> voxelGridded;

			voxelGridded.setDownsampleAllData(true);
			voxelGridded.setInputCloud(normalCloudRotated);

			voxelGridded.setLeafSize(0.00000001, 0.00000001, 0.00000001);
			voxelGridded.filter(*voxelGriddedPCLCloud);
			*/

			//Normal
			pcl::NormalEstimationOMP<pcl::PointXYZ, pcl::Normal> ne;
			ne.setNumberOfThreads(8);
			ne.setInputCloud(zf_cloud6);
			ne.setRadiusSearch(0.01);
			
			//Viewpoint from rover
			Eigen::Vector4f centroid = Eigen::Vector4f(0, 0, -2.0, 1);

			ne.setViewPoint(centroid[0], centroid[1], centroid[2]);
			pcl::PointCloud<pcl::Normal>::Ptr cloud_normals(new pcl::PointCloud<pcl::Normal>());
			ne.compute(*cloud_normals);
			for (size_t i = 0; i < cloud_normals->size(); ++i)
			{
				cloud_normals->points[i].normal_x *= -1;
				cloud_normals->points[i].normal_y *= -1;
				cloud_normals->points[i].normal_z *= -1;
			}
			pcl::PointCloud<pcl::PointNormal>::Ptr cloud_smoothed_normals(new pcl::PointCloud<pcl::PointNormal>());
			concatenateFields(*zf_cloud6, *cloud_normals, *cloud_smoothed_normals);

			LERROR("AFTER POINTCLOUD NORMALS: " << cloud_smoothed_normals->points.size());

			pcl::search::KdTree<pcl::PointNormal>::Ptr kdTree(new pcl::search::KdTree<pcl::PointNormal>);
			kdTree->setInputCloud(cloud_smoothed_normals);
			
			// Greedy projection triangulation algorithm
			pcl::GreedyProjectionTriangulation<pcl::PointNormal> gp3;
			pcl::PolygonMesh triangles;
			LINFO("PointCloud before mesh generation: " << voxelGriddedPCLCloud->size());
			// Set the maximum distance between connected points (maximum edge length)
			gp3.setSearchRadius(levelOfDetail.greedySearchRadius);
			//HIRES
			//gp3.setSearchRadius(0.35);
			gp3.setMu(levelOfDetail.greedyMU);
			gp3.setNormalConsistency(true);
			gp3.setConsistentVertexOrdering(true);
			gp3.setMaximumNearestNeighbors(levelOfDetail.greedyMaxNeighbours);
			gp3.setMaximumSurfaceAngle(M_PI + M_PI_2); // 270 degrees
			gp3.setMinimumAngle(0); // 0 degrees
			gp3.setMaximumAngle(M_PI + M_PI_2); // 270 degrees

			gp3.setInputCloud(cloud_smoothed_normals);
			gp3.setSearchMethod(kdTree);
			gp3.reconstruct(triangles);

			LINFO("Number of triangles in mesh: " << triangles.polygons.size());

			MeshWriter::writeObjFileNoTex(outputFile, pathToDriveFolder, triangles);
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

		void CollapsedMeshGeneration::writeMatrixFile(std::string output_path, glm::dmat4 outputMatrix) {
			std::string matrix_path = output_path + "rotationmatrix.txt";
			
			if (FileSys.fileExists(matrix_path)) {
				return;
			}

			std::ofstream fs;
			fs.open(matrix_path.c_str(), std::ios_base::app);
			for (size_t i = 0; i < 4; ++i) {
				for (size_t j = 0; j < 4; ++j) {
					std::string after = j == 3 ? "\n" : ", ";
					fs << outputMatrix[i][j] << after;
				}
			}
			fs.close();
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

			return output_path + site_number_string + drive_number_string;
		}
	}
}
