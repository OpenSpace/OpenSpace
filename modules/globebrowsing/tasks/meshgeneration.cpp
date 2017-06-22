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

#include <modules/globebrowsing/tasks/pointcloudfilter.h>

#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>

#include <pcl/point_types.h>
#include <pcl/io/obj_io.h>
#include <pcl/features/integral_image_normal.h>
#include <pcl/filters/voxel_grid.h>
#include <pcl/features/normal_3d.h>
#include <pcl/common/transforms.h>
#include <pcl/surface/gp3.h>
#include <pcl/surface/vtk_smoothing/vtk_mesh_subdivision.h>
#include <pcl/surface/vtk_smoothing/vtk_mesh_quadric_decimation.h>
#include <glm/gtx/quaternion.hpp>
#include <pcl/surface/poisson.h>
#include <pcl/filters/passthrough.h>
#include <pcl/segmentation/region_growing.h>
#include <pcl/filters/fast_bilateral_omp.h>
#include <pcl/filters/random_sample.h>
#include <pcl/filters/normal_space.h>
#include <pcl/filters/median_filter.h>
#include <pcl/surface/mls.h>
#include <pcl/filters/covariance_sampling.h>
#include <vtkDecimatePro.h>
#include <pcl/surface/simplification_remove_unused_vertices.h>
#include <pcl/io/ply_io.h>

#include <pcl/surface/vtk_smoothing/vtk_utils.h>
#include <vtkFillHolesFilter.h>
#include <vtkQuadricDecimation.h>
#include <vtkVersion.h>

#include <fstream>

namespace {
	const std::string _loggerCat = "MeshGeneration";
}

namespace openspace {
namespace globebrowsing {
	void MeshGeneration::generateMeshFromBinary(ghoul::Dictionary dictionary) {
		//const std::string binary_path, std::string output_path

		LDEBUG(vtkVersion::GetVTKVersion());

		std::string binary_path;
		dictionary.getValue("BinaryPath", binary_path);

		std::string output_path;
		dictionary.getValue("OutputPath", output_path);

		ghoul::Dictionary extensions;
		dictionary.getValue("Extension", extensions);

		ghoul::Dictionary generationParams;
		dictionary.getValue("GenerationParams", generationParams);

		std::string texture_extension;
		extensions.getValue("TextureExtension", texture_extension);

		ghoul_assert(!binary_path.empty(), "Filename must not be empty");

		// Create filename without path and extension
		std::string file_name_no_extension = binary_path.substr(0, binary_path.find_last_of("."));
		// Strip path
		std::string file_name_stripped = file_name_no_extension;
		file_name_stripped.erase(0, file_name_no_extension.find_last_of('/') + 1);

		LDEBUG("file: " << file_name_no_extension);

		const clock_t begin_time = clock();

		output_path = correctPath(file_name_stripped, output_path);

		if (!FileSys.directoryExists(output_path)) {
			ghoul::Boolean k(true);
			FileSys.createDirectory(output_path, k);
		}

		ImgReader::PointCloudInfo mInfo = ImgReader::readBinaryHeader(binary_path);

		std::vector<std::vector<float>> xyz;

		ImgReader::readBinaryData(binary_path, xyz, mInfo);
		pcl::PointCloud<pcl::PointXYZ>::Ptr fullCloud(new pcl::PointCloud<pcl::PointXYZ>);

		extractCoordinatesFromArray(fullCloud, xyz, mInfo);

		LINFO("POINTS BEFORE DECIMATION: " << fullCloud->points.size());

		if (fullCloud->points.size() == 0) return;

		
		/////////////////////////////////////////FILTERING//////////////////////////////////////////////
		std::unique_ptr<PointCloudFilter> pcf;
		std::string filterType;
		ghoul::Dictionary filters;
		generationParams.getValue("Filters", filters);

		std::vector<std::unique_ptr<PointCloudFilter>> upSamplingFilters;

		for (size_t i = 0; i < filters.size(); ++i) {
			std::string dictKey = std::to_string(i + 1);
			ghoul::Dictionary filterDic = filters.value<ghoul::Dictionary>(dictKey);
			std::string test;
			filterDic.getValue("Type", test);
			LDEBUG("FILTERS USED: " << test);

			upSamplingFilters.push_back(std::move(PointCloudFilter::createFromDictionary(filterDic)));
		}
		
		pcl::PointCloud<pcl::PointXYZ>::Ptr prevCloud(new pcl::PointCloud<pcl::PointXYZ>());
		prevCloud = fullCloud;

		for (size_t i = 0; i < upSamplingFilters.size(); ++i) {
			pcl::PointCloud<pcl::PointXYZ>::Ptr outputCloud(new pcl::PointCloud<pcl::PointXYZ>());
			upSamplingFilters.at(i)->setInputCloud(prevCloud);
			upSamplingFilters.at(i)->filter(outputCloud);
			prevCloud = nullptr;
			prevCloud = outputCloud;
		}
		
		if (prevCloud->isOrganized()) {
			LINFO(" bfProcessedCloud is ORGANIZED");
		}

		//////////////////////////////////////////NORMAL ESTIMATION///////////////////////////////////////
		pcl::PointCloud<pcl::PointXYZ>::Ptr filteredCloudRotatedNoNan(new pcl::PointCloud<pcl::PointXYZ>);
		pcl::PointCloud<pcl::PointNormal>::Ptr filteredCloudNoNanNormal(new pcl::PointCloud<pcl::PointNormal>);

		std::vector<int> vec;
		//pcl::removeNaNNormalsFromPointCloud(*filteredCloud, *filteredCloudNoNanNormal, vec);
		vec.clear();
		pcl::removeNaNFromPointCloud(*prevCloud, *filteredCloudRotatedNoNan, vec);
				
		// Output has the PointNormal type in order to store the normals calculated by MLS
		pcl::PointCloud<pcl::PointNormal>::Ptr normalCloud(new pcl::PointCloud<pcl::PointNormal>);
		// Init object (second point type is for the normals, even if unused)
		pcl::MovingLeastSquares<pcl::PointXYZ, pcl::PointNormal> mls;
		pcl::search::KdTree<pcl::PointXYZ>::Ptr tree(new pcl::search::KdTree<pcl::PointXYZ>);
		tree->setInputCloud(filteredCloudRotatedNoNan);
		// Set parameters
		mls.setInputCloud(filteredCloudRotatedNoNan);
		mls.setPolynomialFit(true);
		mls.setComputeNormals(true);
		mls.setSearchMethod(tree);
		mls.setSearchRadius(0.3);
		
		mls.setUpsamplingMethod(mls.SAMPLE_LOCAL_PLANE);
		mls.setUpsamplingRadius(0.1);
		mls.setUpsamplingStepSize(0.05);

		//mls.setUpsamplingMethod(mls.VOXEL_GRID_DILATION);
		//mls.setDilationIterations(5);
		//mls.setDilationVoxelSize(0.01f);
		// Reconstruct
		LINFO("before normalcloud size : " << prevCloud->points.size());
		mls.process(*normalCloud);

		LINFO("POINTS AFTER NORMAL ESTIMATION: " << normalCloud->points.size());

		Eigen::Quaternionf q(mInfo._roverQuat.at(0), mInfo._roverQuat.at(1), mInfo._roverQuat.at(2), mInfo._roverQuat.at(3));

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
		voxelGridded.setLeafSize(0.05,0.05,0.01);
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

		LERROR("GP3, before vtk dec, nr of triangles: " << triangles.polygons.size());

		/*vtkSmartPointer<vtkPolyData> input;
		pcl::VTKUtils::mesh2vtk(triangles, input);
		LERROR("CREATING VTK");
		vtkSmartPointer<vtkFillHolesFilter> fillHolesFilter = vtkSmartPointer<vtkFillHolesFilter>::New();
		LERROR(input->GetNumberOfPoints());
		if (input->GetNumberOfPoints() <= 0) {
			LERROR("NOT ENOUGH DATA BEFORE FILLING HOLES");
			return;
		}
		fillHolesFilter->SetInputData(input);
		fillHolesFilter->SetHoleSize(1000);
		LERROR("I CRASHED");

		fillHolesFilter->Update();
		
		LERROR("I FAILED");
		vtkSmartPointer<vtkPolyData> polyData = fillHolesFilter->GetOutput();
		*/
		

		/*if (polyData->GetNumberOfPoints() > 0) {
			LERROR("HERE I AM " << polyData->GetNumberOfPoints());
			vtkSmartPointer<vtkDecimatePro> decimate = vtkSmartPointer<vtkDecimatePro>::New();
			decimate->SetInputData(polyData);
			decimate->SetTargetReduction(0.60);
			LERROR("BEFORE UPDATE");
			decimate->Update();
			LERROR("AFTER UPDATE");
			vtkSmartPointer<vtkPolyData> decimated =
				vtkSmartPointer<vtkPolyData>::New();
			decimated->ShallowCopy(decimate->GetOutput());
			pcl::VTKUtils::vtk2mesh(decimated, triangles);
		}
		else {
			LERROR("COULD NOT CREATE OBJ FOR THIS, NO DATA TO DECIMATE");
			return;
		}
		*/
		LERROR("TRIANGLES AFTER: " << triangles.polygons.size());

		pcl::PolygonMesh trianglesDecimated;

		pcl::surface::SimplificationRemoveUnusedVertices sruv;
		sruv.simplify(triangles, trianglesDecimated);

		LERROR("REMOVE UNUSED: " << trianglesDecimated.polygons.size());

		if (trianglesDecimated.polygons.size() < 10) {
			LERROR("THE NUMBER OF POLYGONS ARE TO FEW " << triangles.polygons.size());
			return;
		}

		///////////////////////////////////UV///////////////////////////////////////
		// Texture coordinates
		std::vector<std::vector<Eigen::Vector2f, Eigen::aligned_allocator<Eigen::Vector2f> > > texture_map;
		int nrOfOutside = 0;

		// Vector of texture coordinates for each face
		std::vector<Eigen::Vector2f, Eigen::aligned_allocator<Eigen::Vector2f> > texture_map_tmp;

		int nrOfFound = 0;
		int nrOfClosest = 0;

		pcl::PointXYZ pt;
		size_t idx;
		float maxMinDist = 0.0f;
		
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
		

		mInfo._cameraCenter = center;
		mInfo._cameraAxis = axis;
		mInfo._cameraHorizontal = horizontal;
		mInfo._cameraVector = vector;

		glm::dmat4 totRot = debugModelRotation * rot;
		
		//glm::dvec3 center2 = debugModelRotation * rot * glm::dvec4(0.9151621, 0.6070837, -1.969376, 1);
		//glm::dvec3 axis2 = debugModelRotation * rot * glm::dvec4(0.4535986, -0.7205702, 0.5244301, 1);
		//glm::dvec3 horizontal2 = debugModelRotation * rot * glm::dvec4(11893.36, 6845.262, 402.5473, 1);
		//glm::dvec3 vector2 = debugModelRotation * rot * glm::dvec4(-3605.031, 5617.956, 11992.82, 1);

		//LERROR(center2.x << ", " << center2.y << ", " << center2.z);
		//LERROR(axis2.x << ", " << axis2.y << ", " << axis2.z);
		//LERROR(horizontal2.x << ", " << horizontal2.y << ", " << horizontal2.z);
		//LERROR(vector2.x << ", " << vector2.y << ", " << vector2.z);


		//With x = y, y = x, z = -z 
		//glm::dvec3 center = glm::dvec3(0.324755, 0.80963, 1.8358);
		//glm::dvec3 axis = glm::dvec3(-0.239133, 0.562503, -0.791447);
		//glm::dvec3 horizontal = glm::dvec3(1006.26, 763.497, -401.87);
		//glm::dvec3 vector = glm::dvec3(258.049, -609.992, -1146.34);

		// Create mesh from point cloud
		// TODO: Make this into TextureMeshPtr
		pcl::TextureMesh texMesh;

		std::vector<pcl::Vertices> polygons;

		texMesh.header = trianglesDecimated.header;
		texMesh.cloud = trianglesDecimated.cloud;

		pcl::PointCloud<pcl::PointXYZ>::Ptr finishedPointCloud(new pcl::PointCloud<pcl::PointXYZ>);

		pcl::fromPCLPointCloud2(trianglesDecimated.cloud, *finishedPointCloud);
		
		// Move all the polygons from the pointcloud to mesh
		for (size_t i = 0; i < trianglesDecimated.polygons.size(); i++) {
			polygons.push_back(trianglesDecimated.polygons.at(i));
		}
		texMesh.tex_polygons.push_back(polygons);
		
		if (texMesh.cloud.data.empty()) {
			LERROR("Input point cloud has no data!");
		}
		
		for (size_t i = 0; i < texMesh.tex_polygons[0].size(); ++i) {
			Eigen::Vector2f tmp_VT;
			// For each point of this face
			for (size_t j = 0; j < texMesh.tex_polygons[0][i].vertices.size(); ++j) {

				// Get point in scene
				idx = texMesh.tex_polygons[0][i].vertices[j];
				pt = finishedPointCloud->points[idx];
				
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

				if (tc1.x > 0 || tc1.y > 0 || tc1.x < 1 || tc1.y < 1) {
					texture_map_tmp.push_back(tmp_VT);
				}

			}
		}
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
		
		//MeshWriter::writeObjFile(file_name_stripped, output_path, texMesh);
		
		MeshWriter::writeObjFileNoTex(file_name_stripped, output_path, trianglesDecimated);
		
		//writeObjFile(file_name_stripped, output_path, texMesh);
		
		MeshWriter::writeMtlFile(file_name_stripped, output_path, texMesh);


		writeTxtFile(file_name_stripped, output_path, mInfo);

		//writeMtlFile(file_name_stripped, output_path, texMesh);

		//LINFO("FINISHED IN : " << float(clock() - begin_time) / CLOCKS_PER_SEC);

	}

	void MeshGeneration::writeTxtFile(const std::string filename, std::string output_path, ImgReader::PointCloudInfo mInfo) {
		std::string txt_path = output_path + "filenames.txt";
		
		if (FileSys.fileExists(txt_path)) {
			// If file already exists
			std::ifstream txtFile;
			txtFile.open(txt_path.c_str());
			std::string line;
			while (std::getline(txtFile, line)) {
				if (filename == line) {
					// If the entry is already in txt file
					return;
				}
			}
		}

		// Open file and add entry
		std::ofstream fs;
		fs.open(txt_path.c_str(), std::ios_base::app);

		fs << filename << "\n";
		fs << mInfo._cameraCenter.x << ", " << mInfo._cameraCenter.y << ", " << mInfo._cameraCenter.z << "\n";
		fs << mInfo._cameraAxis.x << ", " << mInfo._cameraAxis.y << ", " << mInfo._cameraAxis.z << "\n";
		fs << mInfo._cameraHorizontal.x << ", " << mInfo._cameraHorizontal.y << ", " << mInfo._cameraHorizontal.z << "\n";
		fs << mInfo._cameraVector.x << ", " << mInfo._cameraVector.y << ", " << mInfo._cameraVector.z << "\n";
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

	void MeshGeneration::extractCoordinatesFromArray(pcl::PointCloud<pcl::PointXYZ>::Ptr inputCloud, std::vector<std::vector<float>> xyz, ImgReader::PointCloudInfo mInfo) {

		int uvTeller = 0;

		inputCloud->width = mInfo._cols;
		inputCloud->height = mInfo._lines;

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
				if (x == 0.0 && y == 0.0 && z == 0.0) {
					float f_nan = std::numeric_limits<float>::quiet_NaN();

					depthPoint.x = f_nan;
					depthPoint.y = f_nan;
					depthPoint.z = f_nan;

				}
				inputCloud->points.push_back(depthPoint);

				uvTeller++;
			}
		}

		if (!inputCloud->isOrganized()) {
			LERROR("THE INPUTCOUD IS NOT ORGANIZED");
		}

	}

}
}
