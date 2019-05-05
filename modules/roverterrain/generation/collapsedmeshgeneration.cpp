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

#include <modules/roverterrain/generation/collapsedmeshgeneration.h>

#include <modules/roverterrain/filehandler/binaryreader.h>
#include <modules/roverterrain/filehandler/meshwriter.h>

#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <glm/gtx/quaternion.hpp>
#include <ghoul/misc/stringconversion.h>
#include <ghoul/filesystem/file.h>

#include <pcl/common/common.h>
#include <pcl/filters/filter.h>
#include <pcl/filters/random_sample.h>
#include <pcl/surface/mls.h>
#include <pcl/common/transforms.h>
#include <pcl/filters/extract_indices.h>
#include <pcl/filters/passthrough.h>
#include <pcl/filters/voxel_grid.h>
#include <pcl/features/normal_3d_omp.h>
#include <pcl/surface/gp3.h>

/*
#include <pcl/surface/poisson.h>
#include <pcl/surface/simplification_remove_unused_vertices.h>
#include <pcl/filters/statistical_outlier_removal.h>
#include <pcl/surface/marching_cubes_rbf.h>
#include <pcl/surface/vtk_smoothing/vtk_mesh_smoothing_laplacian.h>
*/
#include <fstream>

namespace {
    const std::string _loggerCat = "CollapsedMeshGeneration";
}

namespace openspace {

void CollapsedMeshGeneration::generateMeshFromBinaries(ghoul::Dictionary dictionary) {
    std::string asset;
    dictionary.getValue("_asset", asset);
    std::vector<std::string> filenames;

    std::string outputPath;
    dictionary.getValue("_outputPath", outputPath);

    pcl::PointCloud<pcl::PointXYZ>::Ptr fullSite(new pcl::PointCloud<pcl::PointXYZ>);
    BinaryReader::PointCloudInfo mInfo;
    std::string pathToDriveFolder;
    for (size_t k = 0; k < dictionary.size(); ++ k) {
        if (dictionary.hasKey("binaryfile" + std::to_string(k))) {
            std::string binaryFile = dictionary.value<std::string>("binaryfile" + std::to_string(k));

            pathToDriveFolder = correctPath(binaryFile, outputPath);

            filenames.push_back(binaryFile + ".IMG");
        }
    }


    glm::dmat4 outputMatrix;

    for (auto& binaryFile : filenames) {
        const std::string fullBinaryPath = asset + binaryFile;
        mInfo = BinaryReader::readHeader(fullBinaryPath);

        std::vector<std::vector<float>> xyz = BinaryReader::readBody(fullBinaryPath, mInfo);
        pcl::PointCloud<pcl::PointXYZ>::Ptr pizzaSlice(new pcl::PointCloud<pcl::PointXYZ>);

        CollapsedMeshGeneration::extractCoordinatesFromArray(pizzaSlice, xyz, mInfo);

        // TODO: Add filters
        *fullSite += *pizzaSlice;

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

        outputMatrix = debugModelRotation * rot;

        std::string filenameNoExtension = binaryFile.substr(0, binaryFile.find_last_of("."));

        CollapsedMeshGeneration::writeTxtFile(filenameNoExtension, pathToDriveFolder, mInfo);
    }

    Eigen::Vector4f maxDist;

    pcl::getMaxDistance(*fullSite, Eigen::Vector4f(0, 0, 0, 1), maxDist);
    LINFO("Distance from camera is : " + ghoul::to_string(maxDist.norm()));
    LINFO("Max distance from camera: " + ghoul::to_string(maxDist.x()) + ", " + ghoul::to_string(maxDist.y()) + ", " + ghoul::to_string(maxDist.z()));

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
    LINFO("Points before moving least squares size : " + ghoul::to_string(fullSite->points.size()));
    //mls.process(*movingLeastSquaresCloud);
    LINFO("Points after moving least squares: " + ghoul::to_string(movingLeastSquaresCloud->points.size()));


    // =======================================================================================


    Eigen::Quaternionf q(mInfo._roverQuat.at(0), mInfo._roverQuat.at(1), mInfo._roverQuat.at(2), mInfo._roverQuat.at(3));

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
    pcl::PointXYZ camera;
    camera.x = -0.7;
    camera.y = 0;
    camera.z = 0;

    pcl::search::KdTree<pcl::PointXYZ>::Ptr kdTreeFar(new pcl::search::KdTree<pcl::PointXYZ>);
    kdTreeFar->setInputCloud(normalCloudRotated);

    std::vector<int> indicesRadius;
    std::vector<float> indiciesDistance;

    kdTreeFar->radiusSearch(camera, 50, indicesRadius, indiciesDistance);

    boost::shared_ptr<std::vector<int>> indicesRadiusPtr(new std::vector<int>(indicesRadius));

    pcl::ExtractIndices<pcl::PointXYZ> innerFilter(true); // Initializing with true will allow us to extract the removed indices
    innerFilter.setInputCloud(normalCloudRotated);
    innerFilter.setIndices(indicesRadiusPtr);

    innerFilter.filterDirectly(normalCloudRotated);

    pcl::PointCloud<pcl::PointXYZ>::Ptr xf_cloud(new pcl::PointCloud<pcl::PointXYZ>);
    pcl::PointCloud<pcl::PointXYZ>::Ptr yf_cloud(new pcl::PointCloud<pcl::PointXYZ>);
    pcl::PointCloud<pcl::PointXYZ>::Ptr zf_cloud(new pcl::PointCloud<pcl::PointXYZ>);
    pcl::PassThrough<pcl::PointXYZ> passX;
    passX.setInputCloud(normalCloudRotated);
    passX.setFilterFieldName("x");
    passX.setFilterLimits(-1.5, 1.5);
    passX.setNegative(true);
    passX.filter(*xf_cloud);

    pcl::PassThrough<pcl::PointXYZ> passY;
    passY.setInputCloud(normalCloudRotated);
    passY.setFilterFieldName("y");
    passY.setFilterLimits(-1.5, 1.5);
    passY.setNegative(true);
    passY.filter(*yf_cloud);

    *zf_cloud = *yf_cloud + *xf_cloud;

    pcl::PointCloud<pcl::PointXYZ>::Ptr zf_cloud5(new pcl::PointCloud<pcl::PointXYZ>);

    pcl::PointCloud<pcl::PointXYZ>::Ptr xLargerThanZero(new pcl::PointCloud<pcl::PointXYZ>());
    pcl::PointCloud<pcl::PointXYZ>::Ptr xSmallerThanZero(new pcl::PointCloud<pcl::PointXYZ>());

    pcl::PassThrough<pcl::PointXYZ> passXDivider;
    passXDivider.setInputCloud(zf_cloud);
    passXDivider.setFilterFieldName("x");
    passXDivider.setFilterLimits(0, 70);
    passXDivider.filter(*xLargerThanZero);

    passXDivider.setFilterFieldName("x");
    passXDivider.setFilterLimits(-70, 0);
    passXDivider.filter(*xSmallerThanZero);

    pcl::PointCloud<pcl::PointXYZ>::Ptr xAboveZero(new pcl::PointCloud<pcl::PointXYZ>);
    pcl::PointCloud<pcl::PointXYZ>::Ptr xBelowZero(new pcl::PointCloud<pcl::PointXYZ>);

    pcl::VoxelGrid<pcl::PointXYZ> voxelGridOuter;

    voxelGridOuter.setInputCloud(xLargerThanZero);
    voxelGridOuter.setLeafSize(0.2, 0.2, 0.2);
    voxelGridOuter.filter(*xAboveZero);

    voxelGridOuter.setInputCloud(xSmallerThanZero);
    voxelGridOuter.setLeafSize(0.2, 0.2, 0.2);
    voxelGridOuter.filter(*xBelowZero);

    *zf_cloud5 = *xAboveZero + *xBelowZero;


    pcl::PointCloud<pcl::PointXYZ>::Ptr zf_cloud6(new pcl::PointCloud<pcl::PointXYZ>);


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
    ne.setRadiusSearch(1.0);

    //Viewpoint from rover
    Eigen::Vector4f centroid = Eigen::Vector4f(0.0, 0.0, 2.0, 1);

    ne.setViewPoint(centroid[0], centroid[1], centroid[2]);
    pcl::PointCloud<pcl::Normal>::Ptr cloud_normals(new pcl::PointCloud<pcl::Normal>());
    ne.compute(*cloud_normals);

    pcl::PointCloud<pcl::PointNormal>::Ptr cloud_smoothed_normals(new pcl::PointCloud<pcl::PointNormal>());
    concatenateFields(*zf_cloud6, *cloud_normals, *cloud_smoothed_normals);

    LERROR("AFTER POINTCLOUD NORMALS: " + ghoul::to_string(cloud_smoothed_normals->points.size()));

    pcl::search::KdTree<pcl::PointNormal>::Ptr kdTree(new pcl::search::KdTree<pcl::PointNormal>);
    kdTree->setInputCloud(cloud_smoothed_normals);

    // Greedy projection triangulation algorithm
    pcl::GreedyProjectionTriangulation<pcl::PointNormal> gp3;
    pcl::PolygonMesh triangles;
    // Set the maximum distance between connected points (maximum edge length)
    gp3.setSearchRadius(2.0);

    //HIRES
    //gp3.setSearchRadius(0.35);
    gp3.setMu(2.5);
    gp3.setMaximumNearestNeighbors(750);
    gp3.setMaximumSurfaceAngle(M_PI + M_PI_2); // 270 degrees
    gp3.setMinimumAngle(0); // 0 degrees
    gp3.setMaximumAngle(M_PI + M_PI_2); // 270 degrees
    gp3.setNormalConsistency(true);
    gp3.setConsistentVertexOrdering(true);

    gp3.setInputCloud(cloud_smoothed_normals);
    gp3.setSearchMethod(kdTree);

    gp3.reconstruct(triangles);

    LINFO("Number of triangles in mesh: " + ghoul::to_string(triangles.polygons.size()));
    std::string outputFile = "OBJ";
    MeshWriter::writeObjFileNoTex(outputFile, pathToDriveFolder, triangles);
}

void CollapsedMeshGeneration::extractCoordinatesFromArray(pcl::PointCloud<pcl::PointXYZ>::Ptr inputCloud, std::vector<std::vector<float>> xyz, BinaryReader::PointCloudInfo mInfo) {

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

void CollapsedMeshGeneration::writeTxtFile(const std::string filename, std::string output_path, BinaryReader::PointCloudInfo mInfo) {
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


std::string CollapsedMeshGeneration::correctPath(const std::string filename, std::string output_path) {
    std::string txt_filename;
    // Create filename without path and extension
    std::string file_name_stripped = filename.substr(filename.find_last_of("_F") + 1, 7);

    std::string site_number_string = "site" + file_name_stripped.substr(0, 3) + "/";
    std::string drive_number_string = "drive" + file_name_stripped.substr(3, 7) + "/";

    return output_path + site_number_string + drive_number_string;
}

}
