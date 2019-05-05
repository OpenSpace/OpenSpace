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

#include <modules/roverterrain/filehandler/meshwriter.h>

#include <ghoul/logging/logmanager.h>

#include <fstream>

namespace {
    const std::string _loggerCat = "MeshWriter";
}

namespace openspace {

void MeshWriter::writeObjFileNoTex(const std::string filename, std::string output_path, const pcl::PolygonMesh polyMesh) {
    std::string obj_path = output_path + filename + ".obj";// +"NOTEX.obj";

    unsigned int precision = 5;

    // Open file
    std::ofstream fs;
    fs.precision(precision);
    fs.open(obj_path.c_str());

    /* Write 3D information */
    // number of points
    int nr_points = polyMesh.cloud.width * polyMesh.cloud.height;
    // point size
    unsigned point_size = static_cast<unsigned> (polyMesh.cloud.data.size() / nr_points);
    // number of faces for header
    unsigned nr_faces = static_cast<unsigned> (polyMesh.polygons.size());

    // Write the header information
    fs << "####" << '\n';
    fs << "# OBJ dataFile simple version. File name: " << filename << '\n';
    fs << "# Vertices: " << nr_points << '\n';
    fs << "# Vertices normals : " << nr_points << '\n';
    fs << "# Faces: " << nr_faces << '\n';
    fs << "####" << '\n';

    // Write vertex coordinates
    fs << "# List of Vertices, with (x,y,z) coordinates, w is optional." << '\n';
    for (int i = 0; i < nr_points; ++i)
    {
        int xyz = 0;
        for (size_t d = 0; d < polyMesh.cloud.fields.size(); ++d)
        {
            int c = 0;
            // adding vertex
            if ((polyMesh.cloud.fields[d].datatype == pcl::PCLPointField::FLOAT32) && (
                polyMesh.cloud.fields[d].name == "x" ||
                polyMesh.cloud.fields[d].name == "y" ||
                polyMesh.cloud.fields[d].name == "z"))
            {
                if (polyMesh.cloud.fields[d].name == "x")
                    // write vertices beginning with v
                    fs << "v ";

                float value;
                memcpy(&value, &polyMesh.cloud.data[i * point_size + polyMesh.cloud.fields[d].offset + c * sizeof(float)], sizeof(float));
                fs << value;
                if (++xyz == 3)
                    break;
                fs << " ";
            }
        }
        if (xyz != 3)
        {
            LERROR("[pcl::io::saveOBJFile] Input point cloud has no XYZ data!\n");
            return;
        }
        fs << '\n';
    }

    fs << "# " << nr_points << " vertices" << '\n';
    int normal_index = 1;
    if (normal_index != -1)
    {
        fs << "# Normals in (x,y,z) form; normals might not be unit." << '\n';
        // Write vertex normals
        for (int i = 0; i < nr_points; ++i)
        {
            int nxyz = 0;
            for (size_t d = 0; d < polyMesh.cloud.fields.size(); ++d)
            {
                int c = 0;
                // adding vertex
                if ((polyMesh.cloud.fields[d].datatype == pcl::PCLPointField::FLOAT32) && (
                    polyMesh.cloud.fields[d].name == "normal_x" ||
                    polyMesh.cloud.fields[d].name == "normal_y" ||
                    polyMesh.cloud.fields[d].name == "normal_z"))
                {
                    if (polyMesh.cloud.fields[d].name == "normal_x")
                        // write vertices beginning with vn
                        fs << "vn ";

                    float value;
                    memcpy(&value, &polyMesh.cloud.data[i * point_size + polyMesh.cloud.fields[d].offset + c * sizeof(float)], sizeof(float));
                    fs << value;
                    if (++nxyz == 3)
                        break;
                    fs << " ";
                }
            }
            if (nxyz != 3)
            {
                //LERROR("[pcl::io::saveOBJFile] Input point cloud has no normals!\n");
                //return;
            }
            fs << '\n';
        }

        fs << "# " << nr_points << " vertices normals" << '\n';
    }

    fs << "# Face Definitions" << '\n';
    // Write down faces
    if (normal_index == -1)
    {
        for (unsigned i = 0; i < nr_faces; i++)
        {
            fs << "f ";
            size_t j = 0;
            for (; j < polyMesh.polygons[i].vertices.size() - 1; ++j)
                fs << polyMesh.polygons[i].vertices[j] + 1 << " ";
            fs << polyMesh.polygons[i].vertices[j] + 1 << '\n';
        }
    }
    else
    {
        for (unsigned i = 0; i < nr_faces; i++)
        {
            fs << "f ";
            size_t j = 0;
            for (; j < polyMesh.polygons[i].vertices.size() - 1; ++j)
                fs << polyMesh.polygons[i].vertices[j] + 1 << "//" << polyMesh.polygons[i].vertices[j] + 1 << " ";
            fs << polyMesh.polygons[i].vertices[j] + 1 << "//" << polyMesh.polygons[i].vertices[j] + 1 << '\n';
        }
    }
    fs << "# End of File" << std::endl;

    // Close obj file
    fs.close();
    LERROR("GENERATED FILE: " + obj_path);
    return;
}

}
