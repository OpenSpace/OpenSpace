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

#include <modules/globebrowsing/tasks/meshwriter.h>

#include <ghoul/logging/logmanager.h>

#include <fstream>

namespace {
	const std::string _loggerCat = "MeshWriter";
}

namespace openspace {
namespace globebrowsing {
	void MeshWriter::writeObjFile(const std::string filename, std::string output_path, const pcl::TextureMesh texMesh) {

		std::string obj_path = output_path + filename + ".obj";
		//LERROR("Obj path: " << obj_path);

		unsigned precision = 5;
		// Open file
		std::ofstream fs;
		fs.precision(precision);
		fs.open(obj_path.c_str());

		/* Write 3D information */
		// number of points
		unsigned nr_points = texMesh.cloud.width * texMesh.cloud.height;
		unsigned point_size2 = static_cast<unsigned> (texMesh.cloud.data.size() / nr_points);

		// Mesh size
		unsigned nr_meshes = static_cast<unsigned> (texMesh.tex_polygons.size());
		// Number of faces for header
		unsigned nr_faces = 0;
		for (unsigned m = 0; m < nr_meshes; ++m)
			nr_faces += static_cast<unsigned> (texMesh.tex_polygons[m].size());

		// Define material file
		std::string mtl_file_name = filename.substr(0, filename.find_last_of(".")) + ".mtl";
		// Strip path for "mtllib" command
		std::string mtl_file_name_nopath = mtl_file_name;
		mtl_file_name_nopath.erase(0, mtl_file_name.find_last_of('/') + 1);

		// Write the header information
		fs << "####" << '\n';
		fs << "# OBJ dataFile simple version. File name: " << filename << '\n';
		fs << "# Vertices: " << nr_points << '\n';
		fs << "# Faces: " << nr_faces << '\n';
		fs << "# Material information:" << '\n';
		fs << "mtllib " << mtl_file_name_nopath << '\n';
		fs << "####" << '\n';

		// Write vertex coordinates
		fs << "# Vertices" << '\n';
		for (unsigned i = 0; i < nr_points; ++i) {
			int xyz = 0;
			// Only write "v " once
			bool v_written = false;
			for (size_t d = 0; d < texMesh.cloud.fields.size(); ++d) {
				int c = 0;
				// Adding vertex
				if ((texMesh.cloud.fields[d].datatype == pcl::PCLPointField::FLOAT32) && (
					texMesh.cloud.fields[d].name == "x" ||
					texMesh.cloud.fields[d].name == "y" ||
					texMesh.cloud.fields[d].name == "z")) {
					if (!v_written) {
						// Write vertices beginning with v
						fs << "v ";
						v_written = true;
					}
					float value;
					memcpy(&value, &texMesh.cloud.data[i * point_size2 + texMesh.cloud.fields[d].offset + c * sizeof(float)], sizeof(float));

					fs << value;
					if (++xyz == 3)
						break;
					fs << " ";
				}
			}
			if (xyz != 3) {
				LERROR("Input point cloud has no XYZ data!");
			}
			fs << '\n';
		}
		fs << "# " << nr_points << " vertices" << '\n';

		// Write vertex normals
		for (unsigned i = 0; i < nr_points; ++i) {
			int xyz = 0;
			// Only write "vn " once
			bool v_written = false;
			for (size_t d = 0; d < texMesh.cloud.fields.size(); ++d) {
				int c = 0;
				// Adding vertex
				if ((texMesh.cloud.fields[d].datatype == pcl::PCLPointField::FLOAT32) && (
					texMesh.cloud.fields[d].name == "normal_x" ||
					texMesh.cloud.fields[d].name == "normal_y" ||
					texMesh.cloud.fields[d].name == "normal_z")) {
					if (!v_written) {
						// Write vertices beginning with vn
						fs << "vn ";
						v_written = true;
					}
					float value;
					memcpy(&value, &texMesh.cloud.data[i * point_size2 + texMesh.cloud.fields[d].offset + c * sizeof(float)], sizeof(float));
					fs << value;
					if (++xyz == 3)
						break;
					fs << " ";
				}
			}
			if (xyz != 3) {
				//LERROR("Input point cloud has no normals");
			}
			fs << '\n';
		}

		// Write vertex texture with "vt"
		for (unsigned m = 0; m < nr_meshes; ++m) {
			fs << "# " << texMesh.tex_coordinates[m].size() << " vertex textures in submesh " << m << '\n';
			for (size_t i = 0; i < texMesh.tex_coordinates[m].size(); ++i) {
				fs << "vt ";
				fs << texMesh.tex_coordinates[m][i][0] << " " << texMesh.tex_coordinates[m][i][1] << '\n';
			}
		}

		unsigned f_idx = 0;
		for (unsigned m = 0; m < nr_meshes; ++m) {
			if (m > 0) f_idx += static_cast<unsigned> (texMesh.tex_polygons[m - 1].size());

			fs << "# The material will be used for mesh " << m << '\n';
			fs << "usemtl " << texMesh.tex_materials[m].tex_name << '\n';
			fs << "# Faces" << '\n';

			for (size_t i = 0; i < texMesh.tex_polygons[m].size(); ++i) {
				// Write faces with "f"
				fs << "f";
				size_t j = 0;
				// There's one UV per vertex per face, i.e., the same vertex can have
				// different UV depending on the face.
				for (j = 0; j < texMesh.tex_polygons[m][i].vertices.size(); ++j) {
					// + 1 since obj file format starts with 1 and not 0
					uint32_t idx = texMesh.tex_polygons[m][i].vertices[j] + 1;
					fs << " " << idx
						<< "/" << texMesh.tex_polygons[m][i].vertices.size() * (i + f_idx) + j + 1
						<< "/" << idx;
				}
				fs << '\n';
			}
			fs << "# " << texMesh.tex_polygons[m].size() << " faces in mesh " << m << '\n';
		}
		fs << "# End of File" << std::flush;

		// Close obj file
		fs.close();
	}

	void MeshWriter::writeMtlFile(const std::string filename, const std::string output_path, const pcl::TextureMesh texMesh) {
		unsigned precision = 5;
		unsigned nr_meshes = static_cast<unsigned> (texMesh.tex_polygons.size());
		// Write mtl file for the corresponding obj file
		// Not necessary atm since Openspace cannot handle mtl files
		// If this will become useful, the uv coordinates calculation needs to be changed(?)
		std::string mtl_path = output_path + filename + ".mtl";

		//LERROR("mtl file path : " << mtl_path);

		// Open file
		std::ofstream m_fs;
		m_fs.precision(precision);
		m_fs.open(mtl_path.c_str());

		// default
		m_fs << "#" << '\n';
		m_fs << "# Wavefront material file" << '\n';
		m_fs << "#" << '\n';
		for (unsigned m = 0; m < nr_meshes; ++m) {
			m_fs << "newmtl " << texMesh.tex_materials[m].tex_name << '\n';
			m_fs << "Ka " << texMesh.tex_materials[m].tex_Ka.r << " " << texMesh.tex_materials[m].tex_Ka.g << " " << texMesh.tex_materials[m].tex_Ka.b << '\n'; // defines the ambient color of the material to be (r,g,b).
			m_fs << "Kd " << texMesh.tex_materials[m].tex_Kd.r << " " << texMesh.tex_materials[m].tex_Kd.g << " " << texMesh.tex_materials[m].tex_Kd.b << '\n'; // defines the diffuse color of the material to be (r,g,b).
			m_fs << "Ks " << texMesh.tex_materials[m].tex_Ks.r << " " << texMesh.tex_materials[m].tex_Ks.g << " " << texMesh.tex_materials[m].tex_Ks.b << '\n'; // defines the specular color of the material to be (r,g,b). This color shows up in highlights.
			m_fs << "d " << texMesh.tex_materials[m].tex_d << '\n'; // defines the transparency of the material to be alpha.
			m_fs << "Ns " << texMesh.tex_materials[m].tex_Ns << '\n'; // defines the shininess of the material to be s.
			m_fs << "illum " << texMesh.tex_materials[m].tex_illum << '\n'; // denotes the illumination model used by the material.
																			// illum = 1 indicates a flat material with no specular highlights, so the value of Ks is not used.
																			// illum = 2 denotes the presence of specular highlights, and so a specification for Ks is required.
			m_fs << "map_Kd " << texMesh.tex_materials[m].tex_file << '\n';
			m_fs << "###" << '\n';
		}
		m_fs.close();

	}
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
					LERROR("[pcl::io::saveOBJFile] Input point cloud has no normals!\n");
					return;
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
		return;
	}
}
}
