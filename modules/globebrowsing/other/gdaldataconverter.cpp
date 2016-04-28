/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2016                                                               *
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


#include <modules/globebrowsing/other/gdaldataconverter.h>

namespace {
	const std::string _loggerCat = "GdalDataConverter";
}

namespace openspace {

	GdalDataConverter::GdalDataConverter()
	{

	}

	GdalDataConverter::~GdalDataConverter()
	{

	}

	std::shared_ptr<Texture> GdalDataConverter::convertToOpenGLTexture(
		GDALDataset* dataSet,
		const GeodeticTileIndex& tileIndex,
		int GLType)
	{
		int nRasters = dataSet->GetRasterCount();

		ghoul_assert(nRasters > 0, "Bad dataset. Contains no rasterband.");

		GDALRasterBand* firstBand = dataSet->GetRasterBand(1);

		// Level = overviewCount - overview
		int overviewCount = firstBand->GetOverviewCount();
		int overview = overviewCount - tileIndex.level - 1;

		// The output texture will have this size
		int xSizelevel0 = firstBand->GetOverview(overviewCount - 1)->GetXSize();
		int ySizelevel0 = firstBand->GetOverview(overviewCount - 1)->GetYSize();

		// Create all the raster bands (Commonly one for each channel: red, green, blue)
		std::vector<GDALRasterBand*> rasterBands;
		for (size_t i = 0; i < nRasters; i++)
		{
			rasterBands.push_back(dataSet->GetRasterBand(i + 1)->GetOverview(overview));
		}
		// The data that the texture should read
		GLubyte* imageData = new GLubyte[xSizelevel0 * ySizelevel0 * nRasters];

		// Read the data
		for (size_t i = 0; i < nRasters; i++)
		{

			int xBeginRead = tileIndex.x * pow(2, tileIndex.level) *  xSizelevel0;
			int yBeginRead = tileIndex.y * pow(2, tileIndex.level) *  ySizelevel0;
			rasterBands[i]->RasterIO(
				GF_Read,
				xBeginRead,					// Begin read x
				yBeginRead,					// Begin read y
				xSizelevel0,				// width to read x
				ySizelevel0,				// width to read y
				imageData + i,				// Where to put data
				xSizelevel0,				// width to read x in destination
				ySizelevel0,				// width to read y in destination
				GDT_Byte,					// Type
				sizeof(GLubyte) * nRasters,	// Pixel spacing
				0);							// Line spacing
		}

		GdalDataConverter::TextureFormat textrureFormat =
			getTextureFormatFromRasterCount(nRasters);

		// The texture should take ownership of the data
		std::shared_ptr<Texture> texture = std::shared_ptr<Texture>(new Texture(
			static_cast<void*>(imageData),
			glm::uvec3(xSizelevel0, ySizelevel0, 1),
			textrureFormat.ghoulFormat,
			textrureFormat.glFormat,
			GL_UNSIGNED_BYTE,
			Texture::FilterMode::Linear,
			Texture::WrappingMode::Repeat));

		// Do not free imageData since the texture now has ownership of it
		return texture;
	}

	GdalDataConverter::TextureFormat GdalDataConverter::getTextureFormatFromRasterCount(
		int rasterCount)
	{
		TextureFormat format;

		switch (rasterCount)
		{
		case 1:
			format.ghoulFormat = Texture::Format::Red;
			format.glFormat = GL_RED;
			break;
		case 2:
			format.ghoulFormat = Texture::Format::RG;
			format.glFormat = GL_RG;
			break;
		case 3:
			format.ghoulFormat = Texture::Format::RGB;
			format.glFormat = GL_RGB;
			break;
		case 4:
			format.ghoulFormat = Texture::Format::RGBA;
			format.glFormat = GL_RGBA;
			break;
		default:

			break;
		}
		return format;
	}

}  // namespace openspace
