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

	std::shared_ptr<Texture> GdalDataConverter::convertToOpenGLTexture(GDALDataset* dataSet)
	{
		//int nCols = dataSet->GetRasterBand(1)->GetXSize();
		//int nRows = dataSet->GetRasterBand(1)->GetYSize();

		//int nLayers = dataSet->GetLayerCount();
		//int nRasters = dataSet->GetRasterCount();

		int overviewCount = dataSet->GetRasterBand(1)->GetOverviewCount();

		GDALRasterBand* redBand = dataSet->GetRasterBand(1)->GetOverview(overviewCount - 4);
		GDALRasterBand* greenBand = dataSet->GetRasterBand(2)->GetOverview(overviewCount - 4);
		GDALRasterBand* blueBand = dataSet->GetRasterBand(3)->GetOverview(overviewCount - 4);

		int nCols = redBand->GetXSize();
		int nRows = redBand->GetYSize();


		int blockSizeX;
		int blockSizeY;

		redBand->GetBlockSize(&blockSizeX, &blockSizeY);

		int nBlocksX = nCols / blockSizeX;
		int nBlocksY = nRows / blockSizeY;

		// A block where data is copied
		GByte* blockR = (GByte*)CPLMalloc(sizeof(GByte) * nCols * nRows);
		GByte* blockG = (GByte*)CPLMalloc(sizeof(GByte) * nCols * nRows);
		GByte* blockB = (GByte*)CPLMalloc(sizeof(GByte) * nCols * nRows);
		// The data that the texture should use
		GLubyte* imageData = (GLubyte*)CPLMalloc(sizeof(GLubyte) * nCols * nRows * 4);

		redBand->RasterIO(
			GF_Read,
			0,
			0,
			nCols,
			nRows,
			blockR,
			nCols,
			nRows,
			GDT_Byte,
			0,
			0);

		greenBand->RasterIO(
			GF_Read,
			0,
			0,
			nCols,
			nRows,
			blockG,
			nCols,
			nRows,
			GDT_Byte,
			0,
			0);

		blueBand->RasterIO(
			GF_Read,
			0,
			0,
			nCols,
			nRows,
			blockB,
			nCols,
			nRows,
			GDT_Byte,
			0,
			0);

			
		// For each pixel
		for (size_t y = 0; y < nRows; y++)
		{
			for (size_t x = 0; x < nCols; x++)
			{
				size_t pixelIndexInBlock = x + y * nCols;
				size_t globalPixelIndex = (x + y * nCols) * 4;

				GLubyte pixelR = blockR[pixelIndexInBlock];
				GLubyte pixelG = blockG[pixelIndexInBlock];
				GLubyte pixelB = blockB[pixelIndexInBlock];

				imageData[globalPixelIndex + 0] = pixelR;
				imageData[globalPixelIndex + 1] = pixelG;
				imageData[globalPixelIndex + 2] = pixelB;
				imageData[globalPixelIndex + 3] = 255;
			}
		}
		
		/*
		// For each block
		for (size_t blockY = 0; blockY < nBlocksY; blockY++)
		{
			for (size_t blockX = 0; blockX < nBlocksX; blockX++)
			{

				redBand->ReadBlock(blockX, blockY, blockR);
				greenBand->ReadBlock(blockX, blockY, blockG);
				blueBand->ReadBlock(blockX, blockY, blockB);

				size_t blockPixelIndexX = blockSizeX * blockX;
				size_t blockPixelIndexY = blockSizeY * blockY;

				size_t startPixelIndex = blockPixelIndexY * nCols + blockPixelIndexX;


				// For each pixel in each block
				for (size_t yInBlock = 0; yInBlock < blockSizeY; yInBlock++)
				{
					for (size_t xInBlock = 0; xInBlock < blockSizeX; xInBlock++)
					{
						size_t pixelIndexInBlock = xInBlock + yInBlock * blockSizeX;
						size_t globalPixelIndex = (startPixelIndex + xInBlock + yInBlock * nCols) * 4;
						
						GLubyte pixelR = blockR[pixelIndexInBlock];
						GLubyte pixelG = blockG[pixelIndexInBlock];
						GLubyte pixelB = blockB[pixelIndexInBlock];

						imageData[globalPixelIndex + 0] = pixelR;
						imageData[globalPixelIndex + 1] = pixelG;
						imageData[globalPixelIndex + 2] = pixelB;
						imageData[globalPixelIndex + 3] = 255;
					}
				}
			}
		}
		*/
		// The texture should take ownership of the data
		std::shared_ptr<Texture> texture = std::shared_ptr<Texture>(new Texture(
			static_cast<void*>(imageData),
			glm::uvec3(nCols, nRows, 1),
			Texture::Format::RGBA,
			GL_RGBA,
			GL_UNSIGNED_BYTE,
			Texture::FilterMode::Linear,
			Texture::WrappingMode::Repeat));

		// Free the row
		CPLFree(blockR);
		CPLFree(blockG);
		CPLFree(blockB);

		// Do not free imageData since the texture now has ownership of it
		
		return texture;
	}

}  // namespace openspace
