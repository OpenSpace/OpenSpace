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

#include "gtest/gtest.h"

#include "gdal.h"
#include "gdal_priv.h"
#include "cpl_conv.h"

//#include "wms/wmsdriver.h"
//#include "wms/wmsmetadataset.h"


class GdalWmsTest : public testing::Test {};

TEST_F(GdalWmsTest, Simple) {
	//GDALRegister_WMS();
	
	const char* fileName = "C:/Users/kalbl_000/Documents/CPP/OpenSpace-Development/data/scene/debugglobe/textures/earth_bluemarble.jpg";

	GDALDataset  *poDataset;
	GDALAllRegister();
	poDataset = (GDALDataset *)GDALOpen(fileName, GA_ReadOnly);
	if (poDataset == NULL)
	{
		std::cout << "BAD GDAL OPEN" << std::endl;
		return;
	}

	double        adfGeoTransform[6];
	printf("Driver: %s/%s\n",
		poDataset->GetDriver()->GetDescription(),
		poDataset->GetDriver()->GetMetadataItem(GDAL_DMD_LONGNAME));
	printf("Size is %dx%dx%d\n",
		poDataset->GetRasterXSize(), poDataset->GetRasterYSize(),
		poDataset->GetRasterCount());
	if (poDataset->GetProjectionRef() != NULL)
		printf("Projection is `%s'\n", poDataset->GetProjectionRef());
	if (poDataset->GetGeoTransform(adfGeoTransform) == CE_None)
	{
		printf("Origin = (%.6f,%.6f)\n",
			adfGeoTransform[0], adfGeoTransform[3]);
		printf("Pixel Size = (%.6f,%.6f)\n",
			adfGeoTransform[1], adfGeoTransform[5]);
	}

	/*
	GDALDataset  *poDataset;
	GDALAllRegister();
	poDataset = (GDALDataset *)GDALOpen("filename", GA_ReadOnly);
	if (poDataset == NULL)
	{
		std::cout << "NULL" << std::endl;
	}*/
}
