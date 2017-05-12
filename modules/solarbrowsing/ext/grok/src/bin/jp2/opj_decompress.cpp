/*
*    Copyright (C) 2016-2017 Grok Image Compression Inc.
*
*    This source code is free software: you can redistribute it and/or  modify
*    it under the terms of the GNU Affero General Public License, version 3,
*    as published by the Free Software Foundation.
*
*    This source code is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU Affero General Public License for more details.
*
*    You should have received a copy of the GNU Affero General Public License
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*
*
*    This source code incorporates work covered by the following copyright and
*    permission notice:
*
 * The copyright in this software is being made available under the 2-clauses
 * BSD License, included below. This software may be subject to other third
 * party and contributor rights, including patent rights, and no such rights
 * are granted under this license.
 *
 * Copyright (c) 2002-2014, Universite catholique de Louvain (UCL), Belgium
 * Copyright (c) 2002-2014, Professor Benoit Macq
 * Copyright (c) 2001-2003, David Janssens
 * Copyright (c) 2002-2003, Yannick Verschueren
 * Copyright (c) 2003-2007, Francois-Olivier Devaux
 * Copyright (c) 2003-2014, Antonin Descampe
 * Copyright (c) 2005, Herve Drolon, FreeImage Team
 * Copyright (c) 2006-2007, Parvatha Elangovan
 * Copyright (c) 2008, 2011-2012, Centre National d'Etudes Spatiales (CNES), FR
 * Copyright (c) 2012, CS Systemes d'Information, France
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS `AS IS'
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */


extern "C" {

#include "opj_apps_config.h"

#ifdef _WIN32
#include "windirent.h"
#else
#include <dirent.h>
#endif /* _WIN32 */

#ifdef _WIN32
#define strcasecmp _stricmp
#define strncasecmp _strnicmp
#else
#include <strings.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/times.h>
#endif /* _WIN32 */

#include "openjpeg.h"
#include "convert.h"

#ifdef OPJ_HAVE_LIBLCMS
#include <lcms2.h>
#endif
#include "color.h"

#include "format_defs.h"
#include "opj_string.h"

}

#include <string>
#define TCLAP_NAMESTARTSTRING "-"
#include "tclap/CmdLine.h"

using namespace TCLAP;
using namespace std;

typedef struct dircnt {
    /** Buffer for holding images read from Directory*/
    char *filename_buf;
    /** Pointer to the buffer*/
    char **filename;
} dircnt_t;


typedef struct img_folder {
    /** The directory path of the folder containing input images*/
    char *imgdirpath;
    /** Output format*/
    const char *out_format;
    /** Enable option*/
    char set_imgdir;
    /** Enable Cod Format for output*/
    char set_out_format;

} img_fol_t;


/* -------------------------------------------------------------------------- */
/* Declarations                                                               */
int get_num_images(char *imgdirpath);
int load_images(dircnt_t *dirptr, char *imgdirpath, bool verbose);
int get_file_format(const char *filename);
char get_next_file(int imageno, dircnt_t *dirptr, img_fol_t *img_fol, img_fol_t* out_fol, opj_decompress_parameters *parameters);
static int infile_format(const char *fname);

int parse_cmdline_decoder(int argc, 
							char **argv,
							opj_decompress_parameters *parameters,
							img_fol_t *img_fol,
							img_fol_t *out_fol,
							char* plugin_path);
int parse_DA_values( char* inArg, uint32_t *DA_x0, uint32_t *DA_y0, uint32_t *DA_x1, uint32_t *DA_y1);

static opj_image_t* convert_gray_to_rgb(opj_image_t* original);

/* -------------------------------------------------------------------------- */


/**
sample error callback expecting a FILE* client object
*/
static void error_callback(const char *msg, void *client_data)
{
	(void)client_data;
	fprintf(stderr, "[ERROR] %s", msg);
}
/**
sample warning callback expecting a FILE* client object
*/
static void warning_callback(const char *msg, void *client_data)
{
	bool verbose = true;
	if (client_data)
		verbose = *((bool*)client_data);
	if (verbose)
		fprintf(stdout, "[WARNING] %s", msg);
}
/**
sample debug callback expecting no client object
*/
static void info_callback(const char *msg, void *client_data)
{
	bool verbose = true;
	if (client_data)
		verbose = *((bool*)client_data);
	if (verbose)
		fprintf(stdout, "[INFO] %s", msg);
}


static void decode_help_display(void)
{
    fprintf(stdout,"\nThis is the opj_decompress utility from the Grok project.\n"
            "It decompresses JPEG 2000 codestreams to various image formats.\n"
            "It has been compiled against openjp2 library v%s.\n\n",opj_version());

    fprintf(stdout,"Parameters:\n"
            "-----------\n"
            "\n"
			"  [-y | -ImgDir] <directory> \n"
            "	Image file directory path \n"
			"  [-O | -OutFor] <PBM|PGM|PPM|PNM|PAM|PGX|PNG|BMP|TIF|RAW|RAWL|TGA>\n"
            "    REQUIRED only if -ImgDir is used\n"
            "	Output format for decompressed images.\n");
    fprintf(stdout,"  [-i | -InputFile] <compressed file>\n"
            "    REQUIRED only if an Input image directory is not specified\n"
            "    Currently accepts J2K-files, JP2-files and JPT-files. The file type\n"
            "    is identified based on its suffix.\n");
    fprintf(stdout,"  [-o | -OutputFile] <decompressed file>\n"
            "    REQUIRED\n"
            "    Currently accepts formats specified above (see OutFor option)\n"
            "    Binary data is written to the file (not ascii). If a PGX\n"
            "    filename is given, there will be as many output files as there are\n"
            "    components: an indice starting from 0 will then be appended to the\n"
            "    output filename, just before the \"pgx\" extension. If a PGM filename\n"
            "    is given and there are more than one component, only the first component\n"
            "    will be written to the file.\n");
	fprintf(stdout, "  [-a | -OutDir] <output directory>\n"
		"    Output directory where decompressed files are stored.\n");
	fprintf(stdout, "  [-g | -PluginPath] <plugin path>\n"
		"    Path to T1 plugin.\n");
	fprintf(stdout, "  [-H | -NumThreads] <number of threads>\n"
		"    Number of threads used by T1 decode.\n");
	fprintf(stdout, "  [-c|-Compression] <compression>\n"
		"    Compress output image data.Currently, this flag is only applicable when output format is set to `TIF`,\n"
		"    and the only currently supported value is 8, corresponding to COMPRESSION_ADOBE_DEFLATE i.e.zip compression.\n"
		"    The `zlib` library must be available for this compression setting.Default: 0 - no compression.\n");
	fprintf(stdout, "   [L|-CompressionLevel] <compression level>\n"
		"    \"Quality\" of compression. Currently only implemented for PNG format. Default - Z_BEST_COMPRESSION\n");
	fprintf(stdout, "  [-t | -TileIndex] <tile index>\n"
		"    Index of tile to be decoded\n");
	fprintf(stdout, "  [-d | -DecodeRegion] <x0,y0,x1,y1>\n"
		"    Top left-hand corner and bottom right-hand corner of region to be decoded.\n");
    fprintf(stdout,"  [-r | -Reduce] <reduce factor>\n"
            "    Set the number of highest resolution levels to be discarded. The\n"
            "    image resolution is effectively divided by 2 to the power of the\n"
            "    number of discarded levels. The reduce factor is limited by the\n"
            "    smallest total number of decomposition levels among tiles.\n"
			"  [-l | -Layer] <number of quality layers to decode>\n"
            "    Set the maximum number of quality layers to decode. If there are\n"
            "    fewer quality layers than the specified number, all the quality layers\n"
            "    are decoded.\n");
    fprintf(stdout,"  [-p | -Precision] <comp 0 precision>[C|S][,<comp 1 precision>[C|S][,...]]\n"
            "    OPTIONAL\n"
            "    Force the precision (bit depth) of components.\n");
    fprintf(stdout,"    There shall be at least 1 value. Theres no limit on the number of values (comma separated, last values ignored if too much values).\n"
            "    If there are less values than components, the last value is used for remaining components.\n"
            "    If 'C' is specified (default), values are clipped.\n"
            "    If 'S' is specified, values are scaled.\n"
            "    A 0 value can be specified (meaning original bit depth).\n");
    fprintf(stdout,"  [-f | -force-rg]b\n"
            "    Force output image colorspace to RGB\n"
			"  [-u | -upsample]\n"
            "    components will be upsampled to image size\n"
			"  [-s | -split-pnm]\n"
            "    Split output components to different files when writing to PNM\n"
			"  [-c | -compression]\n"
			"    Compression format for output file. Currently, only zip is supported for TIFF output (set parameter to 8)\n"
            "\n");
	fprintf(stdout, "  [-X | -XML]\n"
		"    Store XML metadata to file. File name will be set to \"output file name\" + \".xml\"\n");
    fprintf(stdout,"\n");
}

/* -------------------------------------------------------------------------- */

static bool parse_precision(const char* option, opj_decompress_parameters* parameters)
{
    const char* l_remaining = option;
    bool l_result = true;

    /* reset */
    if (parameters->precision) {
        free(parameters->precision);
        parameters->precision = NULL;
    }
    parameters->nb_precision = 0U;

    for(;;) {
        int prec;
        char mode;
        char comma;
        int count;

        count = sscanf(l_remaining, "%d%c%c", &prec, &mode, &comma);
        if (count == 1) {
            mode = 'C';
            count++;
        }
        if ((count == 2) || (mode==',')) {
            if (mode==',') {
                mode = 'C';
            }
            comma=',';
            count = 3;
        }
        if (count == 3) {
            if ((prec < 1) || (prec > 32)) {
                fprintf(stderr,"Invalid precision %d in precision option %s\n", prec, option);
                l_result = false;
                break;
            }
            if ((mode != 'C') && (mode != 'S')) {
                fprintf(stderr,"Invalid precision mode %c in precision option %s\n", mode, option);
                l_result = false;
                break;
            }
            if (comma != ',') {
                fprintf(stderr,"Invalid character %c in precision option %s\n", comma, option);
                l_result = false;
                break;
            }

            if (parameters->precision == NULL) {
                /* first one */
                parameters->precision = (opj_precision *)malloc(sizeof(opj_precision));
                if (parameters->precision == NULL) {
                    fprintf(stderr,"Could not allocate memory for precision option\n");
                    l_result = false;
                    break;
                }
            } else {
                uint32_t l_new_size = parameters->nb_precision + 1U;
                opj_precision* l_new;

                if (l_new_size == 0U) {
                    fprintf(stderr,"Could not allocate memory for precision option\n");
                    l_result = false;
                    break;
                }

                l_new = (opj_precision *)realloc(parameters->precision, l_new_size * sizeof(opj_precision));
                if (l_new == NULL) {
                    fprintf(stderr,"Could not allocate memory for precision option\n");
                    l_result = false;
                    break;
                }
                parameters->precision = l_new;
            }

            parameters->precision[parameters->nb_precision].prec = (uint32_t)prec;
            switch (mode) {
            case 'C':
                parameters->precision[parameters->nb_precision].mode = OPJ_PREC_MODE_CLIP;
                break;
            case 'S':
                parameters->precision[parameters->nb_precision].mode = OPJ_PREC_MODE_SCALE;
                break;
            default:
                break;
            }
            parameters->nb_precision++;

            l_remaining = strchr(l_remaining, ',');
            if (l_remaining == NULL) {
                break;
            }
            l_remaining += 1;
        } else {
            fprintf(stderr,"Could not parse precision option %s\n", option);
            l_result = false;
            break;
        }
    }

    return l_result;
}

/* -------------------------------------------------------------------------- */

int get_num_images(char *imgdirpath)
{
    DIR *dir;
    struct dirent* content;
    int num_images = 0;

    /*Reading the input images from given input directory*/

    dir= opendir(imgdirpath);
    if(!dir) {
        fprintf(stderr,"Could not open Folder %s\n",imgdirpath);
        return 0;
    }

    while((content=readdir(dir))!=NULL) {
        if(strcmp(".",content->d_name)==0 || strcmp("..",content->d_name)==0 )
            continue;
        num_images++;
    }
    closedir(dir);
    return num_images;
}

/* -------------------------------------------------------------------------- */
int load_images(dircnt_t *dirptr, char *imgdirpath, bool verbose)
{
    DIR *dir;
    struct dirent* content;
    int i = 0;

    /*Reading the input images from given input directory*/

    dir= opendir(imgdirpath);
    if(!dir) {
        fprintf(stderr,"Could not open Folder %s\n",imgdirpath);
        return 1;
    }

    while((content=readdir(dir))!=NULL) {
        if(strcmp(".",content->d_name)==0 || strcmp("..",content->d_name)==0 )
            continue;

        strcpy(dirptr->filename[i],content->d_name);
        i++;
    }
    closedir(dir);
    return 0;
}

/* -------------------------------------------------------------------------- */
int get_file_format(const char *filename)
{
    unsigned int i;
    static const char *extension[] = {"pgx", "pnm", "pgm", "ppm", "bmp","tif", "raw", "rawl", "tga", "png", "j2k", "jp2", "jpt", "j2c", "jpc" };
    static const int format[] = { PGX_DFMT, PXM_DFMT, PXM_DFMT, PXM_DFMT, BMP_DFMT, TIF_DFMT, RAW_DFMT, RAWL_DFMT, TGA_DFMT, PNG_DFMT, J2K_CFMT, JP2_CFMT, JPT_CFMT, J2K_CFMT, J2K_CFMT };
    const char * ext = strrchr(filename, '.');
    if (ext == NULL)
        return -1;
    ext++;
    if(*ext) {
        for(i = 0; i < sizeof(format)/sizeof(*format); i++) {
            if(strcasecmp(ext, extension[i]) == 0) {
                return format[i];
            }
        }
    }

    return -1;
}

static const char* get_path_separator() {
#ifdef _WIN32
	return "\\";
#else
	return "/";
#endif
}


/* -------------------------------------------------------------------------- */
char get_next_file(int imageno, 
					dircnt_t *dirptr,
					img_fol_t *img_fol,
					img_fol_t* out_fol,
					opj_decompress_parameters *parameters) {
	std::string image_filename = dirptr->filename[imageno];
	if (parameters->verbose)
		fprintf(stdout, "File Number %d \"%s\"\n", imageno, image_filename.c_str());
	std::string infilename = img_fol->imgdirpath + std::string(get_path_separator()) + image_filename;
	parameters->decod_format = infile_format(infilename.c_str());
	if (parameters->decod_format == -1)
		return 1;
	if (opj_strcpy_s(parameters->infile, sizeof(parameters->infile), infilename.c_str()) != 0) {
		return 1;
	}
	auto pos = image_filename.find(".");
	if (pos == std::string::npos)
		return 1;
	std::string temp_ofname = image_filename.substr(0,pos);
	if (img_fol->set_out_format == 1) {
		std::string outfilename = out_fol->imgdirpath + std::string(get_path_separator()) + temp_ofname + "." + img_fol->out_format;
		if (opj_strcpy_s(parameters->outfile, sizeof(parameters->outfile), outfilename.c_str()) != 0) {
			return 1;
		}
	}
	return 0;
}

/* -------------------------------------------------------------------------- */
#define JP2_RFC3745_MAGIC "\x00\x00\x00\x0c\x6a\x50\x20\x20\x0d\x0a\x87\x0a"
/* position 45: "\xff\x52" */
#define J2K_CODESTREAM_MAGIC "\xff\x4f\xff\x51"

static int infile_format(const char *fname)
{
    FILE *reader;
    const char *s, *magic_s;
    int ext_format, magic_format;
    unsigned char buf[12];
    size_t l_nb_read;

    reader = fopen(fname, "rb");

    if (reader == NULL)
        return -2;

    memset(buf, 0, 12);
    l_nb_read = fread(buf, 1, 12, reader);
    fclose(reader);
    if (l_nb_read != 12)
        return -1;

    ext_format = get_file_format(fname);

    if (ext_format == JPT_CFMT)
        return JPT_CFMT;

    if (memcmp(buf, JP2_RFC3745_MAGIC, 12) == 0 ) {
        magic_format = JP2_CFMT;
        magic_s = ".jp2";
    } else if (memcmp(buf, J2K_CODESTREAM_MAGIC, 4) == 0) {
        magic_format = J2K_CFMT;
        magic_s = ".j2k or .jpc or .j2c";
    } else
        return -1;

    if (magic_format == ext_format)
        return ext_format;

    s = fname + strlen(fname) - 4;

    fputs("\n===========================================\n", stderr);
    fprintf(stderr, "The extension of this file is incorrect.\n"
            "FOUND %s. SHOULD BE %s\n", s, magic_s);
    fputs("===========================================\n", stderr);

    return magic_format;
}


class GrokOutput : public StdOutput
{
public:
	virtual void usage(CmdLineInterface& c)
	{
		decode_help_display();
	}
};


/* -------------------------------------------------------------------------- */
/**
 * Parse the command line
 */
/* -------------------------------------------------------------------------- */
int parse_cmdline_decoder(int argc, 
							char **argv,
							opj_decompress_parameters *parameters,
							img_fol_t *img_fol,
							img_fol_t *out_fol,
							char* plugin_path)
{
	try {

		// Define the command line object.
		CmdLine cmd("Command description message", ' ', "0.9");
		
		// set the output
		GrokOutput output;
		cmd.setOutput(&output);

		ValueArg<string> imgDirArg("y", "ImgDir", 
									"Image Directory",
									false, "", "string",cmd);
		ValueArg<string> outDirArg("a", "OutDir", 
									"Output Directory",
									false, "", "string",cmd);
		ValueArg<string> outForArg("O", "OutFor",
									"Output Format",
									false, "", "string",cmd);

		SwitchArg forceRgbArg("f", "force-rgb",
								"Force RGB", cmd);
		SwitchArg upsampleArg("u", "upsample", 
								"Upsample", cmd);
		SwitchArg splitPnmArg("s", "split-pnm",
								"Split PNM", cmd);

		ValueArg<string> pluginPathArg("g", "PluginPath",
										"Plugin path", 
										false, "", "string",cmd);
		ValueArg<uint32_t> numThreadsArg("H", "NumThreads", 
										"Number of threads",
										false, 8, "unsigned integer",cmd);
		ValueArg<string> inputFileArg("i", "InputFile", 
										"Input file", 
										false, "", "string",cmd);
		ValueArg<string> outputFileArg("o", "OutputFile",
										"Output file",
										false, "", "string",cmd);
		ValueArg<uint32_t> reduceArg("r", "Reduce",
									"Reduce resolutions", 
									false, 0, "unsigned integer",cmd);
		ValueArg<uint32_t> layerArg("l", "Layer",
									"Layer", 
									false, 0, "unsigned integer",cmd);
		ValueArg<uint32_t> tileArg("t", "TileIndex",
									"Input tile index",
									false, 0, "unsigned integer", cmd);
		ValueArg<string> precisionArg("p", "Precision",
										"Force precision",
										false, "", "string", cmd);
		ValueArg<string> decodeRegionArg("d", "DecodeRegion",
										"Decode Region",
										false, "", "string", cmd);
		ValueArg<uint32_t> compressionArg("c", "Compression",
			"Compression Type",
			false, 0, "unsigned int", cmd);
		ValueArg<int32_t> compressionLevelArg("L", "CompressionLevel",
											"Compression Level",
											false, -65535, "int", cmd);
		ValueArg<uint32_t> durationArg("z", "Duration",
			"Duration in seconds",
			false, 0, "unsigned integer", cmd);

		ValueArg<int32_t> deviceIdArg("G", "DeviceId",
			"Device ID",
			false, 0, "integer", cmd);

		SwitchArg xmlArg("X", "XML",
			"XML metadata",cmd);

		// Kernel build flags:
		// 1 indicates build binary, otherwise load binary
		// 2 indicates generate binaries
		ValueArg<uint32_t> kernelBuildOptionsArg("k", "KernelBuild",
			"Kernel build options",
			false, 0, "unsigned integer", cmd);

		ValueArg<uint32_t> repetitionsArg("e", "Repetitions",
			"Number of encode repetitions, for either a folder or a single file",
			false, 0, "unsigned integer", cmd);

		SwitchArg verboseArg("v", "verbose",
			"Verbose", cmd);
		
		cmd.parse(argc, argv);

		parameters->serialize_xml = xmlArg.isSet();

		if (forceRgbArg.isSet()) {
			parameters->force_rgb = true;
		}
		if (upsampleArg.isSet()) {
			parameters->upsample = true;
		}
		if (splitPnmArg.isSet()) {
			parameters->split_pnm = true;
		}

		if (compressionArg.isSet()) {
			parameters->compression = compressionArg.getValue();
		}
		if (compressionLevelArg.isSet()) {
			parameters->compressionLevel = compressionLevelArg.getValue();
		}
		// process
		if (inputFileArg.isSet()) {
			const char *infile = inputFileArg.getValue().c_str();
			parameters->decod_format = infile_format(infile);
			switch (parameters->decod_format) {
			case J2K_CFMT:
				break;
			case JP2_CFMT:
				break;
			case JPT_CFMT:
				break;
			case -2:
				fprintf(stderr,
					"!! infile cannot be read: %s !!\n\n",
					infile);
				return 1;
			default:
				fprintf(stderr,
					"[ERROR] Unknown input file format: %s \n"
					"        Known file formats are *.j2k, *.jp2, *.jpc or *.jpt\n",
					infile);
				return 1;
			}
			if (opj_strcpy_s(parameters->infile, sizeof(parameters->infile), infile) != 0) {
				fprintf(stderr, "[ERROR] Path is too long\n");
				return 1;
			}
		}

		if (outputFileArg.isSet()) {
			const char *outfile = outputFileArg.getValue().c_str();
			parameters->cod_format = get_file_format(outfile);
			switch (parameters->cod_format) {
			case PGX_DFMT:
				break;
			case PXM_DFMT:
				break;
			case BMP_DFMT:
				break;
			case TIF_DFMT:
				break;
			case RAW_DFMT:
				break;
			case RAWL_DFMT:
				break;
			case TGA_DFMT:
				break;
			case PNG_DFMT:
				break;
			default:
				fprintf(stderr, "Unknown output format image %s [only *.png, *.pnm, *.pgm, *.ppm, *.pgx, *.bmp, *.tif, *.raw or *.tga]!!\n", outfile);
				return 1;
			}
			if (opj_strcpy_s(parameters->outfile, sizeof(parameters->outfile), outfile) != 0) {
				fprintf(stderr, "[ERROR] Path is too long\n");
				return 1;
			}
		}

		if (outForArg.isSet()) {
			char outformat[50];
			const char *of = outForArg.getValue().c_str();
			sprintf(outformat, ".%s", of);
			img_fol->set_out_format = 1;
			parameters->cod_format = get_file_format(outformat);
			switch (parameters->cod_format) {
			case PGX_DFMT:
				img_fol->out_format = "pgx";
				break;
			case PXM_DFMT:
				img_fol->out_format = "ppm";
				break;
			case BMP_DFMT:
				img_fol->out_format = "bmp";
				break;
			case TIF_DFMT:
				img_fol->out_format = "tif";
				break;
			case RAW_DFMT:
				img_fol->out_format = "raw";
				break;
			case RAWL_DFMT:
				img_fol->out_format = "rawl";
				break;
			case TGA_DFMT:
				img_fol->out_format = "raw";
				break;
			case PNG_DFMT:
				img_fol->out_format = "png";
				break;
			default:
				fprintf(stderr, "Unknown output format image %s [only *.png, *.pnm, *.pgm, *.ppm, *.pgx, *.bmp, *.tif, *.raw or *.tga]!!\n", outformat);
				return 1;
			}
		}

		if (imgDirArg.isSet()) {
			img_fol->imgdirpath = (char*)malloc(strlen(imgDirArg.getValue().c_str()) + 1);
			strcpy(img_fol->imgdirpath, imgDirArg.getValue().c_str());
			img_fol->set_imgdir = 1;
		}

		if (outDirArg.isSet()) {
			if (out_fol) {
				out_fol->imgdirpath = (char*)malloc(strlen(outDirArg.getValue().c_str()) + 1);
				strcpy(out_fol->imgdirpath, outDirArg.getValue().c_str());
				out_fol->set_imgdir = 1;
			}
		}

		if (reduceArg.isSet()) {
			parameters->core.cp_reduce = reduceArg.getValue();
		}
		if (layerArg.isSet()) {
			parameters->core.cp_layer = layerArg.getValue();
		}
		if (tileArg.isSet()) {
			parameters->tile_index = tileArg.getValue();
			parameters->nb_tile_to_decode = 1;
		}
		if (precisionArg.isSet()) {
			if (!parse_precision(precisionArg.getValue().c_str(), parameters))
				return 1;
		}
		if (numThreadsArg.isSet()) {
			parameters->core.numThreads = numThreadsArg.getValue();
		}

		if (decodeRegionArg.isSet()) {
			size_t size_optarg = (size_t)strlen(decodeRegionArg.getValue().c_str()) + 1U;
			char *ROI_values = (char*)malloc(size_optarg);
			if (ROI_values == NULL) {
				fprintf(stderr, "[ERROR] Couldn't allocate memory\n");
				return 1;
			}
			ROI_values[0] = '\0';
			memcpy(ROI_values, decodeRegionArg.getValue().c_str(), size_optarg);
			/*printf("ROI_values = %s [%d / %d]\n", ROI_values, strlen(ROI_values), size_optarg ); */
			parse_DA_values(ROI_values, &parameters->DA_x0, &parameters->DA_y0, &parameters->DA_x1, &parameters->DA_y1);
			free(ROI_values);
		}

		if (pluginPathArg.isSet()) {
			if (plugin_path)
				strcpy(plugin_path, pluginPathArg.getValue().c_str());
		}

		if (repetitionsArg.isSet()) {
			parameters->repeats = repetitionsArg.getValue();
		}

		if (kernelBuildOptionsArg.isSet()) {
			parameters->kernelBuildOptions = kernelBuildOptionsArg.getValue();
		}

		if (deviceIdArg.isSet()) {
			parameters->deviceId = deviceIdArg.getValue();
		}

		if (durationArg.isSet()) {
			parameters->duration = durationArg.getValue();
		}


		if (verboseArg.isSet()) {
			parameters->verbose = verboseArg.getValue();
		}

	}
	catch (ArgException &e)  // catch any exceptions
	{
		cerr << "error: " << e.error() << " for arg " << e.argId() << endl;
	}
#if 0
    case 'h': 			/* display an help description */
        decode_help_display();
        return 1;
#endif

    /* check for possible errors */
    if(img_fol->set_imgdir==1) {
        if(!(parameters->infile[0]==0)) {
            fprintf(stderr, "[ERROR] options -ImgDir and -i cannot be used together.\n");
            return 1;
        }
        if(img_fol->set_out_format == 0) {
            fprintf(stderr, "[ERROR] When -ImgDir is used, -OutFor <FORMAT> must be used.\n");
            fprintf(stderr, "Only one format allowed.\n"
                    "Valid format are PGM, PPM, PNM, PGX, BMP, TIF, RAW and TGA.\n");
            return 1;
        }
        if(!((parameters->outfile[0] == 0))) {
            fprintf(stderr, "[ERROR] options -ImgDir and -o cannot be used together.\n");
            return 1;
        }
    } else {
        if((parameters->infile[0] == 0) || (parameters->outfile[0] == 0)) {
            fprintf(stderr, "[ERROR] Required parameters are missing\n"
                    "Example: %s -i image.j2k -o image.pgm\n",argv[0]);
            fprintf(stderr, "   Help: %s -h\n",argv[0]);
            return 1;
        }
    }
    return 0;
}

/* -------------------------------------------------------------------------- */
/**
 * Parse decoding area input values
 * separator = ","
 */
/* -------------------------------------------------------------------------- */
int parse_DA_values( char* inArg, uint32_t *DA_x0, uint32_t *DA_y0, uint32_t *DA_x1, uint32_t *DA_y1)
{
    int it = 0;
    int values[4];
    char delims[] = ",";
    char *result = NULL;
    result = strtok( inArg, delims );

    while( (result != NULL) && (it < 4 ) ) {
        values[it] = atoi(result);
        result = strtok( NULL, delims );
        it++;
    }

	// region must be specified by 4 values exactly
	if (it != 4) {
		fprintf(stdout, "[WARNING] Decode region must be specified by exactly four coordinates. Ignoring specified region\n");
		return EXIT_FAILURE;

	}

	// don't allow negative values
    if ((values[0] < 0 ||
			values[1] < 0 ||
				values[2] < 0 ||
					values[3] < 0)) {
		fprintf(stdout, "[WARNING] Decode region cannot contain negative values. Ignoring specified region (%d,%d,%d,%d).\n",
																						values[0],values[1],values[2],values[3]);
		return EXIT_FAILURE;
	}
	else {
        *DA_x0 = values[0];
        *DA_y0 = values[1];
        *DA_x1 = values[2];
        *DA_y1 = values[3];
        return EXIT_SUCCESS;
    }
}

double grk_clock(void)
{
#ifdef _WIN32
    /* _WIN32: use QueryPerformance (very accurate) */
    LARGE_INTEGER freq , t ;
    /* freq is the clock speed of the CPU */
    QueryPerformanceFrequency(&freq) ;
    /* cout << "freq = " << ((double) freq.QuadPart) << endl; */
    /* t is the high resolution performance counter (see MSDN) */
    QueryPerformanceCounter ( & t ) ;
    return freq.QuadPart ? ((double)t.QuadPart / (double)freq.QuadPart) : 0;
#else
    /* Unix or Linux: use resource usage */
    struct rusage t;
    double procTime;
    /* (1) Get the rusage data structure at this moment (man getrusage) */
    getrusage(0,&t);
    /* (2) What is the elapsed time ? - CPU time = User time + System time */
    /* (2a) Get the seconds */
    procTime = (double)(t.ru_utime.tv_sec + t.ru_stime.tv_sec);
    /* (2b) More precisely! Get the microseconds part ! */
    return ( procTime + (double)(t.ru_utime.tv_usec + t.ru_stime.tv_usec) * 1e-6 ) ;
#endif
}

/* -------------------------------------------------------------------------- */


static void set_default_parameters(opj_decompress_parameters* parameters)
{
    if (parameters) {
        memset(parameters, 0, sizeof(opj_decompress_parameters));

        /* default decoding parameters (command line specific) */
        parameters->decod_format = -1;
        parameters->cod_format = -1;

        /* default decoding parameters (core) */
        opj_set_default_decoder_parameters(&(parameters->core));

		parameters->numThreads = 8;
		parameters->deviceId = -1;
		parameters->repeats = 1;
		parameters->compressionLevel = DECOMPRESS_COMPRESSION_LEVEL_DEFAULT;
    }

}

static void destroy_parameters(opj_decompress_parameters* parameters)
{
    if (parameters) {
        if (parameters->precision) {
            free(parameters->precision);
            parameters->precision = NULL;
        }
    }
}

/* -------------------------------------------------------------------------- */

static opj_image_t* convert_gray_to_rgb(opj_image_t* original)
{
	if (original->numcomps == 0)
		return nullptr;
    uint32_t compno;
    opj_image_t* l_new_image = NULL;
    opj_image_cmptparm_t* l_new_components = NULL;

    l_new_components = (opj_image_cmptparm_t*)malloc((original->numcomps + 2U) * sizeof(opj_image_cmptparm_t));
    if (l_new_components == NULL) {
        fprintf(stderr, "ERROR -> opj_decompress: failed to allocate memory for RGB image!\n");
        opj_image_destroy(original);
        return NULL;
    }

    l_new_components[0].dx   = l_new_components[1].dx   = l_new_components[2].dx   = original->comps[0].dx;
    l_new_components[0].dy   = l_new_components[1].dy   = l_new_components[2].dy   = original->comps[0].dy;
    l_new_components[0].h    = l_new_components[1].h    = l_new_components[2].h    = original->comps[0].h;
    l_new_components[0].w    = l_new_components[1].w    = l_new_components[2].w    = original->comps[0].w;
    l_new_components[0].prec = l_new_components[1].prec = l_new_components[2].prec = original->comps[0].prec;
    l_new_components[0].sgnd = l_new_components[1].sgnd = l_new_components[2].sgnd = original->comps[0].sgnd;
    l_new_components[0].x0   = l_new_components[1].x0   = l_new_components[2].x0   = original->comps[0].x0;
    l_new_components[0].y0   = l_new_components[1].y0   = l_new_components[2].y0   = original->comps[0].y0;

    for(compno = 1U; compno < original->numcomps; ++compno) {
        l_new_components[compno+2U].dx   = original->comps[compno].dx;
        l_new_components[compno+2U].dy   = original->comps[compno].dy;
        l_new_components[compno+2U].h    = original->comps[compno].h;
        l_new_components[compno+2U].w    = original->comps[compno].w;
        l_new_components[compno+2U].prec = original->comps[compno].prec;
        l_new_components[compno+2U].sgnd = original->comps[compno].sgnd;
        l_new_components[compno+2U].x0   = original->comps[compno].x0;
        l_new_components[compno+2U].y0   = original->comps[compno].y0;
    }

    l_new_image = opj_image_create(original->numcomps + 2U, l_new_components, OPJ_CLRSPC_SRGB);
    free(l_new_components);
    if (l_new_image == NULL) {
        fprintf(stderr, "ERROR -> opj_decompress: failed to allocate memory for RGB image!\n");
        opj_image_destroy(original);
        return NULL;
    }

    l_new_image->x0 = original->x0;
    l_new_image->x1 = original->x1;
    l_new_image->y0 = original->y0;
    l_new_image->y1 = original->y1;

    l_new_image->comps[0].decodeScaleFactor        = l_new_image->comps[1].decodeScaleFactor        = l_new_image->comps[2].decodeScaleFactor        = original->comps[0].decodeScaleFactor;
    l_new_image->comps[0].alpha         = l_new_image->comps[1].alpha         = l_new_image->comps[2].alpha         = original->comps[0].alpha;
    l_new_image->comps[0].resno_decoded = l_new_image->comps[1].resno_decoded = l_new_image->comps[2].resno_decoded = original->comps[0].resno_decoded;

    memcpy(l_new_image->comps[0].data, original->comps[0].data, original->comps[0].w * original->comps[0].h * sizeof(int32_t));
    memcpy(l_new_image->comps[1].data, original->comps[0].data, original->comps[0].w * original->comps[0].h * sizeof(int32_t));
    memcpy(l_new_image->comps[2].data, original->comps[0].data, original->comps[0].w * original->comps[0].h * sizeof(int32_t));

    for(compno = 1U; compno < original->numcomps; ++compno) {
        l_new_image->comps[compno+2U].decodeScaleFactor        = original->comps[compno].decodeScaleFactor;
        l_new_image->comps[compno+2U].alpha         = original->comps[compno].alpha;
        l_new_image->comps[compno+2U].resno_decoded = original->comps[compno].resno_decoded;
        memcpy(l_new_image->comps[compno+2U].data, original->comps[compno].data, original->comps[compno].w * original->comps[compno].h * sizeof(int32_t));
    }
    opj_image_destroy(original);
    return l_new_image;
}

/* -------------------------------------------------------------------------- */

static opj_image_t* upsample_image_components(opj_image_t* original)
{
    opj_image_t* l_new_image = NULL;
    opj_image_cmptparm_t* l_new_components = NULL;
    bool l_upsample_need = false;
    uint32_t compno;

	if (!original || !original->comps)
		return nullptr;

    for (compno = 0U; compno < original->numcomps; ++compno) {
		if (!(original->comps+compno))
			return nullptr;
        if (original->comps[compno].decodeScaleFactor > 0U) {
            fprintf(stderr, "ERROR -> opj_decompress: -upsample not supported with reduction\n");
            opj_image_destroy(original);
            return NULL;
        }
        if ((original->comps[compno].dx > 1U) || (original->comps[compno].dy > 1U)) {
            l_upsample_need = true;
            break;
        }
    }
    if (!l_upsample_need) {
        return original;
    }
    /* Upsample is needed */
    l_new_components = (opj_image_cmptparm_t*)malloc(original->numcomps * sizeof(opj_image_cmptparm_t));
    if (l_new_components == NULL) {
        fprintf(stderr, "ERROR -> opj_decompress: failed to allocate memory for upsampled components!\n");
        opj_image_destroy(original);
        return NULL;
    }

    for (compno = 0U; compno < original->numcomps; ++compno) {
        opj_image_cmptparm_t* l_new_cmp = &(l_new_components[compno]);
        opj_image_comp_t*     l_org_cmp = &(original->comps[compno]);

        l_new_cmp->prec = l_org_cmp->prec;
        l_new_cmp->sgnd = l_org_cmp->sgnd;
        l_new_cmp->x0   = original->x0;
        l_new_cmp->y0   = original->y0;
        l_new_cmp->dx   = 1;
        l_new_cmp->dy   = 1;
        l_new_cmp->w    = l_org_cmp->w; /* should be original->x1 - original->x0 for dx==1 */
        l_new_cmp->h    = l_org_cmp->h; /* should be original->y1 - original->y0 for dy==0 */

        if (l_org_cmp->dx > 1U) {
            l_new_cmp->w = original->x1 - original->x0;
        }

        if (l_org_cmp->dy > 1U) {
            l_new_cmp->h = original->y1 - original->y0;
        }
    }

    l_new_image = opj_image_create(original->numcomps, l_new_components, original->color_space);
    free(l_new_components);
    if (l_new_image == NULL) {
        fprintf(stderr, "ERROR -> opj_decompress: failed to allocate memory for upsampled components!\n");
        opj_image_destroy(original);
        return NULL;
    }

    l_new_image->x0 = original->x0;
    l_new_image->x1 = original->x1;
    l_new_image->y0 = original->y0;
    l_new_image->y1 = original->y1;

    for (compno = 0U; compno < original->numcomps; ++compno) {
        opj_image_comp_t* l_new_cmp = &(l_new_image->comps[compno]);
        opj_image_comp_t* l_org_cmp = &(original->comps[compno]);

        l_new_cmp->decodeScaleFactor        = l_org_cmp->decodeScaleFactor;
        l_new_cmp->alpha         = l_org_cmp->alpha;
        l_new_cmp->resno_decoded = l_org_cmp->resno_decoded;

        if ((l_org_cmp->dx > 1U) || (l_org_cmp->dy > 1U)) {
            const int32_t* l_src = l_org_cmp->data;
            int32_t*       l_dst = l_new_cmp->data;
            uint32_t y;
            uint32_t xoff, yoff;

            /* need to take into account dx & dy */
            xoff = l_org_cmp->dx * l_org_cmp->x0 -  original->x0;
            yoff = l_org_cmp->dy * l_org_cmp->y0 -  original->y0;
            if ((xoff >= l_org_cmp->dx) || (yoff >= l_org_cmp->dy)) {
                fprintf(stderr, "ERROR -> opj_decompress: Invalid image/component parameters found when upsampling\n");
                opj_image_destroy(original);
                opj_image_destroy(l_new_image);
                return NULL;
            }

            for (y = 0U; y < yoff; ++y) {
                memset(l_dst, 0U, l_new_cmp->w * sizeof(int32_t));
                l_dst += l_new_cmp->w;
            }

            if(l_new_cmp->h > (l_org_cmp->dy - 1U)) { /* check subtraction overflow for really small images */
                for (; y < l_new_cmp->h - (l_org_cmp->dy - 1U); y += l_org_cmp->dy) {
                    uint32_t x, dy;
                    uint32_t xorg;

                    xorg = 0U;
                    for (x = 0U; x < xoff; ++x) {
                        l_dst[x] = 0;
                    }
                    if (l_new_cmp->w > (l_org_cmp->dx - 1U)) { /* check subtraction overflow for really small images */
                        for (; x < l_new_cmp->w - (l_org_cmp->dx - 1U); x += l_org_cmp->dx, ++xorg) {
                            uint32_t dx;
                            for (dx = 0U; dx < l_org_cmp->dx; ++dx) {
                                l_dst[x + dx] = l_src[xorg];
                            }
                        }
                    }
                    for (; x < l_new_cmp->w; ++x) {
                        l_dst[x] = l_src[xorg];
                    }
                    l_dst += l_new_cmp->w;

                    for (dy = 1U; dy < l_org_cmp->dy; ++dy) {
                        memcpy(l_dst, l_dst - l_new_cmp->w, l_new_cmp->w * sizeof(int32_t));
                        l_dst += l_new_cmp->w;
                    }
                    l_src += l_org_cmp->w;
                }
            }
            if (y < l_new_cmp->h) {
                uint32_t x;
                uint32_t xorg;

                xorg = 0U;
                for (x = 0U; x < xoff; ++x) {
                    l_dst[x] = 0;
                }
                if (l_new_cmp->w > (l_org_cmp->dx - 1U)) { /* check subtraction overflow for really small images */
                    for (; x < l_new_cmp->w - (l_org_cmp->dx - 1U); x += l_org_cmp->dx, ++xorg) {
                        uint32_t dx;
                        for (dx = 0U; dx < l_org_cmp->dx; ++dx) {
                            l_dst[x + dx] = l_src[xorg];
                        }
                    }
                }
                for (; x < l_new_cmp->w; ++x) {
                    l_dst[x] = l_src[xorg];
                }
                l_dst += l_new_cmp->w;
                ++y;
                for (; y < l_new_cmp->h; ++y) {
                    memcpy(l_dst, l_dst - l_new_cmp->w, l_new_cmp->w * sizeof(int32_t));
                    l_dst += l_new_cmp->w;
                }
            }
        } else {
            memcpy(l_new_cmp->data, l_org_cmp->data, l_org_cmp->w * l_org_cmp->h * sizeof(int32_t));
        }
    }
    opj_image_destroy(original);
    return l_new_image;
}

bool store_file_to_disk = true;

#ifdef OPJ_HAVE_LIBLCMS
void MycmsLogErrorHandlerFunction(cmsContext ContextID, cmsUInt32Number ErrorCode, const char *Text) {
	fprintf(stdout, "[WARNING] LCMS error: %s\n", Text);
}
#endif


struct DecompressInitParams {
	DecompressInitParams() : initialized(false) {
		plugin_path[0] = 0;
		memset(&img_fol, 0, sizeof(img_fol));
		memset(&out_fol, 0, sizeof(out_fol));
	}

	~DecompressInitParams() {
		if (img_fol.imgdirpath)
			free(img_fol.imgdirpath);
		if (out_fol.imgdirpath)
			free(out_fol.imgdirpath);
	}
	bool initialized;

	opj_decompress_parameters parameters;	/* compression parameters */
	char plugin_path[OPJ_PATH_LEN];

	img_fol_t img_fol;
	img_fol_t out_fol;

};

static int plugin_pre_decode_callback(opj_plugin_decode_callback_info_t* info);
static int plugin_post_decode_callback(opj_plugin_decode_callback_info_t* info);
static int plugin_main(int argc, char **argv, DecompressInitParams* initParams);


int main(int argc, char **argv)
{

#ifndef NDEBUG
	std::string out;
	for (int i = 0; i < argc; ++i) {
		out += std::string(" ") + argv[i];
	}
	out += "\n";
	printf(out.c_str());
#endif



	int32_t num_images, imageno = 0;
	dircnt_t *dirptr = nullptr;
	int rc = EXIT_SUCCESS;
	double t_cumulative = 0;
	uint32_t num_decompressed_images = 0;

	DecompressInitParams initParams;
	// try to encode with plugin
	int plugin_rc = plugin_main(argc, argv, &initParams);

	// return immediately if either 
	// initParams was not initialized (something was wrong with command line params)
	// or
	// plugin was successful
	if (!initParams.initialized) {
		rc = EXIT_FAILURE;
		goto cleanup;
	}
	if (plugin_rc == EXIT_SUCCESS) {
		rc = EXIT_SUCCESS;
		goto cleanup;
	}
    /* Initialize reading of directory */
    if(initParams.img_fol.set_imgdir==1) {
        int it_image;
        num_images=get_num_images(initParams.img_fol.imgdirpath);
		if (num_images <= 0) {
			fprintf(stderr, "Folder is empty\n");
			rc = EXIT_FAILURE;
			goto cleanup;
		}

        dirptr=(dircnt_t*)malloc(sizeof(dircnt_t));
        if(dirptr) {
            dirptr->filename_buf = (char*)malloc((size_t)num_images*OPJ_PATH_LEN);	/* Stores at max 10 image file names*/
			if (!dirptr->filename_buf) {
				rc = EXIT_FAILURE;
				goto cleanup;
			}

            dirptr->filename = (char**) malloc((size_t)num_images*sizeof(char*));
			if (!dirptr->filename) {
				rc = EXIT_FAILURE;
				goto cleanup;
			}
            for(it_image=0; it_image<num_images; it_image++) {
                dirptr->filename[it_image] = dirptr->filename_buf + it_image*OPJ_PATH_LEN;
            }
        }
        if(load_images(dirptr, initParams.img_fol.imgdirpath, initParams.parameters.verbose)==1) {
			rc = EXIT_FAILURE;
			goto cleanup;
        }
    } else {
        num_images=1;
    }

    t_cumulative = grk_clock();

    /*Decoding image one by one*/
    for (imageno = 0; imageno < num_images; imageno++) {
		if (initParams.parameters.verbose)
			fprintf(stdout, "\n");
        if (initParams.img_fol.set_imgdir == 1) {
			if (get_next_file(imageno, dirptr, &initParams.img_fol, initParams.out_fol.set_imgdir ? &initParams.out_fol : &initParams.img_fol, &initParams.parameters)) {
				if (initParams.parameters.verbose)
					fprintf(stdout, "skipping file...\n");
				continue;
			}
        }

		opj_plugin_decode_callback_info_t info;
		memset(&info, 0, sizeof(opj_plugin_decode_callback_info_t));
		info.decoder_parameters = &initParams.parameters;

		if (plugin_pre_decode_callback(&info)) {
			rc = EXIT_FAILURE;
			continue;
		}
		if (plugin_post_decode_callback(&info)) {
			rc = EXIT_FAILURE;
			continue;
		}
		num_decompressed_images++;
    }
    t_cumulative = grk_clock() - t_cumulative;
    if (initParams.parameters.verbose && num_decompressed_images && rc != EXIT_FAILURE) {
        fprintf(stdout, "decode time: %d ms \n", (int)( (t_cumulative * 1000) / num_decompressed_images));
    }
cleanup:
	if (dirptr) {
		if (dirptr->filename_buf)
			free(dirptr->filename_buf);
		if (dirptr->filename)
			free(dirptr->filename);
		free(dirptr);
	}
	destroy_parameters(&initParams.parameters);
	opj_cleanup();
	return rc;
}


int plugin_main(int argc, char **argv, DecompressInitParams* initParams)
{
	int32_t num_images, imageno = 0;
	dircnt_t *dirptr = nullptr;
	int rc = EXIT_SUCCESS;
	double t_cumulative = 0;
	uint32_t num_decompressed_images = 0;

#ifdef OPJ_HAVE_LIBLCMS
	cmsSetLogErrorHandler(MycmsLogErrorHandlerFunction);
#endif

	/* set decoding parameters to default values */
	set_default_parameters(&initParams->parameters);

	/* parse input and get user encoding parameters */
	if (parse_cmdline_decoder(argc, argv, &initParams->parameters, &initParams->img_fol, &initParams->out_fol, initParams->plugin_path) == 1) {
		return EXIT_FAILURE;
	}

#ifdef OPJ_HAVE_LIBTIFF
	tiffSetErrorAndWarningHandlers(initParams->parameters.verbose);
#endif
	initParams->initialized = true;

	// loads plugin but does not actually create codec
	if (!opj_initialize(initParams->plugin_path))
		return EXIT_FAILURE;


	// create codec
	opj_plugin_init_info_t initInfo;
	initInfo.deviceId = initParams->parameters.deviceId;
	if (!opj_plugin_init(initInfo)) {
		rc = EXIT_FAILURE;
		goto cleanup;
	}

	/* Initialize reading of directory */
	if (initParams->img_fol.set_imgdir == 1) {
		num_images = get_num_images(initParams->img_fol.imgdirpath);
		if (num_images <= 0) {
			fprintf(stderr, "Folder is empty\n");
			rc = EXIT_FAILURE;
			goto cleanup;
		}
		dirptr = (dircnt_t*)malloc(sizeof(dircnt_t));
		if (dirptr) {
			dirptr->filename_buf = (char*)malloc((size_t)num_images*OPJ_PATH_LEN);	/* Stores at max 10 image file names*/
			if (!dirptr->filename_buf) {
				rc =  EXIT_FAILURE;
				goto cleanup;
			}
			dirptr->filename = (char**)malloc(num_images * sizeof(char*));
			if (!dirptr->filename) {
				rc = EXIT_FAILURE;
				goto cleanup;
			}

			for (int it_image = 0; it_image<num_images; it_image++) {
				dirptr->filename[it_image] = dirptr->filename_buf + it_image*OPJ_PATH_LEN;
			}
		}
		if (load_images(dirptr, initParams->img_fol.imgdirpath, initParams->parameters.verbose) == 1) {
			rc = EXIT_FAILURE;
			goto cleanup;
		}
	}
	else {
		num_images = 1;
	}

	t_cumulative = grk_clock();

	/*Decoding image one by one*/
	for (imageno = 0; imageno < num_images; imageno++) {
		fprintf(stderr, "\n");

		if (initParams->img_fol.set_imgdir == 1) {
			if (get_next_file(imageno, dirptr, &initParams->img_fol, initParams->out_fol.set_imgdir ? &initParams->out_fol : &initParams->img_fol, &initParams->parameters)) {
				if (initParams->parameters.verbose)
					fprintf(stdout, "skipping file...\n");
				continue;
			}
		}

		//1. try to decode using plugin
		rc = opj_plugin_decode(&initParams->parameters, plugin_pre_decode_callback, plugin_post_decode_callback);

		//2. fallback
		if (rc == -1 || rc == EXIT_FAILURE) {
			rc = EXIT_FAILURE;
			goto cleanup;
		}
		num_decompressed_images++;

	}
	t_cumulative = grk_clock() - t_cumulative;
	if (initParams->parameters.verbose && num_decompressed_images && rc == EXIT_SUCCESS) {
		fprintf(stdout, "decode time: %d ms \n", (int)((t_cumulative * 1000) / num_decompressed_images));
	}
cleanup:
	opj_cleanup();
	if (dirptr) {
		if (dirptr->filename_buf) 
			free(dirptr->filename_buf);
		if (dirptr->filename) 
			free(dirptr->filename);
		free(dirptr);
	}
	return rc;
}


int plugin_pre_decode_callback(opj_plugin_decode_callback_info_t* info) {
	if (!info)
		return 1;
	int failed = 0;
	opj_decompress_parameters* parameters = info->decoder_parameters;
	opj_image_t* image = NULL;

	/* read the input file and put it in memory */
	/* ---------------------------------------- */
	// use file stream 
	info->l_stream = opj_stream_create_default_file_stream(parameters->infile, true);

	// other option is to use memory mapped stream
	//info->l_stream = opj_stream_create_mapped_file_read_stream(parameters->infile);


	// third option is to read from buffer
	/*
	auto fp = fopen(parameters->infile, "rb");
	if (!fp) {
		fprintf(stderr, "ERROR -> opj_decompress: unable to open file %s for reading", parameters->infile);
		failed = 1;
		goto cleanup;
	}

	auto rc = fseek(fp, 0, SEEK_END);
	if (rc == -1) {
		fprintf(stderr, "ERROR -> opj_decompress: unable to seek on file %s", parameters->infile);
		fclose(fp);
		failed = 1;
		goto cleanup;
	}
	auto lengthOfFile = ftell(fp);
	if (lengthOfFile <= 0) {
		fprintf(stderr, "ERROR -> opj_decompress: Zero or negative length for file %s", parameters->infile);
		fclose(fp);
		failed = 1;
		goto cleanup;
	}
	rewind(fp);
	buffer = new uint8_t[lengthOfFile];
	size_t bytesRead = 0;
	size_t totalBytes = 0;
	while (bytesRead = fread(buffer, 1, lengthOfFile, fp)) {
		totalBytes += bytesRead;
	}
	fclose(fp);
	if (totalBytes != lengthOfFile) {
		fprintf(stderr, "ERROR -> opj_decompress: Unable to read full length of file %s", parameters->infile);
		failed = 1;
		goto cleanup;
	}
	info->l_stream = opj_stream_create_buffer_stream(buffer, lengthOfFile, true);
	*/



	if (!info->l_stream) {
		fprintf(stderr, "ERROR -> failed to create the stream from the file %s\n", parameters->infile);
		failed = 1;
		goto cleanup;
	}

	/* decode the JPEG2000 stream */
	/* ---------------------- */

	switch (parameters->decod_format) {
	case J2K_CFMT: {	/* JPEG-2000 codestream */
						/* Get a decoder handle */
		info->l_codec = opj_create_decompress(OPJ_CODEC_J2K);
		break;
	}
	case JP2_CFMT: {	/* JPEG 2000 compressed image data */
						/* Get a decoder handle */
		info->l_codec = opj_create_decompress(OPJ_CODEC_JP2);
		break;
	}
	case JPT_CFMT: {	/* JPEG 2000, JPIP */
						/* Get a decoder handle */
		info->l_codec = opj_create_decompress(OPJ_CODEC_JPT);
		break;
	}
	default:
		if (parameters->verbose)
			fprintf(stdout, "skipping file..\n");
		failed = 1;
		goto cleanup;
	}

	/* catch events using our callbacks and give a local context */
	opj_set_info_handler(info->l_codec, info_callback, &parameters->verbose);
	opj_set_warning_handler(info->l_codec, warning_callback, &parameters->verbose);
	opj_set_error_handler(info->l_codec, error_callback, nullptr);

	/* Setup the decoder decoding parameters using user parameters */
	if (!opj_setup_decoder(info->l_codec, &(parameters->core))) {
		fprintf(stderr, "ERROR -> opj_decompress: failed to setup the decoder\n");
		failed = 1;
		goto cleanup;
	}

	opj_header_info_t header_info;
	memset(&header_info, 0, sizeof(opj_header_info_t));

	/* Read the main header of the codestream and if necessary the JP2 boxes*/
	if (!opj_read_header_ex(info->l_stream, info->l_codec, &header_info, &image)) {
		info->image = image;
		fprintf(stderr, "ERROR -> opj_decompress: failed to read the header\n");
		failed = 1;
		goto cleanup;
	}

	// store XML to file
	if (header_info.xml_data && header_info.xml_data_len && parameters->serialize_xml) {
		std::string xmlFile = std::string(parameters->outfile) + ".xml";
		auto fp = fopen(xmlFile.c_str(), "wb");
		if (!fp) {
			fprintf(stderr, "ERROR -> opj_decompress: unable to open file %s for writing", xmlFile.c_str());
			failed = 1;
			goto cleanup;
		}
		if (fwrite(header_info.xml_data, 1, header_info.xml_data_len, fp) != header_info.xml_data_len) {
			fprintf(stderr, "ERROR -> opj_decompress: unable to write all data to file file %s for writing", xmlFile.c_str());
			fclose(fp);
			failed = 1;
			goto cleanup;
		}
		fclose(fp);
	}

	info->image = image;

	// limit to 16 bit precision
	for (uint32_t i = 0; i < image->numcomps; ++i) {
		if (image->comps[i].prec > 16) {
			fprintf(stderr, "ERROR -> opj_decompress: Precision = %d not supported:\n", image->comps[i].prec);
			failed = 1;
			goto cleanup;
		}
	}
	if (info->generate_tile_func) {
		info->tile = info->generate_tile_func(info->deviceId,
												info->compressed_tile_id,
												&header_info,
												image);
		info->tile->decode_flag = OPJ_PLUGIN_DECODE_T2;
	}

	/* It is just here to illustrate how to use the resolution after set parameters */
	/*
	if (!opj_set_decoded_resolution_factor(info->l_codec, 0)) {
		fprintf(stderr, "ERROR -> opj_decompress: failed to set the resolution factor tile!\n");
		return -1;
	}
	*/

	/* Optional if you want decode the entire image */
	if (!opj_set_decode_area(info->l_codec, image, parameters->DA_x0,
		parameters->DA_y0,
		parameters->DA_x1,
		parameters->DA_y1)) {
		fprintf(stderr, "ERROR -> opj_decompress: failed to set the decoded area\n");
		failed = 1;
		goto cleanup;
	}

	if (!parameters->nb_tile_to_decode) {
		/* Get the decoded image */
		if (!(opj_decode_ex(info->l_codec,info->tile, info->l_stream, image) && opj_end_decompress(info->l_codec, info->l_stream))) {
			fprintf(stderr, "ERROR -> opj_decompress: failed to decode image!\n");
			failed = 1;
			goto cleanup;
		}
	}
	else {
		if (!opj_get_decoded_tile(info->l_codec, info->l_stream, image, parameters->tile_index)) {
			fprintf(stderr, "ERROR -> opj_decompress: failed to decode tile!\n");
			failed = 1;
			goto cleanup;
		}
		if (parameters->verbose)
			fprintf(stdout, "tile %d is decoded!\n\n", parameters->tile_index);
	}

cleanup:
	if (info->l_stream)
		opj_stream_destroy(info->l_stream);
	info->l_stream = NULL;
	if (info->l_codec)
		opj_destroy_codec(info->l_codec);
	info->l_codec = NULL;
	if (failed) {
		if (image)
			opj_image_destroy(image);
		info->image = NULL;
	}
	return failed;
}

/*
Post-process decompressed image and store in selected image format
*/
int plugin_post_decode_callback(opj_plugin_decode_callback_info_t* info) {
	if (!info)
		return -1;

	int failed = 0;
	opj_decompress_parameters* parameters = info->decoder_parameters;
	opj_image_t* image = info->image;

	
	if (info->tile) {
		info->tile->decode_flag = OPJ_PLUGIN_DECODE_POST_T1 ;
		/* Get the decoded image */
		if (!(opj_decode_ex(info->l_codec, info->tile, info->l_stream, image) && opj_end_decompress(info->l_codec, info->l_stream))) {
			fprintf(stderr, "ERROR -> opj_decompress: failed to decode image!\n");
			failed = 1;
			goto cleanup;
		}
	}

	/* Close the byte stream */
	if (info->l_stream)
		opj_stream_destroy(info->l_stream);
	info->l_stream = NULL;
	if (info->l_codec)
		opj_destroy_codec(info->l_codec);
	info->l_codec = NULL;

	// todo: destroy plugin tile

	if (image->color_space != OPJ_CLRSPC_SYCC
		&& image->numcomps == 3 && image->comps[0].dx == image->comps[0].dy
		&& image->comps[1].dx != 1)
		image->color_space = OPJ_CLRSPC_SYCC;
	else if (image->numcomps <= 2)
		image->color_space = OPJ_CLRSPC_GRAY;

	if (image->color_space == OPJ_CLRSPC_SYCC) {
		color_sycc_to_rgb(image);
	}
	else if ((image->color_space == OPJ_CLRSPC_CMYK) && (parameters->cod_format != TIF_DFMT)) {
		if (color_cmyk_to_rgb(image)) {
			fprintf(stderr, "ERROR -> opj_decompress: CMYK to RGB colour conversion failed !\n");
			failed = 1;
			goto cleanup;
		}
	}
	else if (image->color_space == OPJ_CLRSPC_EYCC) {
		if (color_esycc_to_rgb(image)) {
			fprintf(stderr, "ERROR -> opj_decompress: eSYCC to RGB colour conversion failed !\n");
			failed = 1;
			goto cleanup;
		}
	}

	// A TIFF or PNG image can store the ICC profile, so no need to apply it in this case,
	// (unless we are forcing to RGB). Otherwise, we apply the profile
	if (image->icc_profile_buf && 
		(info->decoder_parameters->force_rgb || (info->decoder_parameters->cod_format != TIF_DFMT &&	info->decoder_parameters->cod_format != PNG_DFMT) ) ) {
#if defined(OPJ_HAVE_LIBLCMS)
		if (image->icc_profile_len) {
			color_apply_icc_profile(image, info->decoder_parameters->force_rgb);
		}
		else {
			color_cielab_to_rgb(image);
		}
		free(image->icc_profile_buf);
		image->icc_profile_buf = NULL;
		image->icc_profile_len = 0;
#endif
	}

	/* Force output precision */
	/* ---------------------- */
	if (parameters->precision != NULL) {
		uint32_t compno;
		for (compno = 0; compno < image->numcomps; ++compno) {
			uint32_t precno = compno;
			uint32_t prec;

			if (precno >= parameters->nb_precision) {
				precno = parameters->nb_precision - 1U;
			}

			prec = parameters->precision[precno].prec;
			if (prec == 0) {
				prec = image->comps[compno].prec;
			}

			switch (parameters->precision[precno].mode) {
			case OPJ_PREC_MODE_CLIP:
				clip_component(&(image->comps[compno]), prec);
				break;
			case OPJ_PREC_MODE_SCALE:
				scale_component(&(image->comps[compno]), prec);
				break;
			default:
				break;
			}
		}
	}

	/* Upsample components */
	/* ------------------- */
	if (parameters->upsample) {
		image = upsample_image_components(image);
		if (image == NULL) {
			fprintf(stderr, "ERROR -> opj_decompress: failed to upsample image components!\n");
			failed = 1;
			goto cleanup;
		}
	}

	/* Force RGB output */
	/* ---------------- */
	if (parameters->force_rgb) {
		switch (image->color_space) {
		case OPJ_CLRSPC_SRGB:
			break;
		case OPJ_CLRSPC_GRAY:
			image = convert_gray_to_rgb(image);
			break;
		default:
			fprintf(stderr, "ERROR -> opj_decompress: don't know how to convert image to RGB colorspace!\n");
			opj_image_destroy(image);
			image = NULL;
			failed = 1;
			goto cleanup;
		}
		if (image == NULL) {
			fprintf(stderr, "ERROR -> opj_decompress: failed to convert to RGB image!\n");
			goto cleanup;
		}
	}

	if (store_file_to_disk) {
		/* create output image */
		/* ------------------- */
		switch (parameters->cod_format) {
		case PXM_DFMT:			/* PNM PGM PPM */
			if (imagetopnm(image, parameters->outfile, parameters->split_pnm)) {
				fprintf(stderr, "[ERROR] Outfile %s not generated\n", parameters->outfile);
				failed = 1;
			}
			else {
				if (parameters->verbose)
					fprintf(stdout, "[INFO] Generated Outfile %s\n", parameters->outfile);
			}
			break;

		case PGX_DFMT:			/* PGX */
			if (imagetopgx(image, parameters->outfile)) {
				fprintf(stderr, "[ERROR] Outfile %s not generated\n", parameters->outfile);
				failed = 1;
			}
			else {
				if (parameters->verbose)
					fprintf(stdout, "[INFO] Generated Outfile %s\n", parameters->outfile);
			}
			break;

		case BMP_DFMT:			/* BMP */
			if (imagetobmp(image, parameters->outfile)) {
				fprintf(stderr, "[ERROR] Outfile %s not generated\n", parameters->outfile);
				failed = 1;
			}
			else {
				if (parameters->verbose)
					fprintf(stdout, "[INFO] Generated Outfile %s\n", parameters->outfile);
			}
			break;
#ifdef OPJ_HAVE_LIBTIFF
		case TIF_DFMT:			/* TIFF */
			if (imagetotif(image, parameters->outfile, parameters->compression, parameters->verbose)) {
				fprintf(stderr, "[ERROR] Outfile %s not generated\n", parameters->outfile);
				failed = 1;
			}
			else {
				if (parameters->verbose)
					fprintf(stdout, "[INFO] Generated Outfile %s\n", parameters->outfile);
			}
			break;
#endif /* OPJ_HAVE_LIBTIFF */
		case RAW_DFMT:			/* RAW */
			if (imagetoraw(image, parameters->outfile)) {
				fprintf(stderr, "[ERROR] Error generating raw file. Outfile %s not generated\n", parameters->outfile);
				failed = 1;
			}
			else {
				if (parameters->verbose)
					fprintf(stdout, "[INFO] Generated Outfile %s\n", parameters->outfile);
			}
			break;

		case RAWL_DFMT:			/* RAWL */
			if (imagetorawl(image, parameters->outfile)) {
				fprintf(stderr, "[ERROR] Error generating rawl file. Outfile %s not generated\n", parameters->outfile);
				failed = 1;
			}
			else {
				if (parameters->verbose)
					fprintf(stdout, "[INFO] Generated Outfile %s\n", parameters->outfile);
			}
			break;

		case TGA_DFMT:			/* TGA */
			if (imagetotga(image, parameters->outfile)) {
				fprintf(stderr, "[ERROR] Error generating tga file. Outfile %s not generated\n", parameters->outfile);
				failed = 1;
			}
			else {
				if (parameters->verbose)
					fprintf(stdout, "[INFO] Generated Outfile %s\n", parameters->outfile);
			}
			break;
#ifdef OPJ_HAVE_LIBPNG
		// case PNG_DFMT:			/* PNG */
		// 	if (imagetopng(image, parameters->outfile, parameters->compressionLevel)) {
		// 		fprintf(stderr, "[ERROR] Error generating png file. Outfile %s not generated\n", parameters->outfile);
		// 		failed = 1;
		// 	}
		// 	else {
		// 		if (parameters->verbose)
		// 			fprintf(stdout, "[INFO] Generated Outfile %s\n", parameters->outfile);
		// 	}
		// 	break;
#endif /* OPJ_HAVE_LIBPNG */
			/* Can happen if output file is TIFF or PNG
			* and OPJ_HAVE_LIBTIF or OPJ_HAVE_LIBPNG is undefined
			*/
		default:
			fprintf(stderr, "[ERROR] Outfile %s not generated\n", parameters->outfile);
			failed = 1;
			break;
		}
	}
cleanup:
	if (info->l_stream)
		opj_stream_destroy(info->l_stream);
	info->l_stream = NULL;
	if (info->l_codec)
		opj_destroy_codec(info->l_codec);
	info->l_codec = NULL;
	if (image)
		opj_image_destroy(image);
	info->image = NULL;
	if (failed)
		(void)remove(parameters->outfile); /* ignore return value */
	return failed;
}

