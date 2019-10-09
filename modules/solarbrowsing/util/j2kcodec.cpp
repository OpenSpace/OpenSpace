/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/solarbrowsing/util/j2kcodec.h>

#include <ghoul/logging/logmanager.h>
#include <chrono>
#include <cstring>
#include <fmt/format.h>
#include <format_defs.h>
#include <fstream>
#include <memory>
#include <vector>

#define JP2_RFC3745_MAGIC "\x00\x00\x00\x0c\x6a\x50\x20\x20\x0d\x0a\x87\x0a"
#define JP2_MAGIC "\x0d\x0a\x87\x0a"
#define J2K_CODESTREAM_MAGIC "\xff\x4f\xff\x51"

namespace {
    constexpr const char* _loggerCat = "J2kCodec";

    int infileFormat(const std::string& fname) {
        const auto get_file_format = [](const char* filename) {
            unsigned int i;
            static const char* extension[] = {
                "pgx", "pnm", "pgm", "ppm", "bmp", "tif", "raw", "tga", "png", "j2k", "jp2",
                "jpt", "j2c", "jpc"
            };
            static const int format[] = {
                PGX_DFMT, PXM_DFMT, PXM_DFMT, PXM_DFMT, BMP_DFMT, TIF_DFMT, RAW_DFMT,
                TGA_DFMT, PNG_DFMT, J2K_CFMT, JP2_CFMT, JPT_CFMT, J2K_CFMT, J2K_CFMT
            };

            const char* ext = strrchr(filename, '.');
            if (!ext) {
                return -1;
            }

            ++ext;
            if (ext) {
                for (i = 0; i < sizeof(format) / sizeof(*format); i++) {
                    if (strncmp(ext, extension[i], 3) == 0) {
                        return format[i];
                    }
                }
            }

            return -1;
        };

        FILE* reader = fopen(fname.c_str(), "rb");
        if (!reader) {
            return -1;
        }

        unsigned char buf[12];
        memset(buf, 0, 12);
        OPJ_SIZE_T l_nb_read = fread(buf, 1, 12, reader);
        fclose(reader);

        if (l_nb_read != 12) {
            return -1;
        }

        int ext_format = get_file_format(fname.c_str());

        if (ext_format == JPT_CFMT) {
            return JPT_CFMT;
        }

        int magic_format;
        const char* magic_s;
        if (memcmp(buf, JP2_RFC3745_MAGIC, 12) == 0 || memcmp(buf, JP2_MAGIC, 4) == 0) {
            magic_format = JP2_CFMT;
            magic_s = ".jp2";
        }
        else if (memcmp(buf, J2K_CODESTREAM_MAGIC, 4) == 0) {
            magic_format = J2K_CFMT;
            magic_s = ".j2k or .jpc or .j2c";
        }
        else {
            return -1;
        }

        if (magic_format == ext_format) {
            return ext_format;
        }

        // ghoul::filesystem::File().fileExtension?
        const char* s = fname.c_str() + strlen(fname.c_str()) - 4;
        LERROR(
            fmt::format("Extension of file is incorrect. Found {} should be {}", s, magic_s)
        );
        return magic_format;
    }
}

namespace openspace {

J2kCodec::J2kCodec(bool verboseMode)
    : _verboseMode(verboseMode)
{}

J2kCodec::~J2kCodec() {
    destroy();
}

void J2kCodec::decodeIntoBuffer(const std::string& path, unsigned char* buffer,
                                int resolutionLevel, int numQualityLayers, int x0,
                                int y0, int x1, int y1, int numThreads)
{
    auto t1 = std::chrono::high_resolution_clock::now();
    createInfileStream(path);
    setupDecoder(resolutionLevel, numQualityLayers, x0, x1, y0, y1, numThreads);

    // TODO(mnoven): It's a waste of resources having to decode into the image object and
    // then copy over the data to our buffer. Would be better if we could decode directly
    // into the buffer.
    // See: https://github.com/uclouvain/openjpeg/issues/837
    if (!opj_decode(_decoder, _infileStream, _image)) {
        LERROR("Could not decode image");
        destroy();
        return;
    }

    if (!opj_end_decompress(_decoder, _infileStream)) {
        LERROR("Could not end decompression");
        destroy();
        return;
    }

    // TODO(mnoven): This is a waste. Can't specify decode precision in
    // openjpeg. See: https://github.com/uclouvain/openjpeg/issues/836)
    std::copy(
        _image->comps[0].data,
        _image->comps[0].data + _image->comps[0].w * _image->comps[0].h,
        buffer
    );
    // std::memcpy(buffer, _image->comps[0].data, _image->comps[0].w * _image->comps[0].h
    // * sizeof(int32_t));
    auto t2 = std::chrono::high_resolution_clock::now();

    if (_verboseMode) {
        LINFO(fmt::format(
            "Decode time of {}: {} ms",
            path,
            std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count()
        ));
    }
}

void J2kCodec::destroy() {
    opj_stream_destroy(_infileStream);
    opj_destroy_codec(_decoder);
    opj_image_destroy(_image);
}

void J2kCodec::createInfileStream(std::string filename) {
    _infileName = std::move(filename);

    _infileStream = opj_stream_create_default_file_stream(_infileName.c_str(), OPJ_TRUE);
    if (!_infileStream) {
        LERROR(fmt::format("Failed to create stream from file '{}'", _infileName));
    }
}

void J2kCodec::encodeAsTiles(const char* outfile, const int32_t* data,
                             unsigned int imageWidth, unsigned int imageHeight,
                             unsigned int tileWidth, unsigned int tileHeight,
                             unsigned int numComps, unsigned int compPrec)
{
    opj_image_cmptparm_t l_params[4];
    opj_image_cmptparm_t* l_current_param_ptr = l_params;
    // Image definition
    for (unsigned int i = 0; i < numComps; ++i) {
        l_current_param_ptr->dx = 1;
        l_current_param_ptr->dy = 1;
        l_current_param_ptr->h = static_cast<OPJ_UINT32>(imageHeight);
        l_current_param_ptr->w = static_cast<OPJ_UINT32>(imageWidth);
        l_current_param_ptr->sgnd = 0;
        l_current_param_ptr->prec = static_cast<OPJ_UINT32>(compPrec);
        l_current_param_ptr->x0 = 0;
        l_current_param_ptr->y0 = 0;
        ++l_current_param_ptr;
    }

    opj_cparameters_t encoderParams;
    opj_set_default_encoder_parameters(&encoderParams);
    encoderParams.tcp_numlayers = 1;
    encoderParams.cp_fixed_quality = 1;
    //encoderParams.tcp_distoratio[0] = 100;
    encoderParams.cp_tx0 = 0;
    encoderParams.cp_ty0 = 0;
    encoderParams.tile_size_on = OPJ_TRUE;
    encoderParams.cp_tdx = tileWidth;
    encoderParams.cp_tdy = tileHeight;
    encoderParams.irreversible = 1;
    encoderParams.numresolution = 6;
    encoderParams.prog_order = OPJ_LRCP;

    unsigned int len = strlen(outfile);
    opj_codec_t* encoder;
    if (strcmp(outfile + len - 4, ".jp2") == 0) {
        encoder = opj_create_compress(OPJ_CODEC_JP2);
    } else {
        encoder = opj_create_compress(OPJ_CODEC_J2K);
    }

    if (!encoder) {
        LERROR("Failed to created codec");
        destroy();
        return;
    }

  //Catch events using our callbacks and give a local context
  // if (_verboseMode) {
  //   opj_set_info_handler(_encoder, [](const char* msg, void* client_data) {
  //                         (void)client_data;
  //                         std::clog << "[INFO]" << msg;
  //                       }, 00);
  //   opj_set_warning_handler(_encoder, [](const char* msg, void* client_data) {
  //                         (void)client_data;
  //                         std::cerr << "[WARNING]" << msg;
  //                       }, 00);
  //   opj_set_error_handler(_encoder, [](const char* msg, void* client_data) {
  //                         (void)client_data;
  //                         std::cerr << "[ERROR]" << msg;
  //                       }, 00);
  // }

    opj_image_t* outImage = opj_image_tile_create(numComps, l_params, OPJ_CLRSPC_GRAY);
    if (!outImage) {
        LERROR("Failed to create image");
        destroy();
        return;
    }

    outImage->x0 = 0;
    outImage->y0 = 0;
    outImage->x1 = imageWidth;
    outImage->y1 = imageHeight;
    outImage->color_space = OPJ_CLRSPC_GRAY;

    if (!opj_setup_encoder(encoder, &encoderParams, outImage)) {
        LERROR("Failed to set up encoder");
        destroy();
        return;
    }

    opj_stream_t* outStream = opj_stream_create_default_file_stream(outfile, OPJ_FALSE);
    if (!outStream) {
        LERROR("Failed to set up out stream");
        destroy();
        return;
    }

    //  ___________ nX
    // |   |   |   |
    // |___|___|___|
    // |   |   |   |
    // |___|___|___|
    // |   |   |   |
    // |___|___|___|
    // nY

    const unsigned int numTilesX = imageWidth / tileWidth;
    const unsigned int numTilesY = imageHeight / tileHeight;

    const OPJ_UINT32 numTiles = static_cast<OPJ_UINT32>(numTilesX) *
                                static_cast<OPJ_UINT32>(numTilesY);
    const OPJ_UINT32 dataSize = static_cast<OPJ_UINT32>(tileWidth) *
                                static_cast<OPJ_UINT32>(tileHeight) *
                                static_cast<OPJ_UINT32>(numComps) *
                                static_cast<OPJ_UINT32>(compPrec / 8);

    std::vector<std::vector<unsigned char>> outvec(numTiles);

    for (size_t i = 0; i < numTilesY; ++i) {
        for (size_t j = 0; j < numTilesX; j++) {
            for (size_t y = 0; y < tileHeight; ++y) {
                for (size_t x = 0; x < tileWidth; ++x) {
                    const int32_t& intensity = data[
                        y * imageWidth + j * tileWidth + i * tileHeight * imageWidth + x
                    ];
                    outvec[j + i * numTilesX].push_back(intensity);
                }
            }
        }
    }

    if (!opj_start_compress(encoder, outImage, outStream)) {
        LERROR("Failed to start compress");
    }

    for (size_t i = 0; i < numTiles; ++i) {
        //if (!opj_write_tile(encoder, i, &outvec[i][0], dataSize, outStream)) {
        if (!opj_write_tile(encoder, i, outvec[i].data(), dataSize, outStream)) {
            LERROR("Failed to write tile");
        }
    }

    if (!opj_end_compress(encoder, outStream)) {
        LERROR("Failed to end compress");
    }

    opj_image_destroy(outImage);
    opj_stream_destroy(outStream);
    opj_destroy_codec(encoder);
}

void J2kCodec::setupDecoder(int resolutionLevel, int numQualityLayers, int x0, int y0,
                            int x1, int y1, int numThreads)
{
    opj_set_default_decoder_parameters(&_decoderParams);
    _decoderParams.decod_format = infileFormat(_infileName);
    //_decoderParams.cp_layer = numQualityLayers;
    _decoderParams.cp_reduce = resolutionLevel;

    switch (_decoderParams.decod_format) {
        case J2K_CFMT: {
            // JPEG-2000 codestream
            _decoder = opj_create_decompress(OPJ_CODEC_J2K);
            break;
        }
        case JP2_CFMT: {
            // JPEG 2000 compressed image data
            _decoder = opj_create_decompress(OPJ_CODEC_JP2);
            break;
        }
        case JPT_CFMT: {
            // JPEG 2000, JPIP
            _decoder = opj_create_decompress(OPJ_CODEC_JPT);
            break;
        }
        default:
            LERROR(fmt::format(
                "Unrecognized format for input {}"
                "[Accept only .j2k (0), .jp2 (1), or .jpc (2), got {}]",
                _decoderParams.infile,
                _decoderParams.decod_format
            ));
            return;
    }

    if (!opj_setup_decoder(_decoder, &_decoderParams)) {
        LERROR("Failed to set up the decoder");
        return;
    }

    // Read the main header of the codestream and if necessary the JP2 boxes
    if (!opj_read_header(_infileStream, _decoder, &_image)) {
        LERROR("Failed to read the header");
        return;
    }



    if (x0 >= 0 && y0 >= 0 && x1 >= 0 && y1 >= 0) {
        if (opj_set_decode_area(_decoder, _image, x0, y0, x1, y1)) {
            LERROR("Failed to set decode area");
            return;
        }
    }

    // TODO(mnoven): Broken internal multithreading. Crashes if we decode on multiple threads
    // Might be fixed in the future.
    // if (opj_has_thread_support()) {
    //     if (!opj_codec_set_threads(_decoder, 4)) {
    //         LERROR("Failed to set multi-threads threads");
    //     };
    // }
}

} // namespace openspace
