#include "simplej2kcodec.h"
#include "format_defs.h"
#include <iostream>
#include <memory>
#include <vector>
#include <cstring>
#include <chrono>

typedef std::chrono::high_resolution_clock Clock;

#define JP2_RFC3745_MAGIC "\x00\x00\x00\x0c\x6a\x50\x20\x20\x0d\x0a\x87\x0a"
#define JP2_MAGIC "\x0d\x0a\x87\x0a"
#define J2K_CODESTREAM_MAGIC "\xff\x4f\xff\x51"

SimpleJ2kCodec::SimpleJ2kCodec(bool verboseMode)
    : _isFileLoaded(false),
      _infileStream(nullptr),
      _decoder(nullptr),
      _image(nullptr),
      _verboseMode(verboseMode),
      _codestreamInfo(nullptr) {}

SimpleJ2kCodec::~SimpleJ2kCodec() {
  Destroy();
}

void SimpleJ2kCodec::DecodeIntoBuffer(unsigned char* buffer, const int numThreads) {
  if (!_isFileLoaded) {
    std::cerr << "File needs to be set up before decoding\n";
    return;
  }

  // if (numThreads) {
  //   opj_codec_set_threads(_decoder, numThreads);
  // }

  // if (!_isDecoderSetup) {
  //   SetupDecoder();
  //   _isDecoderSetup = true;
  // }

  auto t1 = Clock::now();

  if (!opj_decode(_decoder, _infileStream, _image)) {
    std::cerr << "Could not decode image\n";
    Destroy();
    return;
  }

  if (!opj_end_decompress(_decoder, _infileStream)) {
    std::cerr << "Could not end decompression\n";
    Destroy();
    return;
  }




  // TODO(mnoven): This MIGHT be faster and just keep buffer in unsigned char since we know
  // that all values in img are bytes. Why can't we specify decode precision in openjpeg??
  // - For now just keep the PBO buffer in the same size
  std::copy(_image->comps[0].data, _image->comps[0].data + _image->comps[0].w * _image->comps[0].h, buffer);

  auto t2 = Clock::now();
  std::cout << "Decode time "
            << std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count()
            << " ms" << std::endl;

  //std::memcpy(buffer, _image->comps[0].data, _image->comps[0].w * _image->comps[0].h * sizeof(int32_t));
}

std::unique_ptr<ImageData> SimpleJ2kCodec::Decode() {
  if (!_isFileLoaded) {
    std::cerr << "File needs to be set up before decoding\n";
    return nullptr;
  }

  auto t1 = Clock::now();

  // if (!_isDecoderSetup) {
  //   SetupDecoder();
  //   _isDecoderSetup = true;
  // }

  if (!opj_decode(_decoder, _infileStream, _image)) {
    std::cerr << "Could not decode image\n";
    Destroy();
    return nullptr;
  }

  if (!opj_end_decompress(_decoder, _infileStream)) {
      std::cerr << "Could not end decompression\n";
      Destroy();
      return nullptr;
  }


  // auto t2 = Clock::now();
  // std::cout << "Decode time "
  //           << std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count()
  //           << " ms" << std::endl;

  ImageData im = {_image->comps[0].data, _image->comps[0].w, _image->comps[0].h};
  return std::make_unique<ImageData>(im);
}

// void SimpleJ2kCodec::DecodeRegionIntoBuffer(const int& tileId, unsigned char*& buffer,
//                                           int numThreads)
// {
//     if (!_isFileLoaded) {
//         std::cerr << "File needs to be set up before decoding tiles\n";
//         return;
//     }

//     opj_codec_set_threads(_decoder, numThreads);
//     // if (!_isDecoderSetup) {
//     //   SetupDecoder();
//     //   _isDecoderSetup = true;
//     // }

//     auto t1 = Clock::now();
//     // if (!opj_get_decoded_tile(_decoder, _infileStream, _image, tileId)) {
//     //   std::cerr << "Could not decode tile\n";
//     //   Destroy();
//     //   return;
//     // }
//     OPJ_UINT32 l_data_size;
//     OPJ_INT32 l_current_tile_x0, l_current_tile_y0, l_current_tile_x1, l_current_tile_y1;
//     OPJ_UINT32 l_nb_comps = 0;
//     OPJ_BOOL l_go_on = OPJ_TRUE;
//     OPJ_UINT32 l_tile_index;
//     if (!opj_read_tile_header(_decoder, _infileStream, &l_tile_index, &l_data_size,
//                               &l_current_tile_x0, &l_current_tile_y0, &l_current_tile_x1,
//                               &l_current_tile_y1, &l_nb_comps, &l_go_on)) {
//         std::cerr << "Could not read tile header" << std::endl;
//         return;
//     }
//     if (!opj_decode_tile_data(_decoder, l_tile_index, buffer, l_data_size,
//                               _infileStream)) {
//         std::cerr << "Could not decode tile\n";
//         return;
//     }

//     auto t2 = Clock::now();
//     std::cout << "Decode tile time "
//               << std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count()
//               << " ms" << std::endl;

//     // std::copy(_image->comps[0].data,
//     //        _image->comps[0].data + _image->comps[0].w * _image->comps[0].h, buffer);
// }

// void SimpleJ2kCodec::DecodeTileIntoBuffer(const int& tileId, unsigned char*& buffer,
//                                           int numThreads)
// {
//     if (!_isFileLoaded) {
//         std::cerr << "File needs to be set up before decoding tiles\n";
//         return;
//     }

//     //opj_codec_set_threads(_decoder, numThreads);

//     // if (!_isDecoderSetup) {
//     //   SetupDecoder();
//     //   _isDecoderSetup = true;
//     // }

//     // if (!opj_set_decode_area(_decoder, _image, 0, 0, 4096, 4096)) {
//     //     std::cerr << "Failed to set decode area \n";
//     //     return;
//     // }

//     //auto t1 = Clock::now();
//     OPJ_UINT32 l_data_size;
//     OPJ_INT32 l_current_tile_x0, l_current_tile_y0, l_current_tile_x1, l_current_tile_y1;
//     OPJ_UINT32 l_nb_comps = 0;
//     OPJ_BOOL l_go_on = OPJ_TRUE;
//     OPJ_UINT32 l_tile_index;

//     int tilesDecoded = 0;

//     // while (l_go_on) {
//     //   if (!opj_read_tile_header(_decoder, _infileStream, &l_tile_index, &l_data_size,
//     //                           &l_current_tile_x0, &l_current_tile_y0, &l_current_tile_x1,
//     //                           &l_current_tile_y1, &l_nb_comps, &l_go_on)) {
//     //     std::cerr << "Could not read tile header" << std::endl;
//     //     return;
//     //   }
//     //   if (l_go_on) {
//     //     if (!opj_decode_tile_data(_decoder, l_tile_index, buffer + count++*l_data_size, l_data_size,
//     //                               _infileStream)) {
//     //         std::cerr << "Could not decode tile\n";
//     //         return;
//     //     }
//     //     tilesDecoded++;
//     //   }
//     // }

//     if (!opj_read_tile_header(_decoder, _infileStream, &l_tile_index, &l_data_size,
//                               &l_current_tile_x0, &l_current_tile_y0, &l_current_tile_x1,
//                               &l_current_tile_y1, &l_nb_comps, &l_go_on)) {
//         std::cerr << "Could not read tile header" << std::endl;
//         return;
//     }
//     if (!opj_decode_tile_data(_decoder, l_tile_index, buffer, l_data_size,
//                               _infileStream)) {
//         std::cerr << "Could not decode tile\n";
//         return;
//     }

//     // auto t2 = Clock::now();
//     // std::cout << "Decode tile time "
//     //           << std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count()
//     //           << " ms" << std::endl;

//     // std::cout << "Tiles decoded " << tilesDecoded << std::endl;

//     // std::copy(_image->comps[0].data,
//     //        _image->comps[0].data + _image->comps[0].w * _image->comps[0].h, buffer);
// }

std::unique_ptr<ImageData> SimpleJ2kCodec::DecodeTile(const int& tileId) {
  if (!_isFileLoaded) {
    std::cerr << "File needs to be set up before decoding tiles\n";
    return nullptr;
  }

  // if (!_isDecoderSetup) {
  //   SetupDecoder();
  //   _isDecoderSetup = true;
  // }

  if (!opj_get_decoded_tile(_decoder, _infileStream, _image, tileId)) {
    std::cerr << "Could not decode tile\n";
    Destroy();
    return nullptr;
  }
  // Assume greyscale for now
  ImageData im = {_image->comps[0].data, _image->comps[0].w, _image->comps[0].h};
  return std::make_unique<ImageData>(im);
}

void SimpleJ2kCodec::SetResolutionFactor(const int res) {
  if (!opj_set_decoded_resolution_factor(_decoder, res)) {
    std::cerr << "Failed to set resolution factor\n";
    return;
  }
}

void SimpleJ2kCodec::Destroy() {
  if (_verboseMode) {
    std::cerr << "Destroying..\n";
  }
  opj_stream_destroy(_infileStream);
  if (_codestreamInfo) {
    opj_destroy_cstr_info(&_codestreamInfo);
  }
  opj_destroy_codec(_decoder);
  opj_image_destroy(_image);
  _isDecoderSetup = false;
  _isFileLoaded = false;
}

void SimpleJ2kCodec::CreateInfileStream(const std::string& filename) {
  //strcpy(_infileName, filename.c_str());
  _infileName = filename;

  _infileStream = opj_stream_create_default_file_stream(_infileName.c_str(), 1);
  if (!_infileStream){
    std::cerr << "Failed to create stream from file " << _infileName << "\n";
    return;
  }

  // Infile changed, refresh decoder
  _isDecoderSetup = false;
  _isFileLoaded = true;
}

void SimpleJ2kCodec::EncodeAsTiles(const char* outfile,
                                   const int32_t* data,
                                   const unsigned int imageWidth,
                                   const unsigned int imageHeight,
                                   const unsigned int tileWidth,
                                   const unsigned int tileHeight,
                                   const unsigned int numComps,
                                   const unsigned int compPrec) {
  opj_image_cmptparm_t l_params[4];
  opj_image_cmptparm_t* l_current_param_ptr;
  opj_cparameters_t _encoderParams;
  opj_image_t* _outImage;
  opj_codec_t* _encoder;

  l_current_param_ptr = l_params;
  // Image definition
  for (int i = 0; i < numComps; ++i) {
    l_current_param_ptr->dx = 1;
    l_current_param_ptr->dy = 1;
    l_current_param_ptr->h = (OPJ_UINT32)imageHeight;
    l_current_param_ptr->w = (OPJ_UINT32)imageWidth;
    l_current_param_ptr->sgnd = 0;
    l_current_param_ptr->prec = (OPJ_UINT32)compPrec;
    l_current_param_ptr->x0 = 0;
    l_current_param_ptr->y0 = 0;
    ++l_current_param_ptr;
  }

  opj_set_default_encoder_parameters(&_encoderParams);
  _encoderParams.tcp_numlayers = 1;
  _encoderParams.cp_fixed_quality = 1;
  //_encoderParams.tcp_distoratio[0] = 100;
  _encoderParams.cp_tx0 = 0;
  _encoderParams.cp_ty0 = 0;
  _encoderParams.tile_size_on = OPJ_TRUE;
  _encoderParams.cp_tdx = tileWidth;
  _encoderParams.cp_tdy = tileHeight;
  _encoderParams.irreversible = 1;
  _encoderParams.numresolution = 6;
  _encoderParams.prog_order = OPJ_LRCP;

  unsigned int len = strlen(outfile);
  if (strcmp(outfile + len - 4, ".jp2") == 0) {
    _encoder = opj_create_compress(OPJ_CODEC_JP2);
  } else {
    _encoder = opj_create_compress(OPJ_CODEC_J2K);
  }

  if (!_encoder) {
    std::cerr << "Failed to create codec" << std::endl;
    Destroy();
    return;
  }

  // No "region" of interest, more precisally component
  // l_param.roi_compno = -1;
  // l_param.roi_shift = 0;
  // We are not using multiple tile parts for a tile.
  // l_param.tp_on = 0;
  // l_param.tp_flag = 0;

  //Catch events using our callbacks and give a local context
  if (_verboseMode) {
    opj_set_info_handler(_encoder, [](const char* msg, void* client_data) {
                          (void)client_data;
                          std::clog << "[INFO]" << msg;
                        }, 00);
    opj_set_warning_handler(_encoder, [](const char* msg, void* client_data) {
                          (void)client_data;
                          std::cerr << "[WARNING]" << msg;
                        }, 00);
    opj_set_error_handler(_encoder, [](const char* msg, void* client_data) {
                          (void)client_data;
                          std::cerr << "[ERROR]" << msg;
                        }, 00);
  }

  _outImage = opj_image_tile_create(numComps, l_params, OPJ_CLRSPC_GRAY);
  if (!_outImage) {
    std::cerr << "Failed to create image \n";
    Destroy();
    return;
  }

  _outImage->x0 = 0;
  _outImage->y0 = 0;
  _outImage->x1 = imageWidth;
  _outImage->y1 = imageHeight;
  _outImage->color_space = OPJ_CLRSPC_GRAY;

  if (!opj_setup_encoder(_encoder, &_encoderParams, _outImage)) {
    std::cerr << "Failed to set up encoder\n";
    Destroy();
    return;
  }

  opj_stream_t* outStream = opj_stream_create_default_file_stream(outfile, OPJ_FALSE);
  if (!outStream) {
    std::cerr << "Failed to set up out stream\n";
    Destroy();
    return;
  }

  const unsigned int numTiles = (OPJ_UINT32)(imageWidth/tileWidth) * (OPJ_UINT32)(imageHeight/tileHeight);
  const OPJ_UINT32 dataSize = (OPJ_UINT32)tileWidth * (OPJ_UINT32) tileHeight * (OPJ_UINT32) numComps * (OPJ_UINT32)(compPrec / 8);
  OPJ_BYTE* outData = (OPJ_BYTE*)malloc(dataSize * sizeof(OPJ_BYTE));

  std::vector<std::vector<unsigned char>> outvec(numTiles);

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

  for (int i = 0; i < numTilesY; ++i) {
    for (int j = 0; j < numTilesX; j++) {
      for (int y = 0; y < tileHeight; ++y) {
        for (int x = 0; x < tileWidth; ++x) {
          const int32_t& intensity =  data[y * imageWidth + j * tileWidth +
                  i * tileHeight * imageWidth + x];
          outvec[j + i * numTilesX].push_back(intensity);
        }
      }
    }
  }

  if (!outData) {
    std::cerr << "Failed to allocate data \n";
    Destroy();
    return;
  }

  if (!opj_start_compress(_encoder, _outImage, outStream)) {
    std::cerr << "Failed to start compress\n";
    Destroy();
    return;
  }

  for (int i = 0; i < numTiles; ++i) {
    if (!opj_write_tile(_encoder, i, &outvec[i][0], dataSize, outStream)) {
      std::cerr << "Failed to write tile\n";
      Destroy();
      return;
    }
  }

  if (!opj_end_compress(_encoder, outStream)) {
    std::cerr << "Failed to end compress\n";
    Destroy();
    return;
  }

  opj_image_destroy(_outImage);
  opj_stream_destroy(outStream);
  opj_destroy_codec(_encoder);
}

void SimpleJ2kCodec::SetupDecoder(const int resolutionLevel) {
  opj_set_default_decoder_parameters(&_decoderParams);
  //strcpy(_decoderParams.infile, _infileName);
  _decoderParams.decod_format = GetInfileFormat(_infileName.c_str());
  //_decoderParams.m_verbose = false;
  _decoderParams.cp_layer = 1;
  _decoderParams.cp_reduce = resolutionLevel;

  switch (_decoderParams.decod_format) {
    case J2K_CFMT: { // JPEG-2000 codestream
      _decoder = opj_create_decompress(OPJ_CODEC_J2K);
      break;
    }
    case JP2_CFMT: { // JPEG 2000 compressed image data
      _decoder = opj_create_decompress(OPJ_CODEC_JP2);
      break;
    }
    case JPT_CFMT: { // JPEG 2000, JPIP
      _decoder = opj_create_decompress(OPJ_CODEC_JPT);
      break;
    }
    default:
      std::cerr << "Unrecognized format for input " << _decoderParams.infile
                << " - Accept only .j2k, .jp2, .jpc or .jpt]\n";
      return;
  }

  if (!opj_setup_decoder(_decoder, &_decoderParams)) {
    std::cerr << "Failed to set up the decoder\n";
    Destroy();
    return;
  }

  // Read the main header of the codestream and if necessary the JP2 boxes
  if (!opj_read_header(_infileStream, _decoder, &_image)) {
    std::cerr << "Failed to read the header\n";
    Destroy();
    return;
  }

  // Extract some info from the code stream
  _codestreamInfo = opj_get_cstr_info(_decoder);

  if (_verboseMode) {
    fprintf(stdout, "The file contains %dx%d tiles\n", _codestreamInfo->tw, _codestreamInfo->th);
    //Catch events using our callbacks and give a local context
    opj_set_info_handler(_decoder, [](const char* msg, void* client_data) {
                          (void)client_data;
                          std::clog << "[INFO]" << msg;
                        }, 00);
    opj_set_warning_handler(_decoder, [](const char* msg, void* client_data) {
                          (void)client_data;
                          std::cerr << "[WARNING]" << msg;
                        }, 00);
    opj_set_error_handler(_decoder, [](const char* msg, void* client_data) {
                          (void)client_data;
                          std::cerr << "[ERROR]" << msg;
                        }, 00);
  }
}

const int SimpleJ2kCodec::GetInfileFormat(const char *fname) {
  const auto get_file_format = [](const char* filename) {
    unsigned int i;
    static const char *extension[] = {"pgx", "pnm", "pgm", "ppm", "bmp","tif", "raw", "tga", "png", "j2k", "jp2", "jpt", "j2c", "jpc" };
    static const int format[] = { PGX_DFMT, PXM_DFMT, PXM_DFMT, PXM_DFMT, BMP_DFMT, TIF_DFMT, RAW_DFMT, TGA_DFMT, PNG_DFMT, J2K_CFMT, JP2_CFMT, JPT_CFMT, J2K_CFMT, J2K_CFMT };
    const char * ext = strrchr(filename, '.');
    if (ext == NULL)
      return -1;
    ext++;
    if(ext) {
      for(i = 0; i < sizeof(format)/sizeof(*format); i++) {
        if(strncmp(ext, extension[i], 3) == 0) {
          return format[i];
        }
      }
    }

    return -1;
  };

  FILE *reader;
  const char *s, *magic_s;
  int ext_format, magic_format;
  unsigned char buf[12];
  OPJ_SIZE_T l_nb_read;

  reader = fopen(fname, "rb");

  if (reader == NULL) {
    return -1;
  }

  memset(buf, 0, 12);
  l_nb_read = fread(buf, 1, 12, reader);
  fclose(reader);
  if (l_nb_read != 12) {
    return -1;
  }

  ext_format = get_file_format(fname);

  if (ext_format == JPT_CFMT) {
    return JPT_CFMT;
  }

  if (memcmp(buf, JP2_RFC3745_MAGIC, 12) == 0 || memcmp(buf, JP2_MAGIC, 4) == 0) {
    magic_format = JP2_CFMT;
    magic_s = ".jp2";
  } else if (memcmp(buf, J2K_CODESTREAM_MAGIC, 4) == 0) {
    magic_format = J2K_CFMT;
    magic_s = ".j2k or .jpc or .j2c";
  } else {
    return -1;
  }

  if (magic_format == ext_format) {
    return ext_format;
  }

  s = fname + strlen(fname) - 4;
  std::cerr << "Extension of file is incorrect! Found " << s << " should be " << magic_s;
  return magic_format;
}
