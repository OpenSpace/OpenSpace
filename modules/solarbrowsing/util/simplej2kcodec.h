#ifndef SIMPLEJ2KCODEC_H
#define SIMPLEJ2KCODEC_H

#include "openjpeg.h"
#include <string>
#include <memory>

#define ALL_THREADS 0

struct ImageData {
    int32_t* data;
    uint32_t w;
    uint32_t h;
};

class SimpleJ2kCodec {
public:
  SimpleJ2kCodec(bool verboseMode = false);
  ~SimpleJ2kCodec();
  // Decode and return image object
  std::shared_ptr<ImageData> Decode(const std::string& path, const int resolutionLevel,
                                    const int numQualityLayers = 1, const int x0 = -1,
                                    const int y0 = -1, const int x1 = -1, const int y1 = -1,
                                    const int numThreads = ALL_THREADS);

  // Decode into a client allocated buffer
  void DecodeTileIntoBuffer(const int tileId, const std::string& path,
                            unsigned char& buffer, const int resolutionLevel,
                            const int numQualityLayers = 1, const int x0 = -1,
                            const int y0 = -1, const int x1 = -1, const int y1 = -1,
                            const int numThreads = ALL_THREADS);

  void DecodePGMIntoBuffer(const std::string& path, unsigned char* buffer);
  void DecodeBMPIntoBuffer(const std::string& path, unsigned char* buffer);

  void DecodeIntoBuffer(const std::string& path, unsigned char* buffer,
                        const int resolutionLevel, const int numQualityLayers = 1,
                        const int x0 = -1, const int y0 = -1, const int x1 = -1,
                        const int y1 = -1, const int numThreads = ALL_THREADS);

  // Encodes current loaded file
  void EncodeAsTiles(const char* outfile,
                     const int32_t* data,
                     const unsigned int imageWidth,
                     const unsigned int imageHeight,
                     const unsigned int tileWidth,
                     const unsigned int tileHeight,
                     const unsigned int numComps,
                     const unsigned int compPrec);
private:
    void Destroy();
    void CreateInfileStream(const std::string& filename);
    void SetupDecoder(const int resolutionLevel, const int numQualityLayers, const int x0,
                      const int x1, const int y0, const int y1, const int numThreads);

    opj_codestream_info_v2_t* _codestreamInfo;
    opj_codec_t* _decoder;
    opj_dparameters_t _decoderParams;
    opj_image_t* _image;

    std::string _infileName;
    opj_stream_t* _infileStream;
    bool _verboseMode;
};

#endif // SIMPLEJ2KCODEC_H
