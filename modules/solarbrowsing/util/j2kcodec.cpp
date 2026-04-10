/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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
#include <format_defs.h>
#include <chrono>
#include <format>
#include <fstream>
#include <optional>
#include <vector>

#define JP2_RFC3745_MAGIC "\x00\x00\x00\x0c\x6a\x50\x20\x20\x0d\x0a\x87\x0a"
#define JP2_MAGIC "\x0d\x0a\x87\x0a"
#define J2K_CODESTREAM_MAGIC "\xff\x4f\xff\x51"

namespace {
    constexpr std::string_view _loggerCat = "J2kCodec";

    enum class FileFormat : int {
        J2K = J2K_CFMT,
        JP2 = JP2_CFMT,
        JPT = JPT_CFMT,
        PXM = PXM_DFMT,
        PGX = PGX_DFMT,
        BMP = BMP_DFMT,
        TIF = TIF_DFMT,
        RAW = RAW_DFMT,
        TGA = TGA_DFMT,
        PNG = PNG_DFMT,
    };

    std::string_view toString(FileFormat f) {
        switch (f) {
            case FileFormat::J2K: return ".j2k/.j2c/.jpc";
            case FileFormat::JP2: return ".jp2";
            case FileFormat::JPT: return ".jpt";
            case FileFormat::PXM:  return ".pnm/.pgm/.ppm";
            case FileFormat::PGX:  return ".pgx";
            case FileFormat::BMP:  return ".bmp";
            case FileFormat::TIF:  return ".tif";
            case FileFormat::RAW:  return ".raw";
            case FileFormat::TGA:  return ".tga";
            case FileFormat::PNG:  return ".png";
            default:
                throw ghoul::MissingCaseException();
        }
    }

    std::optional<FileFormat> fromExtension(const std::filesystem::path& ext) {
        if (ext == ".j2k" || ext == ".j2c" || ext == ".jpc") return FileFormat::J2K;
        else if (ext == ".jp2") return FileFormat::JP2;
        else if (ext == ".jpt") return FileFormat::JPT;
        else if (ext == ".pnm" || ext == ".pgm" || ext == ".ppm") return FileFormat::PXM;
        else if (ext == ".pgx") return FileFormat::PGX;
        else if (ext == ".bmp") return FileFormat::BMP;
        else if (ext == ".tif") return FileFormat::TIF;
        else if (ext == ".raw") return FileFormat::RAW;
        else if (ext == ".tga") return FileFormat::TGA;
        else if (ext == ".png") return FileFormat::PNG;

        return std::nullopt;
    }

    // (anden88 2026-02-03): This function opens the file, reads some number of bytes from
    // the metadata header and compares to some specific byte string. Further it compares
    // that the read bytestring matches the extension. Imo, this is quite verbose, I think
    // we could get away with only looking at the file extension. Did some measurements
    // and it is in the ballpark of ~200-300 microseconds of work. Compared to setting up
    // the `inFileStream` which is ~100ms
    std::optional<FileFormat> infileFormat(const std::filesystem::path& filePath) {
        std::ifstream file(filePath, std::ios::binary);
        if (!file) {
            return std::nullopt;
        }

        std::array<unsigned char, 12> buf{};
        file.read(reinterpret_cast<char*>(buf.data()), buf.size());
        if (!file) {
            return std::nullopt;
        }

        const std::optional<FileFormat> extFormat = fromExtension(filePath.extension());

        // JPT is only detectable via extension, no magic bytes to check
        if (extFormat && *extFormat == FileFormat::JPT) {
            return FileFormat::JPT;
        }

        // Try to read the magic bytes of the file
        std::optional<FileFormat> magicFormat;
        if (std::memcmp(buf.data(), JP2_RFC3745_MAGIC, buf.size()) == 0 ||
            std::memcmp(buf.data(), JP2_MAGIC, 4) == 0)
        {
            magicFormat = FileFormat::JP2;
        }
        else if (std::memcmp(buf.data(), J2K_CODESTREAM_MAGIC, 4) == 0) {
            magicFormat = FileFormat::J2K;
        }
        else {
            return std::nullopt;
        }

        if (extFormat && magicFormat != extFormat) {
            LERROR(std::format(
                "Extension of file is incorrect. Found {} should be {}",
                filePath.extension(), toString(*magicFormat)
            ));
        }

        return magicFormat;
    }
} // namespace

namespace openspace {

J2kCodec::J2kCodec(bool shouldPrintTiming)
    : _sholdPrintTiming(shouldPrintTiming)
{}

J2kCodec::~J2kCodec() {
    destroy();
}

void J2kCodec::decodeIntoBuffer(const std::filesystem::path& path, unsigned char* buffer,
                                int downsamplingLevel)
{
    auto t1 = std::chrono::high_resolution_clock::now();
    createInfileStream(path);
    setupDecoder(downsamplingLevel);

    // @TODO (mnoven): It's a waste of resources having to decode into the image object and
    // then copy over the data to our buffer. Would be better if we could decode directly
    // into the buffer
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

    // @TODO (mnoven): This is a waste. Can't specify decode precision in
    // openjpeg. See: https://github.com/uclouvain/openjpeg/issues/836)
    std::copy(
        _image->comps[0].data,
        _image->comps[0].data + _image->comps[0].w * _image->comps[0].h,
        buffer
    );

    auto t2 = std::chrono::high_resolution_clock::now();
    if (_sholdPrintTiming) {
        LINFO(std::format(
            "Decode time of {}: {} ms",
            path, std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count()
        ));
    }
}

void J2kCodec::destroy() {
    opj_stream_destroy(_infileStream);
    opj_destroy_codec(_decoder);
    opj_image_destroy(_image);
}

void J2kCodec::createInfileStream(const std::filesystem::path& path) {
    _filePath = path;
    _infileStream = opj_stream_create_default_file_stream(path.string().c_str(), OPJ_TRUE);
    if (!_infileStream) {
        LERROR(std::format("Failed to create stream from file '{}'", _filePath));
    }
}

void J2kCodec::setupDecoder(int downsamplingLevel) {
    opj_set_default_decoder_parameters(&_decoderParams);
    _decoderParams.cp_reduce = downsamplingLevel;

    const std::optional<FileFormat> format = infileFormat(_filePath);
    if (!format) {
        LERROR(std::format("Unrecognized format for input {}", _filePath));
        return;
    }
    _decoderParams.decod_format = static_cast<int>(*format);

    OPJ_CODEC_FORMAT codec;
    switch (*format) {
        case FileFormat::J2K:
            // JPEG-2000 codestream
            codec = OPJ_CODEC_J2K;
            break;
        case FileFormat::JP2:
            // JPEG 2000 compressed image data
            codec = OPJ_CODEC_JP2;
            break;
        case FileFormat::JPT:
            // JPEG 2000, JPIP
            codec = OPJ_CODEC_JPT;
            break;
        default:
            LERROR(std::format(
                "Unsupported format {} for input {}",
                toString(*format), _filePath));
            return;
    }

    _decoder = opj_create_decompress(codec);

    if (!opj_setup_decoder(_decoder, &_decoderParams)) {
        LERROR("Failed to set up the decoder");
        return;
    }

    // Read the main header of the codestream and if necessary the JP2 boxes
    if (!opj_read_header(_infileStream, _decoder, &_image)) {
        LERROR("Failed to read the header");
        return;
    }
}

} // namespace openspace
