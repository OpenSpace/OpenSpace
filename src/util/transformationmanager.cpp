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
#include <openspace/util/transformationmanager.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/logging/logmanager.h>

#define _USE_MATH_DEFINES
#include <math.h>

 namespace {
    const std::string _loggerCat = "TransformationManager";
 }

 namespace openspace{
    TransformationManager::TransformationManager(){
#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
        _kameleon = std::make_shared<ccmc::Kameleon>();
#else
        LWARNING("Kameleon module needed for transformations with dynamic frames");
#endif

        _kameleonFrames =   { "J2000", "GEI", "GEO", "MAG", "GSE", "GSM", "SM", "RTN", "GSEQ",   //geocentric
                              "HEE", "HAE", "HEEQ"                                              //heliocentric
                            };
        // _dipoleFrames = {"GSM", "MAG"};
    }

    TransformationManager::~TransformationManager(){
#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
        _kameleon = nullptr;
#endif
    }

    glm::dmat3 TransformationManager::kameleonTransformationMatrix( std::string from,
                                                                    std::string to,
                                                                    double ephemerisTime) const
    {
        ccmc::Position in0 = {1.f, 0.f, 0.f};
        ccmc::Position in1 = {0.f, 1.f, 0.f};
        ccmc::Position in2 = {0.f, 0.f, 1.f};

        ccmc::Position out0;
        ccmc::Position out1;
        ccmc::Position out2;

        _kameleon->_cxform(from.c_str(), to.c_str(), ephemerisTime, &in0, &out0);
        _kameleon->_cxform(from.c_str(), to.c_str(), ephemerisTime, &in1, &out1);
        _kameleon->_cxform(from.c_str(), to.c_str(), ephemerisTime, &in2, &out2);

        glm::dmat3 out = glm::dmat3(
                    out0.c0 , out0.c1   , out0.c2,
                    out1.c0 , out1.c1   , out1.c2,
                    out2.c0 , out2.c1   , out2.c2
                );

        // Need to rotate 90 degrees around x-axis becuase kameleon is flipped
        out = glm::dmat3(glm::rotate(glm::mat4(out), (float)M_PI_2, glm::vec3(1.0f, 0.0f, 0.0f)));
        return out;
    }

    glm::dmat3 TransformationManager::frameTransformationMatrix(std::string from,
                                                                std::string to,
                                                                double ephemerisTime) const
    {
#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
        auto fromit = _dipoleFrames.find(from);
        auto toit   = _dipoleFrames.find(to);

        // //diopole frame to J200 makes the frame rotate.
        // if(fromit != _dipoleFrames.end()) from = "GSE";
        // if(toit   != _dipoleFrames.end()) to   = "GSE";

        fromit = _kameleonFrames.find(from);
        toit   = _kameleonFrames.find(to);

        bool fromKameleon   = (fromit != _kameleonFrames.end());
        bool toKameleon     = (toit   != _kameleonFrames.end());
        
        ccmc::Position in0 = {1.f, 0.f, 0.f};
        ccmc::Position in1 = {0.f, 1.f, 0.f};
        ccmc::Position in2 = {0.f, 0.f, 1.f};

        glm::dmat3 in(
            in0.c0, in0.c1, in0.c2,
            in1.c0, in1.c1, in1.c2,
            in2.c0, in2.c1, in2.c2
        );

        ccmc::Position out0;
        ccmc::Position out1;
        ccmc::Position out2;

        if(!fromKameleon && !toKameleon){
            return  SpiceManager::ref().frameTransformationMatrix(from, to, ephemerisTime);
        }

        if(fromKameleon && toKameleon){
            return kameleonTransformationMatrix(from, to, ephemerisTime);

        }else if(fromKameleon && !toKameleon){
            glm::dmat3 kameleonTransformation = kameleonTransformationMatrix(from, "J2000", ephemerisTime);
            glm::dmat3 spiceTransformation = SpiceManager::ref().frameTransformationMatrix("J2000", to, ephemerisTime);

            return spiceTransformation*kameleonTransformation;
        
        }else if(!fromKameleon && toKameleon){
            glm::dmat3 spiceTransformation = SpiceManager::ref().frameTransformationMatrix(from, "J2000", ephemerisTime);
            glm::dmat3 kameleonTransformation = kameleonTransformationMatrix("J2000", to, ephemerisTime);

            return kameleonTransformation*spiceTransformation;
        }
#else
        LERROR("Can not transform dynamic frames without kameleon module enabled");
#endif
        return glm::dmat3(1.0);
    }
 }