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

#ifndef __MESSAGESTRUCTURES_H__
#define __MESSAGESTRUCTURES_H__

//std includes
#include <string>
#include <vector>

//glm includes
#include <glm/gtx/quaternion.hpp>

//openspace includes
#include <openspace/util/powerscaledcoordinate.h>

namespace openspace{
    
    namespace network{
        
        namespace datamessagestructures{
            enum type{
                PositionData = 0,
                TimeData,
                ScriptData
            };
        
            struct PositionKeyframe{
                glm::quat _viewRotationQuat;
                psc _position;
                double _timeStamp;
                
                void serialize(std::vector<char> &buffer){
                    //add position
                    buffer.insert(buffer.end(), reinterpret_cast<char*>(&_position), reinterpret_cast<char*>(&_position) + sizeof(_position));
                    
                    //add orientation
                    buffer.insert(buffer.end(), reinterpret_cast<char*>(&_viewRotationQuat), reinterpret_cast<char*>(&_viewRotationQuat) + sizeof(_viewRotationQuat));
                    
                    //add timestamp
                    buffer.insert(buffer.end(), reinterpret_cast<char*>(&_timeStamp), reinterpret_cast<char*>(&_timeStamp) + sizeof(_timeStamp));
                };
                
                void deserialize(const std::vector<char> &buffer){
                    int offset = 0;
                    int size = 0;
                    
                    //position
                    size = sizeof(_position);
                    memcpy(&_position, buffer.data() + offset, size);
                    offset += size;
                    
                    //orientation
                    size = sizeof(_viewRotationQuat);
                    memcpy(&_viewRotationQuat, buffer.data() + offset, size);
                    offset += size;
                    
                    //timestamp
                    size = sizeof(_timeStamp);
                    memcpy(&_timeStamp, buffer.data() + offset, size);
                };
            };
            
            struct TimeKeyframe{

                double _time;
                double _dt;
                bool _paused;
                bool _requiresTimeJump;
                
                void serialize(std::vector<char> &buffer){
                    //add current time
                    buffer.insert(buffer.end(), reinterpret_cast<char*>(&_time), reinterpret_cast<char*>(&_time) + sizeof(_time));
                    
                    //add delta time
                    buffer.insert(buffer.end(), reinterpret_cast<char*>(&_dt), reinterpret_cast<char*>(&_dt) + sizeof(_dt));
                    
                    //add wether time is paused or not
                    buffer.insert(buffer.end(), reinterpret_cast<char*>(&_paused), reinterpret_cast<char*>(&_paused) + sizeof(_paused));
                    
                    //add wether a time jump is necessary (recompute paths etc)
                    buffer.insert(buffer.end(), reinterpret_cast<char*>(&_requiresTimeJump), reinterpret_cast<char*>(&_requiresTimeJump) + sizeof(_requiresTimeJump));
                };
                
                void deserialize(const std::vector<char> &buffer){
                    int offset = 0;
                    int size = 0;
                    
                    //current time
                    size = sizeof(_time);
                    memcpy(&_time, buffer.data() + offset, size);
                    offset += size;
                    
                    //delta time
                    size = sizeof(_dt);
                    memcpy(&_dt, buffer.data() + offset, size);
                    offset += size;
                    
                    //is time paused?
                    size = sizeof(_paused);
                    memcpy(&_paused, buffer.data() + offset, size);
                    offset += sizeof(_paused);
    
                    //is a time jump required?
                    size = sizeof(_requiresTimeJump);
                    memcpy(&_requiresTimeJump, buffer.data() + offset, size);
                };
            };
            
            struct ScriptMessage{
                
                uint16_t _scriptlen;
                std::string _script;
                
                void serialize(std::vector<char> &buffer){
                    //add script length
                    buffer.insert(buffer.end(), reinterpret_cast<char*>(&_scriptlen), reinterpret_cast<char*>(&_scriptlen) + sizeof(_scriptlen));
                    
                    //add script
                    buffer.insert(buffer.end(), _script.begin(), _script.end());
                    
                };
                
                void deserialize(const std::vector<char> &buffer){
                    int offset = 0;
                    int size = 0;
                    
                    //size of script
                    size = sizeof(uint16_t);
                    memcpy(&_scriptlen, buffer.data() + offset, size);
                    offset += size;
                    
                    //actual script
                    _script.assign(buffer.begin() + offset, buffer.end());
                };
            };
            
        } //namespace messagestructures

    } // namespace network
    
} // namespace openspace

#endif // __MESSAGESTRUCTURES_H__