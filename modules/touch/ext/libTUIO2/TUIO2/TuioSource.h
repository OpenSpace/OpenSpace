/*
 TUIO2 C++ Library
 Copyright (c) 2009-2014 Martin Kaltenbrunner <martin@tuio.org>
 
 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 3.0 of the License, or (at your option) any later version.
 
 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 Lesser General Public License for more details.
 
 You should have received a copy of the GNU Lesser General Public
 License along with this library.
 */

#ifndef INCLUDED_TUIOSOURCE_H
#define INCLUDED_TUIOSOURCE_H

#include <sstream>
#include <string.h>
#include <stdlib.h>
#include <iostream>

namespace TUIO2 {
    
    /**
     * The TuioSource class encapsulates the meta data for the TUIO source attributes provided in /tuio2/frm.
     *
     * @author Martin Kaltenbrunner
     * @version 2.0.a0
     */
    
    class LIBDECL TuioSource {
        
    protected:
        /**
         * The ID of the TUIO source
         */
        unsigned int source_id;
        /**
         * The name of the TUIO source
         */
        std::string source_name;
        /**
         * The instance of the TUIO source
         */
        unsigned int source_instance;
        /**
         * The address of the TUIO source
         */
        std::string source_address;
        /**
         * The encoded sensor dimension
         */
        unsigned int dimension;
        /**
         * The TuioTime of the last frame
         */
        TuioTime frameTime;
        
    public:
        
        /**
         * Sets the ID, name and address of the TUIO source
         */
        TuioSource() {
            source_id = 0;
            source_name = "default";
            source_instance = 0;
            source_address = "localhost";
            dimension = 0;
        };
        
        /**
         * Sets the ID, name and address of the TUIO source
         *
         * @param	src_name	the name of the TUIO source
         */
        TuioSource(std::string src_name) {
            source_id = 0;
            source_name = src_name;
            source_instance = 0;
            source_address = "localhost";
            dimension = 0;
        };
        
        /**
         * Sets the ID, name and address of the TUIO source
         *
         * @param	src_name	the name of the TUIO source
         */
        TuioSource(unsigned int sid, std::string src_string, unsigned int dim) {
            source_id = sid;
            setSourceString(src_string);
            setDimension(dim);
        };
        
        /**
         * Sets the ID, name and address of the TUIO source
         *
         * @param	src_name	the name of the TUIO source
         * @param	src_inst	the instance of the TUIO source
         * @param	src_addr	the address of the TUIO source
         */
        TuioSource(std::string src_name, unsigned int src_inst, std::string src_addr) {
            source_id = 0;
            source_name = src_name;
            source_instance = src_inst;
            source_address = src_addr;
        };
        
        /**
         * Sets the ID, name and address of the TUIO source
         *
         * @param	src_id		the ID of the TUIO source
         * @param	src_name	the name of the TUIO source
         * @param	src_inst	the instance of the TUIO source
         * @param	src_addr	the address of the TUIO source
         */
        TuioSource(unsigned int src_id, std::string src_name, unsigned int src_inst, std::string src_addr) {
            source_id = src_id;
            source_name = src_name;
            source_instance = src_inst;
            source_address = src_addr;
        };
        
        ~TuioSource() {};
        
        void setSourceString(std::string src_string) {
            
			if (src_string.length()==0) return;
            char *data = strdup(src_string.c_str());

            char *name_inst = strtok(data, "@");

            char *addr = strtok(NULL, "@");
            if (addr!=NULL) source_address = std::string(addr);
            else source_address = (char*)"0x7F000001";
            
            char *name = strtok(name_inst, ":");
            source_name = std::string(name);
            
            char *inst = strtok(NULL, ":");
            if (inst!=NULL) source_instance = atoi(inst);
            else source_instance = 0;
        }
        
        void setSourceString(unsigned int src_id, std::string src_string) {
            
            source_id = src_id;
            setSourceString(src_string);
        }
        
        std::string getSourceString() {
            
            std::stringstream src_stream;
            src_stream << source_name << ":" << source_instance << "@" << source_address;
            return src_stream.str();
        }
        
        /**
         * Returns the ID of the TUIO source
         */
        unsigned int getSourceID() { return source_id; }
        
        /**
         * Returns the name of the TUIO source
         */
        const char* getSourceName() { return source_name.c_str(); }
        
        /**
         * Returns the instance of the TUIO source
         */
        unsigned int getSourceInstance() { return source_instance; }
        
        /**
         * Returns the address of the TUIO source
         */
        const char* getSourceAddress() { return source_address.c_str(); }
        
        /**
         * Encodes the sensor dimension
         * @param   w   the sensor width
         * @param   h   the sensor height
         */
        void setDimension(unsigned short w, unsigned short h) {
            dimension = w << 16 | h;
        }
      
        /**
         * Sets the encoded sensor dimension
         * @param   d   the sensor dimension
         */
        void setDimension(unsigned int d) {
            dimension = d;
        }
        
        /**
         * Returns the encoded sensor dimension
         * @return	the encoded sensor dimension
         */
        unsigned int getDimension() {
            return dimension;
        }
        
        /**
         * Returns the decoded sensor width
         * @return	the decoded sensor width
         */
        unsigned short getWidth() {
            unsigned short width = dimension >> 16;
            return width;
        }
        
        /**
         * Returns the decoded sensor height
         * @return	the decoded sensor height
         */
        unsigned short getHeight() {
            unsigned short height = dimension & 0x0000FFFF;
            return height;
        }
        
        /**
         * Sets the last frame time
         * @param   ttime   the TuioTime of the last frame
         */
        void setFrameTime(TuioTime ttime) {
            frameTime = ttime;
        }
        
        /**
         * Returns the last frame time
         * @return	the TuioTime of the last frame
         */
        TuioTime getFrameTime() {
            return frameTime;
        }
    };
}
#endif // INCLUDED_TUIOSOURCE_H
