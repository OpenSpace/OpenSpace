/*
 TUIO C++ Library
 Copyright (c) 2005-2016 Martin Kaltenbrunner <martin@tuio.org>
 
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

#ifndef INCLUDED_LIBEXPORT_H
#define INCLUDED_LIBEXPORT_H

#ifdef WIN32
	#pragma warning(disable: 4251) // disable annoying template exporting warnings
	#pragma warning(disable: 4275) // disable warning caused by not exported OSC classes

	#ifdef LIB_EXPORT
		#define LIBDECL __declspec(dllexport)
	#else
//		#define LIBDECL __declspec(dllimport)
		#define LIBDECL
	#endif
#else
	#define LIBDECL
#endif

#endif
