//
// TFFlashLCSHMEM.h
// Touché
//
// Created by Georg Kaindl on 16/3/09.
//
//  Copyright (C) 2009 Georg Kaindl
//
//  This file is part of Touché.
//
//  Touché is free software: you can redistribute it and/or modify
//  it under the terms of the GNU Lesser General Public License as
//  published by the Free Software Foundation, either version 3 of
//  the License, or (at your option) any later version.
//
//  Touché is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU Lesser General Public License for more details.
//
//  You should have received a copy of the GNU Lesser General Public
//  License along with Touché. If not, see <http://www.gnu.org/licenses/>.


#if !defined(__TFFlashLCSHMEM_H__)
#define __TFFlashLCSHMEM_H__ 1

#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */ 
	
#ifdef WIN32
#include <windows.h>
typedef	DWORD u_int32_t;
#else
#include <sys/types.h>
#include <semaphore.h>
#endif
	
#define TFLCS_LISTENER_NAME_MAX_LEN		(64)
#define TFLCS_LISTENER_METHOD_MAX_LEN	(64)
	
	typedef struct TFLCSLocalConnection_t {
#ifdef WIN32
		HANDLE	semaphore;
		HANDLE	mapFile;
		LPVOID	mapAddress;	
#else
		int		shmid;
		sem_t*	semaphore;
#endif
		int		open;
		char*	data;
		char	listenerName[TFLCS_LISTENER_NAME_MAX_LEN];
		char	listenerMethod[TFLCS_LISTENER_METHOD_MAX_LEN];
	} TFLCSLocalConnection_t;
	
	typedef enum {
		TFLCSErrorSuccess					= 0,
		TFLCSErrorInvalidArgument			= 1,
		TFLCSErrorShmemKeyNotFound			= 2,
		TFLCSErrorSemaphoreCreationFailed	= 3,
		TFLCSErrorShmemIDNotFound			= 4,
		TFLCSErrorShmemOpeningFailed		= 5,
		TFLCSErrorOutOfMemory				= 6
	} TFLCSError_t;
	
	// much like errno, this will hold the last error that happened
	extern TFLCSError_t	TFLCSErrno;
	
	// connect to a Flash LocalConnection
	// returns pointer to structure on success, NULL otherwise
	// if the connection could not be opened right away (because Flash is not yet running),
	// a non-null pointer is returned, but the "open" member of the structure will be 0
	// and TFLCSErrno will be set accordingly. the connection is truly open once "open" is
	// non-zero.
	TFLCSLocalConnection_t* TFLCSConnect(const char* listenerName,
										 const char* listenerMethod,
										 void* shmemKey,
										 void* semaphoreKey);
	
	// disconnect a Flash LocalConnection
	void TFLCSDisconnect(TFLCSLocalConnection_t* connection);
	
	// changes the connection listener name
	void TFLCSChangeListenerName(TFLCSLocalConnection_t* connection, const char* newListenerName);
	
	// changes the connection method name
	void TFLCSChangeMethodName(TFLCSLocalConnection_t* connection, const char* newMethodName);
	
	// returns non-zero if the given connection's Flash client is connected, 0 otherwise
	int TFLCSConnectionHasConnectedClient(TFLCSLocalConnection_t* connection);
	
	// tries to determine the key for the shared memory region used by the Flash LocalConnection
	// returns a pointer to static memory on success, NULL on error.
	void* TFLCSGuessShmemKey();
	
	// get the current tick count (milliseconds since the system was started)
	u_int32_t TFLCSGetTickCount();
	
	// write an AMF3-encoded integer into a buffer
	// returns the updated buffer position
	int TFLCSWriteAMF3Integer(char* buffer, int value, int pos);
	
	// write an AMF-encoded string into a buffer
	// returns the updated buffer position
	int TFLCSWriteAMFString(char* buffer, const char * str, int pos);
	
	// write an AMF3-encoded byte array into a buffer
	// returns the updated buffer position
	int TFLCSWriteAMF3ByteArray(char* buffer, const char* bytes, int pos, int len);
	
	// writes the AMF envelope header for an LC method call
	// returns the position in the buffer
	int TFLCSWriteLCAMFEnvelopeHeader(TFLCSLocalConnection_t* connection);
	
	// writes the AMF envelope trailer for an LC method call
	// returns the updated buffer position
	int TFLCSWriteLCAMFEnvelopeTrailer(TFLCSLocalConnection_t* connection, int pos);
	
	// sends the given data of length len over the given connection as a ByteArray
	// returns 1 on success, 0 otherwise
	int TFLCSSendByteArray(TFLCSLocalConnection_t* connection, const char* bytes, int len);
	
	// gets the (string) names of all connected LC connections
	// returns the number of found strings.
	int TFLCSGetConnectedConnectionNames(TFLCSLocalConnection_t* connection, char* dest, int destLen);
	
	// dumps a given buffer (size bytes starting at offset) to stdout in a nice hex/ascii format.
	// useful for debugging purposes.
	void TFLCSDumpMemory(char* buffer, int offset, int size);
	
#ifdef __cplusplus
}
#endif /* __cplusplus */ 		
#endif //__TFFlashLCSHMEM_H__

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

#ifndef INCLUDED_FLASHSENDER_H
#define INCLUDED_FLASHSENDER_H

#include "OscSender.h"

#define MAX_FLASH_SIZE (40976)
#define DEFAULT_LC_CONN_NAME	"_OscDataStream"
#define DEFAULT_LC_METH_NAME    "receiveOscData"

namespace TUIO {
	
	/**
	 * The FlashSender implements the Flash LocalConnection transport method for OSC
	 *
	 * @author Martin Kaltenbrunner
	 * @version 1.1.6
	 */ 
	class LIBDECL FlashSender : public OscSender {
				
	public:

		/**
		 * The default constructor creates a FlashSender using a Flash LocalConnection on localhost
		 */
		FlashSender();

		/**
		 * The default constructor creates a FlashSender using a Flash LocalConnection on localhost
		 */
		FlashSender(const char *conn_name, const char *meth_name);
		
		/**
		 * The destructor closes the connection. 
		 */
		virtual ~FlashSender();
		
		/**
		 * This method delivers the provided OSC data
		 *
		 * @param *bundle  the OSC stream to deliver
		 * @return true if the data was delivered successfully
		 */
		
		bool sendOscPacket (osc::OutboundPacketStream *bundle);

		/**
		 * This method returns the connection state
		 *
		 * @return true if the connection is alive
		 */
		bool isConnected ();
		
		const char* tuio_type() { return "TUIO/FLC"; }
		
	private:
		TFLCSLocalConnection_t* lcConnection;
	};
}
#endif /* INCLUDED_FLASHSENDER_H */
