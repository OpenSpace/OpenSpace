/*
 TUIO C++ Library
 Copyright (c) 2009-2016 Martin Kaltenbrunner <martin@tuio.org>
 WebSockSender (c) 2015 Florian Echtler <floe@butterbrot.org>
 
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

#include "WebSockSender.h"

#ifdef  WIN32
#if not 
	defined int32_t
	typedef DWORD int32_t;
#endif
#endif

using namespace TUIO;

WebSockSender::WebSockSender()
	:TcpSender( 8080 )
{
	local = true;
	buffer_size = MAX_TCP_SIZE;
	port_no = 8080;
}

WebSockSender::WebSockSender(int port)
	:TcpSender( port )
{
	local = true;
	buffer_size = MAX_TCP_SIZE;
	port_no = port;
}

bool WebSockSender::sendOscPacket (osc::OutboundPacketStream *bundle) {
	if (!connected) return false; 
	if ( bundle->Size() > buffer_size ) return false;
	if ( bundle->Size() == 0 ) return false;
	
#ifdef OSC_HOST_LITTLE_ENDIAN             
	data_size[0] =  bundle->Size()>>24;
	data_size[1] = (bundle->Size()>>16) & 255;
	data_size[2] = (bundle->Size()>>8) & 255;
	data_size[3] = (bundle->Size()) & 255;
#else
	*((int32_t*)data_size) = bundle->Size();
#endif

#ifdef WIN32
	std::list<SOCKET>::iterator client;
#else
	std::list<int>::iterator client;
#endif
	
	for (client = tcp_client_list.begin(); client!=tcp_client_list.end(); client++) {
		int len = bundle->Size();
		// add WebSocket header on top
		uint8_t header[4] = {
			0x82,
			(uint8_t)( len & 0xFF), 
			(uint8_t)((len >>8) & 0xFF),
			(uint8_t)( len & 0xFF)
		};
		int hs = 2;
		if (len > 125) { hs = 4; header[1] = 126; }
		memcpy(&data_buffer[0], &header, hs);
		memcpy(&data_buffer[hs], bundle->Data(), bundle->Size());
		send((*client),data_buffer, hs+bundle->Size(),0);
	}

	return true;
}

void WebSockSender::newClient( int tcp_client ) {

	// socket -> file descriptor
#ifdef WIN32
	FILE* conn = _fdopen( tcp_client, "r+" );
#else
	FILE* conn = fdopen( tcp_client, "r+" );
#endif

	// websocket challenge-response
	uint8_t digest[SHA1_HASH_SIZE];
	char buf[1024] = "...";
	char key[1024];

	// read client handshake challenge
	while ((buf[0] != 0) && (buf[0] != '\r')) {
		fgets( buf, sizeof(buf), conn );
		if (strncmp(buf,"Sec-WebSocket-Key: ",19) == 0) {
			strncpy(key,buf+19,sizeof(key));
			key[strlen(buf)-21] = 0;
			break;
		}
	}

	strncat(key,"258EAFA5-E914-47DA-95CA-C5AB0DC85B11",sizeof(key)-strlen(key)-1);
	sha1(digest,(uint8_t*)key,strlen(key));

	snprintf(buf, sizeof(buf),
		"HTTP/1.1 101 Switching Protocols\r\n"
		"Upgrade: websocket\r\n"
		"Connection: Upgrade\r\n"
		"Access-Control-Allow-Origin: *\r\n"
		"Sec-WebSocket-Accept: %s\r\n\r\n",
		base64( digest, SHA1_HASH_SIZE ).c_str() ); 

	send(tcp_client,buf, strlen(buf),0);
}


/*
 * Incredibly minimal implementation of SHA1.
 * Totally independent of any other code (even libc) so it can be
 * run on bare hardware.
 *
 * Copyright (C) 2009 John Stumpo
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *		 * Redistributions of source code must retain the above copyright
 *			 notice, this list of conditions and the following disclaimer.
 *		 * Redistributions in binary form must reproduce the above copyright
 *			 notice, this list of conditions and the following disclaimer in the
 *			 documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY JOHN STUMPO ''AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL JOHN STUMPO BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

void WebSockSender::sha1( uint8_t digest[SHA1_HASH_SIZE], const uint8_t* inbuf, size_t length) {
	
	size_t i, j;
	int remaining_bytes;
	uint32_t h0, h1, h2, h3, h4, a, b, c, d, e, temp;
	uint32_t w[80];
	unsigned char buf[64];
	
	/* Initialize SHA1 hash state. */
	h0 = 0x67452301;
	h1 = 0xefcdab89;
	h2 = 0x98badcfe;
	h3 = 0x10325476;
	h4 = 0xc3d2e1f0;
	
	/* The extra 9 bytes are the pad byte (0x80) and 64-bit bit count that
	 are appended to the data being hashed.	(There will more than likely
	 also be some zeroes in between the 0x80 and the bit count so that we
	 operate on a multiple of 64 bytes; 9 bytes, though, is the minimal
	 amount of extra data.)	*/
	for (i = 0; i < length + 9; i += 64) {
		
		/* Perform any padding necessary. */
		remaining_bytes = length - i;
		if (remaining_bytes >= 64) {
			memcpy(buf, inbuf + i, 64);
		} else if (remaining_bytes >= 0) {
			memcpy(buf, inbuf + i, remaining_bytes);
			memset(buf + remaining_bytes, 0, 64 - remaining_bytes);
			buf[remaining_bytes] = 0x80;
		} else {
			memset(buf, 0, 64);
		}
		
		if (remaining_bytes < 56)
			*(uint32_t*)(buf + 60) = SWAP(length * 8);
		
		/* Build the input array. */
		for (j = 0; j < 16; j++)
			w[j] = SWAP(*(uint32_t*)(buf + j * 4));
		
		for (j = 16; j < 80; j++)
			w[j] = ROL(w[j - 3] ^ w[j - 8] ^ w[j - 14] ^ w[j - 16], 1);
		
		/* Load hash state. */
		a = h0;
		b = h1;
		c = h2;
		d = h3;
		e = h4;
		
		for (j = 0; j < 80; j++) {
			
			if (j < 20)
				temp = ((b & c) | ((~b) & d)) + 0x5a827999;
			else if (j < 40)
				temp = (b ^ c ^ d) + 0x6ed9eba1;
			else if (j < 60)
				temp = ((b & c) | (b & d) | (c & d)) + 0x8f1bbcdc;
			else
				temp = (b ^ c ^ d) + 0xca62c1d6;
			
			temp += ROL(a, 5) + e + w[j];
			
			e = d;
			d = c;
			c = ROR(b, 2);
			b = a;
			a = temp;
		}
		
		/* Incorporate the results of the hash operation. */
		h0 += a;
		h1 += b;
		h2 += c;
		h3 += d;
		h4 += e;
	}
	
	/* Write the hash into the output buffer. */
	*(uint32_t*)(digest) = SWAP(h0);
	*(uint32_t*)(digest + 4) = SWAP(h1);
	*(uint32_t*)(digest + 8) = SWAP(h2);
	*(uint32_t*)(digest + 12) = SWAP(h3);
	*(uint32_t*)(digest + 16) = SWAP(h4);
}

/* 
 * a very simple base64 encoder, licensed as public domain. original source:
 * https://en.wikibooks.org/wiki/Algorithm_Implementation/Miscellaneous/Base64
 */

const static unsigned char encodeLookup[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
const static unsigned char padCharacter = '=';

std::string WebSockSender::base64( uint8_t* cursor, size_t size ) {
	
	std::string encodedString;
	uint32_t temp;
	
	encodedString.reserve(((size/3) + (size % 3 > 0)) * 4);
	
	for (size_t idx = 0; idx < size/3; idx++) {
		
		temp  = (*cursor++) << 16;
		temp += (*cursor++) << 8;
		temp += (*cursor++);
		
		encodedString.append( 1, encodeLookup[(temp & 0x00FC0000) >> 18] );
		encodedString.append( 1, encodeLookup[(temp & 0x0003F000) >> 12] );
		encodedString.append( 1, encodeLookup[(temp & 0x00000FC0) >> 6 ] );
		encodedString.append( 1, encodeLookup[(temp & 0x0000003F)      ] );
	}
	
	switch (size % 3) {
		case 1:
			temp = (*cursor++) << 16;
			encodedString.append( 1, encodeLookup[(temp & 0x00FC0000) >> 18] );
			encodedString.append( 1, encodeLookup[(temp & 0x0003F000) >> 12] );
			encodedString.append( 2, padCharacter );
			break;
		case 2:
			temp  = (*cursor++) << 16;
			temp += (*cursor++) << 8;
			encodedString.append( 1, encodeLookup[(temp & 0x00FC0000) >> 18] );
			encodedString.append( 1, encodeLookup[(temp & 0x0003F000) >> 12] );
			encodedString.append( 1, encodeLookup[(temp & 0x00000FC0) >> 6 ] );
			encodedString.append( 1, padCharacter);
			break;
	}
	
	return encodedString;
}


