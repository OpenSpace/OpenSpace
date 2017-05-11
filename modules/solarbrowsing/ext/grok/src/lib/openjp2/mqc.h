/*
*    Copyright (C) 2016-2017 Grok Image Compression Inc.
*
*    This source code is free software: you can redistribute it and/or  modify
*    it under the terms of the GNU Affero General Public License, version 3,
*    as published by the Free Software Foundation.
*
*    This source code is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU Affero General Public License for more details.
*
*    You should have received a copy of the GNU Affero General Public License
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*
*
*    This source code incorporates work covered by the following copyright and
*    permission notice:
*
 * The copyright in this software is being made available under the 2-clauses
 * BSD License, included below. This software may be subject to other third
 * party and contributor rights, including patent rights, and no such rights
 * are granted under this license.
 *
 * Copyright (c) 2002-2014, Universite catholique de Louvain (UCL), Belgium
 * Copyright (c) 2002-2014, Professor Benoit Macq
 * Copyright (c) 2001-2003, David Janssens
 * Copyright (c) 2002-2003, Yannick Verschueren
 * Copyright (c) 2003-2007, Francois-Olivier Devaux
 * Copyright (c) 2003-2014, Antonin Descampe
 * Copyright (c) 2005, Herve Drolon, FreeImage Team
 * Copyright (c) 2008, Jerome Fimes, Communications & Systemes <jerome.fimes@c-s.fr>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS `AS IS'
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */


#pragma once

/**
@file mqc.h
@brief Implementation of an MQ-Coder (MQC)

The functions in MQC.C have for goal to realize the MQ-coder operations. The functions
in MQC.C are used by some function in T1.C.
*/

/** @defgroup MQC MQC - Implementation of an MQ-Coder */
/*@{*/

/**
This struct defines the state of a context.
*/
typedef struct grk_mqc_state {
    /** the probability of the Least Probable Symbol (0.75->0x8000, 1.5->0xffff) */
    uint16_t qeval;
    /** the Most Probable Symbol (0 or 1) */
    uint8_t mps;
    /** next state if the next encoded symbol is the MPS */
    grk_mqc_state *nmps;
    /** next state if the next encoded symbol is the LPS */
    grk_mqc_state *nlps;
} grk_mqc_state_t;

#define MQC_NUMCTXS 19

/**
MQ coder
*/

typedef struct grk_mqc {
    uint32_t C;
    uint32_t A;
    uint32_t COUNT;
    uint8_t *bp;
	bool currentByteIs0xFF;
    uint8_t *start;
    uint8_t *end;
    grk_mqc_state_t *ctxs[MQC_NUMCTXS];
    grk_mqc_state_t **curctx;

	plugin_debug_mqc_t debug_mqc;

} grk_mqc_t;

/** @name Exported functions */
/*@{*/
/* ----------------------------------------------------------------------- */
/**
Create a new MQC handle
@return Returns a new MQC handle if successful, returns NULL otherwise
*/
grk_mqc_t* grk_mqc_create(void);
/**
Destroy a previously created MQC handle
@param mqc MQC handle to destroy
*/
void grk_mqc_destroy(grk_mqc_t *mqc);
/**
Return the number of bytes written/read since initialisation
@param mqc MQC handle
@return Returns the number of bytes already encoded
*/
int32_t grk_mqc_numbytes(grk_mqc_t *mqc);
/**
Reset the states of all the context of the coder/decoder
(each context is set to a state where 0 and 1 are more or less equiprobable)
@param mqc MQC handle
*/
void grk_mqc_resetstates(grk_mqc_t *mqc);

/**
Initialize the encoder
@param mqc MQC handle
@param bp Pointer to the start of the buffer where the bytes will be written
*/
void grk_mqc_init_enc(grk_mqc_t *mqc, uint8_t *bp);
/**
Set the current context used for coding/decoding
@param mqc MQC handle
@param ctxno Number that identifies the context
*/
void grk_mqc_setcurctx(grk_mqc_t *mqc, uint8_t ctxno);
/**
Encode a symbol using the MQ-coder
@param mqc MQC handle
@param d The symbol to be encoded (0 or 1)
*/
void grk_mqc_encode(grk_mqc_t *mqc, uint32_t d);
/**
Flush the encoder, so that all remaining data is written
@param mqc MQC handle
*/
void grk_mqc_flush(grk_mqc_t *mqc);
/**
BYPASS mode switch, initialization operation.
JPEG 2000 p 505.
<h2>Not fully implemented and tested !!</h2>
@param mqc MQC handle
*/
void grk_mqc_bypass_init_enc(grk_mqc_t *mqc);
/**
BYPASS mode switch, coding operation.
JPEG 2000 p 505.
<h2>Not fully implemented and tested !!</h2>
@param mqc MQC handle
@param d The symbol to be encoded (0 or 1)
*/
void grk_mqc_bypass_enc(grk_mqc_t *mqc, uint32_t d);
/**
BYPASS mode switch, flush operation
<h2>Not fully implemented and tested !!</h2>
@param mqc MQC handle
*/
void grk_mqc_bypass_flush_enc(grk_mqc_t *mqc);
/**
RESTART mode switch (TERMALL)
@param mqc MQC handle
@return Returns 1 (always)
*/
uint32_t grk_mqc_restart_enc(grk_mqc_t *mqc);
/**
RESTART mode switch (TERMALL) reinitialisation
@param mqc MQC handle
*/
void grk_mqc_restart_init_enc(grk_mqc_t *mqc);
/**
ERTERM mode switch (PTERM)
@param mqc MQC handle
*/
void grk_mqc_erterm_enc(grk_mqc_t *mqc);
/**
SEGMARK mode switch (SEGSYM)
@param mqc MQC handle
*/
void grk_mqc_segmark_enc(grk_mqc_t *mqc);
/**
Initialize the decoder
@param mqc MQC handle
@param bp Pointer to the start of the buffer from which the bytes will be read
@param len Length of the input buffer
*/
void grk_mqc_init_dec(grk_mqc_t *mqc, uint8_t *bp, uint32_t len);
/**
Decode a symbol
@param mqc MQC handle
@return Returns the decoded symbol (0 or 1)
*/
uint8_t grk_mqc_decode(grk_mqc_t * const mqc);
/* ----------------------------------------------------------------------- */
/*@}*/

/*@}*/


