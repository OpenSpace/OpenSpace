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

#include "grk_includes.h"

/** @defgroup MQC MQC - Implementation of an MQ-Coder */
/*@{*/

/** @name Local static functions */
/*@{*/

/**
Output a byte, doing bit-stuffing if necessary.
After a 0xff byte, the next byte must be smaller than 0x90.
@param mqc MQC handle
*/
static void grk_mqc_byteout(grk_mqc_t *mqc);
/**
Renormalize mqc->A and mqc->C while encoding, so that mqc->A stays between 0x8000 and 0x10000
@param mqc MQC handle
*/
static void grk_mqc_renorme(grk_mqc_t *mqc);
/**
Encode the most probable symbol
@param mqc MQC handle
*/
static void grk_mqc_codemps(grk_mqc_t *mqc);
/**
Encode the most least symbol
@param mqc MQC handle
*/
static void grk_mqc_codelps(grk_mqc_t *mqc);
/**
Fill mqc->C with 1's for flushing
@param mqc MQC handle
*/
static void grk_mqc_setbits(grk_mqc_t *mqc);
/**
Set the state of a particular context
@param mqc MQC handle
@param ctxno Number that identifies the context
@param msb The MSB of the new state of the context
@param prob Number that identifies the probability of the symbols for the new state of the context
*/
static void grk_mqc_setstate(grk_mqc_t *mqc, uint32_t ctxno, uint32_t msb, int32_t prob);

/**
FIXME DOC
@param mqc MQC handle
@return
*/
static inline uint8_t grk_mqc_mpsexchange(grk_mqc_t *const mqc);
/**
FIXME DOC
@param mqc MQC handle
@return
*/
static inline uint8_t grk_mqc_lpsexchange(grk_mqc_t *const mqc);
/**
Input a byte
@param mqc MQC handle
*/
static inline void grk_mqc_bytein(grk_mqc_t *const mqc);
/**
Renormalize mqc->A and mqc->C while decoding
@param mqc MQC handle
*/
static inline void grk_mqc_renormd(grk_mqc_t *const mqc);
/*@}*/

/*@}*/

/* <summary> */
/* This array defines all the possible states for a context. */
/* </summary> */
static grk_mqc_state_t mqc_states[47 * 2] = {
    {0x5601, 0, &mqc_states[2], &mqc_states[3]},
    {0x5601, 1, &mqc_states[3], &mqc_states[2]},
    {0x3401, 0, &mqc_states[4], &mqc_states[12]},
    {0x3401, 1, &mqc_states[5], &mqc_states[13]},
    {0x1801, 0, &mqc_states[6], &mqc_states[18]},
    {0x1801, 1, &mqc_states[7], &mqc_states[19]},
    {0x0ac1, 0, &mqc_states[8], &mqc_states[24]},
    {0x0ac1, 1, &mqc_states[9], &mqc_states[25]},
    {0x0521, 0, &mqc_states[10], &mqc_states[58]},
    {0x0521, 1, &mqc_states[11], &mqc_states[59]},
    {0x0221, 0, &mqc_states[76], &mqc_states[66]},
    {0x0221, 1, &mqc_states[77], &mqc_states[67]},
    {0x5601, 0, &mqc_states[14], &mqc_states[13]},
    {0x5601, 1, &mqc_states[15], &mqc_states[12]},
    {0x5401, 0, &mqc_states[16], &mqc_states[28]},
    {0x5401, 1, &mqc_states[17], &mqc_states[29]},
    {0x4801, 0, &mqc_states[18], &mqc_states[28]},
    {0x4801, 1, &mqc_states[19], &mqc_states[29]},
    {0x3801, 0, &mqc_states[20], &mqc_states[28]},
    {0x3801, 1, &mqc_states[21], &mqc_states[29]},
    {0x3001, 0, &mqc_states[22], &mqc_states[34]},
    {0x3001, 1, &mqc_states[23], &mqc_states[35]},
    {0x2401, 0, &mqc_states[24], &mqc_states[36]},
    {0x2401, 1, &mqc_states[25], &mqc_states[37]},
    {0x1c01, 0, &mqc_states[26], &mqc_states[40]},
    {0x1c01, 1, &mqc_states[27], &mqc_states[41]},
    {0x1601, 0, &mqc_states[58], &mqc_states[42]},
    {0x1601, 1, &mqc_states[59], &mqc_states[43]},
    {0x5601, 0, &mqc_states[30], &mqc_states[29]},
    {0x5601, 1, &mqc_states[31], &mqc_states[28]},
    {0x5401, 0, &mqc_states[32], &mqc_states[28]},
    {0x5401, 1, &mqc_states[33], &mqc_states[29]},
    {0x5101, 0, &mqc_states[34], &mqc_states[30]},
    {0x5101, 1, &mqc_states[35], &mqc_states[31]},
    {0x4801, 0, &mqc_states[36], &mqc_states[32]},
    {0x4801, 1, &mqc_states[37], &mqc_states[33]},
    {0x3801, 0, &mqc_states[38], &mqc_states[34]},
    {0x3801, 1, &mqc_states[39], &mqc_states[35]},
    {0x3401, 0, &mqc_states[40], &mqc_states[36]},
    {0x3401, 1, &mqc_states[41], &mqc_states[37]},
    {0x3001, 0, &mqc_states[42], &mqc_states[38]},
    {0x3001, 1, &mqc_states[43], &mqc_states[39]},
    {0x2801, 0, &mqc_states[44], &mqc_states[38]},
    {0x2801, 1, &mqc_states[45], &mqc_states[39]},
    {0x2401, 0, &mqc_states[46], &mqc_states[40]},
    {0x2401, 1, &mqc_states[47], &mqc_states[41]},
    {0x2201, 0, &mqc_states[48], &mqc_states[42]},
    {0x2201, 1, &mqc_states[49], &mqc_states[43]},
    {0x1c01, 0, &mqc_states[50], &mqc_states[44]},
    {0x1c01, 1, &mqc_states[51], &mqc_states[45]},
    {0x1801, 0, &mqc_states[52], &mqc_states[46]},
    {0x1801, 1, &mqc_states[53], &mqc_states[47]},
    {0x1601, 0, &mqc_states[54], &mqc_states[48]},
    {0x1601, 1, &mqc_states[55], &mqc_states[49]},
    {0x1401, 0, &mqc_states[56], &mqc_states[50]},
    {0x1401, 1, &mqc_states[57], &mqc_states[51]},
    {0x1201, 0, &mqc_states[58], &mqc_states[52]},
    {0x1201, 1, &mqc_states[59], &mqc_states[53]},
    {0x1101, 0, &mqc_states[60], &mqc_states[54]},
    {0x1101, 1, &mqc_states[61], &mqc_states[55]},
    {0x0ac1, 0, &mqc_states[62], &mqc_states[56]},
    {0x0ac1, 1, &mqc_states[63], &mqc_states[57]},
    {0x09c1, 0, &mqc_states[64], &mqc_states[58]},
    {0x09c1, 1, &mqc_states[65], &mqc_states[59]},
    {0x08a1, 0, &mqc_states[66], &mqc_states[60]},
    {0x08a1, 1, &mqc_states[67], &mqc_states[61]},
    {0x0521, 0, &mqc_states[68], &mqc_states[62]},
    {0x0521, 1, &mqc_states[69], &mqc_states[63]},
    {0x0441, 0, &mqc_states[70], &mqc_states[64]},
    {0x0441, 1, &mqc_states[71], &mqc_states[65]},
    {0x02a1, 0, &mqc_states[72], &mqc_states[66]},
    {0x02a1, 1, &mqc_states[73], &mqc_states[67]},
    {0x0221, 0, &mqc_states[74], &mqc_states[68]},
    {0x0221, 1, &mqc_states[75], &mqc_states[69]},
    {0x0141, 0, &mqc_states[76], &mqc_states[70]},
    {0x0141, 1, &mqc_states[77], &mqc_states[71]},
    {0x0111, 0, &mqc_states[78], &mqc_states[72]},
    {0x0111, 1, &mqc_states[79], &mqc_states[73]},
    {0x0085, 0, &mqc_states[80], &mqc_states[74]},
    {0x0085, 1, &mqc_states[81], &mqc_states[75]},
    {0x0049, 0, &mqc_states[82], &mqc_states[76]},
    {0x0049, 1, &mqc_states[83], &mqc_states[77]},
    {0x0025, 0, &mqc_states[84], &mqc_states[78]},
    {0x0025, 1, &mqc_states[85], &mqc_states[79]},
    {0x0015, 0, &mqc_states[86], &mqc_states[80]},
    {0x0015, 1, &mqc_states[87], &mqc_states[81]},
    {0x0009, 0, &mqc_states[88], &mqc_states[82]},
    {0x0009, 1, &mqc_states[89], &mqc_states[83]},
    {0x0005, 0, &mqc_states[90], &mqc_states[84]},
    {0x0005, 1, &mqc_states[91], &mqc_states[85]},
    {0x0001, 0, &mqc_states[90], &mqc_states[86]},
    {0x0001, 1, &mqc_states[91], &mqc_states[87]},
    {0x5601, 0, &mqc_states[92], &mqc_states[92]},
    {0x5601, 1, &mqc_states[93], &mqc_states[93]},
};

/*
==========================================================
   local functions
==========================================================
*/

static void grk_mqc_byteout(grk_mqc_t *mqc)
{
    if (mqc->bp < mqc->start) {
        mqc->bp++;
        *mqc->bp = (uint8_t)(mqc->C >> 19);
        mqc->C &= 0x7ffff;
        mqc->COUNT = 8;
    } else if (*mqc->bp == 0xff) {
        mqc->bp++;
        *mqc->bp = (uint8_t)(mqc->C >> 20);
        mqc->C &= 0xfffff;
        mqc->COUNT = 7;
    } else {
        if ((mqc->C & 0x8000000) == 0) {
            mqc->bp++;
            *mqc->bp = (uint8_t)(mqc->C >> 19);
            mqc->C &= 0x7ffff;
            mqc->COUNT = 8;
        } else {
            (*mqc->bp)++;
            if (*mqc->bp == 0xff) {
                mqc->C &= 0x7ffffff;
                mqc->bp++;
                *mqc->bp = (uint8_t)(mqc->C >> 20);
                mqc->C &= 0xfffff;
                mqc->COUNT = 7;
            } else {
                mqc->bp++;
                *mqc->bp = (uint8_t)(mqc->C >> 19);
                mqc->C &= 0x7ffff;
                mqc->COUNT = 8;
            }
        }
    }
}

static void grk_mqc_renorme(grk_mqc_t *mqc)
{
    do {
        mqc->A <<= 1;
        mqc->C <<= 1;
        mqc->COUNT--;
        if (mqc->COUNT == 0) {
            grk_mqc_byteout(mqc);
        }
    } while ((mqc->A & 0x8000) == 0);
}

static void grk_mqc_codemps(grk_mqc_t *mqc)
{
    mqc->A -= (*mqc->curctx)->qeval;
    if ((mqc->A & 0x8000) == 0) {
        if (mqc->A < (*mqc->curctx)->qeval) {
            mqc->A = (*mqc->curctx)->qeval;
        } else {
            mqc->C += (*mqc->curctx)->qeval;
        }
        *mqc->curctx = (*mqc->curctx)->nmps;
        grk_mqc_renorme(mqc);
    } else {
        mqc->C += (*mqc->curctx)->qeval;
    }
}

static void grk_mqc_codelps(grk_mqc_t *mqc)
{
    mqc->A -= (*mqc->curctx)->qeval;
    if (mqc->A < (*mqc->curctx)->qeval) {
        mqc->C += (*mqc->curctx)->qeval;
    } else {
        mqc->A = (*mqc->curctx)->qeval;
    }
    *mqc->curctx = (*mqc->curctx)->nlps;
    grk_mqc_renorme(mqc);
}

static void grk_mqc_setbits(grk_mqc_t *mqc)
{
    uint32_t tempc = mqc->C + mqc->A;
    mqc->C |= 0xffff;
    if (mqc->C >= tempc) {
        mqc->C -= 0x8000;
    }
}

static inline uint8_t grk_mqc_mpsexchange(grk_mqc_t *const mqc)
{
	uint8_t d;
    if (mqc->A < (*mqc->curctx)->qeval) {
        d = (uint8_t)(1 - (*mqc->curctx)->mps);
        *mqc->curctx = (*mqc->curctx)->nlps;
    } else {
        d = (*mqc->curctx)->mps;
        *mqc->curctx = (*mqc->curctx)->nmps;
    }
    return d;
}

static inline uint8_t grk_mqc_lpsexchange(grk_mqc_t *const mqc)
{
	uint8_t d;
    if (mqc->A < (*mqc->curctx)->qeval) {
        mqc->A = (*mqc->curctx)->qeval;
        d = (*mqc->curctx)->mps;
        *mqc->curctx = (*mqc->curctx)->nmps;
    } else {
        mqc->A = (*mqc->curctx)->qeval;
        d = (uint8_t)(1 - (*mqc->curctx)->mps);
        *mqc->curctx = (*mqc->curctx)->nlps;
    }
    return d;
}

static void grk_mqc_bytein(grk_mqc_t *const mqc)
{
    if (mqc->bp < mqc->end) {
        uint8_t nextByte = (mqc->bp + 1 < mqc->end) ? *(mqc->bp + 1) : 0xFF;
        if (mqc->currentByteIs0xFF) {
            if (nextByte > 0x8F) {
				// found termination marker - synthesize 1's in C register and do not increment bp
                mqc->C += 0xFF;
                mqc->COUNT = 8;
            } 
			else {
				// bit stuff next byte and add to C register
                mqc->bp++;
                mqc->C += nextByte << 1;
                mqc->COUNT = 7;
            }
        } else {
			// add next byte to C register
            mqc->bp++;
            mqc->C += nextByte;
            mqc->COUNT = 8;
        }
		mqc->currentByteIs0xFF = nextByte == 0xFF;
    } else {
		// end of code stream has been reached - synthesize 1's in C register and do not increment bp
        mqc->C += 0xFF;
        mqc->COUNT = 8;
    }
}


static inline void grk_mqc_renormd(grk_mqc_t *const mqc)
{
    do {
        if (mqc->COUNT == 0) {
            grk_mqc_bytein(mqc);
        }
        mqc->A <<= 1;
        mqc->C <<= 1;
        mqc->COUNT--;
    } while (mqc->A < 0x8000);
}

/*
==========================================================
   MQ-Coder interface
==========================================================
*/

void grk_mqc_setcurctx(grk_mqc_t *mqc, uint8_t ctxno) {
	if (mqc->debug_mqc.debug_state & OPJ_PLUGIN_STATE_DEBUG) {
		mqc->debug_mqc.context_number = ctxno;
	}
	mqc->curctx = &mqc->ctxs[(uint32_t)ctxno];
}

grk_mqc_t* grk_mqc_create(void)
{
    grk_mqc_t *mqc = (grk_mqc_t*)grk_calloc(1,sizeof(grk_mqc_t));
    return mqc;
}

void grk_mqc_destroy(grk_mqc_t *mqc)
{
    if(mqc) {
        grk_free(mqc);
    }
}

// beware: always outputs ONE LESS than actual number of encoded bytes, until after flush is called.
// After flush, the result returned is correct.
int32_t grk_mqc_numbytes(grk_mqc_t *mqc)
{
    ptrdiff_t diff = mqc->bp - mqc->start;
    return (int32_t)diff;
}

void grk_mqc_init_enc(grk_mqc_t *mqc, uint8_t *bp)
{
	grk_mqc_resetstates(mqc);
    grk_mqc_setcurctx(mqc, 0);
    mqc->A = 0x8000;
    mqc->C = 0;
    mqc->bp = bp - 1;
    mqc->COUNT = 12;
    mqc->start = bp;
	if (opj_plugin_get_debug_state() & OPJ_PLUGIN_STATE_DEBUG) {
		mqc->debug_mqc.contextStream = NULL;
		mqc->debug_mqc.contextCacheCount = 0;
		mqc->debug_mqc.contextStreamByteCount = 0;
		mqc->debug_mqc.debug_state = opj_plugin_get_debug_state();
	}
}

void grk_mqc_encode(grk_mqc_t *mqc, uint32_t d)
{
	if ((mqc->debug_mqc.debug_state  & OPJ_PLUGIN_STATE_DEBUG) &&
		!(mqc->debug_mqc.debug_state & OPJ_PLUGIN_STATE_PRE_TR1)) {
		nextCXD(&mqc->debug_mqc, d);
	}
    if ((*mqc->curctx)->mps == d) {
        grk_mqc_codemps(mqc);
    } else {
        grk_mqc_codelps(mqc);
    }
}

void grk_mqc_flush(grk_mqc_t *mqc)
{
    grk_mqc_setbits(mqc);
    mqc->C <<= mqc->COUNT;
    grk_mqc_byteout(mqc);
    mqc->C <<= mqc->COUNT;
    grk_mqc_byteout(mqc);

    if (*mqc->bp != 0xff) {
        mqc->bp++;
    }
}

void grk_mqc_bypass_init_enc(grk_mqc_t *mqc)
{
    mqc->C = 0;
    mqc->COUNT = 8;
	//note: mqc->bp is guaranteed to be greater than mqc->start, since we have already performed
	// at least one flush
	mqc->bp--;
	if (*mqc->bp == 0xff) {
		mqc->COUNT = 7;
	}

}

void grk_mqc_bypass_enc(grk_mqc_t *mqc, uint32_t d)
{
    mqc->COUNT--;
    mqc->C = mqc->C + (d << mqc->COUNT);
    if (mqc->COUNT == 0) {
        mqc->bp++;
        *mqc->bp = (uint8_t)mqc->C;
        mqc->COUNT = 8;
		mqc->C = 0;
		// bit stuffing ensures that most significant bit equals zero
		// for byte following 0xFF
        if (*mqc->bp == 0xff) {
            mqc->COUNT = 7;
        }
    }
}

void grk_mqc_bypass_flush_enc(grk_mqc_t *mqc)
{
    uint8_t bit_padding = 0;
    if (mqc->COUNT != 8) {
        while (mqc->COUNT > 0) {
            mqc->COUNT--;
            mqc->C += (uint32_t)(bit_padding << mqc->COUNT);
            bit_padding = (bit_padding + 1) & 0x01;
        }
        mqc->bp++;
        *mqc->bp = (uint8_t)mqc->C;
	}
	if (*mqc->bp != 0xff) {
		mqc->bp++;
	}
}

uint32_t grk_mqc_restart_enc(grk_mqc_t *mqc)
{
    uint32_t correction = 1;

    /* <flush part> */
    int32_t n = (int32_t)(27 - 15 - mqc->COUNT);
    mqc->C <<= mqc->COUNT;
    while (n > 0) {
        grk_mqc_byteout(mqc);
        n -= (int32_t)mqc->COUNT;
        mqc->C <<= mqc->COUNT;
    }
    grk_mqc_byteout(mqc);

    return correction;
}

void grk_mqc_restart_init_enc(grk_mqc_t *mqc)
{
	grk_mqc_setcurctx(mqc, 0);
	mqc->A = 0x8000;
	mqc->C = 0;
	mqc->COUNT = 12;
	if (mqc->bp >= mqc->start) {
		mqc->bp--;
		if (*mqc->bp == 0xff) {
			mqc->COUNT = 13;
		}
	}
}

void grk_mqc_erterm_enc(grk_mqc_t *mqc)
{
    int32_t k = (int32_t)(11 - mqc->COUNT + 1);

    while (k > 0) {
        mqc->C <<= mqc->COUNT;
        mqc->COUNT = 0;
        grk_mqc_byteout(mqc);
        k -= (int32_t)mqc->COUNT;
    }

    if (*mqc->bp != 0xff) {
        grk_mqc_byteout(mqc);
    }
}

void grk_mqc_segmark_enc(grk_mqc_t *mqc)
{
    uint32_t i;
    grk_mqc_setcurctx(mqc, 18);

    for (i = 1; i < 5; i++) {
        grk_mqc_encode(mqc, i % 2);
    }
}

void grk_mqc_init_dec(grk_mqc_t *mqc, uint8_t *bp, uint32_t len)
{
    grk_mqc_setcurctx(mqc, 0);
    mqc->start = bp;
    mqc->end = bp + len;
    mqc->bp = bp;
	uint8_t currentByte = (len > 0) ? *mqc->bp : 0xFF;
	mqc->currentByteIs0xFF = currentByte == 0xFF;
	mqc->C = (uint32_t)(currentByte << 8);
    grk_mqc_bytein(mqc);
    mqc->C <<= 7;
    mqc->COUNT -= 7;
    mqc->A = 0x8000;
}

uint8_t grk_mqc_decode(grk_mqc_t *const mqc)
{
	uint8_t d;
    mqc->A -= (*mqc->curctx)->qeval;
    if ((mqc->C >> 8) < (*mqc->curctx)->qeval) {
        d = grk_mqc_lpsexchange(mqc);
        grk_mqc_renormd(mqc);
    } else {
        mqc->C -= (*mqc->curctx)->qeval << 8;
        if ((mqc->A & 0x8000) == 0) {
            d = grk_mqc_mpsexchange(mqc);
            grk_mqc_renormd(mqc);
        } else {
            d = (*mqc->curctx)->mps;
        }
    }

    return d;
}

void grk_mqc_resetstates(grk_mqc_t *mqc)
{
    uint32_t i;
    for (i = 0; i < MQC_NUMCTXS; i++) {
        mqc->ctxs[i] = mqc_states;
    }
	grk_mqc_setstate(mqc, T1_CTXNO_UNI, 0, 46);
	grk_mqc_setstate(mqc, T1_CTXNO_AGG, 0, 3);
	grk_mqc_setstate(mqc, T1_CTXNO_ZC, 0, 4);
}

void grk_mqc_setstate(grk_mqc_t *mqc, uint32_t ctxno, uint32_t msb, int32_t prob)
{
    mqc->ctxs[ctxno] = &mqc_states[msb + (uint32_t)(prob << 1)];
}


