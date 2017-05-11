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
 * Copyright (c) 2006-2007, Parvatha Elangovan
 * Copyright (c) 2008, Jerome Fimes, Communications & Systemes <jerome.fimes@c-s.fr>
 * Copyright (c) 2011-2012, Centre National d'Etudes Spatiales (CNES), France
 * Copyright (c) 2012, CS Systemes d'Information, France
 *
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
@file j2k.h
@brief The JPEG-2000 Codestream Reader/Writer (J2K)

The functions in J2K.C have for goal to read/write the several parts of the codestream: markers and data.
*/

/** @defgroup J2K J2K - JPEG-2000 codestream reader/writer */
/*@{*/

#define J2K_CP_CSTY_PRT 0x01
#define J2K_CP_CSTY_SOP 0x02
#define J2K_CP_CSTY_EPH 0x04
#define J2K_CCP_CSTY_PRT 0x01
#define J2K_CCP_CBLKSTY_LAZY 0x01     /**< Selective arithmetic coding bypass */
#define J2K_CCP_CBLKSTY_RESET 0x02    /**< Reset context probabilities on coding pass boundaries */
#define J2K_CCP_CBLKSTY_TERMALL 0x04  /**< Termination on each coding pass */
#define J2K_CCP_CBLKSTY_VSC 0x08      /**< Vertically stripe causal context */
#define J2K_CCP_CBLKSTY_PTERM 0x10    /**< Predictable termination */
#define J2K_CCP_CBLKSTY_SEGSYM 0x20   /**< Segmentation symbols are used */
#define J2K_CCP_QNTSTY_NOQNT 0
#define J2K_CCP_QNTSTY_SIQNT 1
#define J2K_CCP_QNTSTY_SEQNT 2

#define OPJ_J2K_DEFAULT_CBLK_DATA_SIZE 8192

/* ----------------------------------------------------------------------- */

#define J2K_MS_SOC 0xff4f	/**< SOC marker value */
#define J2K_MS_SOT 0xff90	/**< SOT marker value */
#define J2K_MS_SOD 0xff93	/**< SOD marker value */
#define J2K_MS_EOC 0xffd9	/**< EOC marker value */
#define J2K_MS_SIZ 0xff51	/**< SIZ marker value */
#define J2K_MS_COD 0xff52	/**< COD marker value */
#define J2K_MS_COC 0xff53	/**< COC marker value */
#define J2K_MS_RGN 0xff5e	/**< RGN marker value */
#define J2K_MS_QCD 0xff5c	/**< QCD marker value */
#define J2K_MS_QCC 0xff5d	/**< QCC marker value */
#define J2K_MS_POC 0xff5f	/**< POC marker value */
#define J2K_MS_TLM 0xff55	/**< TLM marker value */
#define J2K_MS_PLM 0xff57	/**< PLM marker value */
#define J2K_MS_PLT 0xff58	/**< PLT marker value */
#define J2K_MS_PPM 0xff60	/**< PPM marker value */
#define J2K_MS_PPT 0xff61	/**< PPT marker value */
#define J2K_MS_SOP 0xff91	/**< SOP marker value */
#define J2K_MS_EPH 0xff92	/**< EPH marker value */
#define J2K_MS_CRG 0xff63	/**< CRG marker value */
#define J2K_MS_COM 0xff64	/**< COM marker value */
#define J2K_MS_CBD 0xff78	/**< CBD marker value */
#define J2K_MS_MCC 0xff75	/**< MCC marker value */
#define J2K_MS_MCT 0xff74	/**< MCT marker value */
#define J2K_MS_MCO 0xff77	/**< MCO marker value */

#define J2K_MS_UNK 0		/**< UNKNOWN marker value */

/* ----------------------------------------------------------------------- */

/**
 * Values that specify the status of the decoding process when decoding the main header.
 * These values may be combined with a | operator.
 * */
typedef enum J2K_STATUS {
    J2K_DEC_STATE_NONE  =  0x0000, /**< a SOC marker is expected */
    J2K_DEC_STATE_MHSOC  = 0x0001, /**< a SOC marker is expected */
    J2K_DEC_STATE_MHSIZ  = 0x0002, /**< a SIZ marker is expected */
    J2K_DEC_STATE_MH     = 0x0004, /**< the decoding process is in the main header */
    J2K_DEC_STATE_TPHSOT = 0x0008, /**< the decoding process is in a tile part header and expects a SOT marker */
    J2K_DEC_STATE_TPH    = 0x0010, /**< the decoding process is in a tile part header */
    J2K_DEC_STATE_MT     = 0x0020, /**< the EOC marker has just been read */
    J2K_DEC_STATE_NEOC   = 0x0040, /**< the decoding process must not expect a EOC marker because the codestream is truncated */
	J2K_DEC_STATE_DATA	 = 0x0080, /**< the decoding process is expecting to read tile data from the code stream */
    J2K_DEC_STATE_EOC	 = 0x0100, /**< the decoding process has encountered the EOC marker */
    J2K_DEC_STATE_ERR    = 0x8000  /**< the decoding process has encountered an error (FIXME warning V1 = 0x0080)*/
} J2K_STATUS;

/**
 * Type of elements storing in the MCT data
 */
typedef enum MCT_ELEMENT_TYPE {
    MCT_TYPE_INT16 = 0,		/** MCT data is stored as signed shorts*/
    MCT_TYPE_INT32 = 1,		/** MCT data is stored as signed integers*/
    MCT_TYPE_FLOAT = 2,		/** MCT data is stored as floats*/
    MCT_TYPE_DOUBLE = 3		/** MCT data is stored as doubles*/
} J2K_MCT_ELEMENT_TYPE;

/**
 * Type of MCT array
 */
typedef enum MCT_ARRAY_TYPE {
    MCT_TYPE_DEPENDENCY = 0,
    MCT_TYPE_DECORRELATION = 1,
    MCT_TYPE_OFFSET = 2
} J2K_MCT_ARRAY_TYPE;

/* ----------------------------------------------------------------------- */

/**
T2 encoding mode
*/
typedef enum T2_MODE {
    THRESH_CALC = 0,	/** Function called in Rate allocation process*/
    FINAL_PASS = 1		/** Function called in Tier 2 process*/
} J2K_T2_MODE;

/**
 * Quantization stepsize
 */
typedef struct opj_stepsize {
    /** exponent */
    uint32_t expn;
    /** mantissa */
    uint32_t mant;
} opj_stepsize_t;

/**
Tile-component coding parameters
*/
typedef struct opj_tccp {
    /** coding style */
    uint32_t csty;
    /** number of resolutions */
    uint32_t numresolutions;
    /** log2(code-blocks width) */
    uint32_t cblkw;
    /** log2(code-blocks height) */
    uint32_t cblkh;
    /** code-block coding style */
    uint32_t cblksty;
    /** discrete wavelet transform identifier */
    uint32_t qmfbid;
	// true if there is a QCC marker, otherwise false
	bool hasQCC;
    /** quantisation style */
    uint32_t qntsty;
    /** stepsizes used for quantization */
    opj_stepsize_t stepsizes[OPJ_J2K_MAXBANDS];
	// number of step sizes read from QCC marker
	uint32_t numStepSizes;
    /** number of guard bits */
    uint32_t numgbits;
    /** Region Of Interest shift */
    uint32_t roishift;
    /** precinct width (power of 2 exponent) */
    uint32_t prcw[OPJ_J2K_MAXRLVLS];
    /** precinct height (power of 2 exponent) */
    uint32_t prch[OPJ_J2K_MAXRLVLS];
    /** the dc_level_shift **/
    int32_t m_dc_level_shift;
}
opj_tccp_t;



/**
 * FIXME DOC
 */
typedef struct grk_mct_data {
    J2K_MCT_ELEMENT_TYPE m_element_type;
    J2K_MCT_ARRAY_TYPE	 m_array_type;
    uint32_t			 m_index;
    uint8_t *			 m_data;
    uint32_t			 m_data_size;
}
grk_mct_data_t;

/**
 * FIXME DOC
 */
typedef struct opj_simple_mcc_decorrelation_data {
    uint32_t			 m_index;
    uint32_t			 m_nb_comps;
    grk_mct_data_t *	 m_decorrelation_array;
    grk_mct_data_t *	 m_offset_array;
    uint32_t			 m_is_irreversible : 1;
}
opj_simple_mcc_decorrelation_data_t;

typedef struct opj_ppx_struct {
    uint8_t*   m_data; /* m_data == NULL => Zppx not read yet */
    uint32_t	m_data_size;
} opj_ppx;

/**
Tile coding parameters :
this structure is used to store coding/decoding parameters common to all
tiles (information like COD, COC in main header)
*/
struct grk_tcp_t {
	grk_tcp_t();

    /** coding style */
    uint32_t csty;
    /** progression order */
    OPJ_PROG_ORDER prg;
    /** number of layers */
    uint32_t numlayers;
    uint32_t num_layers_to_decode;
    /** multi-component transform identifier */
    uint32_t mct;
    /** rates of layers */
    double rates[100];
    /** number of progression order changes */
    uint32_t numpocs;
    /** progression order changes */
    opj_poc_t pocs[32];

    /** number of ppt markers (reserved size) */
    uint32_t ppt_markers_count;
    /** ppt markers data (table indexed by Zppt) */
    opj_ppx* ppt_markers;

    /** packet header store there for future use in t2_decode_packet */
    uint8_t *ppt_data;
    /** used to keep a track of the allocated memory */
    uint8_t *ppt_buffer;
    /** Number of bytes stored inside ppt_data*/
    uint32_t ppt_data_size;
    /** size of ppt_data*/
    uint32_t ppt_len;
    /** fixed_quality */
    double distoratio[100];
	// quantization style as read from QCD marker
	uint32_t qntsty;
	// number of step sizes as read from QCD marker
	uint32_t numStepSizes;
    /** tile-component coding parameters */
    opj_tccp_t *tccps;
    /** number of tile parts for the tile. */
    uint32_t m_nb_tile_parts;

    opj_seg_buf_t* m_data;

    /** encoding norms */
    double *	mct_norms;
    /** the mct decoding matrix */
    float *	m_mct_decoding_matrix;
    /** the mct coding matrix */
    float *	m_mct_coding_matrix;
    /** mct records */
    grk_mct_data_t * m_mct_records;
    /** the number of mct records. */
    uint32_t m_nb_mct_records;
    /** the max number of mct records. */
    uint32_t m_nb_max_mct_records;
    /** mcc records */
    opj_simple_mcc_decorrelation_data_t * m_mcc_records;
    /** the number of mct records. */
    uint32_t m_nb_mcc_records;
    /** the max number of mct records. */
    uint32_t m_nb_max_mcc_records;


    /***** FLAGS *******/
    /** If cod == 1 --> there was a COD marker for the present tile */
    uint32_t cod : 1;
    /** If ppt == 1 --> there was a PPT marker for the present tile */
    uint32_t ppt : 1;
    /** indicates if a POC marker has been used O:NO, 1:YES */
    uint32_t POC : 1;
} ;




typedef struct opj_encoding_param {
    /** Maximum rate for each component. If == 0, component size limitation is not considered */
    uint32_t m_max_comp_size;
    /** Position of tile part flag in progression order*/
    uint32_t m_tp_pos;
    /** Flag determining tile part generation*/
    uint8_t m_tp_flag;
    /** allocation by rate/distortion */
    uint32_t m_disto_alloc : 1;
    /** allocation by fixed_quality */
    uint32_t m_fixed_quality : 1;
    /** Enabling Tile part generation*/
    uint32_t m_tp_on : 1;
	/* rate control algorithm */
	uint32_t rateControlAlgorithm;
}
opj_encoding_param_t;

typedef struct opj_decoding_param {
    /** if != 0, then original dimension divided by 2^(reduce); if == 0 or not used, image is decoded to the full resolution */
    uint32_t m_reduce;
    /** if != 0, then only the first "layer" layers are decoded; if == 0 or not used, all the quality layers are decoded */
    uint32_t m_layer;
}
opj_decoding_param_t;


/**
 * Coding parameters
 */
typedef struct opj_cp {
    /** Size of the image in bits*/
    /*int img_size;*/
    /** Rsiz*/
    uint16_t rsiz;
    /** XTOsiz */
    uint32_t tx0; 
    /** YTOsiz */
    uint32_t ty0; 
    /** XTsiz */
    uint32_t tdx;
    /** YTsiz */
    uint32_t tdy;
    /** comment */
    char *comment;
    /** number of tiles in width */
    uint32_t tw;
    /** number of tiles in height */
    uint32_t th;

    /** number of ppm markers (reserved size) */
    uint32_t ppm_markers_count;
    /** ppm markers data (table indexed by Zppm) */
    opj_ppx* ppm_markers;

    /** packet header store there for future use in t2_decode_packet */
    uint8_t *ppm_data;
    /** size of the ppm_data*/
    uint32_t ppm_len;
    /** size of the ppm_data*/
    uint32_t ppm_data_read;

    uint8_t *ppm_data_current;

    /** packet header storage original buffer */
    uint8_t *ppm_buffer;
    /** pointer remaining on the first byte of the first header if ppm is used */
    uint8_t *ppm_data_first;
    /** Number of bytes actually stored inside the ppm_data */
    uint32_t ppm_data_size;
    /** use in case of multiple marker PPM (number of info already store) */
    int32_t ppm_store;
    /** use in case of multiple marker PPM (case on non-finished previous info) */
    int32_t ppm_previous;

    /** tile coding parameters */
    grk_tcp_t *tcps;

    union {
        opj_decoding_param_t m_dec;
        opj_encoding_param_t m_enc;
    }
    m_specific_param;

    /******** FLAGS *********/
    /** if ppm == 1 --> there was a PPM marker*/
    uint32_t ppm : 1;
    /** tells if the parameter is a coding or decoding one */
    uint32_t m_is_decoder : 1;

} opj_cp_t;


typedef struct grk_j2k_dec {
    /** Decoder state: used to indicate in which part of the codestream the decoder is (main header, tile header, end) */
    uint32_t m_state;

     //store decoding parameters common to all tiles (information like COD, COC in main header)
     grk_tcp_t *m_default_tcp;
    uint8_t  *m_header_data;
    uint32_t m_header_data_size;
    /** to tell the tile part length */
    uint64_t m_sot_length;
    /** Only tile indices in the correct range will be decoded.*/
    uint32_t m_start_tile_x;
    uint32_t m_start_tile_y;
    uint32_t m_end_tile_x;
    uint32_t m_end_tile_y;

     // Decoded area set by the user
    uint32_t m_DA_x0;
    uint32_t m_DA_y0;
    uint32_t m_DA_x1;
    uint32_t m_DA_y1;

    /** Index of the tile to decode (used in get_tile); initialized to -1 */
    int32_t m_tile_ind_to_dec;
    /** Position of the last SOT marker read */
    int64_t m_last_sot_read_pos;

    /**
     * Indicate that the current tile-part is assumed to be the last tile part of the codestream.
     * This is useful in the case when PSot is equal to zero. The sot length will be computed in the
     * SOD reader function. FIXME NOT USED for the moment
     */
    bool   m_last_tile_part;
    /** to tell that a tile can be decoded. */
    uint32_t m_can_decode			: 1;
    uint32_t m_discard_tiles		: 1;
    uint32_t m_skip_data			: 1;
    /** TNsot correction : see issue 254 **/
    uint32_t m_nb_tile_parts_correction_checked : 1;
    uint32_t m_nb_tile_parts_correction : 1;

} grk_j2k_dec_t;

struct grk_j2k_enc_t {
    /** Tile part number, regardless of poc, for each new poc, tp is reset to 1*/
    uint32_t m_current_poc_tile_part_number; /* tp_num */

    /** Tile part number currently coding, taking into account POC. m_current_tile_part_number holds the total number of tile parts while encoding the last tile part.*/
    uint32_t m_current_tile_part_number; /*cur_tp_num */

    /**
    locate the start position of the TLM marker
    after encoding the tilepart, a jump (in j2k_write_sod) is done to the TLM marker to store the value of its length.
    */
    int64_t m_tlm_start;
    /**
     * Stores the sizes of the tlm.
     */
    uint8_t * m_tlm_sot_offsets_buffer;
    /**
     * The current offset of the tlm buffer.
     */
    uint8_t * m_tlm_sot_offsets_current;

    /** Total num of tile parts in whole image = num tiles* num tileparts in each tile*/
    /** used in TLMmarker*/
    uint32_t m_total_tile_parts;	 /* totnum_tp */

	EncodedTileData* tile;
	EncodedTileData* tileHeader;
} ;



struct grk_tcd_t;
/**
JPEG-2000 codestream reader/writer
*/
typedef struct grk_j2k {
    /* J2K codestream is decoded*/
    bool m_is_decoder;

    /* FIXME DOC*/
    union {
        grk_j2k_dec_t m_decoder;
        grk_j2k_enc_t m_encoder;
    }
    m_specific_param;

    /** pointer to the internal/private encoded / decoded image */
    opj_image_t* m_private_image;

    /* pointer to the output image (decoded)*/
    opj_image_t* m_output_image;

    /** Coding parameters */
    opj_cp_t m_cp;

    /** the list of procedures to exec **/
    grk_procedure_list_t *	m_procedure_list;

    /** the list of validation procedures to follow to make sure the code is valid **/
    grk_procedure_list_t *	m_validation_list;

    /** helper used to write the index file */
    opj_codestream_index_t *cstr_index;

    /** number of the tile currently concern by coding/decoding */
    uint32_t m_current_tile_number;

    /** the current tile coder/decoder **/
    grk_tcd_t *	m_tcd;

	uint32_t numThreads;

}
grk_j2k_t;




/** @name Exported functions */
/*@{*/
/* ----------------------------------------------------------------------- */

/**
Setup the decoder decoding parameters using user parameters.
Decoding parameters are returned in j2k->cp.
@param j2k J2K decompressor handle
@param parameters decompression parameters
*/
void grk_j2k_setup_decoder(void* j2k_void, opj_dparameters_t *parameters);

/**
 * Creates a J2K compression structure
 *
 * @return Returns a handle to a J2K compressor if successful, returns NULL otherwise
*/
grk_j2k_t* grk_j2k_create_compress(void);


bool grk_j2k_setup_encoder(	grk_j2k_t *p_j2k,
                            opj_cparameters_t *parameters,
                            opj_image_t *image,
                            grk_event_mgr_t * p_manager);

/**
Converts an enum type progression order to string type
*/
char *grk_j2k_convert_progression_order(OPJ_PROG_ORDER prg_order);

/* ----------------------------------------------------------------------- */
/*@}*/

/*@}*/

/**
 * Ends the decompression procedures and possibiliy add data to be read after the
 * codestream.
 */
bool grk_j2k_end_decompress(grk_j2k_t *j2k,
                            grk_stream_private_t *p_stream,
                            grk_event_mgr_t * p_manager);

/**
 * Reads a jpeg2000 codestream header structure.
 *
 * @param p_stream the stream to read data from.
 * @param p_j2k the jpeg2000 codec.
 * @param p_image FIXME DOC
 * @param p_manager the user event manager.
 *
 * @return true if the box is valid.
 */
bool grk_j2k_read_header(	grk_stream_private_t *p_stream,
                            grk_j2k_t* p_j2k,
							opj_header_info_t* header_info,
							opj_image_t** p_image,
							grk_event_mgr_t* p_manager );


/**
 * Destroys a jpeg2000 codec.
 *
 * @param	p_j2k	the jpeg20000 structure to destroy.
 */
void grk_j2k_destroy (grk_j2k_t *p_j2k);

/**
 * Destroys a codestream index structure.
 *
 * @param	p_cstr_ind	the codestream index parameter to destroy.
 */
void j2k_destroy_cstr_index (opj_codestream_index_t *p_cstr_ind);

/**
 * Decode tile data.
 * @param	p_j2k		the jpeg2000 codec.
 * @param	p_tile_index
 * @param p_data       FIXME DOC
 * @param p_data_size  FIXME DOC
 * @param	p_stream			the stream to write data to.
 * @param	p_manager	the user event manager.
 */
bool grk_j2k_decode_tile (  grk_j2k_t * p_j2k,
                            uint32_t p_tile_index,
                            uint8_t * p_data,
                            uint64_t p_data_size,
                            grk_stream_private_t *p_stream,
                            grk_event_mgr_t * p_manager );

/**
 * Reads a tile header.
 * @param	p_j2k		the jpeg2000 codec.
 * @param	p_tile_index FIXME DOC
 * @param	p_data_size FIXME DOC
 * @param	p_tile_x0 FIXME DOC
 * @param	p_tile_y0 FIXME DOC
 * @param	p_tile_x1 FIXME DOC
 * @param	p_tile_y1 FIXME DOC
 * @param	p_nb_comps FIXME DOC
 * @param	p_go_on FIXME DOC
 * @param	p_stream			the stream to write data to.
 * @param	p_manager	the user event manager.
 */
bool grk_j2k_read_tile_header ( grk_j2k_t * p_j2k,
                                uint32_t * p_tile_index,
                                uint64_t * p_data_size,
                                uint32_t * p_tile_x0,
                                uint32_t * p_tile_y0,
                                uint32_t * p_tile_x1,
                                uint32_t * p_tile_y1,
                                uint32_t * p_nb_comps,
                                bool * p_go_on,
                                grk_stream_private_t *p_stream,
                                grk_event_mgr_t * p_manager );


/**
 * Sets the given area to be decoded. This function should be called right after grk_read_header and before any tile header reading.
 *
 * @param	p_j2k			the jpeg2000 codec.
 * @param	p_image     FIXME DOC
 * @param	p_start_x		the left position of the rectangle to decode (in image coordinates).
 * @param	p_start_y		the up position of the rectangle to decode (in image coordinates).
 * @param	p_end_x			the right position of the rectangle to decode (in image coordinates).
 * @param	p_end_y			the bottom position of the rectangle to decode (in image coordinates).
 * @param	p_manager		the user event manager
 *
 * @return	true			if the area could be set.
 */
bool grk_j2k_set_decode_area(	grk_j2k_t *p_j2k,
                                opj_image_t* p_image,
                                uint32_t p_start_x, uint32_t p_start_y,
                                uint32_t p_end_x, uint32_t p_end_y,
                                grk_event_mgr_t * p_manager );

/**
 * Creates a J2K decompression structure.
 *
 * @return a handle to a J2K decompressor if successful, NULL otherwise.
 */
grk_j2k_t* grk_j2k_create_decompress(void);


/**
 * Dump some elements from the J2K decompression structure .
 *
 *@param p_j2k				the jpeg2000 codec.
 *@param flag				flag to describe what elements are dumped.
 *@param out_stream			output stream where dump the elements.
 *
*/
void j2k_dump (grk_j2k_t* p_j2k, int32_t flag, FILE* out_stream);



/**
 * Dump an image header structure.
 *
 *@param image			the image header to dump.
 *@param dev_dump_flag		flag to describe if we are in the case of this function is use outside j2k_dump function
 *@param out_stream			output stream where dump the elements.
 */
void j2k_dump_image_header(opj_image_t* image, bool dev_dump_flag, FILE* out_stream);

/**
 * Dump a component image header structure.
 *
 *@param comp		the component image header to dump.
 *@param dev_dump_flag		flag to describe if we are in the case of this function is use outside j2k_dump function
 *@param out_stream			output stream where dump the elements.
 */
void j2k_dump_image_comp_header(opj_image_comp_t* comp, bool dev_dump_flag, FILE* out_stream);

/**
 * Get the codestream info from a JPEG2000 codec.
 *
 *@param	p_j2k				the component image header to dump.
 *
 *@return	the codestream information extract from the jpg2000 codec
 */
opj_codestream_info_v2_t* j2k_get_cstr_info(grk_j2k_t* p_j2k);

/**
 * Get the codestream index from a JPEG2000 codec.
 *
 *@param	p_j2k				the component image header to dump.
 *
 *@return	the codestream index extract from the jpg2000 codec
 */
opj_codestream_index_t* j2k_get_cstr_index(grk_j2k_t* p_j2k);

/**
 * Decode an image from a JPEG-2000 codestream
 * @param j2k J2K decompressor handle
 * @param p_stream  FIXME DOC
 * @param p_image   FIXME DOC
 * @param p_manager FIXME DOC
 * @return FIXME DOC
*/
bool grk_j2k_decode(grk_j2k_t *j2k,
					opj_plugin_tile_t* tile,
                    grk_stream_private_t *p_stream,
                    opj_image_t *p_image,
                    grk_event_mgr_t *p_manager);


bool grk_j2k_get_tile(	grk_j2k_t *p_j2k,
                        grk_stream_private_t *p_stream,
                        opj_image_t* p_image,
                        grk_event_mgr_t * p_manager,
                        uint32_t tile_index );

bool grk_j2k_set_decoded_resolution_factor(grk_j2k_t *p_j2k,
        uint32_t res_factor,
        grk_event_mgr_t * p_manager);


/**
 * Writes a tile.
 * @param	p_j2k		the jpeg2000 codec.
 * @param p_tile_index FIXME DOC
 * @param p_data FIXME DOC
 * @param p_data_size FIXME DOC
 * @param	p_stream			the stream to write data to.
 * @param	p_manager	the user event manager.
 */
bool grk_j2k_write_tile (	grk_j2k_t * p_j2k,
                            uint32_t p_tile_index,
                            uint8_t * p_data,
                            uint64_t p_data_size,
                            grk_stream_private_t *p_stream,
                            grk_event_mgr_t * p_manager );

/**
 * Encodes an image into a JPEG-2000 codestream
 */
bool grk_j2k_encode(	grk_j2k_t * p_j2k,
						opj_plugin_tile_t* tile,
                        grk_stream_private_t *cio,
                        grk_event_mgr_t * p_manager );

/**
 * Starts a compression scheme, i.e. validates the codec parameters, writes the header.
 *
 * @param	p_j2k		the jpeg2000 codec.
 * @param	p_stream			the stream object.
 * @param	p_image FIXME DOC
 * @param	p_manager	the user event manager.
 *
 * @return true if the codec is valid.
 */
bool grk_j2k_start_compress(grk_j2k_t *p_j2k,
                            grk_stream_private_t *p_stream,
                            opj_image_t * p_image,
                            grk_event_mgr_t * p_manager);

/**
 * Ends the compression procedures and possibility add data to be read after the
 * codestream.
 */
bool grk_j2k_end_compress( 	grk_j2k_t *p_j2k,
                            grk_stream_private_t *cio,
                            grk_event_mgr_t * p_manager);

bool grk_j2k_setup_mct_encoding (grk_tcp_t * p_tcp, opj_image_t * p_image);
