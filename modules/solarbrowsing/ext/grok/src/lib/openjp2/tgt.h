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
 * Copyright (c) 2011-2012, Centre National d'Etudes Spatiales (CNES), France
 * Copyright (c) 2012, CS Systemes d'Information, France
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

const uint32_t tag_tree_uninitialized_node_value = 999;

/**
Tag node
*/
struct TagTreeNode {

	TagTreeNode() : parent(nullptr), value(0), low(0), known(0) {}

    TagTreeNode *parent;
    int32_t value;
    int32_t low;
    uint32_t known;
} ;

/**
Tag tree
*/
class TagTree {

public:

	/**
	Create a tag tree
	@param numleafsh Width of the array of leafs of the tree
	@param numleafsv Height of the array of leafs of the tree
	@return Returns a new tag tree if successful, returns NULL otherwise
	*/
	TagTree(uint32_t numleafsh, uint32_t numleafsv, grk_event_mgr_t *manager);
	~TagTree();


	/**
	* Reinitialises a tag tree
	*
	* @param	p_num_leafs_h		the width of the array of leafs of the tree
	* @param	p_num_leafs_v		the height of the array of leafs of the tree
	* @param p_manager       the event manager
	* @return	true if successful, false otherwise
	*/
	bool init(uint32_t  p_num_leafs_h,	uint32_t  p_num_leafs_v, grk_event_mgr_t *p_manager);

	/**
	Reset a tag tree (set all leaves to 0)
	*/
	void reset();
	/**
	Set the value of a leaf of a tag tree
	@param leafno Number that identifies the leaf to modify
	@param value New value of the leaf
	*/
	void setvalue(	uint32_t leafno,	int32_t value);
	/**
	Encode the value of a leaf of the tag tree up to a given threshold
	@param bio Pointer to a BIO handle
	@param leafno Number that identifies the leaf to encode
	@param threshold Threshold to use when encoding value of the leaf
	*/
	void encode(BitIO *bio,	uint32_t leafno,	int32_t threshold);
	/**
	Decode the value of a leaf of the tag tree up to a given threshold
	@param bio Pointer to a BIO handle
	@param leafno Number that identifies the leaf to decode
	@param threshold Threshold to use when decoding value of the leaf
	@return Returns 1 if the node's value < threshold, returns 0 otherwise
	*/
	uint8_t decode(BitIO *bio,	uint32_t leafno,	int32_t threshold);


	/**
	Decode the value of a leaf of the tag tree up to a given threshold
	@param bio Pointer to a BIO handle
	@param leafno Number that identifies the leaf to decode
	@param threshold Threshold to use when decoding value of the leaf
	@return Returns the node's value
	*/
	int32_t decodeValue(BitIO *bio, uint32_t leafno, int32_t threshold);

private:

    uint32_t  numleafsh;
    uint32_t  numleafsv;
    uint32_t numnodes;
    TagTreeNode *nodes;
    uint32_t  nodes_size;		/* maximum size taken by nodes */


};





