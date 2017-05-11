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
 * @file function_list.h
 * @brief Implementation of a list of procedures.

 * The functions in validation.c aims to have access to a list of procedures.
*/

/** @defgroup VAL VAL - validation procedure*/
/*@{*/

/**************************************************************************************************
 ***************************************** FORWARD DECLARATION ************************************
 **************************************************************************************************/

/**
 * declare a function pointer
 */
typedef void (*grk_procedure)(void);

/**
 * A list of procedures.
*/
typedef struct grk_procedure_list {
    /**
     * The number of validation procedures.
     */
    uint32_t m_nb_procedures;
    /**
     * The number of the array of validation procedures.
     */
    uint32_t m_nb_max_procedures;
    /**
     * The array of procedures.
     */
    grk_procedure * m_procedures;

} grk_procedure_list_t;

/* ----------------------------------------------------------------------- */

/**
 * Creates a validation list.
 *
 * @return	the newly created validation list.
 */
grk_procedure_list_t *  grk_procedure_list_create(void);

/**
 * Destroys a validation list.
 *
 * @param p_list the list to destroy.
 */
void  grk_procedure_list_destroy(grk_procedure_list_t * p_list);

/**
 * Adds a new validation procedure.
 *
 * @param	p_validation_list the list of procedure to modify.
 * @param	p_procedure		the procedure to add.
 *
 * @return	true if the procedure could be added.
 */
bool grk_procedure_list_add_procedure (grk_procedure_list_t * p_validation_list, grk_procedure p_procedure, grk_event_mgr_t* p_manager);

/**
 * Gets the number of validation procedures.
 *
 * @param	p_validation_list the list of procedure to modify.
 *
 * @return the number of validation procedures.
 */
uint32_t grk_procedure_list_get_nb_procedures (grk_procedure_list_t * p_validation_list);

/**
 * Gets the pointer on the first validation procedure. This function is similar to the C++
 * iterator class to iterate through all the procedures inside the validation list.
 * the caller does not take ownership of the pointer.
 *
 * @param	p_validation_list the list of procedure to get the first procedure from.
 *
 * @return	a pointer to the first procedure.
 */
grk_procedure* grk_procedure_list_get_first_procedure (grk_procedure_list_t * p_validation_list);


/**
 * Clears the list of validation procedures.
 *
 * @param	p_validation_list the list of procedure to clear.
 *
 */
void grk_procedure_list_clear (grk_procedure_list_t * p_validation_list);
/*@}*/

