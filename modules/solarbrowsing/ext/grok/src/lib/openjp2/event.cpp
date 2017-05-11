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
 * Copyright (c) 2005, Herve Drolon, FreeImage Team
 * Copyright (c) 2008, 2011-2012, Centre National d'Etudes Spatiales (CNES), FR
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

#include "grk_includes.h"

/* ==========================================================
     Utility functions
   ==========================================================*/
/* ----------------------------------------------------------------------- */
/**
 * Default callback function.
 * Do nothing.
 */
static void grk_default_callback (const char *msg, void *client_data)
{
    OPJ_ARG_NOT_USED(msg);
    OPJ_ARG_NOT_USED(client_data);
}

/* ----------------------------------------------------------------------- */


/* ----------------------------------------------------------------------- */
bool grk_event_msg(grk_event_mgr_t* p_event_mgr, int32_t event_type, const char *fmt, ...)
{
#define OPJ_MSG_SIZE 512 /* 512 bytes should be more than enough for a short message */
    opj_msg_callback msg_handler = nullptr;
    void * l_data = nullptr;

    if(p_event_mgr != nullptr) {
        switch(event_type) {
        case EVT_ERROR:
            msg_handler = p_event_mgr->error_handler;
            l_data = p_event_mgr->m_error_data;
            break;
        case EVT_WARNING:
            msg_handler = p_event_mgr->warning_handler;
            l_data = p_event_mgr->m_warning_data;
            break;
        case EVT_INFO:
            msg_handler = p_event_mgr->info_handler;
            l_data = p_event_mgr->m_info_data;
            break;
        default:
            break;
        }
        if(msg_handler == nullptr) {
            return false;
        }
    } else {
        return false;
    }

    if ((fmt != nullptr) && (p_event_mgr != nullptr)) {
        va_list arg;
        size_t str_length/*, i, j*/; 
        char message[OPJ_MSG_SIZE];
        memset(message, 0, OPJ_MSG_SIZE);
        /* initialize the optional parameter list */
        va_start(arg, fmt);
        /* check the length of the format string */
        str_length = (strlen(fmt) > OPJ_MSG_SIZE) ? OPJ_MSG_SIZE : strlen(fmt);
        (void)str_length;
        /* parse the format string and put the result in 'message' */
        vsnprintf(message, OPJ_MSG_SIZE, fmt, arg); 
        /* deinitialize the optional parameter list */
        va_end(arg);

        /* output the message to the user program */
        msg_handler(message, l_data);
    }

    return true;
}

void grk_set_default_event_handler(grk_event_mgr_t * p_manager)
{
    p_manager->m_error_data = nullptr;
    p_manager->m_warning_data = nullptr;
    p_manager->m_info_data = nullptr;
    p_manager->error_handler = grk_default_callback;
    p_manager->info_handler = grk_default_callback;
    p_manager->warning_handler = grk_default_callback;
}

