/* ekffld.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      EKFFLD ( EK, finish fast write ) */
/* Subroutine */ int ekffld_(integer *handle, integer *segno, integer *rcptrs)
{
    extern /* Subroutine */ int zzeksdsc_(integer *, integer *, integer *), 
	    chkin_(char *, ftnlen);
    integer stype, segdsc[24];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int zzekff01_(integer *, integer *, integer *);

/* $ Abstract */

/*     Complete a fast write operation on a new E-kernel segment. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     EK */

/* $ Keywords */

/*     EK */

/* $ Declarations */
/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */


/*     Include Section:  EK Segment Descriptor Parameters */

/*        eksegdsc.inc  Version 8  06-NOV-1995 (NJB) */


/*     All `base addresses' referred to below are the addresses */
/*     *preceding* the item the base applies to.  This convention */
/*     enables simplied address calculations in many cases. */

/*     Size of segment descriptor.  Note:  the include file ekcoldsc.inc */
/*     must be updated if this parameter is changed.  The parameter */
/*     CDOFF in that file should be kept equal to SDSCSZ. */


/*     Index of the segment type code: */


/*     Index of the segment's number.  This number is the segment's */
/*     index in the list of segments contained in the EK to which */
/*     the segment belongs. */


/*     Index of the DAS integer base address of the segment's integer */
/*     meta-data: */


/*     Index of the DAS character base address of the table name: */


/*     Index of the segment's column count: */


/*     Index of the segment's record count: */


/*     Index of the root page number of the record tree: */


/*     Index of the root page number of the character data page tree: */


/*     Index of the root page number of the double precision data page */
/*     tree: */


/*     Index of the root page number of the integer data page tree: */


/*     Index of the `modified' flag: */


/*     Index of the `initialized' flag: */


/*     Index of the shadowing flag: */


/*     Index of the companion file handle: */


/*     Index of the companion segment number: */


/*     The next three items are, respectively, the page numbers of the */
/*     last character, d.p., and integer data pages allocated by the */
/*     segment: */


/*     The next three items are, respectively, the page-relative */
/*     indices of the last DAS word in use in the segment's */
/*     last character, d.p., and integer data pages: */


/*     Index of the DAS character base address of the column name list: */


/*     The last descriptor element is reserved for future use.  No */
/*     parameter is defined to point to this location. */


/*     End Include Section:  EK Segment Descriptor Parameters */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   File handle. */
/*     SEGNO      I   Segment number. */
/*     RCPTRS     I   Record pointers. */

/* $ Detailed_Input */

/*     HANDLE         the handle of an EK file that is open for writing. */
/*                    A "begin segment for fast write" operation must */
/*                    have already been performed for the designated */
/*                    segment. */

/*     SEGNO          is the number of the segment to complete. */

/*     RCPTRS         is an array of record pointers for the input */
/*                    segment.  This array is obtained as an output */
/*                    from EKIFLD, the routine called to initiate a */
/*                    fast write. */

/* $ Detailed_Output */

/*     None. */

/*     See the $Particulars section for a description of the */
/*     effects of this routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If HANDLE is invalid, the error will be diagnosed by routines */
/*         called by this routine. */

/*     2)  If an attempt is made to finish a segment other than the */
/*         one last initialized by EKIFLD, the error will be diagnosed by */
/*         routines called by this routine. */

/*     3)  If an I/O error occurs while reading or writing the indicated */
/*         file, the error will be diagnosed by routines called by this */
/*         routine. */

/* $ Files */

/*     See the EK Required Reading for a discussion of the EK file */
/*     format. */

/* $ Particulars */

/*     This routine completes an EK segment after the data has been */
/*     written via the fast column writer routines.  The segment must */
/*     have been created by a call to ELIFLD.  The fast column */
/*     writer routines are: */

/*        EKACLC {EK, add column, character} */
/*        EKACLD {EK, add column, double precision} */
/*        EKACLI {EK, add column, integer} */

/*     The segment is not guaranteed to be readable until all columns */
/*     have been added.  After the columns have been added, the segment */
/*     may be extended by inserting more records and filling in those */
/*     records using the EKACEx routines. */

/* $ Examples */


/*     1)  Suppose we have an E-kernel named ORDER_DB.EK which contains */
/*         records of orders for data products.  The E-kernel has a */
/*         table called DATAORDERS that consists of the set of columns */
/*         listed below: */

/*            DATAORDERS */

/*               Column Name     Data Type */
/*               -----------     --------- */
/*               ORDER_ID        INTEGER */
/*               CUSTOMER_ID     INTEGER */
/*               LAST_NAME       CHARACTER*(*) */
/*               FIRST_NAME      CHARACTER*(*) */
/*               ORDER_DATE      TIME */
/*               COST            DOUBLE PRECISION */

/*         The order database also has a table of items that have been */
/*         ordered.  The columns of this table are shown below: */

/*            DATAITEMS */

/*               Column Name     Data Type */
/*               -----------     --------- */
/*               ITEM_ID         INTEGER */
/*               ORDER_ID        INTEGER */
/*               ITEM_NAME       CHARACTER*(*) */
/*               DESCRIPTION     CHARACTER*(*) */
/*               PRICE           DOUBLE PRECISION */


/*         We'll suppose that the file ORDER_DB.EK contains two segments, */
/*         the first containing the DATAORDERS table and the second */
/*         containing the DATAITEMS table. */

/*         Below, we show how we'd open a new EK file and create the */
/*         first of the segments described above. */


/*            C */
/*            C     Open a new EK file.  For simplicity, we will not */
/*            C     reserve any space for the comment area, so the */
/*            C     number of reserved comment characters is zero. */
/*            C     The variable IFNAME is the internal file name. */
/*            C */
/*                  NRESVC  =  0 */
/*                  IFNAME  =  'Test EK/Created 20-SEP-1995' */

/*                  CALL EKOPN ( 'ORDER_DB.EK', IFNAME, NRESVC, HANDLE ) */

/*            C */
/*            C     Set up the table and column names and declarations */
/*            C     for the DATAORDERS segment.  We'll index all of */
/*            C     the columns.  All columns are scalar, so we omit */
/*            C     the size declaration.  Only the COST column may take */
/*            C     null values. */
/*            C */
/*                  TABLE     =  'DATAORDERS' */
/*                  NCOLS     =  6 */

/*                  CNAMES(1) =  'ORDER_ID' */
/*                  CDECLS(1) =  'DATATYPE = INTEGER, INDEXED = TRUE' */

/*                  CNAMES(2) =  'CUSTOMER_ID' */
/*                  CDECLS(2) =  'DATATYPE = INTEGER, INDEXED = TRUE' */

/*                  CNAMES(3) =  'LAST_NAME' */
/*                  CDECLS(3) =  'DATATYPE = CHARACTER*(*),' // */
/*                 .             'INDEXED  = TRUE' */

/*                  CNAMES(4) =  'FIRST_NAME' */
/*                  CDECLS(4) =  'DATATYPE = CHARACTER*(*),' // */
/*                 .             'INDEXED  = TRUE' */

/*                  CNAMES(5) =  'ORDER_DATE' */
/*                  CDECLS(5) =  'DATATYPE = TIME, INDEXED  = TRUE' */

/*                  CNAMES(6) =  'COST' */
/*                  CDECLS(6) =  'DATATYPE = DOUBLE PRECISION,' // */
/*                 .             'INDEXED  = TRUE'           // */
/*                 .             'NULLS_OK = TRUE' */

/*            C */
/*            C     Start the segment.  We presume the number of  rows */
/*            C     of data is known in advance. */
/*            C */
/*                  CALL EKIFLD ( HANDLE,  TABNAM,  NCOLS,  NROWS, */
/*                 .              CNAMES,  CDECLS,  SEGNO,  RCPTRS ) */

/*            C */
/*            C     At this point, arrays containing data for the */
/*            C     segment's columns may be filled in.  The names */
/*            C     of the data arrays are shown below. */
/*            C */
/*            C        Column           Data array */
/*            C */
/*            C        'ORDER_ID'       ORDIDS */
/*            C        'CUSTOMER_ID'    CSTIDS */
/*            C        'LAST_NAME'      LNAMES */
/*            C        'FIRST_NAME'     FNAMES */
/*            C        'ORDER_DATE'     ONAMES */
/*            C        'COST'           COSTS */
/*            C */

/*                     [ Fill in data arrays here.] */

/*            C */
/*            C     The SIZES array shown below is ignored for scalar */
/*            C     and fixed-size array columns, so we need not */
/*            C     initialize it.  For variable-size arrays, the */
/*            C     Ith element of the SIZES array must contain the size */
/*            C     of the Ith column entry in the column being added. */
/*            C     Normally, the SIZES array would be reset for each */
/*            C     variable-size column. */
/*            C */
/*            C     The NLFLGS array indicates which entries are null. */
/*            C     It is ignored for columns that don't allow null */
/*            C     values.  In this case, only the COST column allows */
/*            C     nulls. */
/*            C */
/*            C     Add the columns of data to the segment.  All of the */
/*            C     data for each column is added in one shot. */
/*            C */
/*                  CALL EKACLI ( HANDLE, SEGNO,  'ORDER_ID', */
/*                 .              ORDIDS, SIZES,  NLFLGS,  WKINDX ) */

/*                  CALL EKACLI ( HANDLE, SEGNO,  'CUSTOMER_ID', */
/*                 .              CSTIDS, SIZES,  NLFLGS,  WKINDX ) */

/*                  CALL EKACLC ( HANDLE, SEGNO,  'LAST_NAME', */
/*                 .              LNAMES, SIZES,  NLFLGS,  WKINDX ) */

/*                  CALL EKACLC ( HANDLE, SEGNO,  'FIRST_NAME', */
/*                 .              FNAMES, SIZES,  NLFLGS,  WKINDX ) */


/*                  CALL UTC2ET ( ODATE,  ET ) */
/*                  CALL EKACLD ( HANDLE, SEGNO,  'ORDER_DATE', */
/*                 .              ODATES, SIZES,  NLFLGS,  WKINDX ) */


/*                     [Set the NLFLGS array here.] */

/*                  CALL EKACLD ( HANDLE, SEGNO,  'COST', */
/*                 .              COSTS,  SIZES,  NLFLGS,  WKINDX ) */

/*            C */
/*            C     Complete the segment.  The RCPTRS array is that */
/*            C     returned by EKIFLD. */
/*            C */
/*                  CALL EKFFLD ( HANDLE, SEGNO, RCPTRS ) */

/*            C */
/*            C     At this point, the second segment could be */
/*            C     created by an analogous process.  In fact, the */
/*            C     second segment could be created at any time; it is */
/*            C     not necessary to populate the first segment with */
/*            C     data before starting the second segment. */
/*            C */

/*            C */
/*            C     The file must be closed by a call to EKCLS. */
/*            C */
/*                  CALL EKCLS ( HANDLE ) */


/* $ Restrictions */

/*     1)  Only one segment can be created at a time using the fast */
/*         write routines. */

/*     2)  No other EK operation may interrupt a fast write.  For */
/*         example, it is not valid to issue a query while a fast write */
/*         is in progress. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.2, 09-JAN-2002 (NJB) */

/*        Documentation change:  instances of the phrase "fast load" */
/*        were replaced with "fast write." */

/* -    SPICELIB Version 1.1.1, 18-JUN-1999 (WLT) */

/*        Corrected CHKOUT value to be same as CHKIN. */

/* -    SPICELIB Version 1.0.1, 31-MAR-1998 (NJB) */

/*        Made miscellaneous header corrections. */

/* -    SPICELIB Version 1.0.0, 08-NOV-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     finish fast write of an EK segment */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("EKFFLD", (ftnlen)6);
    }

/*     Read in the segment descriptor, and get the segment's type. */

    zzeksdsc_(handle, segno, segdsc);
    stype = segdsc[0];

/*     Complete the fast write preparations appropriate to the segment's */
/*     type. */

    if (stype == 1) {
	zzekff01_(handle, segno, rcptrs);
    } else if (stype == 2) {

/*        Currently, no actions are taken to complete a type 2 segment. */

    } else {
	setmsg_("Segment type # is not currently supported.", (ftnlen)42);
	errint_("#", &stype, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("EKFFLD", (ftnlen)6);
	return 0;
    }
    chkout_("EKFFLD", (ftnlen)6);
    return 0;
} /* ekffld_ */

