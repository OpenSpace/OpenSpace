/*

-Procedure dafac_c ( DAF add comments )

-Abstract
 
   Add comments from a buffer of character strings to the comment 
   area of a binary DAF file, appending them to any comments which 
   are already present in the file's comment area. 
 
-Disclaimer
 
   THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE 
   CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. 
   GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE 
   ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE 
   PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" 
   TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY 
   WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A 
   PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC 
   SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE 
   SOFTWARE AND RELATED MATERIALS, HOWEVER USED. 
 
   IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA 
   BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT 
   LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, 
   INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, 
   REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE 
   REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. 
 
   RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF 
   THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY 
   CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE 
   ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. 
 
-Required_Reading
 
   DAF 
 
-Keywords
 
   FILES 
   UTILITY 
 
*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZst.h"
   #undef   dafac_c

   void dafac_c ( SpiceInt      handle,
                  SpiceInt      n,
                  SpiceInt      lenvals,
                  const void  * buffer  ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   handle     I    handle of a DAF opened with write access. 
   n          I    Number of comments to put into the comment area. 
   lenvals    I    Length of elements
   buffer     I    Buffer of comments to put into the comment area. 
 
-Detailed_Input
 
   handle   is the file handle of a binary DAF which has been opened 
            with write access. 
 
   n        is the number of rows in the array `buffer'.  This is
            also the number of comment lines in `buffer' that are to be
            added to the comment area of the binary DAF attached to
            `handle'.
 
   buffer   A string buffer containing comments which are to be added 
            to the comment area of the binary DAF attached to `handle'. 
            buffer should be declared by the caller has follows:

               SpiceChar    buffer[n][lenvals];
            
            Each row of the buffer should contain one comment line.

-Detailed_Output
 
   None. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the number of comments to be added is not positive, the 
      error SPICE(INVALIDARGUMENT) will be signaled. 
 
   2) If a non printing ASCII character is encountered in the 
      comments, the error SPICE(ILLEGALCHARACTER) will be signaled.
 
   3) If the binary DAF file attached to HANDLE is not open with 
      write access an error will be signalled by a routine called by
      this routine.
 
   4) If the end of the comments cannot be found, i.e., the end of 
      comments marker is missing on the last comment record, the error
      SPICE(BADCOMMENTAREA) will be signaled.
 
   5) If the input pointer `buffer' is null, the error
      SPICE(NULLPOINTER) will be signaled.
  
   6) If the input buffer string length indicated by `lenvals' 
      is less than 2, the error SPICE(STRINGTOOSHORT) will be signaled. 

-Files
 
   See argument `handle' in $ Detailed_Input. 
 
-Particulars
 
   A binary DAF contains a data area which is reserved for storing
   annotations or descriptive textual information about the data
   contained in a file. This area is referred to as the ``comment
   area'' of the file. The comment area of a DAF is a line oriented
   medium for storing textual information. The comment area preserves
   leading or embedded white space in the line(s) of text which are
   stored so that the appearance of the information will be unchanged
   when it is retrieved (extracted) at some other time. Trailing
   blanks, however, are NOT preserved, due to the way that character
   strings are represented in standard Fortran 77.
 
   This routine will take a buffer of text lines and add (append) them
   to the comment area of a binary DAF. If there are no comments in the
   comment area of the file, then space will be allocated and the text
   lines in `buffer' will be placed into the comment area. The text lines
   may contain only printable ASCII characters (decimal values 32 -
   126).
 
   There is NO maximum length imposed on the significant portion of a
   text line that may be placed into the comment area of a DAF. The
   maximum length of a line stored in the comment area should be
   reasonable, however, so that they may be easily extracted. A good
   maximum value for this would be 255 characters, as this can easily
   accommodate ``screen width'' lines as well as long lines which may
   contain some other form of information.
 
-Examples
 
   1) Let 
 
         handle   be the handle for a DAF which has been opened with 
                  write access. 
 
         n        be the number of lines of text to be added to the 
                  comment area of the binary DAF attached to handle. 
 
         lenvals  be the length of the rows of a string buffer.

         buffer   is an array of text lines to be added to the comment 
                  area of the binary DAF attached to handle. `buffer'
                  normally is declared

                     SpiceChar buffer [n][lenvals];
                  
      The call 
 
         dafac_c ( handle, n, lenvals, buffer );
 
      will append the first n line(s) in `buffer' to the comment area 
      of the binary DAF attached to `handle'. 
 
-Restrictions
 
   1) This routine uses constants that are specific to the ASCII 
      character sequence. The results of using this routine with 
      a different character sequence are unpredictable. 
 
   2) This routine is only used to extract records on environments 
      whose characters are a single byte in size.  Updates to this 
      routine and routines in its call tree may be required to 
      properly handle other cases. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL)
   K.R. Gehringer (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 16-NOV-2006 (NJB) (KRG)

-Index_Entries
 
   add comments to a binary daf file 
   append comments to a daf file comment area 
 
-&
*/

{ /* Begin dafac_c */


   /*
   Local variables
   */
   SpiceChar             * fCvalsArr;
   
   SpiceInt                fCvalsLen;


   /*
   Participate in error tracing.
   */
   chkin_c ( "dafac_c" );


   /*
   Make sure the input string pointer for the `buffer' array is non-null 
   and that the length lenvals is sufficient.  
   */
   CHKOSTR ( CHK_STANDARD, "dafac_c", buffer, lenvals );
   
   /*
   The input buffer contains C-style strings; we must pass a 
   Fortran-style buffer to dafac_.
   */
   C2F_MapStrArr ( "dafac_c", 
                   n, lenvals, buffer, &fCvalsLen, &fCvalsArr );

   if ( failed_c() )
   {
      chkout_c ( "dafac_c" );
      return;
   }


   /*
   Call the f2c'd routine.
   */
   dafac_ ( ( integer * ) &handle,
            ( integer * ) &n,
            ( char    * ) fCvalsArr,
            ( ftnlen    ) fCvalsLen );

   /*
   Free the dynamically allocated array.
   */
   free ( fCvalsArr );


   chkout_c ( "dafac_c" );

} /* End dafac_c */
