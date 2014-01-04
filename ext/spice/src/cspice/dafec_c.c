/*

-Procedure dafec_c ( DAF extract comments )

-Abstract
 
   Extract comments from the comment area of a binary DAF. 
 
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

   void dafec_c ( SpiceInt          handle,
                  SpiceInt          bufsiz,
                  SpiceInt          lenout,
                  SpiceInt        * n,
                  void            * buffer,
                  SpiceBoolean    * done    ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
    handle    I   Handle of binary DAF opened with read access. 
    bufsiz    I   Maximum size, in lines, of buffer. 
    lenout    I   Length of strings in output buffer.
    n         O   Number of extracted comment lines. 
    buffer    O   Buffer where extracted comment lines are placed. 
    done      O   Indicates whether all comments have been extracted. 
 
-Detailed_Input
 
   handle   is the file handle of a binary DAF which has been opened with
            read access.
 
   bufsiz   is the maximum number of comments that may be placed into
            buffer. This would typically be the declared array size for
            the Fortran character string array passed into this
            routine.
 
   lenout   is the allowed length of each string element of the output
            buffer. This length must large enough to hold the longest
            output string plus the null terminator.  The SPICE system
            imposes no limit on the length of comment lines, so `lenout'
            normally should be set to a "generous" value that is unlikely
            to be exceeded.           
 
-Detailed_Output
 
   n        is the number of comment lines extracted from the comment area
            of the binary DAF associated with `handle'.  `n' will be
            less than or equal to `bufsiz' on output.
 
   buffer   is an array containing comment lines read from the DAF
            associated with `handle'.  `buffer' should be declared

               SpiceChar  buffer[bufsiz][lenout];

            On output, the first `n' strings of `buffer' will contain
            comment text, with one comment line per string.
 
   done     is a logical flag indicating whether or not all of the 
            comment lines from the comment area of the DAF have 
            been read. This variable has the value SPICETRUE after the 
            last comment line has been read. It will have the value 
            SPICEFALSE otherwise. 
 
            If there are no comments in the comment area, this 
            variable will have the value SPICETRUE.
 
-Parameters
 
   None. 
 
-Exceptions
 
   1)   If the size of the output line buffer is is not positive, 
        the error SPICE(INVALIDARGUMENT) will be signaled. 
 
   3)   If a comment line in a DAF is longer than the length 
        of a character string array element of BUFFER, the error 
        SPICE(COMMENTTOOLONG) will be signaled. 
 
   3)   If the end of the comments cannot be found, i.e., the end of 
        comments marker is missing on the last comment record, the 
        error SPICE(BADCOMMENTAREA) will be signaled. 
 
   4)   If the number of comment characters scanned exceeds the 
        number of comment characters computed, the error 
        SPICE(BADCOMMENTAREA) will be signaled. 
 
   5)   If the binary DAF attached to HANDLE is not open for 
        reading,an error will be signaled by a routine called by 
        this routine. 
 
   6)   If the output buffer pointer is null the error SPICE(NULLPOINTER)
        will be signaled.

   7)   If the output buffer string length is less than 2, the error
        SPICE(STRINGTOOSHORT) will be signaled.

-Files
 
   See argument `handle' in $ Detailed_Input. 
 
-Particulars
 
   A binary DAF contains an area which is reserved for storing 
   annotations or descriptive textual information describing the data 
   contained in a file. This area is referred to as the ``comment 
   area'' of the file. The comment area of a DAF is a line 
   oriented medium for storing textual information. The comment 
   area preserves any leading or embedded white space in the line(s) 
   of text which are stored, so that the appearance of the of 
   information will be unchanged when it is retrieved (extracted) at 
   some other time. Trailing blanks, however, are NOT preserved, 
   due to the way that character strings are represented in 
   standard Fortran 77. 
 
   This routine will read the comments from the comment area of 
   a binary DAF, placing them into a line buffer. If the line 
   buffer is not large enough to hold the entire comment area, 
   the portion read will be returned to the caller, and the DONE 
   flag will be set to SPICEFALSE. This allows the comment area to be 
   read in ``chunks,'' a buffer at a time. After all of the comment 
   lines have been read, the `done' flag will be set to SPICETRUE. 
 
   This routine can be used to ``simultaneously'' extract comments 
   from the comment areas of multiple binary DAFs. See Example 
   2 in the $ Examples section. 
 
-Examples
 
   1) The following example will extract the entire comment area of a 
      binary DAF, displaying the comments on the terminal screen. 
 
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         #define FILSIZ          256
         #define LINLEN          1001
         #define BUFFSZ          25

         SpiceBoolean            done = SPICEFALSE;

         SpiceChar               daf    [FILSIZ];
         SpiceChar               buffer [BUFFSZ][LINLEN];

         SpiceInt                handle;
         SpiceInt                i;
         SpiceInt                n;


         prompt_c ( "Enter name of DAF > ", FILSIZ, daf );

         dafopr_c ( daf, &handle );

         while ( !done )
         {
            dafec_c ( handle, BUFFSZ, LINLEN, &n, buffer, &done );

            for ( i = 0;  i < n;  i++ )
            {
               printf ( "%s\n", buffer[i] );
            }    
         }

         return ( 0 );
      }


 
-Restrictions
 
   1) The comment area may consist only of printing ASCII characters, 
      decimal values 32 - 126. 
 
   2) There is NO maximum length imposed on the significant portion 
      of a text line that may be placed into the comment area of a 
      DAF. The maximum length of a line stored in the comment area 
      should be kept reasonable, so that they may be easily 
      extracted. A good value for this might be 1000 characters, as 
      this can easily accommodate ``screen width'' lines as well as 
      long lines which may contain some other form of information. 
 
   3) This routine is only used to read records on environments 
      whose characters are a single byte in size.  Updates 
      to this routine and routines in its call tree may be 
      required to properly handle other cases. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL)
   K.R. Gehringer (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 16-NOV-2006 (NJB) (KRG)

-Index_Entries
 
    extract comments from a DAF 
 
-&
*/

{ /* Begin dafec_c */


   /*
   Local variables
   */
   logical            fin;


   /*
   Participate in error tracing.
   */
   chkin_c ( "dafec_c" );

   /*
   Make sure the string pointer for the buffer array is non-null 
   and that the length lenvals is sufficient.  
   */
   CHKOSTR ( CHK_STANDARD, "dafec_c", buffer, lenout );
   

   /*
   Call the f2c'd routine. 
   */
   dafec_ ( ( integer * ) &handle,
            ( integer * ) &bufsiz,
            ( integer * ) n,
            ( char    * ) buffer,
            ( logical * ) &fin,
            ( ftnlen    ) lenout-1 );
  
   /* 
   Set the output SpiceBoolean found flag. 
   */
   *done = fin;

   if ( *n > 0 )
   {
      /*
      `cvals' now contains the requested data in a single Fortran-style
      string containing (lenout-1)*n significant characters.

      We need to convert `cvals' into an array
      of n null-terminated strings each `lenout' long.
      */
      F2C_ConvertTrStrArr ( *n, lenout, (char *)buffer );
   }
 
   chkout_c ( "dafec_c" );

} /* End dafec_c */
