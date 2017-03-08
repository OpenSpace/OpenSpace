//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman

#ifndef FITS_H
#define FITS_H 1

// exception
#include <exception>
// string
#include <string>
// map
#include <map>
// ExtHDU
#include "ExtHDU.h"
// HDUCreator
#include "HDUCreator.h"
// FitsError
#include "FitsError.h"

namespace CCfits {
  class FITSBase;
  class PHDU;
  class Table;

} // namespace CCfits
//class PHDU;
extern "C"
{
#       include <sys/stat.h>
}           
#include <memory>


namespace CCfits {
/*!  

     \class FITS
     \brief Memory object representation of a disk FITS file

     Constructors are provided to get FITS data from an existing file or to create
     new FITS data sets. Overloaded versions allow the user to 

     a) read from one or more specified extensions, specified by EXTNAME and VERSION
        or by HDU number.
     b  either just header information or  data on construction
     c) to specify scalar keyword values to be read on construction
     d) to open and read an extension that has specified keyword values
     e) create a new FITS object and corresponding file, including an
        empty primary header.

     The memory fits object as constructed is always an image of a valid
     FITS object, i.e. a primary HDU is created on construction.

     calling the destructor closes the disk file, so that FITS files are automatically
     deleted at the end of scope unless other arrangements are made.


*/

/*!   @defgroup FITSexcept FITS Exceptions

*/

/*!
     \class FITS::NoSuchHDU
     @ingroup FITSexcept
     @brief exception thrown by HDU retrieval methods. 
*/

/*! \fn FITS::NoSuchHDU::NoSuchHDU(const String& diag, bool silent)

        \brief Exception ctor, prefixes the string "FITS Error: Cannot read HDU in FITS file:" before the specific message.

        \param diag A specific diagnostic message, usually the name of the extension whose read was attempted.
        \param silent if true, print message whether FITS::verboseMode is set or not.


*/

/*!
     \class FITS::CantOpen
     @ingroup FITSexcept
     @brief thrown on failure to open existing file
*/

/*! \fn FITS::CantOpen::CantOpen(const String& diag, bool silent)

     \brief Exception ctor prefixes the string: "FITS Error: Cannot create file " before specific message

     This exception will be thrown if users attempt to open an existing file for
     write access to which they do not have permission, or of course if the file does
     not exist. 

        \param diag A specific diagnostic message, the name of the file that was to be created.
        \param silent if true, print message whether FITS::verboseMode is set or not.

*/


/*!
     \class FITS::CantCreate
     @ingroup FITSexcept
     @brief thrown on failure to create new file

*/

/*! \fn FITS::CantCreate::CantCreate(const String& msg, bool silent)

     \brief Exception ctor prefixes the string: "FITS Error: Cannot create file " before specific message

     This exception will be thrown if the user attempts to write to a protected directory or 
     attempts to create a new file with the same name as an existing file
     without specifying overwrite [overwrite is specified by adding the character '!' before
     the filename, following the cfitsio convention].

        \param msg A specific diagnostic message, the name of the file that was to be created.
        \param silent if true, print message whether FITS::verboseMode is set or not.

*/


/*!
     \class FITS::OperationNotSupported
     @ingroup FITSexcept
     @brief thrown for unsupported operations, such as attempted to select rows from
      an image extension.

*/
/*! \fn FITS::OperationNotSupported::OperationNotSupported(const String& msg, bool silent)

        \brief Exception ctor, prefixes the string "FITS Error: Operation not supported:" before the specific message.

        \param msg A specific diagnostic message.
        \param silent if true, print message whether FITS::verboseMode is set or not.


*/


/*!   
      \fn FITS::FITS(const FITS &right)
      \brief copy constructor

*/

/*!   \fn  FITS::FITS(const String &name, RWmode mode, bool readDataFlag, const std::vector<String>& primaryKeys) 
      \brief basic constructor

      This basic constructor makes a FITS object from the given filename.  The file name is the only required
      argument.  The file name string is passed directly to the cfitsio library: thus the extended
      file name syntax described in the cfitsio manual should work as documented.
      (Though the extended file name feature which allows the opening of a particular
      image located in the row of a table is currently unsupported.)

      If the mode is Read [default]: It will read all of the headers in the file, and all of the data if the
      readDataFlag is supplied as true. It will also read optional primary keys.  Upon completion, the
      the last header in the file will become the current extension.  (However if the file name includes 
      extended syntax selecting a particular extension, that extension will be the current one.) 

      If the mode is Write and the file already exists:  The file is opened in read-write mode, all
      of the headers of the file are read, and all of the data if the readDataFlag is supplied as true.  It will
      also read optional primary keys.  For backwards compatibility with older versions of
      CCfits (which only read the primary when in Write mode for pre-existing files), the primary will become
      the current extension.

      If the mode is Write and the file does NOT exist (or is overwritten using '!' syntax):  
      A default primary HDU will be created in the file with BITPIX=8 and NAXIS=0.
      However if you wish to create a new file with image data in the primary, the version of the
      FITS constructor that specifies the data type and number of axes should be used instead.


      \param name The name of the FITS file to be read/written
      \param mode The read/write mode: must be Read or Write
      \param readDataFlag boolean: read data on construction if true
      \param primaryKeys Allows optional reading of primary header keys on construction


      \exception NoSuchHDU thrown on HDU seek error either by index or {name,version}
      \exception FitsError thrown on non-zero status code from cfitsio when not overriden
                 by FitsException error to produce more illuminating message.

*/



/*!      \fn  FITS::FITS (const String &name, RWmode mode, const std::vector<String>& searchKeys, const std::vector<String> &searchValues, bool readDataFlag = false, const std::vector<String>& hduKeys = std::vector<String>(), const std::vector<String>& primaryKey = std::vector<String>(), int version = 1);
      \brief open fits file and read HDU which contains supplied keywords
           with [optional] specified values (sometimes one just wants to know that the keyword is present).

        Optional parameters allows the reading of specified primary HDU keys and 
        specified columns and keywords in the HDU of interest.  

      \param name The name of the FITS file to be read
      \param mode The read/write mode: must be Read or Write
      \param searchKeys A string vector of keywords to search for in each header

      \param searchValues A string vector of values those keywords are required to have for success.
                          Note that the keys must be of type string. If any value does not
                          need to be checked the corresponding searchValue element can be empty.
      \param readDataFlag boolean: if true, read data if HDU is found                    
      \param hduKeys Allows optional reading of keys in the HDU that is searched for if it 
                      is successfully found
      \param primaryKey Allows optional reading of primary header keys on construction
      \param version   Optional version number. If specified, checks the EXTVER keyword.


      \exception FitsError thrown on non-zero status code from cfitsio when not overriden
                 by FitsException error to produce more illuminating message.

*/


/*!   \fn FITS::FITS(const String &name, RWmode mode, const string &hduName, bool readDataFlag, const std::vector<String>& hduKeys, const std::vector<String>& primaryKey, int version);
      \brief Open a FITS file and read a single specified HDU.      

      This and similar constructor variants support reading table data.

      Optional arguments allow the reading of primary header keys and specified
      data from hduName, the HDU to be read. An object representing the primary
      HDU is always created: if it contains an image, that
      image may be read by subsequent calls.

      If extended file name syntax is used and selects an extension other than hduName, a
      FITS::OperationNotSupported exception will be thrown.

      \param name The name of the FITS file to be read
      \param mode The read/write mode: takes values Read or Write
      \param hduName The name of the HDU to be read.
      \param hduKeys Optional array of keywords to be read from the HDU
      \param version Optional version number. If not supplied the first HDU
             with name <I>hduName</I> is read
      see above for other parameter definitions

*/


  /*! \fn FITS::FITS(const String &name, RWmode mode, const std::vector<String>& hduNames, bool readDataFlag, const std::vector<String>& primaryKey);

     This is intended as a convenience where the file consists of single versions of
     HDUs and data only, not keys are to be read.

      If extended file name syntax is used and selects an extension not listed in hduNames, a
      FITS::OperationNotSupported exception will be thrown.
     \param hduNames array of HDU names to be read.

     see above for other parameter definitions.
  */

  /*! \fn FITS::FITS(const String &name, RWmode mode, const std::vector<String>& hduNames, const std::vector<std::vector<String> >& hduKeys, bool readDataFlag, const std::vector<String>& primaryKeys, const std::vector<int>& hduVersions);
      \brief FITS read constructor in full generality.

      \param hduVersions an optional version number for each HDU to be read
      \param hduKeys an array of keywords for each HDU to be read.
     see above for other parameter definitions.
  */  

    /*! \fn FITS::FITS(const String& name, int bitpix, int naxis, long *naxes) ;
      \brief Constructor for creating new FITS objects containing images.

      This constructor is only called for creating new files (mode is not an argument)
      and creates a new primary HDU with the datatype & axes specified by bitpix,
      naxis, and naxes. The data are added to the new fits object and file by
      subsequent calls to FITS::pHDU().write( <arguments> )

      A file with a compressed image may be creating by appending to the end of the file name
      the same "[compress ...]" syntax specified in the cfitsio manual.  Note however that
      the compressed image will be placed in the first extension and NOT in the primary HDU. 

      If the filename corresponds to an existing file and does not start with the '!'
      character the construction will fail with a CantCreate exception.




     The arguments are:
     \param name The file to be written to disk
     \param bitpix the datatype of the primary image. <i>bitpix</i> may be one of the following CFITSIO constants: BYTE_IMG, SHORT_IMG, LONG_IMG,
     FLOAT_IMG, DOUBLE_IMG, USHORT_IMG, ULONG_IMG, LONGLONG_IMG.  Note that if you send in a <i>bitpix</i> of 
     USHORT_IMG or ULONG_IMG, CCfits will set HDU::bitpix() to its signed equivalent (SHORT_IMG
     or LONG_IMG), and then set BZERO to 2^15 or 2^31.

     \param naxis the data dimension of the primary image
     \param naxes the array of axis lengths for the primary image. Ignored if naxis =0,
            i.e. the primary header is empty.
     extensions can be added arbitrarily to the file after this constructor is
     called. The constructors should write header information to disk:
 */

    /*! \fn FITS::FITS(const string &name, RWmode mode, int hduIndex, bool readDataFlag, const std::vector<String>& hduKeys, const std::vector<String>& primaryKey) ;
      \brief read a single numbered HDU. 

      Constructor analogous to the version that reads by name. This is required since
      HDU extensions are not required to have the EXTNAME or HDUNAME keyword by
      the standard. If there is no name, a dummy name based on the HDU number is
      created and becomes the key.

      If extended file name syntax is used and selects an extension other than hduIndex, a
      FITS::OperationNotSupported exception will be thrown.

      \param hduIndex The index of the HDU to be read.
     see above for other parameter definitions.
  */

    /*! \fn FITS::~FITS()
     \brief destructor

    */

    /*! \fn FITS::FITS (const String& fileName, const FITS& source)
      \brief create a new FITS object and corresponding file with copy of the primary header of the source
      If the filename corresponds to an existing file and does not start with the '!'
      character the construction will fail with a CantCreate exception.


      \param fileName New file to be created.
      \param source A previously created FITS object to be copied.

     see above for other parameter definitions.  
  */

  /*! \fn static void FITS::clearErrors()

        \brief clear the error stack and set status to zero.

  */

   /*! \fn void FITS::deleteExtension(const String& doomed, int version=1)

        \brief Delete extension specified by name and version number.

        Removes extension from FITS object and memory copy.  The index
        numbers of all HDU objects which follow this in the file will be
        decremented by 1.

        \param doomed the name of the extension to be deleted
        \param version an optional version number, the EXTVER keyword, defaults to 1

        \exception NoSuchHDU Thrown if there is no extension with the specified version number
        \exception FitsError Thrown if there is a non-zero status code from cfitsio, e.g.
                if the delete operation is applied to a FITS file opened for read only access.
    */

   /*! \fn void FITS::deleteExtension(int doomed)

        \brief Delete extension specified by extension number

        The index numbers of all HDU objects which follow this in the 
        file will be decremented by 1.
        
    */



   /*! \fn void FITS::read(const String &hduName, bool readDataFlag, const std::vector<String> &keys, int version=1) ;
       \brief get data from single HDU from disk file.

       This is provided to allow the adding of additional HDUs to the FITS
       object after construction of the FITS object.
       After the read() functions have been called for the FITS object, subsequent
       read method to the Primary, ExtHDU, and Column objects will retrieve data
       from the FITS object in memory (those methods can be called to read data
       in those HDU objects that was not read when the HDU objects were constructed.

       All the read functions will throw NoSuchHDU exceptions 
       on seek errors since they involve constructing HDU objects.  

       The parameter definitions are as documented for the corresponding constructor.
   */     

 /*!      \fn  FITS::read (const std::vector<String>& searchKeys, const std::vector<String> &searchValues, bool readDataFlag, const std::vector<String>& hduKeys,  int version=1) ;

        \brief read method for read header or HDU that contains specified  keywords.

      \param searchKeys A string vector of keywords to search for in each header

      \param searchValues A string vector of values those keywords are required to have for success.
                          Note that the keys must be of type string. If any value does not
                          need to be checked the corresponding searchValue element can be empty.
      \param readDataFlag boolean: if true, read data if HDU is found                    
      \param hduKeys Allows optional reading of keys in the HDU that is searched for if it 
                      is successfully found
      \param version   Optional version number. If specified, checks the EXTVER keyword.


*/

/*!\fn void FITS::copy(const HDU& source);
        \brief copy the HDU source into the FITS object.

        This function adds a copy of an HDU from another file into *this. It
        does not create a duplicate of an HDU in the file associated with *this.

*/



   /*! \fn void FITS::read(const std::vector<String> &hduNames, bool readDataFlag)
      \brief get data from a set of HDUs from disk file.

      This is provided to allow  reading of  HDUs after construction.  
      see above for parameter definitions. 
  */ 

   /*! \fn void FITS::read(const std::vector<String> &hduNames, const std::vector<std::vector<String> > &keys, bool readDataFlag = false, const std::vector<int>& hduVersions = std::vector<int>());
      \brief get data from a set of HDUs from disk file, specifying keys and version numbers.

      This is provided to allow  reading of  HDUs after construction.  
      see above for parameter definitions. 
  */ 

    /*! \fn void FITS::read (int hduIndex, bool readDataFlag = false, const std::vector<String> &keys = std::vector<String>()) ;

       \brief read an HDU specified by index number.

      This is provided to allow  reading of  HDUs after construction.
      see above for parameter definitions. 

   */

  /*! \fn const String& FITS::currentExtensionName () const
      \brief return the name of the extension that the fitsfile is currently addressing.

      If the extension in question does not have an EXTNAME or HDUNAME keyword, then
      the function returns $HDU$n, where n is the sequential HDU index number (primary
      HDU = 0).

  */ 


  /*! \fn int FITS::open(RWmode mode= Read) ;
      \brief open the file with mode as specified on construction.
      Returns the 0-based current HDU index.
  */ 

  /*! \fn bool FITS::create() ;
      \brief create new fits file on disk and construct the fitsfile pointer
  */ 

  /*! \fn void FITS::close() throw();
      \brief close the file. 
  */ 

  /*! \fn const FITS::ExtHDU&  FITS::extension(int i) const
      \brief return FITS extension by index number. 
      N.B. The input index number is currently defined as enumerating
      extensions, so the extension(1) returns HDU number 2.
  */ 

  /*! \fn  FITS::ExtHDU&  FITS::extension(int i) ;
      \brief return FITS extension by index number. non-const version.
      see const version for details.
  */ 

    /*! \fn friend std::ostream & operator << (std::ostream &s, const FITS& right)
      \brief Output operator. Calls output operators for HDUs in turn.

      This operator acts similarly to the ftool fdump for a fits file,
      except that there is no freedom to output partial information.

      The current implementation of this operator for PHDU objects
	only outputs the array sizes, not the data, which that for
	tables prints the data also.

	Provision of this operator is intended largely for debugging
	purposes.



  */

  /*! \fn  const ExtHDU& FITS::extension(const String& hduName, int version) const
      \brief return FITS extension by name and (optionally) version number.
  */ 

  /*! \fn  ExtHDU& FITS::extension(const String& hduName, int version) 
      \brief return FITS extension by name and (optionally) version number.
  */ 


  /*! \fn  const std::multimap<std::String,ExtHDU*>& FITS::extension () const;
      \brief return const reference to the extension container

      This is useful for such operations as extension().size() etc.
  */ 


   /*! \fn  Table* FITS::addTable (const String& hduName, 
			int rows, 
                        const std::vector<String>& columnName,
        		const std::vector<String>& columnFmt,
        		const std::vector<String>& columnUnit,
        		HduType type, 
			int version);

       \brief Add a table extension to an existing FITS object. 
       Add extension to FITS object for file with w or rw access.
       \param rows    The number of rows in the table to be created.
       \param columnName A vector containing the table column names
       \param columnFmt  A vector containing the table column formats
       \param columnUnit (Optional) a vector giving the units of the columns.
       \param type The table type - AsciiTbl or BinaryTbl (defaults to BinaryTbl)
      the lists of columns are optional - one can create an empty table extension
      but if supplied, colType, columnName and colFmt must have equal dimensions.

	\todo the code should one day check that the version keyword is higher than any other versions already added to the FITS object (although cfitsio doesn't
do this either).

   */

   /*! \fn void FITS::addImage (const String& hduName, int bpix, std::vector<long>& naxes, int version)
     \brief Add an image extension to an existing FITS object. (File with w or rw access).

     Does not make primary images, which are built in the constructor for the FITS file.
     The image data is not added here: it can be added by a call to one of the ExtHDU::write 
     functions.  

     <i>bpix</i> may be one of the following CFITSIO constants: BYTE_IMG, SHORT_IMG, LONG_IMG,
     FLOAT_IMG, DOUBLE_IMG, USHORT_IMG, ULONG_IMG, LONGLONG_IMG.  Note that if you send in a <i>bpix</i> of 
     USHORT_IMG or ULONG_IMG, CCfits will set HDU::bitpix() to its signed equivalent (SHORT_IMG
     or LONG_IMG), and then set BZERO to 2^15 or 2^31.

     \todo Add a function for replacing the primary image
   */


   /*! \fn FITS::resetPosition() ;

      \brief explicit call to set the fits file pointer to the primary.
   */   

   /*! \fn PHDU& FITS::pHDU()

      \brief return a reference to the primary HDU.
   */   

   /*! \fn const PHDU& FITS::pHDU() const

      \brief return a const reference to the primary HDU.
   */   

   /*! \fn FITS::currentExtension();

      \brief return a non-const reference to whichever is the current extension.
   */   


   /*! \fn       Table& FITS::filter (const String& expression, ExtHDU& inputTable, bool overwrite = true, bool readData = false);


      \brief Filter the rows of the inputTable with the condition expression, and return a reference
             to the resulting Table.

             This function provides an object oriented version of cfitsio's fits_select_rows call. 
             The expression string is any boolean expression involving the names of the columns in the
             input table (e.g., if there were a column called "density", a valid expression
             might be "DENSITY > 3.5": see the cfitsio documentation for further details).              

             [N.B. the "append" functionality described below does not work when linked with
             cfitsio 2.202 or prior because of a known issue with that version of the library.
             This causes the output to be a new extension with a correct header copy and
             version number but without the filtered data].
             If the inputTable is an Extension HDU of this FITS object, then if overwrite is
             true the operation will overwrite the inputTable with the filtered version, otherwise
             it will append a new HDU with the same extension name but the next highest
             version (EXTVER) number available.

   */   



   /*! \fn void FITS::destroy() throw()

      \brief Erase FITS object and close corresponding file.

      Force deallocation and erase of elements of a FITS memory object. Allows a reset of everything
      inside the FITS object, and closes the file. The object is inaccessible after this call.

      destroy is public to allow users to reuse a symbol for a new file,
      but it is identical in operation to the destructor.

   */

   /*! \fn void FITS::flush()

        \brief flush buffer contents to disk

        Provides manual control of disk writing operation. Image data are flushed
        automatically to disk after the write operation is completed, but not
        column data.

   */

   /*! \fn const std::string& FITS::name () const

        \brief return filename of file corresponding to FITS object
   */

   /*! \fn static const bool FITS::verboseMode ()

        \brief return verbose setting for library

        If true, all messages that are reported by exceptions are printed to std::cerr.
   */

   /*! \fn static void FITS::setVerboseMode (bool ) 

        \brief set verbose setting for library


   */

   /*! \fn  void FITS::setCompressionType (int compType)
       \brief set the compression algorithm to be used when adding image extensions to the FITS object.

       \param compType Currently 3 symbolic constants are defined in cfitsio for 
       specifying compression algorithms:  GZIP_1, RICE_1, and PLIO_1.  See the cfitsio
       documentation for more information about these algorithms.  Entering NULL for
       compType will turn off compression and cause normal FITS images to be written.
   */

   /*! \fn  void FITS::setTileDimensions (const std::vector<long>& tileSizes)
       \brief Set the dimensions of the tiles into which the image is divided during compression.

       \param tileSizes A vector of length N containing the tile dimesions.  If
       N is less than the number of dimensions of the image it is applied to, the
       unspecified dimensions will be assigned a size of 1 pixel.  If N is larger
       than the number of image dimensions, the extra dimensions will be ignored.

       The default cfitsio behavior is to create tiles with dimensions NAXIS1 x 1 x 1 etc. up
       to the number of image dimensions.
   */

   /*! \fn  void FITS::setNoiseBits (int noiseBits)
       \brief Set the cfitsio noisebits parameter used when compressing floating-point images.

       The default value is 4.  Decreasing the value of noisebits will improve the 
       overall compression efficiency at the expense of losing more information.
   */

   /*! \fn  int FITS::getCompressionType () const
       \brief Get the int specifying the compression algorithm to be used when adding an image extension.       
   */

   /*! \fn  void FITS::getTileDimensions (std::vector<long>& tileSizes) const
       \brief Get the current settings of dimension sizes for tiles used in image compression.

       \param tileSizes A vector to be filled with cfitsio's current tile dimension settings.
       CCfits will resize this vector to contain the proper number of values.
   */

   /*! \fn  int FITS::getNoiseBits () const
       \brief Get the cfitsio noisebits parameter used when compressing floating-point images.
   */

   /*! \fn fitsfile* FITS::fitsPointer() const
       \brief return the CFITSIO fitsfile pointer for this FITS object

   */
  //	! The FITS object class. Contains a primary HDU and Extensions indexed by name.



  class FITS 
  {

    public:



      class NoSuchHDU : public FitsException  //## Inherits: <unnamed>%396C90CB0236
      {
        public:
            NoSuchHDU (const String& diag, bool silent = true);

        protected:
        private:
        private: //## implementation
      };



      class OperationNotSupported : public FitsException  //## Inherits: <unnamed>%39806C7600D5
      {
        public:
            OperationNotSupported (const String& msg, bool silent = true);

        protected:
        private:
        private: //## implementation
      };



      class CantOpen : public FitsException  //## Inherits: <unnamed>%39C8EB1D02C0
      {
        public:
            CantOpen (const String& diag, bool silent = true);

        protected:
        private:
        private: //## implementation
      };



      struct CantCreate : public FitsException  //## Inherits: <unnamed>%39C8EB10020B
      {
            CantCreate (const String& diag, bool silent = false);

        public:
        protected:
        private:
        private: //## implementation
      };
        FITS (const String &name, RWmode mode = Read, bool readDataFlag = false, const std::vector<String>& primaryKeys = std::vector<String>());
        //	Open a file and read a specified HDU.
        //
        //	Optional parameter allows the reading of specified primary HDU keys.
        FITS (const String &name, RWmode mode, const string &hduName, bool readDataFlag = false, const std::vector<String>& hduKeys = std::vector<String>(), const std::vector<String>& primaryKey = std::vector<String>(), int version = 1);
        //	Read data from a set of specified HDUs. keywords can only be specified for the primary here.
        //	The code will call a different constructor for the case where keywords are required for
        //	the extensions.
        FITS (const String &name, RWmode mode, const std::vector<String>& hduNames, bool readDataFlag = false, const std::vector<String>& primaryKey = std::vector<String>());
        //	Initialize a new FITS file object with the primary from a
        //	different file.
        FITS (const String& fileName, const FITS& source);
        //	Fully general FITS HDU reader. May read any part of fits file by
        //	supplying HDU names and version numbers, and optionally
        //	the data read flag.
        FITS (const String &name, RWmode mode, const std::vector<String>& hduNames, const std::vector<std::vector<String> >& hduKeys, bool readDataFlag = false, const std::vector<String>& primaryKeys = std::vector<String>(), const std::vector<int>& hduVersions = std::vector<int>());
        //	Writing constructor. Takes a name and information to create an empty
        //	Primary HDU which can then be filled with calls to HDU methods.
        FITS (const String& name, int bitpix, int naxis, long *naxes);
        //	Open a file and read a specified HDU.
        //
        //	Optional parameter allows the reading of specified primary HDU keys.
        FITS (const string &name, RWmode mode, int hduIndex, bool readDataFlag = false, const std::vector<String>& hduKeys = std::vector<String>(), const std::vector<String>& primaryKey = std::vector<String>());
        //	Open a file and read a HDU that contains  specified
        //	search keywords with [optional] specified values
        //	(sometimes one just wants to know that the keyword is present).
        //
        //	Optional parameters allows the reading of specified primary HDU keys and specified keywords in
        //	the HDU of interest.
        FITS (const String &name, RWmode mode, const std::vector<String>& searchKeys, const std::vector<String> &searchValues, bool readDataFlag = false, const std::vector<String>& hduKeys = std::vector<String>(), const std::vector<String>& primaryKey = std::vector<string>(), int version = 1);
        ~FITS();

        static void clearErrors ();
        void deleteExtension (const String& doomed, int version = 1);
        //	Read keys and data from a single ExtHDU in the file.
        void read (const String &hduName, bool readDataFlag = false, const std::vector<String> &keys = std::vector<String>(), int version = 1);
        //	Read multiple ExtHDUs. If the version number needs to be specified then one must call a
        //	different method.
        void read (const std::vector<String> &hduNames, bool readDataFlag = false);
        //	Read selected data from multiple ExtHDUs
        void read (const std::vector<String> &hduNames, const std::vector<std::vector<String> > &keys, bool readDataFlag = false, const std::vector<int>& hduVersions = std::vector<int>());
        //	Read keys and data from a single ExtHDU in the file.
        void read (int hduIndex, 	// Construct and Read HDU specified by number. One can add further HDUs by number using
        	// the HDUCreator factory.
        bool readDataFlag = false, const std::vector<String> &keys = std::vector<String>());
        //	Open a file and read a HDU that contains  specified
        //	search keywords with [optional] specified values
        //	(sometimes one just wants to know that the keyword is present).
        //
        //	Optional parameters allows the reading of specified primary HDU keys and specified keywords in
        //	the HDU of interest.
        void read (const std::vector<String>& searchKeys, const std::vector<String> &searchValues, bool readDataFlag = false, const std::vector<String>& hduKeys = std::vector<String>(), int version = 1);
        const ExtHDU& extension (int i) const;
        fitsfile* fitsPointer () const;
        ExtHDU& extension (int i);
        const ExtHDU& extension (const String& hduName, int version = 1) const;
        const PHDU& pHDU () const;
        PHDU& pHDU ();
        ExtHDU& extension (const String& hduName, int version = 1);
        friend std::ostream& operator << (std::ostream& s, const FITS& right);
        //	! add a new Table extension to a FITS object
        Table* addTable (const String& hduName, int rows, 	// ! Number of rows in new table. Mandatory
        const std::vector<String>& columnName = std::vector<String>(), 	// ! Optional set of column names for new table
        const std::vector<String>& columnFmt = std::vector<String>(), 	// ! Column formats for column units. Mandatory if columnName is specified
        const std::vector<String>& columnUnit = std::vector<String>(), 	// ! Column formats for column units. Optional
        HduType type = BinaryTbl, int version = 1);
        //	! add a new ImageExt (image extension) to the FITS object. A "writing" method.
        ExtHDU* addImage (const String& hduName, int bpix, std::vector<long>& naxes, int version = 1);
        //	Force destruction of the FITS object. Essentially
        //	is a manual destructor call.
        void destroy () throw ();
        void flush ();
        const String& currentExtensionName () const;
        const ExtMap& extension () const;
        void resetPosition ();
        void currentExtensionName (const String& extName);
        const String& name () const;
        void copy (const HDU& source);
        Table& filter (const String& expression, ExtHDU& inputTable, bool overwrite = true, bool readData = false);
        ExtHDU& currentExtension ();
        void deleteExtension (int doomed);
        void setCompressionType (int compType);
        void setTileDimensions (const std::vector<long>& tileSizes);
        void setNoiseBits (int noiseBits);
        int getCompressionType () const;
        void getTileDimensions (std::vector<long>& tileSizes) const;
        int getNoiseBits () const;
        static bool verboseMode ();
        static void setVerboseMode (bool value);

    public:
      // Additional Public Declarations

    protected:
      // Additional Protected Declarations

    private:
        FITS(const FITS &right);
        FITS & operator=(const FITS &right);

        void unmapExtension (ExtHDU& doomed);
        int nextVersionNumber (const String& inputName) const;
        //	read the primary HDU. Read the image if
        //	readDataFlag is true.
        void read (bool readDataFlag = false, const std::vector<String>& keys = std::vector<String>());
        //	Returns index of current HDU where primary = 0.  (Extended file syntax may cause a shift to an
        //	extension.)
        int open (RWmode mode = Read);
        //	Create returns true if a new file was created or an
        //	existing file overwritten, false if appending.
        //
        //
        //	It throws exception CantCreate or CantOpen if either fails.
        bool create ();
        //	Close the fits file.
        //
        //	Called in destructors so must not throw.
        int close () throw ();
        std::ostream & put (std::ostream &s) const;
        ExtHDU& extbyVersion (const String& hduName, int version) const;
        void pHDU (PHDU* value);
        void readExtensions (bool readDataFlag = false);
        ExtHDU* addExtension (ExtHDU* ext);
        void swap (FITS& right);
        ExtMap& extensionMap ();
        String nameOfUnmapped (int hduNum) const;
        void cloneHeader (const ExtHDU& source);

        // Check if caller is requesting an already read ExtHDU (ie. one
        // that's already been added to ExtMap).  If hduIdx=0, check by
        // matching name and optional version.  Otherwise check by matching
        // hduIdx.  If found, returns pointer to the ExtHDU.  Otherwise
        // returns 0.  This will not throw.
        ExtHDU* checkAlreadyRead(const int hduIdx, 
                    const String& hduName = string(""), const int version=1) const throw();

      // Additional Private Declarations

    private: //## implementation
      // Data Members for Class Attributes
        static bool s_verboseMode;

      // Data Members for Associations
        FITSBase* m_FITSImpl;

      // Additional Implementation Declarations
      friend void HDU::makeThisCurrent() const;
  };

  // Class CCfits::FITS::NoSuchHDU 

  // Class CCfits::FITS::OperationNotSupported 

  // Class CCfits::FITS::CantOpen 

  // Class CCfits::FITS::CantCreate 

  // Class CCfits::FITS 

  inline ExtHDU& FITS::extension (const String& hduName, int version)
  {

    return extbyVersion(hduName,version);
  }

  inline std::ostream& operator << (std::ostream& s, const FITS& right)
  {

    return right.put(s);
  }

  inline bool FITS::verboseMode ()
  {
    return s_verboseMode;
  }

  inline void FITS::setVerboseMode (bool value)
  {
    s_verboseMode = value;
  }

} // namespace CCfits


#endif
