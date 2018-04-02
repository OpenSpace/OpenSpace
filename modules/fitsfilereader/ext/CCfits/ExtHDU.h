//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman

#ifndef EXTHDU_H
#define EXTHDU_H 1

// CCfitsHeader
#include "CCfits.h"
// HDU
#include "HDU.h"
// FitsError
#include "FitsError.h"

namespace CCfits {
  class Column;

} // namespace CCfits


namespace CCfits {

/*! \class ExtHDU::WrongExtensionType
    @ingroup FITSexcept
    @brief Exception to be thrown on unmatched extension types

    This exception is to be thrown if the user requested a particular
    extension and it does not correspond to the expected type.


*/

/*! \fn ExtHDU::WrongExtensionType::WrongExtensionType (const String& msg, bool silent);

        \brief Exception ctor, prefixes the string "Fits Error: wrong extension type" before the specific message.

        \param msg A specific diagnostic message
        \param silent if true, print message whether FITS::verboseMode is set or not.

*/        

/*! \class ExtHDU

    \brief base class for all FITS extension HDUs, i.e. Image Extensions and Tables.

    ExtHDU needs to have the combined public interface of Table objects and images.
    It achieves this by providing the same set of read and write operations as PHDU,
    and also providing the same operations for extracting columns from the extension
    as does Table [after which the column interface is accessible]. Differentiation
    between extension types operates by exception handling: .i.e. attempting to
    access image data structures on a Table object through the ExtHDU interface
    will or trying to return a Column reference from an Image extension will both
    throw an exception


*/


/*! \fn      ExtHDU::ExtHDU(const ExtHDU &right);
        \brief copy constructor         

*/

/*! \fn      virtual ExtHDU::~ExtHDU();
        \brief destructor

*/


/*! \fn      static void ExtHDU::readHduName (const fitsfile* fptr, int hduIndex, String& hduName, int& hduVersion);
        \brief read extension name. 


        Used primarily to allow extensions to be specified by HDU number and provide
        their name for the associative array that contains them. Alternatively, if
        there is no name keyword in the extension, one is synthesized from the 
        index.

*/

/*! \fn      virtual void ExtHDU::readData (bool readFlag = false, const std::vector<String>& keys = std::vector<String>()) = 0;

        \brief read data from HDU depending on readFlag and keys.


*/

/*! \fn      const String& ExtHDU::name () const;
        \brief return the name of the extension.

*/

/*! \fn      virtual ExtHDU * ExtHDU::clone (FITSBase* p) const = 0;
        \brief virtual copy constructor

*/

/*! \fn  virtual Column& ExtHDU::column (const String& colName, bool caseSensitive = true) const ;

        \brief return a reference to a Table column specified by name. 

        If the <i>caseSensitive</i> parameter is set to false, the search will 
        be case-insensitive. The overridden base class implementation 
        ExtHDU::column throws an exception, which is thus the action to be taken 
        if self is an image extension

        \exception WrongExtensionType see above

*/

/*! \fn  virtual Column& ExtHDU::column (int colIndex) const;

        \brief return a reference to a Table column specified by column index. 

        This version is provided for convenience; the 'return by name' version is
        more efficient because columns are stored in an associative array sorted
        by name.     


        \exception WrongExtensionType thrown if *this is an image extension.

*/                       

/*! \fn   const ColMap& ExtHDU::column () const;

        \brief return a reference to the multimap containing the columns. 

        \exception WrongExtensionType thrown if *this is an image extension.
*/

/*! \fn     int ExtHDU::numCols () const;


        \brief return the number of Columns in the Table (the TFIELDS keyword).


        \exception WrongExtensionType thrown if *this is an image extension.
*/



/*! \fn      const int ExtHDU::version () const;
        \brief return the extension version number.

*/

/*! \fn      void ExtHDU::version (int value);
        \brief set the extension version number

*/


/*! \fn      long ExtHDU::rows () const;
        \brief return the number of rows in the extension.

        \exception WrongExtensionType thrown if *this is an image extension.

*/


/*! \fn      ExtHDU::ExtHDU (FITSBase* p, HduType xtype, const String &hduName, int version);
        \brief default constructor, required as Standard Library Container content.

*/

/*! \fn      ExtHDU::ExtHDU (FITSBase* p, HduType xtype, const String &hduName, int bitpix, int naxis, const std::vector<long>& axes, int version);
        \brief	writing constructor. 

      The writing constructor forces the user to supply a name for the HDU. The bitpix,
      naxes and naxis data required by this constructor are required FITS keywords for
      any HDUs.
*/

/*! \fn      ExtHDU::ExtHDU (FITSBase* p, HduType xtype, int number);
      \brief ExtHDU constructor for getting ExtHDUs by number.

      Necessary since EXTNAME is a reserved, not required, keyword. But
      a synthetic name is supplied by static ExtHDU::readHduName which
      is called by this constructor.


*/

/*!  \fn      virtual void ExtHDU::addColumn (ValueType type, const String& columnName, long repeatWidth, const String& colUnit = String(""), long decimals = -1, size_t  columnNumber = 0);

        \brief add a new column to an existing table HDU.

        \param type The data type of the column to be added
        \param columnName The name of the column to be added
        \param repeatWidth for a string valued, this is the width of a string. For
            a numeric column it supplies the vector length of the rows. It is ignored for
            ascii table numeric data.
        \param colUnit an optional field specifying the units of the data (TUNITn)
        \param decimals optional parameter specifying the number of decimals for an ascii numeric column
        \param columnNumber optional parameter specifying column number to be created. If not specified
                        the column is added to the end. If specified, the column is inserted and the
                        columns already read are reindexed.
                        This parameter is provided as a convenience to support existing code rather than 
                        recommended.                


*/

/*! \fn      virtual void ExtHDU::copyColumn(const Column& inColumn, int colIndx, bool insertNewCol=true);

       \brief copy a column (from different or same HDU and file) into an existing table HDU.
       
       This is meant to provide the same functionality as CFITSIO's fits_copy_col, and therefore
       does not work with columns with variable length fields.
       Copying a column from an AsciiTable to a BinTable is prohibited.  colIndx range should be
       from 1 to nCurrentCols+1 if inserting, or 1 to nCurrentCols if replacing.  
         
       \param inColumn The Column object which is to be copied
       \param colIndx The position for which the copied Column will be placed (first colIndx = 1).
       \param insertNewCol If 'true', new Column will be inserted in or appended to table.
                           If 'false', Column will replace current Column at position = colIndx.
                           
       
*/

/*! \fn      virtual void ExtHDU::deleteColumn(const String& columnName);

        \brief delete a column in a Table extension by name.

        \param  columnName The name of the column to be deleted.

        \exception  WrongExtensionType if extension is an image.
*/

/*! \fn      template <typename S> void ExtHDU::read (std::valarray<S>& image); 

        \brief Read image data into container.

        The container image contains the entire image array after the call.
        This and all the other variants of read() throw a WrongExtensionType
        exception if called for a Table object.

*/


/*! \fn      template<typename S> void ExtHDU::read (std::valarray<S>& image, 
                                        long first,
                                        long nElements, 
                                        S* nullValue) ; 

                \brief read part of an image array, processing null values.

                Implicit data conversion is supported (i.e. user does not need to know the 
                type of the data stored. A WrongExtensionType extension is thrown
                if *this is not an image. 

                \param image The receiving container, a std::valarray reference 
                \param first The first pixel from the array to read [a long value]
                \param nElements The number of values to read
                \param nullValue A pointer containing the value in the table to be 
                        considered as undefined. See cfitsio for details


*/      

/*! \fn      template<typename S> void ExtHDU::read (std::valarray<S>& image, 
                                const std::vector<long>& first, 
		                long nElements, 
                                S* nullValue)  ;

             \brief read part of an image array, processing null values.

             As above except for 

             \param first a vector<long> representing an n-tuple giving the
                    coordinates in the image of the first pixel.


*/      
/*! \fn      template<typename S> void ExtHDU::read (std::valarray<S>& image, 
                                const std::vector<long>& firstVertex, 
		                const std::vector<long>& lastVertex, 
		                const std::vector<long>& stride, 
                                S* nullValue) ; 


             \brief read an image subset into valarray image, processing null values

                The image subset is defined by two vertices and a stride
                indicating the 'denseness' of the values to be picked in each
                dimension (a stride = (1,1,1,...) means picking every 
                pixel in every dimension, whereas stride = (2,2,2,...)
                means picking every other value in each dimension.


*/      
/*! \fn      template<typename S> void ExtHDU::read (std::valarray<S>& image, 
                                long first,
                                long nElements) ; 

              \brief read an image section starting at a specified pixel

*/      

/*! \fn      template<typename S> void ExtHDU::read (std::valarray<S>& image, 
                                const std::vector<long>& first,
                                long nElements); 

              \brief read an image section starting at a location specified by an n-tuple
*/      
/*! \fn      template<typename S> void ExtHDU::read (std::valarray<S>& image, 
                                const std::vector<long>& firstVertex, 
		                const std::vector<long>& lastVertex, 
		                const std::vector<long>& stride) ;    


                \brief read an image subset

*/

/*! \fn      template <typename S> void ExtHDU::write(const std::vector<long>& first,
                    long nElements,
                    const std::valarray<S>& data,
                    S* nullValue);

        \brief Write a set of pixels to an image extension with the first pixel specified by an n-tuple, processing undefined data

        All the overloaded versions of ExtHDU::write perform operations on *this if
        it is an image and throw a WrongExtensionType exception if not.
        Where appropriate, alternate versions allow undefined data to be processed

        \param first an n-tuple of dimension equal to the image dimension specifying the 
                first pixel in the range to be written
        \param nElements number of pixels to be written
        \param data array of data to be written
        \param nullValue pointer to null value (data with this value written as undefined; needs
                the BLANK keyword to have been specified).

*/


/*! \fn template <typename S> void ExtHDU::write(long first,
                    long nElements, const std::valarray<S>& data, S* nullValue);

        \brief write array to image starting with a specified pixel and allowing undefined data to be processed

        parameters after the first are as for version with n-tuple specifying first element.
        these two version are equivalent, except that it is possible for the
        first pixel number to exceed the range of 32-bit integers, which is
        how long datatype is commonly implemented.                
*/

/*! \fn template <typename S> void ExtHDU::write(const std::vector<long>& first,
                    long nElements, const std::valarray<S>& data);

        \brief write array starting from specified n-tuple, without undefined data processing
*/

/*! \fn template <typename S> void ExtHDU::write(long first, long nElements,
                    const std::valarray<S>& data);

        \brief write array starting from specified pixel number, without undefined data processing


*/

/*! \fn      template <typename S> void ExtHDU::write(const std::vector<long>& firstVertex,
                    const std::vector<long>& lastVertex,
                    const std::valarray<S>& data);     

              \brief write a subset (generalize slice) of data to the image

                A generalized slice/subset is a subset of the image (e.g. one plane
                of a data cube of size <= the dimension of the cube). It is specified
                by two opposite vertices. The equivalent cfitsio call does not support
                undefined data processing so there is no version that allows a null
                value to be specified.

                \param firstVertex the coordinates specifying lower and upper vertices of the n-dimensional slice 
                \param lastVertex
                \param data The data to be written              
*/

/*! \fn      const long ExtHDU::pcount () const;
        \brief return required pcount keyword value

*/

/*! \fn      void ExtHDU::pcount (long value);
        \brief set required pcount keyword value

*/

/*! \fn      const long ExtHDU::gcount () const;
        \brief return required gcount keyword value

*/

/*! \fn      void ExtHDU::gcount (long value);
        \brief set required gcount keyword value

*/

/*! \fn      const HduType ExtHDU::xtension () const;
        \brief return the extension type

             allowed values are ImageHDU, AsciiTbl, and BinaryTbl

*/

/*! \fn      void ExtHDU::xtension (HduType value);
        \brief set the extension type

*/


/*! \fn      void ExtHDU::checkXtension ();
        \brief check that the extension type read is what was expected.

*/

/*! \fn   virtual long ExtHDU::getRowsize () const;
       \brief return the optimal number of rows to read or write at a time

       A wrapper for the CFITSIO function fits_get_rowsize, 
       useful for obtaining maximum I/O efficiency. This will throw
       if it is not called for a Table extension.
*/

 /*! \fn bool ExtHDU::isCompressed () const;
      \brief return true if image is stored using compression.

      This is simply a wrapper around the CFITSIO fits_is_compressed_image function.
      It will throw if this is not an Image extension.
 */



  class ExtHDU : public HDU  //## Inherits: <unnamed>%38048213E7A8
  {

    public:



      class WrongExtensionType : public FitsException  //## Inherits: <unnamed>%39E61E630349
      {
        public:
            WrongExtensionType (const String& msg, bool silent = true);

        protected:
        private:
        private: //## implementation
      };
        ExtHDU(const ExtHDU &right);
        virtual ~ExtHDU();
        friend bool operator<(const ExtHDU &left,const ExtHDU &right);

        friend bool operator>(const ExtHDU &left,const ExtHDU &right);

        friend bool operator<=(const ExtHDU &left,const ExtHDU &right);

        friend bool operator>=(const ExtHDU &left,const ExtHDU &right);

        static void readHduName (const fitsfile* fptr, int hduIndex, String& hduName, int& hduVersion);
        virtual void readData (bool readFlag = false, const std::vector<String>& keys = std::vector<String>()) = 0;
        const String& name () const;
        virtual HDU * clone (FITSBase* p) const = 0;
        //	By all means necessary, set the fitsfile pointer so that
        //	this HDU is the current HDU.
        //
        //	This would appear to be a good candidate for the public
        //	interface.
        virtual void makeThisCurrent () const;
        virtual Column& column (const String& colName, bool caseSensitive = true) const;
        virtual Column& column (int colIndex) const;
        virtual long rows () const;
        virtual void addColumn (ValueType type, const String& columnName, long repeatWidth, const String& colUnit = String(""), long decimals = -1, size_t columnNumber = 0);
        virtual void copyColumn(const Column& inColumn, int colIndx, bool insertNewCol=true);
        virtual void deleteColumn (const String& columnName);
        virtual long getRowsize () const;
        virtual int numCols () const;
        virtual const ColMap& column () const;

        bool isCompressed () const;
        int version () const;
        void version (int value);
        static const String& missHDU ();
        static void setMissHDU (const String& value);

    public:
      // Additional Public Declarations

      // interface is virtually identical to PHDU. The implementation is
      // similar apart from a check for wrong extension type.




      template <typename S>
      void write(const std::vector<long>& first,
                    long nElements,
                    const std::valarray<S>& data,
                    S* nullValue);


      template <typename S>
      void write(long first,
                    long nElements,
                    const std::valarray<S>& data,
                    S* nullValue);                

      template <typename S>
      void write(const std::vector<long>& first,
                    long nElements,
                    const std::valarray<S>& data);


      template <typename S>
      void write(long first,
                    long nElements,
                    const std::valarray<S>& data);

      template <typename S>
      void write(const std::vector<long>& firstVertex,
                    const std::vector<long>& lastVertex,
                    const std::valarray<S>& data);     

      // read image data & return the array. Can't return a reference because type
      // conversion in general requires allocating a new object.
      // note semantics of reading column data are easily distinguished: they require
      // the user to perform the operation EXT.column({name,index}).read(...)

      template <typename S>
      void read (std::valarray<S>& image) ; 

      template<typename S> 
      void read (std::valarray<S>& image, 
                      long first,
                      long nElements, 
                      S* nullValue) ; 

      template<typename S>
      void read (std::valarray<S>& image, 
                      const std::vector<long>& first, 
		      long nElements, 
                      S* nullValue) ; 

      template<typename S>
      void read (std::valarray<S>& image, 
                      const std::vector<long>& firstVertex, 
		      const std::vector<long>& lastVertex, 
		      const std::vector<long>& stride) ; 

      template<typename S>
      void read (std::valarray<S>& image, 
                      long first,
                      long nElements) ; 

      template<typename S>
      void read (std::valarray<S>& image, 
                      const std::vector<long>& first,
                      long nElements) ; 

      template<typename S>
      void read (std::valarray<S>& image, 
                      const std::vector<long>& firstVertex, 
		      const std::vector<long>& lastVertex, 
		      const std::vector<long>& stride, 
                      S* nullValue) ; 

    protected:
        //	ExtHDU needs a default constructor. This is it.
        ExtHDU (FITSBase* p, HduType xtype, const String &hduName, int version);
        //	The writing constructor. Forces the user to supply a name
        //	for the HDU
        ExtHDU (FITSBase* p, HduType xtype, const String &hduName, int bitpix, int naxis, const std::vector<long>& axes, int version);
        //	ExtHDU constructor for getting ExtHDUs by number.
        //	Necessary since EXTNAME is a reserved not required
        //	keyword.
        ExtHDU (FITSBase* p, HduType xtype, int number);

        virtual std::ostream & put (std::ostream &s) const = 0;
        virtual void setColumn (const String& colname, Column* value);
        virtual void checkExtensionType () const;
        int getVersion ();
        long pcount () const;
        void pcount (long value);
        long gcount () const;
        void gcount (long value);
        HduType xtension () const;
        void xtension (HduType value);

      // Additional Protected Declarations

    private:
        virtual void initRead () = 0;
        void checkXtension ();

      // Additional Private Declarations

    private: //## implementation
      // Data Members for Class Attributes
        long m_pcount;
        long m_gcount;
        int m_version;
        HduType m_xtension;
        static String s_missHDU;

      // Data Members for Associations
        String m_name;

      // Additional Implementation Declarations

  };

  // Class CCfits::ExtHDU::WrongExtensionType 

  // Class CCfits::ExtHDU 

  inline bool operator<(const ExtHDU &left,const ExtHDU &right)
  {
        if (left.m_name < right.m_name) return true; 
        if (left.m_name > right.m_name) return false;
        if (left.m_name == right.m_name)
        {
                if (left.m_version < right.m_version) return true;
        }      
        return false;       
  }

  inline bool operator>(const ExtHDU &left,const ExtHDU &right)
  {
     return !operator<=(left,right);
  }

  inline bool operator<=(const ExtHDU &left,const ExtHDU &right)
  {
        if (left.m_name <= right.m_name) 
        {
                if (left.m_version <= right.m_version) return true;
        }
        return false;     
  }

  inline bool operator>=(const ExtHDU &left,const ExtHDU &right)
  {
    return !operator<(left,right);    
  }


  inline const String& ExtHDU::name () const
  {

    return m_name;
  }

  inline long ExtHDU::pcount () const
  {
    return m_pcount;
  }

  inline void ExtHDU::pcount (long value)
  {
    m_pcount = value;
  }

  inline long ExtHDU::gcount () const
  {
    return m_gcount;
  }

  inline void ExtHDU::gcount (long value)
  {
    m_gcount = value;
  }

  inline int ExtHDU::version () const
  {
    return m_version;
  }

  inline void ExtHDU::version (int value)
  {
    m_version = value;
  }

  inline HduType ExtHDU::xtension () const
  {
    return m_xtension;
  }

  inline void ExtHDU::xtension (HduType value)
  {
    m_xtension = value;
  }

  inline const String& ExtHDU::missHDU ()
  {
    return s_missHDU;
  }

  inline void ExtHDU::setMissHDU (const String& value)
  {
    s_missHDU = value;
  }

} // namespace CCfits


#endif
