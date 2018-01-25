//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman

#ifndef COLUMN_H
#define COLUMN_H 1
#include <iostream>

// CCfitsHeader
#include "CCfits.h"
// Table
#include "Table.h"
// FitsError
#include "FitsError.h"
// FITSUtil
#include "FITSUtil.h"

#include <complex>


namespace CCfits {



/*! \class Column

    \brief Abstract base class for Column objects. 

   Columns are the data containers used in FITS tables. Columns of scalar type
   (one entry per cell) are implemented by the template subclass ColumnData<T>.
   Columns of vector type (vector and variable rows) are implemented with the 
   template subclass ColumnVectorData<T>. AsciiTables may only contain Columns
   of type ColumnData<T>,  where T is an implemented FITS data type (see the
   CCfits.h header for a complete list. This requirement is enforced by ensuring
   that AsciiTable's addColumn method may only create an AsciiTable compatible
   column. The ColumnData<T> class stores its data in a std::vector<T> object.


   BinTables may contain either ColumnData<T> or ColumnVectorData<T>. 
   For ColumnVectorData, T must be a numeric type: string vectors are 
   handled by ColumnData<T>; string arrays are not supported. The internal
   representation of the data is a std::vector<std::valarray<T> > object.
   The std::valarray class is designed for efficient numeric processing and
   has many vectorized numeric and transcendental functions defined on it.

   Member template functions for read/write operations are provided in multiple
   overloads as the interface to data operations. Implicit data type conversions
   are supported but where they are required make the operations less efficient.
   Reading numeric column data as character arrays, supported by cfitsio, is
   not supported by CCfits.

   As a base class, Column provides protected accessor/mutator inline functions to 
   allow only its subclasses to access data members.

*/

/*! \class Column::RangeError
    @ingroup FITSexcept
    @brief exception to be thrown for inputs that cause range errors in column read operations.

    Range errors here mean  (last < first) in a request to read a range of rows.
*/



/*! \fn Column::RangeError::RangeError (const String& msg, bool silent);

        \brief Exception ctor, prefixes the string "FitsError: Range error in operation " before the specific message.

        \param msg A specific diagnostic message
        \param silent if true, print message whether FITS::verboseMode is set or not.

*/ 

/*! \class Column::InvalidDataType

        @ingroup FITSExcept
        @brief Exception thrown for invalid data type inputs

        This exception is thrown if the user requests an implicit data type conversion
        to a datatype that is not one of the supported types (see fitsio.h for details).
*/




/*! \fn Column::InvalidDataType::InvalidDataType (const String& str, bool silent);

        \brief Exception ctor, prefixes the string "FitsError: Incorrect data type:  " before the specific message.

        \param str A specific diagnostic message
        \param silent if true, print message whether FITS::verboseMode is set or not.

*/        


/*! \class Column::InvalidRowParameter

        @ingroup FITSExcept
        @brief Exception thrown on incorrect row writing request

        This exception is thrown if the user requests writing more data than a 
        fixed width row can accommodate. An exception is thrown rather than
        a truncation because it is likely that the user will not otherwise
        realize that data loss is happening.
*/


/*! \fn Column::InvalidRowParameter::InvalidRowParameter (const String& diag, bool silent);

        \brief Exception ctor, prefixes the string "FitsError: row offset or length incompatible with column declaration  " before the specific message.

        \param diag A specific diagnostic message, usually the column name
        \param silent if true, print message whether FITS::verboseMode is set or not.

*/        


/*! \class Column::WrongColumnType

        @ingroup FITSExcept
        @brief Exception thrown on attempting to access a scalar column as vector data.

        This exception will be thrown if the user tries to call a read/write operation
        with a signature appropriate for a vector column on a scalar column, or vice versa.
        For example in the case of write operations, 
        the vector versions require the specification of (a) a number of rows to write
        over, (b) a vector of lengths to write to each row or (c) a subset specification.
        The scalar versions only require a number of rows if the input array is a plain
        C-array, otherwise the range to be written is the size of the input vector.



*/

/*! \fn  Column::WrongColumnType::WrongColumnType (const String& diag, bool silent);

        \brief Exception ctor, prefixes the string "FitsError: Attempt to return scalar data from vector column, or vice versa - Column: " before the specific message.

        \param diag A specific diagnostic message, usually the column name.
        \param silent if true, print message whether FITS::verboseMode is set or not.

*/        


/*! \class Column::InvalidRowNumber

        @ingroup FITSExcept
        @brief Exception thrown on attempting to read a row number beyond the end of a table.

*/

/*! \fn Column::InvalidRowNumber::InvalidRowNumber (const String& diag, bool silent);

        \brief Exception ctor, prefixes the string "FitsError: Invalid Row Number - Column: " before the specific message.

        \param diag A specific diagnostic message, usually the column name.
        \param silent if true, print message whether FITS::verboseMode is set or not.

*/        



/*! \class Column::InsufficientElements

        @ingroup FITSExcept
        @brief Exception thrown if the data supplied for a write operation is less than declared.

        This circumstance generates an exception to avoid unexpected behaviour after
        the write operation is completed. It  can be avoided by resizing the input
        array appropriately.

*/

/*! \fn Column::InsufficientElements::InsufficientElements (const String& msg, bool silent);

        \brief Exception ctor, prefixes the string "FitsError: not enough elements supplied for write operation:  " before the specific message.

        \param msg A specific diagnostic message, usually the column name
        \param silent if true, print message whether FITS::verboseMode is set or not.

*/        


/*! \class Column::NoNullValue

        @ingroup FITSExcept
        @brief Exception thrown  if a null value is specified without support from existing column header.

        This exception is analogous to the fact that cfitsio returns a non-zero status code if
        TNULLn doesn't exist an a null value (convert all input data with the null value to the TNULLn keyword) is
        specified. It is only relevant for integer type data (see cfitsio manual for details).
*/

/*! \fn Column::NoNullValue::NoNullValue (const String& diag, bool silent);

        \brief Exception ctor, prefixes the string "Fits Error: No null value specified for column: " before the specific message.

        \param diag A specific diagnostic message
        \param silent if true, print message whether FITS::verboseMode is set or not.

*/        


/*! \class Column::InvalidNumberOfRows

        @ingroup FITSExcept
        @brief Exception thrown  if user enters a non-positive number for the number of rows to write.

*/


/*! \fn Column::InvalidNumberOfRows::InvalidNumberOfRows (int number, bool silent);

        \brief Exception ctor, prefixes the string "Fits Error: number of rows to write must be positive " before the specific message.

        \param number The number of rows entered.
        \param silent if true, print message whether FITS::verboseMode is set or not.

*/        

/*!  \fn Column::Column(const Column& right);

      \brief copy constructor, used in copying Columns to standard library containers.

      The copy constructor is for internal use only: it does not affect the disk
      fits file associated with the object.


*/


/*!  \fn virtual Column::~Column();

      \brief destructor.


*/

/*!   \fn  template <typename S> void Column::read(std::vector<S>& vals, long first, long last);

        \brief Retrieve data from a scalar column into a std::vector

        This and the following functions perform implicit data conversions. An
        exception will be thrown if no conversion exists.

        \param vals The output container. The function will resize this as necessary
        \param first,last the span of row numbers to read.

*/

/*!   \fn  template <typename S> void Column::read(std::vector<S>& vals, long first, long last, S* nullValue) ;

        \brief Retrieve data from a scalar column into a std::vector>, applying nullValue when relevant.

        If both <i>nullValue</i> and <i>*nullValue</i> are not 0, then any undefined 
        values in the file will be converted to <i>*nullValue</i> when copied into the
        <i>vals</i> vector.  See cfitsio documentation for further details

        \param vals The output container. The function will resize this as necessary
        \param first,last the span of row numbers to read.
        \param nullValue pointer to value to be applied to undefined elements.

*/
         /*!  \fn   template <typename S> void Column::read(std::valarray<S>& vals, long first, long last) ;

        \brief Retrieve data from a scalar column into a std::valarray

        \param vals The output container. The function will resize this as necessary
        \param first,last the span of row numbers to read.


*/    

/*!  \fn   template <typename S> void Column::read(std::valarray<S>& vals, long first, long last, S* nullValue) ;

        \brief Retrieve data from a scalar column into a std::valarray, applying nullValue when relevant.

        If both <i>nullValue</i> and <i>*nullValue</i> are not 0, then any undefined 
        values in the file will be converted to <i>*nullValue</i> when copied into the
        <i>vals</i> valarray.  See cfitsio documentation for further details

        \param vals The output container. The function will resize this as necessary
        \param first,last the span of row numbers to read.
        \param nullValue pointer to value to be applied to undefined elements.

*/

/*!  \fn template <typename S> void Column::read(std::valarray<S>& vals, long row) ;

        \brief return a single row of a vector column into a std::valarray

        \param vals The output valarray object
        \param row The row number to be retrieved (starting at 1).
*/

/*!  \fn template <typename S> void Column::read(std::vector<S>& vals, long row) ;

        \brief return a single row of a vector column into a std::vector

        \param vals The output vector object
        \param row The row number to be retrieved (starting at 1).
*/

/*!  \fn  template <typename S> void Column::read(std::valarray<S>& vals, long row, S* nullValue) ;

        \brief return a single row of a vector column into a std::valarray, setting undefined values

*/

/*!  \fn  template <typename S> void Column::read(std::valarray<S>& vals, long row, S* nullValue) ;

        \brief return a single row of a vector column into a std::vector, setting undefined values

*/

/*!  \fn    template <typename S> void Column::readArrays(std::vector<std::valarray<S> >& vals, long first,long last);

        \brief return a set of rows of a vector column into a vector of valarrays

        \param vals The output container. The function will resize this as necessary
        \param first,last the span of row numbers to read.
*/

/*!  \fn  template <typename S>  void Column::readArrays(std::vector<std::valarray<S> >& vals, long first, long last, S* nullValue);
        \brief return a set of rows of a vector column into a container, setting undefined values

        \param vals The output container. The function will resize this as necessary
        \param first,last the span of row numbers to read.
        \param nullValue pointer to integer value regarded as undefined

*/  


/*!   \fn  template <typename S> void Column::write(const std::vector<S>& indata, long firstRow);

      \brief write a vector of values into a scalar column starting with firstRow

      \param indata   The data to be written.
      \param firstRow The first row to be written
*/  

/*!   \fn  template <typename S> void Column::write(const std::valarray<S>& indata, long firstRow);

      \brief write a valarray of values into a scalar column starting with firstRow

      \param indata   The data to be written.
      \param firstRow The first row to be written
*/   

 /*!   \fn  template <typename S> void Column::write(const std::vector<S>& indata, long firstRow,  S* nullValue);

      \brief write a vector of values into a scalar column starting with firstRow, replacing elements equal to 
             *nullValue with the FITS null value.

      If <i>nullValue</i> is not 0, the appropriate FITS null value will be
      substituted for all elements of <i>indata</i> equal to <i>*nullValue</i>.
      For integer type columns there must be a pre-existing TNULLn keyword to
      define the FITS null value, otherwise a FitsError exception is thrown.
      For floating point columns, the FITS null is the IEEE NaN (Not-a-Number) value.
      See the cfitsio fits_write_colnull function documentation for more details.

      \param indata   The data to be written.
      \param firstRow The first row to be written
      \param nullValue Pointer to the value for which equivalent <i>indata</i> elements
                       will be replaced in the file with the appropriate FITS null value.
*/




 /*!   \fn  template <typename S> void Column::write(const std::valarray<S>& indata, long firstRow,  S* nullValue);

      \brief write a valarray of values into a scalar column starting with firstRow, replacing elements equal to
             *nullValue with the FITS null value.

      If <i>nullValue</i> is not 0, the appropriate FITS null value will be
      substituted for all elements of <i>indata</i> equal to <i>*nullValue</i>.
      For integer type columns there must be a pre-existing TNULLn keyword to
      define the FITS null value, otherwise a FitsError exception is thrown.
      For floating point columns, the FITS null is the IEEE NaN (Not-a-Number) value.
      See the cfitsio fits_write_colnull function documentation for more details.

      \param indata   The data to be written.
      \param firstRow The first row to be written
      \param nullValue Pointer to the value for which equivalent <i>indata</i> elements
                       will be replaced in the file with the appropriate FITS null value.
*/





/*!   \fn  template <typename S> void Column::write(S* indata, long nRows, long firstRow);

      \brief write a C array of size nRows into a scalar Column starting with row firstRow.

      \param indata   The data to be written.
      \param nRows    The size of the data array to be written
      \param firstRow The first row to be written


*/



/*!   \fn  template <typename S> void Column::write(S* indata, long nRows, long firstRow,  S* nullValue);

      \brief write a C array into a scalar Column, processing undefined values.

      \param indata   The data to be written.
      \param nRows    The size of the data array to be written
      \param firstRow The first row to be written
      \param nullValue Pointer to the value in the input array to be set to undefined values


*/

/*!   \fn   template <typename S>  void Column::write (const std::valarray<S>& indata, long nRows, long firstRow);

        \brief write a valarray of values into a range of rows of a vector column.

        The primary use of this is for fixed width columns, in which case Column's repeat attribute is 
        used to determine how many elements are written to each row; if indata.size() is too small an 
        exception will be thrown. If the column is variable width, the call will write indata.size()/nRows 
        elements to each row.

      \param indata   The data to be written.
      \param nRows    the number of rows to which to write the data.
      \param firstRow The first row to be written

*/


/*!   \fn   template <typename S>  void Column::write (const std::valarray<S>& indata, long nRows, long firstRow, S* nullValue);

        \brief write a valarray of values into a range of rows of a vector column.

        see version without undefined processing for details.

*/


/*!   \fn   template <typename S>  void Column::write (const std::vector<S>& indata, long nRows, long firstRow);

        \brief write a vector of values into a range of rows of a vector column

        The primary use of this is for fixed width columns, in which case Column's repeat attribute is 
        used to determine how many elements are written to each row; if indata.size() is too small an 
        exception will be thrown. If the column is variable width, the call will write indata.size()/nRows 
        elements to each row.

      \param indata   The data to be written.
      \param nRows    the number of rows to which to write the data.
      \param firstRow The first row to be written



*/

/*!   \fn   template <typename S>  void Column<S>::write (const std::vector<S>& indata, long nRows, long firstRow, S* nullValue);

        \brief write a vector of values into a range of rows of a vector column, processing undefined values 

         see version without undefined processing for details.

*/

/*!   \fn   template <typename S>  void Column::write (S* indata, long nElements, long nRows, long firstRow);

        \brief write a C array of values into a range of rows of a vector column

        Details are as for vector input; only difference is the need to supply the size of the C-array. 

      \param indata    The data to be written.
      \param nElements The size of indata
      \param nRows     the number of rows to which to write the data.
      \param firstRow  The first row to be written

*/

/*!   \fn   template <typename S> void Column::write (S* indata, long nElements, long nRows, long firstRow, S* nullValue);

        \brief write a C array of values into a range of rows of a vector column, processing undefined values.

         see version without undefined processing for details.

*/        

/*!   \fn   template <typename S>   void Column::write (const std::valarray<S>& indata,  
                        const std::vector<long>& vectorLengths, 
                        long firstRow);        

        \brief write a valarray of values into a column with specified number of entries written per row.

        Data are written into vectorLengths.size() rows, with vectorLength[n] elements written to
        row n+firstRow -1. Although primarily intended for wrapping calls to multiple variable-width
        vector column rows, it may also be used to write a variable number of elements to fixed-width
        column rows. 

        When writing to fixed-width column rows, if the number of elements sent to a particular row
        are fewer than the column's repeat value, the remaining elements in the row will not be modified.

        Since cfitsio does not support null value processing for variable width columns
        this function and its variants do not have version which process undefined values

        \param indata The data to be written
        \param vectorLengths the number of elements to write to each successive row.
        \param firstRow the first row to be written.        

*/

/*!   \fn   template <typename S> void Column::write (const std::vector<S>& indata, 
                        const std::vector<long>& vectorLengths, 
                        long firstRow);

        \brief write a vector of values into a column with specified number of entries written per row.

        Intended for writing a varying number of elements to multiple rows in a vector column, this
        may be used for either variable or fixed-width columns.  See the indata valarray version 
        of this function for a complete description.

*/

/*!   \fn   template <typename S>  void Column::write (S* indata, long nElements,  
                        const std::vector<long>& vectorLengths, 
                        long firstRow);

        \brief write a C-array of values of size nElements into a vector column with specified number of entries written per row.

        Intended for writing a varying number of elements to multiple rows in a vector column, this
        may be used for either variable or fixed-width columns.  See the indata valarray version 
        of this function for a complete description.

*/

/*!   \fn   template <typename S> void Column::writeArrays (const std::vector<std::valarray<S> >& indata, long firstRow); 

        \brief write a vector of valarray objects to the column, starting at row firstRow >= 1

        Intended for writing a varying number of elements to multiple rows in a vector column, this
        may be used for either variable or fixed-width columns.  When writing to fixed-width column rows, 
        if the number of elements sent to a particular row are fewer than the column's repeat value, the 
        remaining elements in the row will not be modified.

        \param indata The data to be written
        \param firstRow the first row to be written.        

*/

/*!   \fn   template <typename S>  void Column::writeArrays (const std::vector<std::valarray<S> >& indata, long firstRow, S* nullValue);  

        \brief write a vector of valarray objects to the column, starting at row firstRow >= 1, processing undefined values

         see version without undefined processing for details.

*/              


/*! \fn   template <typename T>   void Column::addNullValue(T nullVal);

          \brief Set the TNULLn keyword for the column

          Only relevant for integer valued columns, TNULLn is the value
          used by cfitsio in undefined processing. All entries in the
          table equal to an input "null value" are set equal to the value
          of TNULLn. (For floating point columns a system NaN value is used). 


*/

/*!  \fn  template <typename T>  bool Column::getNullValue(T* nullVal) const;

           \brief Get the value of the TNULLn keyword for the column

           Only relevant for integer valued columns.  If the TNULLn keyword
           is present, its value will be written to <i>*nullVal</i> and the
           function returns <i>true</i>.  If the keyword is not found or 
           its value is undefined, the function returns <i>false</i> and 
           <i>*nullVal</i> is not modified.

           \param nullVal  A pointer to the variable for storing the TNULLn value.
*/

/*!        \fn virtual void Column::readData (long firstRow = 1,  long nelements = 1, long firstElem = 1) = 0;

      \brief Read (or reread) data from the disk into the Column object's internal arrays.

      This function normally does not need to be called.  See the <b>resetRead</b>
      function for an alternative way of performing a reread from disk.  
      \param firstRow The first row to be read
      \param nelements The number of elements to read
      \param firstElem The number of the element on the first row to start at 
              (ignored for scalar columns)


*/



/*!        \fn void Column::setDisplay ();

      \brief set the TDISPn keyword


*/

/*!        \fn void Column::rows() const;

      \brief return the number of rows in the table.

*/


/*!   \fn virtual void Column::setDimen ();

      \brief set the TDIMn keyword.


*/

/*!   \fn std::ostream& operator <<(std::ostream& s, const Column& right);

      \brief output operator for Column objects. 
*/


/*!        \fn int Column::index () const;

      \brief get the Column index (the n in TTYPEn etc).



*/


/*!         \fn size_t Column::repeat () const;

      \brief get the repeat count for the rows



*/

/*!         \fn const bool Column::varLength () const;

      \brief boolean, set to true if Column has variable length vector rows.



*/


/*!        \fn const double Column::scale () const;

      \brief get TSCALn value

      TSCALn is  used to convert a data array represented
      on disk in integer format as floating. Useful for compact storage of digitized
      data.


*/


/*!        \fn const double Column::zero () const;

      \brief get TZEROn value

      TZEROn is an integer offset used in the implementation of unsigned data



*/



/*!        \fn const String& Column::name () const;

      \brief  return  name of Column (TTYPEn keyword)



*/


/*!        \fn const String& Column::unit () const;

      \brief get units of data in Column (TUNITn keyword)
*/


/*!        \fn Column::Column (int columnIndex, const String &columnName,const ValueType type, 
                const String &format,
                const String &unit,
                Table* p,
                int  rpt = 1, 
                long w = 1, 
                const String &comment = "");

      \brief new column creation constructor

      This constructor allows the specification of:

      \param columnIndex The column number
      \param columnName The column name, keyword TTYPEn
      \param type     used for determining class of T in  ColumnData<T>, ColumnVectorData<T>
      \param format   the column data format, TFORMn keyword
      \param unit   the column data unit, TUNITn keyword
      \param p      the Table pointer
      \param rpt    (optional) repeat count for the row ( == 1 for AsciiTables)
      \param w      the row width
      \param comment comment to be added to the header.

*/

/*!        \fn Column::Column (Table* p = 0);

      \brief Simple constructor to be called by subclass reading ctors.

*/



/*!        \fn fitsfile* Column::fitsPointer ();

      \brief fits pointer corresponding to fits file containing column data.



*/

/*!        \fn void Column::makeHDUCurrent ();

      \brief make HDU containing this the current HDU of the fits file.



*/

/*!        \fn int Column::rows () const;

      \brief return number of rows in the Column




*/

/*!        \fn virtual std::ostream& Column::put (std::ostream& s) const;

      \brief internal implementation of << operator.



*/

/*!        \fn const ValueType  Column::type () const;

      \brief returns the data type of the column



*/


/*!        \fn  const bool Column::isRead () const;

      \brief flag set to true if the entire column data has been read from disk

*/

/*!    \fn void Column::resetRead();

      \brief reset the Column's isRead flag to false

      This forces the data to be reread from the disk the next time a
      read command is called on the Column, rather than simply retrieving the
      data already stored in the Column object's internal arrays.  This
      may be useful for example if trying to reread a Column using a 
      different nullValue argument than for an earlier read.
*/

/*!        \fn long Column::width () const;

      \brief return column data width



*/      


/*!        \fn String& Column::display () const;

      \brief return TDISPn keyword

      TDISPn  is suggested format for output of column data.



*/

/*!        \fn String& Column::dimen () const;

      \brief return TDIMn keyword

      represents dimensions of data arrays in vector columns.
      for scalar columns, returns a default value.



*/

/*!        \fn const String& Column::format () const;

      \brief return TFORMn keyword

      TFORMn specifies data format stored in disk file.


*/


/*!        \fn const String& Column::comment () const;

      \brief retrieve comment for Column

*/

/*!   \fn Table* Column::parent() const;
     \brief return a pointer to the Table which owns this Column

*/



  class Column 
  {

    public:



      class RangeError : public FitsException  //## Inherits: <unnamed>%3946526D031A
      {
        public:
            RangeError (const String& msg, bool silent = true);

        protected:
        private:
        private: //## implementation
      };



      class InvalidDataType : public FitsException  //## Inherits: <unnamed>%3947CF30033E
      {
        public:
            InvalidDataType (const String& str = string(), bool silent = true);

        protected:
        private:
        private: //## implementation
      };



      class InvalidRowParameter : public FitsException  //## Inherits: <unnamed>%39B5310F01A0
      {
        public:
            InvalidRowParameter (const String& diag, bool silent = true);

        protected:
        private:
        private: //## implementation
      };



      class WrongColumnType : public FitsException  //## Inherits: <unnamed>%39B545780082
      {
        public:
            WrongColumnType (const String& diag, bool silent = true);

        protected:
        private:
        private: //## implementation
      };



      class UnspecifiedLengths : public FitsException  //## Inherits: <unnamed>%3A018C9D007D
      {
        public:
            UnspecifiedLengths (const String& diag, bool silent = true);

        protected:
        private:
        private: //## implementation
      };



      class InvalidRowNumber : public FitsException  //## Inherits: <unnamed>%3B0A850F0307
      {
        public:
            InvalidRowNumber (const String& diag, bool silent = true);

        protected:
        private:
        private: //## implementation
      };



      class InsufficientElements : public FitsException  //## Inherits: <unnamed>%3B0BE611010A
      {
        public:
            InsufficientElements (const String& msg, bool silent = true);

        protected:
        private:
        private: //## implementation
      };



      class NoNullValue : public FitsException  //## Inherits: <unnamed>%3B0D589A0092
      {
        public:
            NoNullValue (const String& diag, bool silent = true);

        protected:
        private:
        private: //## implementation
      };



      class InvalidNumberOfRows : public FitsException  //## Inherits: <unnamed>%3B20EB8B0205
      {
        public:
            InvalidNumberOfRows (int number, bool silent = true);

        protected:
        private:
        private: //## implementation
      };
        Column(const Column &right);
        virtual ~Column();
        bool operator==(const Column &right) const;

        bool operator!=(const Column &right) const;

        virtual void readData (long firstRow, long nelements, long firstElem = 1) = 0;
        //	Virtual copy constructor.
        virtual Column * clone () const = 0;
        int rows () const;
        void setDisplay ();
        virtual void setDimen ();
        friend std::ostream& operator << (std::ostream& s, const Column& right);
        
        // The parent SET function is needed by Table classes, but 
        //  should not be part of the user interface.  It is deliberately left
        //   out of the document section up top.
        Table* parent () const;
        void setParent(Table* parent);
        
        //	Inequality operators for imposing sort order on columns.
        friend bool operator < (const Column& left, const Column& right);
        //	Inequality operators for imposing sort order on columns.
        friend bool operator > (const Column& left, const Column& right);
        void setLimits (ValueType type);
        void unit (const String& value);
        void resetRead ();
        int index () const;
        void index (int value);
        bool isRead () const;
        void isRead (bool value);
        long width () const;
        void width (long value);
        size_t repeat () const;
        bool varLength () const;
        double scale () const;
        void scale (double value);
        double zero () const;
        void zero (double value);
        const String& display () const;
        const String& dimen () const;
        void dimen (const String& value);
        ValueType type () const;
        void type (ValueType value);
        static const String& TFORM ();
        static const String& TDISP ();
        static const String& TSCAL ();
        static const String& TZERO ();
        static const String& TDIM ();
        const String& format () const;
        const String& unit () const;
        const String& name () const;

    public:
      // Additional Public Declarations

        // scalar column interface. Column's Data Member is a std::vector<T>,
        // input data is std::vector<S>, std::valarray<S> or S* where S is not
        // in general the same as T.

        template <typename S>                   
        void write (const std::vector<S>& indata, long firstRow);

        void write (const std::vector<std::complex<float> >& indata, long firstRow);

        void write (const std::vector<std::complex<double> >& indata, long firstRow);

        template <typename S>                   
        void write (const std::valarray<S>& indata, long firstRow);

        void write (const std::valarray<std::complex<float> >& indata, long firstRow);

        void write (const std::valarray<std::complex<double> >& indata, long firstRow);

        template <typename S>                   
        void write (S* indata, long nRows, long firstRow);


        template <typename S>                   
        void write (const std::vector<S>& indata, long firstRow, S* nullValue);

        template <typename S>                   
        void write (const std::valarray<S>& indata, long firstRow, S* nullValue);

        template <typename S>                   
        void write (S* indata, long nRows, long firstRow, S* nullValue);        
        // vector column interface. We provide an interface that allows input of a vector, valarray and C-array.
	// there are versions that write variable numbers of elements per row as specified
        // in the vectorLengths argument. The user also can directly write a vector<valarray<T> >
        // object which is how the data are stored in the ColumnVectorData object. 
        // this last one is also used internally to implement the variable lengths versions.

        // fixed length write to binary table from valarray.


        template <typename S>
        void write (const std::valarray<S>& indata, long nRows, long firstRow);

        void write (const std::valarray<std::complex<float> >& indata, long nRows, long firstRow);

        void write (const std::valarray<std::complex<double> >& indata, long nRows, long firstRow);


        template <typename S>
        void write (const std::vector<S>& indata, long nRows, long firstRow);

        void write (const std::vector<std::complex<float> >& indata, long nRows, long firstRow);

        void write (const std::vector<std::complex<double> >& indata, long nRows, long firstRow);



        template <typename S>
        void write (S* indata, long nElements, long nRows, long firstRow);


        template <typename S>
        void write (const std::valarray<S>& indata, long nRows, long firstRow, S* nullValue);


        template <typename S>
        void write (const std::vector<S>& indata, long nRows, long firstRow, S* nullValue);

        template <typename S>
        void write (S* indata, long nElements, long nRows, long firstRow, S* nullValue);

        // variable-length write to vector column from valarray or vector.

        template <typename S>
        void write (const std::valarray<S>& indata,  
                        const std::vector<long>& vectorLengths, 
                        long firstRow);        

        void write (const std::valarray<std::complex<float> >& indata,  
                        const std::vector<long>& vectorLengths, 
                        long firstRow);        

        void write (const std::valarray<std::complex<double> >& indata,  
                        const std::vector<long>& vectorLengths, 
                        long firstRow);      

        template <typename S>
        void write (const std::vector<S>& indata, 
                        const std::vector<long>& vectorLengths, 
                        long firstRow);



        void write (const std::vector<std::complex<float> >& indata, 
                        const std::vector<long>& vectorLengths, 
                        long firstRow);

        void write (const std::vector<std::complex<double> >& indata, 
                        const std::vector<long>& vectorLengths, 
                        long firstRow);

        template <typename S>
        void write (S* indata, long nElements,  
                        const std::vector<long>& vectorLengths, 
                        long firstRow);

        template <typename S>
        void writeArrays (const std::vector<std::valarray<S> >& indata, long firstRow); 

        void writeArrays (const std::vector<std::valarray<std::complex<float> > >& indata, long firstRow); 

        void writeArrays (const std::vector<std::valarray<std::complex<double> > >& indata, long firstRow); 

	template <typename S>
        void writeArrays (const std::vector<std::valarray<S> >& indata, long firstRow, S* nullValue);  

        // get specified elements of a scalar column, returned as a std::vector
        // S is NOT the type of the column data itself, it is the type of the returned
        // data.

        template <typename S>
        void read(std::vector<S>& vals, long first, long last) ;

        // VC++, at least, won't compile these as template covering std::complex instances.
        void read(std::vector< std::complex<float> >& , long first, long last);

        void read(std::vector< std::complex<double> >& , long first, long last);

        void read(std::vector<String>& vals, long first, long last);

        // return a set of rows from a scalar column as a valarray.
        template <typename S>
        void read(std::valarray<S>& vals, long first, long last) ;

        void read(std::valarray<std::complex<float> >& vals, long first, long last) ;

        void read(std::valarray<std::complex<double> >& vals, long first, long last) ;

        // return a single  row of a vector column.
        template <typename S>
        void read(std::valarray<S>& vals, long rows) ;
        template <typename S>
        void read(std::vector<S>& vals, long rows);
        void read(std::valarray<std::complex<float> >& vals, long rows) ;
        void read(std::valarray<std::complex<double> >& vals, long rows) ;
        void read(std::vector<std::complex<float> >& vals, long rows) ;
        void read(std::vector<std::complex<double> >& vals, long rows) ;

        // get a set of rows from a vector column.
        template <typename S>
        void readArrays(std::vector<std::valarray<S> >& vals, long first, long last) ;

        void readArrays(std::vector<std::valarray<std::complex<float> > >& vals, long first, long last) ;

        void readArrays(std::vector<std::valarray<std::complex<double> > >& vals, long first, long last) ;

        // nullValue has no meaning when the target column has floating point/std::complex
        // type. Also, implict conversion of std::complex to pure real is not supported
        // by cfitsio.

        template <typename S>
        void read(std::vector<S>& vals, long first, long last, S* nullValue) ;

        // return a set of rows from a scalar column as a valarray.
        template <typename S>
        void read(std::valarray<S>& vals, long first, long last, S* nullValue);

        // return a single  row of a vector column.
        template <typename S>
        void read(std::valarray<S>& vals, long rows, S* nullValue) ;

        template <typename S>
        void read(std::vector<S>& vals, long rows, S* nullValue) ;
        
        // get a set of rows from a vector column.
        template <typename S>
        void readArrays(std::vector<std::valarray<S> >& vals, long first, long last, S* nullValue);

        // add a null value to the column 
        template <typename T>
        void addNullValue(T nullVal);

        // get the TNULL setting
        template <typename T>
        bool getNullValue(T* nullVal) const;

        void write (const std::vector<String>& indata, long firstRow);

        friend void Table::insertRows(long first, long number);

        friend void Table::deleteRows(long first, long number);

        friend void Table::deleteRows(const std::vector<long>& rowList);

        friend void Table::initRead();

        friend void Table::reindex(int startNum, bool isInsert);

    protected:
        Column (int columnIndex, 	// The column index, i.e. the integer n in the keyword TCOLn
        const String &columnName, 	// The column name, curiously TTYPEn
        ValueType type, const String &format, 	// The TFORMn keyword.
        const String &unit, 	// The TUNITn keyword
        Table* p, 	// ! The Table containing the Column object
        int rpt = 1, long w = 1, const String &comment = "");
        Column (Table* p = 0);

        virtual bool compare (const Column &right) const;
        fitsfile* fitsPointer ();
        //	Protected method to set the current HDU to be the one containing this Column object. For use in
        //	public read/write methods to ensure that data regarding numbers of rows and width relate to the
        //	right HDU
        void makeHDUCurrent ();
        virtual std::ostream& put (std::ostream& s) const;
        void varLength (bool value);
        static const String& TBCOL ();
        static const String& TTYPE ();
        static const String& TUNIT ();
        static const String& TNULL ();
        static const String& TLMIN ();
        static const String& TLMAX ();
        static const String& TDMAX ();
        static const String& TDMIN ();
        static const std::vector<String>& columnKeys ();
        const String& comment () const;

      // Additional Protected Declarations

    private:
        Column & operator=(const Column &right);

        //	Insert one or more blank rows into a FITS column.
        virtual void insertRows (long first, long number = 1) = 0;
        virtual void deleteRows (long first, long number = 1) = 0;
        static void loadColumnKeys ();
        void name (const String& value);
        void format (const String& value);
        long numberOfElements (long& first, long& last);

      // Data Members for Class Attributes
        static const String s_TBCOL;
        static const String s_TTYPE;
        static const String s_TFORM;
        static const String s_TDISP;
        static const String s_TUNIT;
        static const String s_TSCAL;
        static const String s_TZERO;
        static const String s_TDIM;
        static const String s_TNULL;
        static const String s_TLMIN;
        static const String s_TLMAX;
        static const String s_TDMAX;
        static const String s_TDMIN;

      // Additional Private Declarations

    private: //## implementation
      // Data Members for Class Attributes
        int m_index;
        bool m_isRead;
        long m_width;
        size_t m_repeat;
        bool m_varLength;
        double m_scale;
        double m_zero;
        String m_display;
        String m_dimen;
        ValueType m_type;
        static const short LLIMITSHORT;
        static const long LLIMITLONG;
        static const unsigned short LLIMITUSHORT;
        static const unsigned long LLIMITULONG;
        static const unsigned char LLIMITUCHAR;
        static const float LLIMITFLOAT;
        static const double LLIMITDOUBLE;
        static const short ULIMITSHORT;
        static const long ULIMITLONG;
        static const unsigned short ULIMITUSHORT;
        static const unsigned long ULIMITULONG;
        static const unsigned char ULIMITUCHAR;
        static const float ULIMITFLOAT;
        static const double ULIMITDOUBLE;
        static const int LLIMITINT;
        static const int ULIMITINT;
        static const unsigned int LLIMITUINT;
        static const unsigned int ULIMITUINT;
        static const LONGLONG LLIMITLONGLONG;
        static const LONGLONG ULIMITLONGLONG;

      // Data Members for Associations
        Table* m_parent;
        static std::vector<String> s_columnKeys;
        String m_comment;
        String m_format;
        String m_unit;
        String m_name;

      // Additional Implementation Declarations

  };

  // Class CCfits::Column::RangeError 

  // Class CCfits::Column::InvalidDataType 

  // Class CCfits::Column::InvalidRowParameter 

  // Class CCfits::Column::WrongColumnType 

  // Class CCfits::Column::UnspecifiedLengths 

  // Class CCfits::Column::InvalidRowNumber 

  // Class CCfits::Column::InsufficientElements 

  // Class CCfits::Column::NoNullValue 

  // Class CCfits::Column::InvalidNumberOfRows 

  // Class CCfits::Column 

  inline void Column::setDimen ()
  {
  // default implementation: do nothing. Overridden by ColumnVectorData.
  }

  inline std::ostream& operator << (std::ostream& s, const Column& right)
  {

    return right.put(s);
  }

  inline bool operator < (const Column& left, const Column& right)
  {

    return left.m_index < right.m_index;
  }

  inline bool operator > (const Column& left, const Column& right)
  {

    return left.m_index > right.m_index;
  }

  inline void Column::resetRead ()
  {
     m_isRead = false;
  }

  inline int Column::index () const
  {
    return m_index;
  }

  inline void Column::index (int value)
  {
    m_index = value;
  }

  inline bool Column::isRead () const
  {
    return m_isRead;
  }

  inline void Column::isRead (bool value)
  {
    m_isRead = value;
  }

  inline long Column::width () const
  {
    return m_width;
  }

  inline void Column::width (long value)
  {
    m_width = value;
  }

  inline size_t Column::repeat () const
  {
    return m_repeat;
  }

  inline bool Column::varLength () const
  {
    return m_varLength;
  }

  inline double Column::scale () const
  {
    return m_scale;
  }

  inline void Column::scale (double value)
  {
    m_scale = value;
    int status(0);
    if (fits_set_tscale(fitsPointer(),m_index,value,m_zero,&status)) throw FitsError(status);
  }

  inline double Column::zero () const
  {
    return m_zero;
  }

  inline void Column::zero (double value)
  {
    m_zero = value;
  }

  inline const String& Column::display () const
  {
    return m_display;
  }

  inline const String& Column::dimen () const
  {
    return m_dimen;
  }

  inline void Column::dimen (const String& value)
  {
    m_dimen = value;
  }

  inline ValueType Column::type () const
  {
    return m_type;
  }

  inline void Column::type (ValueType value)
  {
    m_type = value;
  }

  inline const String& Column::TBCOL ()
  {
    return s_TBCOL;
  }

  inline const String& Column::TTYPE ()
  {
    return s_TTYPE;
  }

  inline const String& Column::TFORM ()
  {
    return s_TFORM;
  }

  inline const String& Column::TDISP ()
  {
    return s_TDISP;
  }

  inline const String& Column::TUNIT ()
  {
    return s_TUNIT;
  }

  inline const String& Column::TSCAL ()
  {
    return s_TSCAL;
  }

  inline const String& Column::TZERO ()
  {
    return s_TZERO;
  }

  inline const String& Column::TDIM ()
  {
    return s_TDIM;
  }

  inline const String& Column::TNULL ()
  {
    return s_TNULL;
  }

  inline const String& Column::TLMIN ()
  {
    return s_TLMIN;
  }

  inline const String& Column::TLMAX ()
  {
    return s_TLMAX;
  }

  inline const String& Column::TDMAX ()
  {
    return s_TDMAX;
  }

  inline const String& Column::TDMIN ()
  {
    return s_TDMIN;
  }

  inline const std::vector<String>& Column::columnKeys ()
  {
    return s_columnKeys;
  }

  inline const String& Column::comment () const
  {
    return m_comment;
  }

  inline const String& Column::format () const
  {
    return m_format;
  }

  inline const String& Column::unit () const
  {
    return m_unit;
  }

  inline const String& Column::name () const
  {
    return m_name;
  }

} // namespace CCfits


#endif
