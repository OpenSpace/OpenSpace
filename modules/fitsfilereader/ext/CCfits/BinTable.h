//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman

#ifndef BINTABLE_H
#define BINTABLE_H 1

// HDUCreator
#include "HDUCreator.h"
// Table
#include "Table.h"

// needed for CLONE_DEFECT
#ifdef _MSC_VER
#include "MSconfig.h"
#endif


namespace CCfits {

/*!  \class BinTable

     \brief Class Representing Binary Table Extensions. Contains columns with scalar or vector row entries


     BinTable (re)implements functions prescribed in the Table abstract class.
     The implementations allow the calling of cfitsio specialized routines for
     BinTable header construction. functions particular to the BinTable class
     include those dealing with variable width columns

     Direct instantiation of BinTable objects is disallowed: they are created
     by explicit calls to FITS::addTable( ... ), FITS::read(...) or internally
     by one of the FITS ctors on initialization. For addTable, creation of 
     BinTables is the default.

*/




/*! \fn BinTable::~BinTable();
    \brief destructor.    
*/


/*!  \fn     virtual void BinTable::readData (bool readFlag = false, const std::vector<String>& keys = std::vector<String>());

    \brief read columns and keys specified in the input array.

    See Table class documentation for further details.
*/

/*!  \fn      virtual void BinTable::addColumn (ValueType type, const String& columnName, long repeatWidth, const String& colUnit = String(""), long decimals = 0, size_t  columnNumber = 0);

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

/*! \fn     BinTable::BinTable (FITSBase* p, const String &hduName, bool readFlag, const std::vector<String>& keys, int version);

    \brief  reading constructor. 

    Construct a BinTable extension from an extension of an existing disk file.
    The Table is specified by name and optional version number within the file.
    An array of strings representing columns or keys indicates which data are to
    be read. The column data are only  read if readFlag is true. Reading on construction
    is optimized, so it is more efficient to read data at the point of instantiation.
    This favours a "resource acquisition is initialization" model of data management.

    \param p        Pointer to FITSBase class, an internal implementation detail
    \param hduName  name of BinTable object to be read.
    \param readFlag flag to determine whether to read data on construction
    \param keys     (optional) a list of keywords/columns to be read. The implementation
                     will determine which are keywords. If none are specified, the 
                     constructor will simply read the  header
    \param version (optional) version number. If not specified, will read the first
                    extension that matches hduName.
*/

/*!  \fn   BinTable::BinTable (FITSBase* p, const String &hduName, int rows,  const std::vector<String>& columnName, const std::vector<String>& columnFmt, const std::vector<String>& columnUnit, int version);

    \brief writing constructor

    The constructor creates a valid HDU which is ready for Column::write or
    insertRows operations. The disk FITS file is update accordingly. The data type
    of each column is determined by the columnFmt argument (TFORM keywords). See
    cfitsio documentation for acceptable values.

    \param p        Pointer to FITSBase class, an internal implementation detail
    \param hduName  name of BinTable object to be written
    \param rows     number of rows in the table (NAXIS2)    
    \param columnName     array of column names for columns to be constructed.
    \param columnFmt      array of column formats for columns to be constructed.
    \param columnUnit     (optional)  array of units for data in columns.
    \param version        (optional) version number for HDU.

    The dimensions of columnType, columnName and columnFmt must match, but this
    is not enforced.


*/

/*!  \fn    BinTable::BinTable (FITSBase* p, int number);

    \brief read BinTable with HDU number \p number from existing file represented by fitsfile pointer p.
*/





  class BinTable : public Table  //## Inherits: <unnamed>%3804A7E75F10
  {

    public:
        virtual BinTable * clone (FITSBase* p) const;
        virtual void readData (bool readFlag = false, const std::vector<String>& keys = std::vector<String>());
        virtual void addColumn (ValueType type, const String& columnName, long repeatWidth, const String& colUnit = String(""), long decimals = 0, size_t columnNumber = 0);

      // Additional Public Declarations

    protected:
        BinTable (FITSBase* p, const String &hduName = String(""), bool readFlag = false, const std::vector<String>& keys = std::vector<String>(), int version = 1);
        BinTable (FITSBase* p, const String &hduName, int rows, const std::vector<String>& columnName = std::vector<String>(), const std::vector<String>& columnFmt = std::vector<String>(), const std::vector<String>& columnUnit = std::vector<String>(), int version = 1);
        //	ExtHDU constructor for getting ExtHDUs by number.
        //	Necessary since EXTNAME is a reserved not required
        //	keyword.
        BinTable (FITSBase* p, int number);
        ~BinTable();

      // Additional Protected Declarations

    private:
        BinTable(const BinTable &right);

        virtual void readTableHeader (int ncols, std::vector<String>& colName, std::vector<String>& colFmt, std::vector<String>& colUnit);
        void readVariableColumns (const std::vector<String> &varColumns);

      // Additional Private Declarations

    private: //## implementation
      // Additional Implementation Declarations
      friend class HDUCreator;
  };

  // Class CCfits::BinTable 

} // namespace CCfits


#endif
