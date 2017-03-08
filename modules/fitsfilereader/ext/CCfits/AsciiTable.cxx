//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author:  Ben Dorman
#ifdef _MSC_VER
#include "MSconfig.h" // for truncation warning
#endif

// ColumnCreator
#include "ColumnCreator.h"
// Column
#include "Column.h"
// AsciiTable
#include "AsciiTable.h"
#ifdef SSTREAM_DEFECT
#include <strstream>
#else
#include <sstream>
#endif



namespace CCfits {

  // Class CCfits::AsciiTable 

  AsciiTable::AsciiTable(const AsciiTable &right)
      : Table(right)
  {
  }

  AsciiTable::AsciiTable (FITSBase* p, const String &hduName, bool readFlag, const std::vector<String>& keys, int version)
      : Table(p, AsciiTbl,hduName,version)
  {

  init(readFlag,keys);
  }

  AsciiTable::AsciiTable (FITSBase* p, const String &hduName, int rows, const std::vector<String>& columnName, const std::vector<String>& columnFmt, const std::vector<String>& columnUnit, int version)
      : Table(p, AsciiTbl , hduName,  rows,  columnName, columnFmt, columnUnit,  version)
  {
        long           width=0;
        int            decimals=0;
        int            status=0;
        int            colType=0;

        ColumnCreator create(this);


        for (int i=0; i< numCols(); i++)
        {

           status = fits_ascii_tform(const_cast<char *>(columnFmt[i].c_str()), 
		          &colType, &width, &decimals, &status);

           if (status != 0)  throw FitsError(status);
           Column *newCol = create.createColumn(i+1,ValueType(colType),
                   columnName[i], columnFmt[i], columnUnit[i],1,width);
           setColumn(columnName[i],newCol);

           newCol->setLimits(ValueType(colType));

        }
  }

  AsciiTable::AsciiTable (FITSBase* p, int number)
    : Table(p,AsciiTbl,number)
  {

  init();
  }


  AsciiTable::~AsciiTable()
  {
  }


  void AsciiTable::readTableHeader (int ncols, std::vector<String>& colName, std::vector<String>& colFmt, std::vector<String>& colUnit)
  {
   long  rowlen=0;
   long  *tbcol = new long[ncols];
   int   status=0;
   // hduName has already been set in ExtHDU so it is allocated and
   // read here only because cfitsio requires it.
   char  hduName[FLEN_KEYWORD];

   char** columnName = new char*[ncols];
   char** columnFmt  = new char*[ncols];
   char** columnUnit = new char*[ncols];

   int i = 0; // for MS VC++
   for(; i < ncols; i++)
   {
      columnName[i] = new char[FLEN_KEYWORD];
      columnFmt[i]  = new char[FLEN_KEYWORD];
      columnUnit[i] = new char[FLEN_KEYWORD];
   }

   long nRows = 0;
   int fields = 0;
   status = fits_read_atblhdr(fitsPointer(), ncols, &rowlen, &nRows, &fields,
			      columnName, tbcol, columnFmt, columnUnit,
			      hduName, &status);

   rows(nRows);
   numCols(fields);

   for( i = 0; i < ncols; i++)
   {
      colName[i] = String(columnName[i]);
      colFmt[i]  = String(columnFmt[i]);
      colUnit[i] = String(columnUnit[i]);
      delete [] columnName[i];
      delete [] columnFmt[i];
      delete [] columnUnit[i];
   }

   delete [] columnName;
   delete [] columnFmt;
   delete [] columnUnit;

   if (status != 0)  throw FitsError(status);

   delete [] tbcol;
  }

  AsciiTable * AsciiTable::clone (FITSBase* p) const
  {
  AsciiTable* cloned = new AsciiTable(*this);
  cloned->parent() = p;
  return cloned;
  }

  void AsciiTable::readData (bool readFlag, const std::vector<String>& keys)
  {
   int             rowsRead=0;
   int status = 0;
   long rowSize = 0;

   // grab the optimal rowsize using the get_rowsize call. It just
   // might have changed since the Table HDU was constructed so it should
   // be set just before reading takes place.

      if (fits_get_rowsize(fitsPointer(), &rowSize, &status))  throw FitsError(status);

   size_t keysRead = 0;
   size_t nkey=keys.size();
   ColMap::iterator endColumn = column().end();
  // if a set of strings were supplied, interpret these as
   // keywords and columns to be read.
   // get a container for keys which correspond to columns.
   std::vector<String> colKeys;
   colKeys.reserve(nkey);

   if (nkey > 0)
   {
        for (keysRead = 0; keysRead < nkey; keysRead++)
        {
                try
                {
                        // after the read function is called by the ctor,
                        // the columns in the table have been indexed.
                        // check that the key in question is not a column
                        // name. 
                        if (column().find(keys[keysRead]) == endColumn)   readKeyword(keys[keysRead]);
                        else colKeys.push_back(keys[keysRead]);
                }
                catch (HDU::NoSuchKeyword)
                {
                        continue;
                }
                catch (...)
                {
                        throw;
                }
        } 
   }  

   // if readFlag is false, don't get any data. 

   if (!readFlag) return;      

   // the data consist of scalars in each row. For an ASCII table,
   // rowSize is probably quite big because there's just one element per row.
   for (rowsRead=0; rowsRead< rows() ; rowsRead+=rowSize)
   {     
        ColMap::iterator col;
        if (colKeys.size() > 0)
        {
                for (size_t i=0; i < colKeys.size() ; i++)
                {
                        // if a set of keys was entered, read the data in the ones
                        // that correspond to columns, as checked earlier
                        col = column().find(colKeys[i]); 
                        Column&  current = *((*col).second);
                        current.readData(rowsRead+1,
                                        current.repeat()*std::min<size_t>(rowSize,rows()-rowsRead),1);
                }
        }
        else
        {
                // if no keys that correspond to column names were supplied, read all the data.
	        for(col = column().begin(); col != column().end(); col++ )
                {      
                        Column& current = *((*col).second);
                        current.readData(rowsRead+1,
                                          current.repeat()*std::min<size_t>(rowSize,rows() - rowsRead),1);

                }
         }
   }

   if (colKeys.size() ==  0)
   {
        // mark all columns read
        for (ColMap::iterator col = column().begin(); 
                        col != column().end(); ++col) 
        {
                (*col).second->isRead(true);
        }
   }
   else
   {
        for (size_t i=0; i < colKeys.size() ; i++)
        {
                // if a set of keys was entered, read the data in the ones
                // that correspond to columns, as checked earlier
                ColMap::iterator col = column().find(colKeys[i]); 
                (*col).second->isRead(true);
        }

   }
  }

  void AsciiTable::addColumn (ValueType type, const String& columnName, long repeatWidth, const String& colUnit, long decimals, size_t columnNumber)
  {
    String diag("");   

    // repeatWidth has the following semantics:

    // for non-string represents the multiplicity of entries in the column cell, and
    // is therefore always 1 for numeric datatypes.
    // for strings it represents the string width.
    // In cfitsio, the information about datatypes is carried in the width field,
    // but here this is carried by the type value.

    if (type < 0 ) 
    {
        diag += " writing vector-valued column to ASCII table: ";
        diag += name(); // name of ascii table extension 
        throw InvalidColumnSpecification(diag);      
    }    
    if (Tstring && repeatWidth < 1)
    {   
            diag += " length of string values unspecified for Column:  ";
            diag += columnName;
        throw InvalidColumnSpecification(diag);      

    }  
    if ( ( type == Tfloat || type == Tdouble ) )
    {
        if ( decimals < 0 || decimals > repeatWidth )
        {    
                diag += " invalid specification for floating point data format in Column  " ;
                diag += columnName;
                throw InvalidColumnSpecification(diag); 
        }    

    }
#ifdef SSTREAM_DEFECT
                std::ostrstream tformStr;
#else
                std::ostringstream tformStr;          
#endif



    switch (type)
    {
                case Tstring:
                        tformStr << 'A' << repeatWidth; 
                        break;
                case Tshort:
                case Tint:
                        tformStr << 'I' << 6;
                        break;
                case Tlong:
                        tformStr << 'I' << 12; 
                        break;
                case Tfloat:
                        if (repeatWidth <= 7)
                        {
                                tformStr << 'E' << repeatWidth << '.' << decimals;
                        }
                        else
                        {
                                tformStr << 'D' << repeatWidth << '.' << decimals;
                        }
                        break;
                case Tdouble:
                        tformStr << 'D' << repeatWidth << '.' << decimals;
                        break;                    
                default:
                        diag += "Invalid data type for ASCII table ";
                        diag += name();
                        throw WrongExtensionType(diag);
    }
# ifdef SSTREAM_DEFECT
        tformStr << std::ends;  
# endif
    makeThisCurrent();   
    // the user is allowed to specify which column number to create although
    // this is frowned down upon because the user shouldn't care how the data
    // are written internally.

    int colNum(0);
    int status(0);
    if ( columnNumber == 0)
    {

            if (fits_get_num_cols(fitsPointer(),&colNum,&status)) throw FitsError(status);
            colNum +=1;
    }
    else colNum = columnNumber;

    //string ColumnName(FITSUtil::upperCase(columnName));

    String tfString(tformStr.str());
    char* tform = const_cast<char*>(tfString.c_str());    


    char* ttype = const_cast<char*>(columnName.c_str());

    if (fits_insert_col(fitsPointer(),colNum,ttype,tform,&status)) throw FitsError(status);

    if (colUnit.size()) 
    {           
                std::ostringstream ustream;
                char unitComment[] = "";
                ustream << "TUNIT" << colNum;
                if (fits_write_key (fitsPointer(), Tstring,
                           const_cast<char*>(ustream.str().c_str()),
                           const_cast<char*>(colUnit.c_str()), 
                           unitComment, &status))
                {
                           throw FitsError (status);
                }

    }

    ColumnCreator create(this);
    Column *newCol=create.createColumn(colNum,type,columnName,tfString,
                      colUnit,1,repeatWidth);
    if (type != Tstring) newCol->setLimits(type);
    if (columnNumber != 0) reindex(columnNumber, true);
    setColumn(columnName,newCol);
  }

  // Additional Declarations

} // namespace CCfits
