//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman

// ColumnCreator
#include "ColumnCreator.h"
// Column
#include "Column.h"
// BinTable
#include "BinTable.h"
#ifdef SSTREAM_DEFECT
#include <strstream>
#else
#include <sstream>
#endif
#include <utility>

namespace CCfits {

  // Class CCfits::BinTable 

  BinTable::BinTable(const BinTable &right)
      : Table(right)
  {
  }

  BinTable::BinTable (FITSBase* p, const String &hduName, bool readFlag, const std::vector<String>& keys, int version)
      : Table(p, BinaryTbl, hduName, version)
  {

  init(readFlag,keys);
  }

  BinTable::BinTable (FITSBase* p, const String &hduName, int rows, const std::vector<String>& columnName, const std::vector<String>& columnFmt, const std::vector<String>& columnUnit, int version)
      : Table(p, BinaryTbl, hduName, rows,  columnName, columnFmt, columnUnit, version)
  {
  long           repeat=0;
  long           width=0;
  int            status=0;
  int            colType=0;

  ColumnCreator create(this);

  for (int i=0; i < numCols(); i++)
  {
     status = fits_binary_tform(const_cast<char *>(columnFmt[i].c_str()), 
				&colType, &repeat, &width, &status);

     string unitString("");
     if (i < static_cast<int>(columnUnit.size()))
        unitString = columnUnit[i];
     Column *newCol = create.createColumn(i+1, ValueType(colType),
                                columnName[i], columnFmt[i], 
                                unitString,repeat,width);
     setColumn(columnName[i], newCol);
     newCol->setLimits(ValueType(colType));

  }
  }

  BinTable::BinTable (FITSBase* p, int number)
        : Table(p,BinaryTbl,number)
  {
  init();
  }


  BinTable::~BinTable()
  {
  }


  void BinTable::readTableHeader (int ncols, std::vector<String>& colName, std::vector<String>& colFmt, std::vector<String>& colUnit)
  {
   int   status=0;
   char  hduName[FLEN_KEYWORD];

   char** columnName = new char*[ncols];
   char** columnFmt  = new char*[ncols];
   char** columnUnit = new char*[ncols];
   int i = 0; // for MS VC++  
   for( ; i < ncols; i++)
   {
      columnName[i] = new char[FLEN_KEYWORD];
      columnFmt[i]  = new char[FLEN_KEYWORD];
      columnUnit[i] = new char[FLEN_KEYWORD];
   }

   long pct = 0;
   long nr  = 0;
   int tf  = 0;
   status = fits_read_btblhdr(fitsPointer(), ncols, &nr, &tf, columnName,
			      columnFmt, columnUnit, hduName, &pct,&status);
   pcount(pct);
   rows(nr);
   numCols(tf);

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

   // throw the exception after the garbage has been collected.

   if (status != 0)  throw FitsError(status);
  }

  BinTable * BinTable::clone (FITSBase* p) const
  {
  BinTable* cloned = new BinTable(*this);
  cloned->parent() = p;
  return cloned;
  }

  void BinTable::readData (bool readFlag, const std::vector<String>& keys)
  {
   int             rowsRead=0;
   std::vector<String>     varCols;
   int status = 0;
   long rowSize = 0;

   // grab the optimal rowsize using the get_rowsize call. It just
   // might have changed since the Table HDU was constructed so it should
   // be set just before reading takes place.
   if (fits_get_rowsize(fitsPointer(), &rowSize, &status))  throw FitsError(status);

   // if a set of strings were supplied, interpret these as
   // keywords and columns to be read.

   ColMap::iterator endColumn = column().end();
   size_t keysRead = 0;
   size_t nkey=keys.size();

   // get a container for keys which correspond to columns.
   std::vector<String> colKeys;
   colKeys.reserve(nkey);


   if (nkey > 0)
   {
       // first, look for keywords if strings are supplied and read them.
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
	                // if the column is not of variable repeat count... 
                        if (!current.varLength())
                        {
                             current.readData(rowsRead+1,
                                             current.repeat()*std::min<size_t>(rowSize,rows()-rowsRead),1);
                        }
                        else
                        {
                        // store the column numbers of variable columns for later.
                             varCols.push_back(current.name());
                        }
                }
        }
        else
        {
                // if no keys that correspond to column names were supplied, read all the data.
	        for(col = column().begin(); col != column().end(); col++ )
                {      
                        Column& current = *(*col).second;
	                if (!current.varLength()) 
                        { 
                                current.readData(rowsRead+1,
                                          current.repeat()*std::min<size_t>(rowSize,rows() - rowsRead),1);
                        }
                        else
                        {
                                varCols.push_back(current.name());
                        }

                }
         }
   }

   // if successful, mark read columns.

   if (colKeys.size() ==  0)
   {
        // mark all columns read
        for (ColMap::iterator col = column().begin(); 
                col != column().end(); ++col) 
        {
                if (!(*col).second->varLength()) (*col).second->isRead(true);
        }
   }
   else
   {
        for (size_t i=0; i < colKeys.size() ; i++)
        {
                // if a set of keys was entered, read the data in the ones
                // that correspond to columns, as checked earlier.
                ColMap::iterator col = column().find(colKeys[i]);

                if (!(*col).second->varLength()) (*col).second->isRead(true);
        }

   }


   // now read the variable length columns that were found earlier.

   if (varCols.size() > 0 ) readVariableColumns(varCols);
  }

  void BinTable::readVariableColumns (const std::vector<String> &varColumns)
  {
    int   rowsRead=0;
    int   status=0;
    int   sz=varColumns.size();


    int i = 0;
    while ( i < sz && status == 0 )  
    {
       // Get bin size for the current column 
       Column& thisColumn = column(varColumns[i]);
       int colnum = thisColumn.index();
       if (thisColumn.type() == VTstring)
       {
          // Variable-length string columns must be treated differently.
          //  They are still represented as scalar columns rather than
          //  vector columns.  Their variation refers to the string lengths,
          //  not the number of elements in a row.
          thisColumn.readData(1, rows(), 1);
       }
       else
       {
          FITSUtil::auto_array_ptr<long> pBinSizes(new long[rows()]);
          long*  binSizes = pBinSizes.get();
          FITSUtil::auto_array_ptr<long> pOffsets(new long[rows()]);
          long*  offsets  = pOffsets.get();
          status = fits_read_descripts(fitsPointer(), colnum, 1, rows() , binSizes,
				       offsets, &status);
          if (status != 0) break;
          
          // Read vector column rows one at a time.
          for (rowsRead=0; rowsRead < rows() ; rowsRead++)
          {
	     if (binSizes[rowsRead] > 0)
	        thisColumn.readData(rowsRead+1, binSizes[rowsRead], 1);
          }
       }
       column(varColumns[i]).isRead(true);
       i++;

    }


    if (status != 0)  throw FitsError(status);
  }

  void BinTable::addColumn (ValueType type, const String& columnName, long repeatWidth, const String& colUnit, long decimals, size_t columnNumber)
  {
#ifdef SSTREAM_DEFECT
        std::ostrstream tformStr;
#else
        std::ostringstream tformStr;          
#endif

        // we do NOT support the extension allowed by cfitsio whereby multiple
        // strings can be saved in a binary table column cell.

        String diag;   

        if ( repeatWidth != 1 && type != Tstring && type != VTstring) 
        { 
                tformStr << repeatWidth;       
        }

        if (type < 0) tformStr << 'P';

        switch (type)
        {
                case Tstring:
                        tformStr << repeatWidth << 'A';
                        break;
                case VTstring:
                   // For variable string cols, cannot precede 'PA' with a number > 1.
                   tformStr << 'A';
                   break;
                case Tbyte:
                case VTbyte:
                        tformStr << 'B';
                        break;                
                case Tbit:
                case VTbit:
                        tformStr << 'X';             
                        break; 

                case Tlogical:
                case VTlogical:
                        tformStr << 'L';             
                        break;   

                case Tushort:
                case VTushort:
                        tformStr << 'U';
                        break;

                case Tshort:
                case VTshort:
                        tformStr << 'I';
                        break;

                case Tulong:
                case VTulong:
                        tformStr << 'V';
                        break;

                case Tlong:
                case VTlong:
                        tformStr << 'J';
                        break;   

                case Tlonglong:
                case VTlonglong:
                        tformStr << 'K';
                        break;        
                case Tuint:
                case VTuint:
                        tformStr << 'V';
                        break;

                case Tint:
                case VTint:
                        tformStr << 'J';
                        break;  

                case Tfloat:
                case VTfloat:
                        tformStr << 'E';
                        break;  

                case Tdouble:
                case VTdouble:
                        tformStr << 'D';
                        break;  

                case Tcomplex:
                case VTcomplex:
                        tformStr << 'C';
                        break;  

                case Tdblcomplex:
                case VTdblcomplex:
                        tformStr << 'M';
                        break;  

                default:           
                        diag += "Unrecognized data type for column: ";
                        diag += columnName;
                        throw InvalidColumnSpecification(diag);            
        }
#ifdef SSTREAM_DEFECT
        tformStr << std::ends;
#endif

        makeThisCurrent(); 
          
        int status(0);
        int colNum(0);
        if ( columnNumber == 0)
        {
            // add one to number of existing columns.
            if (fits_get_num_cols(fitsPointer(),&colNum,&status)) throw FitsError(status);
            colNum +=1;
        }
        else 
        {
                colNum = columnNumber;
        }

        String tfString(tformStr.str());
        // the C prototypes don't use const, so these casts are necessary.       
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
        if (columnNumber != 0) reindex(columnNumber, true);
        if (type == Tstring)
        {
           // For fixed-width scalar string cols, TFORM is "wA" where 'w' is the
           // string length.  Store this value in Column::m_width and just set
           // m_repeat to 1. [For vector columns the format would be
           // "rAw" where r corresponds to m_repeat, but these are not currently
           // supported in CCfits.]
           setColumn(columnName,create.createColumn(colNum,type,columnName,tfString,
                 colUnit,1,repeatWidth));                
        }
        else if (type == VTstring)
        {
           // For variable string column, Column::m_width could only refer to the
           //  width of the largest string in the col (the value 'w' in "PA(w)")
           //  This can't be known at column creation time (CFITSIO determines the
           //  'w' each time the file is closed), so just set the m_width to 1.
           setColumn(columnName,create.createColumn(colNum,type,columnName,tfString,
                 colUnit,1,1));                
        }
        else
        {
           Column *newCol = create.createColumn(colNum,type,columnName,tfString,
                colUnit,repeatWidth,1);
           setColumn(columnName,newCol);
           newCol->setLimits(type);
        }
        

  }
  

  // Additional Declarations

} // namespace CCfits
