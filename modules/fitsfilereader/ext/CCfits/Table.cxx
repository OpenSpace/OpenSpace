//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman

// Column
#include "Column.h"
// ColumnCreator
#include "ColumnCreator.h"
// Table
#include "Table.h"
// FITS
#include "FITS.h"

#include <algorithm>
#include <sstream>
#include <utility>


namespace CCfits {

  // Class CCfits::Table::NoSuchColumn 

  Table::NoSuchColumn::NoSuchColumn (const String& name, bool silent)
  : FitsException("Fits Error: cannot find column named: ",silent)
  {
    addToMessage(name);
    if (!silent || FITS::verboseMode() ) std::cerr << name << '\n';
  }

  Table::NoSuchColumn::NoSuchColumn (int index, bool silent)
  : FitsException("Fits Error: no column numbered: ",silent)
  {
    std::ostringstream oss;
    oss << index;
    addToMessage(oss.str());
    if (!silent || FITS::verboseMode() )  std::cerr << index << '\n';
  }


  // Class CCfits::Table::InvalidColumnSpecification 

  Table::InvalidColumnSpecification::InvalidColumnSpecification (const String& msg, bool silent)
    : FitsException("Fits Error: illegal column specification ",silent)
  {
    addToMessage(msg);
    if (!silent || FITS::verboseMode() )  std::cerr << msg << '\n';
  }


  // Class CCfits::Table 

  Table::Table(const Table &right)
  : ExtHDU(right),        
  m_numCols(right.m_numCols),
  m_column()
  {

  // deep-copy the right hand side data.
  copyData(right);
  }

  Table::Table (FITSBase* p, HduType xtype, const String &hduName, int rows, const std::vector<String>& columnName, const std::vector<String>& columnFmt, const std::vector<String>& columnUnit, int version)
      : ExtHDU(p, xtype, hduName,  8, 2, std::vector<long>(2), version),
	 m_numCols(columnName.size()), m_column()
  {

   int      status=0;

   // remember this is the writing constructor so user must specify
   // what kind of extension this is.
   int tblType = xtype;

   naxes(1) = rows;

   const size_t n(columnName.size());
   //FITSUtil::auto_array_ptr<char*> pCname(new char*[n]);
   //FITSUtil::auto_array_ptr<char*> pCformat(new char*[n]);
   //FITSUtil::auto_array_ptr<char*> pCunit(new char*[n]);
   char** cname = new char*[n];
   char** cformat = new char*[n];
   char** cunit = new char*[n];
   char nullString[] = {'\0'};

   for (size_t i = 0; i < n; ++i)
   {
        cname[i] = const_cast<char*>(columnName[i].c_str());       
        cformat[i] =  const_cast<char*>(columnFmt[i].c_str());
        if (i < columnUnit.size())       
           cunit[i]  = const_cast<char*>(columnUnit[i].c_str());
        else
           cunit[i] = nullString;
   }


   fits_create_tbl(fitsPointer(), tblType, rows , m_numCols, cname,
	  cformat, cunit, const_cast<char*>(hduName.c_str()), &status);
   if (!status && version > 1)
   {
      char extver[] = "EXTVER";
      fits_write_key(fitsPointer(), Tint, extver, &version, 0, &status);
   } 
   
   if (status)
   {
           delete [] cname;
           delete [] cformat;
           delete [] cunit;
           throw FitsError(status);
   } 

   delete[] cname;
   delete[] cformat;
   delete[] cunit;
  }

  Table::Table (FITSBase* p, HduType xtype, const String &hduName, int version)
      : ExtHDU(p,xtype,hduName,version), m_numCols(0), m_column()
  {
    getVersion();
  }

  Table::Table (FITSBase* p, HduType xtype, int number)
  : ExtHDU(p,xtype,number),
  m_numCols(0),
  m_column()
  {
    getVersion();
  }


  Table::~Table()
  {
        // destroy existing data (garbage collection for heap objects).
        clearData();
  }


  void Table::initRead ()
  {
   int        ncols=0;
   int        i=0;
   int        status=0;

   status = fits_get_num_cols(fitsPointer(), &ncols, &status);
   if (status != 0)  throw FitsError(status);


   std::vector<String> colName(ncols,"");
   std::vector<String> colFmt(ncols,"");
   std::vector<String> colUnit(ncols,"");

   ColumnCreator create(this);
   // virtual
   readTableHeader(ncols, colName, colFmt, colUnit);


   for(i=0; i<m_numCols; i++)
   {
      Column* newCol = create.getColumn(i+1, colName[i], colFmt[i],colUnit[i]);
      m_column.insert(std::make_pair(colName[i], newCol));
      newCol->setLimits(newCol->type()); 
   }

  }

  std::ostream & Table::put (std::ostream &s) const
  {
  s << "FITS Table::  "  <<  " Name: " << name() << " BITPIX "<< bitpix() << "\n";

  s <<  " Number of Rows (NAXIS2) " << axis(1)  << "\n";

  s << " HISTORY: " << history() << '\n';

  s << " COMMENTS: " << comment() << '\n';

  s <<  " HDU number: " <<  index() + 1 << " No. of Columns: " << numCols() ;

  if ( version() ) s << " Version " << version();

  s << "\nNumber of keywords read: " << keyWord().size() <<  "\n";




  for (std::map<String,Keyword*>::const_iterator ki = keyWord().begin();
        ki != keyWord().end(); ki++)
  {
        s << *((*ki).second) << std::endl;              
  }  

  std::vector<Column*> __tmp;
  ColMap::const_iterator ciEnd(m_column.end());
  ColMap::const_iterator ci(m_column.begin());
  while (ci != ciEnd) 
  {
        __tmp.push_back((*ci).second);
        ++ci;
  } 
  std::sort(__tmp.begin(),__tmp.end(),FITSUtil::ComparePtrIndex<Column>());

  for (std::vector<Column*>::iterator lci = __tmp.begin(); lci != __tmp.end(); ++lci) 
  {
          s << **lci << std::flush;
  }


  return s;
  }

  void Table::init (bool readFlag, const std::vector<String>& keys)
  {

  // read and defined the columns from the header.  
  initRead();

  // read data or keys if any are requested.
  if (readFlag || keys.size() > 0) readData(readFlag,keys);
  }

  void Table::clearData ()
  {
  // obliterate current contents, then remove pointers from the map.
  for (ColMap::const_iterator col = m_column.begin(); 
          col != m_column.end(); col++)
  {
        delete (*col).second;
  }

  m_column.clear();
  }

  void Table::copyData (const Table& right)
  {

  // ensure deep copy. clone() calls the copy constructor for Column.
  // Column has 'deep copy' because all its data members that need
  // to be copied (i.e. not the pointer to the fits file) are allocated
  // on the stack by being Container types.
  ColMap newColumnContainer;

  try
  {
        for (ColMap::const_iterator col = right.m_column.begin(); 
                col != right.m_column.end(); col++)
        {
           Column* colCopy = (*col).second->clone();
           colCopy->setParent(this);
           newColumnContainer.insert(std::make_pair(col->first, colCopy));
        }
        m_column = newColumnContainer;
  }
  catch (...)  { throw; }
  }

  Column& Table::column (const String& colName, bool caseSensitive) const
  {
     // For backwards compatibility with older m_column implementation as 
     //   a map (not a multimap), just return the first key that matches.
  bool isFound = false;
  ColMap::const_iterator key = m_column.begin();
  if (caseSensitive)
  {
     key = m_column.find(colName);
     isFound = (key != m_column.end());
  }
  else
  {
     const string lcColName = FITSUtil::lowerCase(colName);
     while (!isFound && key != m_column.end())
     {
        isFound = (lcColName == FITSUtil::lowerCase(key->first));
        if (!isFound)
           ++key;
     }
  }

  if ( !isFound ) throw NoSuchColumn(colName);

  return *(key->second);
  }

  Column& Table::column (int colIndex) const
  {
  ColMap::const_iterator key;

  for ( key = m_column.begin(); key != m_column.end(); key++)
  {
        if ( ((*key).second)->index() == colIndex) break;    

  }

  if ( key == m_column.end() ) throw NoSuchColumn(colIndex); 

  return *((*key).second); 
  }

  void Table::updateRows ()
  {
    long numrows(0);
    int status(0);
    if (fits_get_num_rows(fitsPointer(),&numrows,&status) ) throw FitsError(status);
    naxes(1,numrows);
  }

  void Table::setColumn (const String& colname, Column* value)
  {
    m_column.insert(std::make_pair(colname, value));
  }

  void Table::deleteColumn (const String& columnName)
  {
     std::pair<ColMap::iterator,ColMap::iterator> itRange = m_column.equal_range(columnName);
     if (itRange.first == itRange.second)
        throw NoSuchColumn(columnName);
     ColMap::iterator itCol = itRange.first;
     while (itCol != itRange.second)
     {
        // Don't assume if multiple cols share columnName that
        //  equal_range returns them sorted by col idx.
        // Note that both fits_delete_col and reindex() are O(N)
        //  where N = total number of columns in table.
        Column* doomed = itCol->second;
        int status=0;
        if (fits_delete_col(fitsPointer(), doomed->index(), &status))
            throw FitsError(status);
        m_column.erase(itCol++);
        reindex(doomed->index(), false);
        delete doomed;
     }
     updateRows();
  }

  void Table::insertRows (long first, long number)
  {
    int status(0);

    if (fits_insert_rows(fitsPointer(),first,number,&status)) throw FitsError(status);
    // cfitsio's semantics are that rows are insert after row first,
    // while vector's insert semantic is to insert before the initial.
    ColMap::iterator ci = m_column.begin();
    ColMap::iterator ciEnd = m_column.end();

    while (ci != ciEnd)
    {
        ((*ci).second)->insertRows(first,number);     
        ++ci;  
    }

    updateRows();
  }

  void Table::deleteRows (long first, long number)
  {
     // this should only be able to throw the FitsError exception and
    // should not leak resources as per the standard library guarantee for vector.
    makeThisCurrent();
    int status(0);

    // cfitsio will reject invalid values of first, number. (e.g. first <= 0).
    if (fits_delete_rows(fitsPointer(),first,number,&status)) throw FitsError(status);

    ColMap::iterator ci = m_column.begin();
    ColMap::iterator ciEnd = m_column.end();

    while (ci != ciEnd)
    {
        ((*ci).second)->deleteRows(first,number);     
        ++ci;  
    }

    updateRows();
  }

  void Table::deleteRows (const std::vector<long>& rowList)
  {
    int status(0);
    makeThisCurrent();
    FITSUtil::CVarray<long> convert;
    FITSUtil::auto_array_ptr<long> pDoomedRows(convert(rowList));
    long* doomedRows = pDoomedRows.get();
    if (fits_delete_rowlist(fitsPointer(),doomedRows,rowList.size(),&status)) 
                         throw FitsError(status);


    ColMap::iterator ci = m_column.begin();
    ColMap::iterator ciEnd = m_column.end();

    const size_t N(rowList.size());

    while (ci != ciEnd)
    {
        for (size_t j = 0; j < N; ++j)
        {
                ((*ci).second)->deleteRows(rowList[j],1);
        }     
        ++ci;  
    }

    updateRows();
  }

  void Table::reindex (int startNum, bool isInsert)
  {
     // If isInsert, assume this is called PRIOR to insertion into
     //   column container.
     // If !isInsert, assume this is called AFTER deletion.
     makeThisCurrent();
     ColMap::iterator itCol = m_column.begin();
     ColMap::iterator itColEnd = m_column.end();
     while (itCol != itColEnd)
     {
        Column* col = itCol->second;
        int oldIdx = col->index();
        if (isInsert)
        {
           if (oldIdx >= startNum)
              col->index(oldIdx+1);
        }
        else
        {
           if (oldIdx > startNum)
              col->index(oldIdx-1);
        }
        ++itCol;
     }       
  }

  long Table::getRowsize () const
  {
     int status = 0;
     long rowSize = 0;
     if (fits_get_rowsize(fitsPointer(), &rowSize, &status))
        throw FitsError(status);
     return rowSize;
  }

  void Table::copyColumn(const Column& inColumn, int colIndx, bool insertNewCol)
  {
     int status=0;
     fitsfile *infptr = inColumn.parent()->fitsPointer();
     const int inHduNum = inColumn.parent()->index();
     makeThisCurrent();
     
     // fits_copy_col will prevent copying from Binary to AsciiTable,
     //   so don't need to explicitly check for that here.
     if (fitsPointer() == infptr && inColumn.parent() != this)
     {
        // Can't give the same fitsfile pointer for in/out in fits_copy_col.
        fitsfile *tmpfptr=0;
        FITSUtil::auto_array_ptr<char> pFileName(new char[FLEN_FILENAME]);
        if (fits_file_name(infptr, pFileName.get(),&status))
           throw FitsError(status);
        if (fits_open_file(&tmpfptr,pFileName.get(), 1, &status))
           throw FitsError(status);        
        if (fits_movabs_hdu(tmpfptr, index()+1, 0, &status))
        {
           fits_close_file(tmpfptr,&status);
           throw FitsError(status);
        }  
        if (fits_movabs_hdu(infptr, inHduNum+1, 0, &status))
        {
           fits_close_file(tmpfptr,&status);
           throw FitsError(status);
        }
        
        if (fits_copy_col(infptr, tmpfptr, inColumn.index(), colIndx, (int)insertNewCol, &status))
        {
           fits_close_file(tmpfptr,&status);
           throw FitsError(status);
        }  
        fits_close_file(tmpfptr,&status); 
     }
     else
     {                     
        inColumn.parent()->makeThisCurrent();
        if (fits_copy_col(infptr,fitsPointer(), inColumn.index(), colIndx, (int)insertNewCol,&status))
           throw FitsError(status);
     } 
     
     if (insertNewCol)
     {
        Column* colCopy = inColumn.clone();
        colCopy->setParent(this);
        colCopy->index(colIndx);
        // Not going to assume we can reuse inColumn's m_data array.
        //  For one thing, it might not even have the same number of rows
        //  as this table.  It's safest just to force a read-from-disk
        //  next time a read operation is called.
        colCopy->resetRead();
        reindex(colIndx, true);
        column().insert(std::make_pair(colCopy->name(), colCopy));
     }
     else
     {
        // Can assume the write to existing column succeeded, else
        //   bad status would have thrown above.
        ColMap::iterator itCol = column().begin();
        ColMap::iterator itColEnd = column().end();
        bool isFound=false;
        while (!isFound && itCol != itColEnd)
        {
           if (itCol->second->index() == colIndx)
              isFound = true;
           else
              ++itCol;
        }
        if (itCol != itColEnd)
        {
           // For a non-inserted column copy, the various column keywords
           //  (ie. TSCALn, TTYPEn,...) of the destination column ARE NOT
           //  replaced with the incoming copy values.  Therefore DON'T
           //  replace CCfits Column object with a clone of the input.
           //  Simply reset isRead flag to false.  m_data array will be
           //  properly refilled upon next read operation.
           itCol->second->resetRead();
        }
     }    
  }
  // Additional Declarations

} // namespace CCfits
