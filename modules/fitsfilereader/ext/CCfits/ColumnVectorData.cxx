//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman

// ColumnVectorData
#include "ColumnVectorData.h"

namespace CCfits
{

#ifndef SPEC_TEMPLATE_IMP_DEFECT
#ifndef SPEC_TEMPLATE_DECL_DEFECT
	    // duplicated for each complex type to work around imagined or
        // actual compiler deficiencies.
        template <>
        void ColumnVectorData<std::complex<float> >::readColumnData(long firstRow, 
                                long nelements, long firstElem, std::complex<float>* null )
        {
            int   status=0;
            float nulval (0);
            FITSUtil::auto_array_ptr<float> pArray(new float[2*nelements]); 
            float*     array = pArray.get();
            int    anynul(0);

            if (fits_read_col_cmp(fitsPointer(),index(),firstRow, firstElem,
                            nelements,nulval,array,&anynul,&status) ) throw FitsError(status);

            if (m_data.size() != static_cast<size_t>(rows())) m_data.resize(rows());

            std::valarray<std::complex<float> > readData(nelements);
            for (long j = 0; j < nelements; ++j)
            {
                    readData[j] = std::complex<float>(array[2*j],array[2*j+1]);
            }
            size_t countRead = 0;
            const size_t ONE = 1;

            if (m_data.size() != static_cast<size_t>(rows())) m_data.resize(rows());
            size_t vectorSize(0);
            if (!varLength())
            {
                 vectorSize = std::max(repeat(),ONE); // safety check.
            }
            else
            {
                 // assume that the user specified the correct length for 
                 // variable columns. This should be ok since readVariableColumns
                 // uses fits_read_descripts to return this information from the
                 // fits pointer, and this is passed as nelements here.
                 vectorSize = nelements;       
            }
            size_t n = nelements; 

            int i = firstRow;
            int ii = i - 1;
            while ( countRead < n)
            {
                    std::valarray<complex<float> >& current = m_data[ii];
                    if (current.size() != vectorSize) current.resize(vectorSize,0.);
                    int elementsInFirstRow = vectorSize-firstElem + 1;
                    bool lastRow = ( (nelements - countRead) < vectorSize);
                    if (lastRow)
                    {
                            int elementsInLastRow = nelements - countRead;
                            std::copy(&readData[countRead],&readData[0]+nelements,&current[0]);
                            countRead += elementsInLastRow;
                    }             
                    // what to do with complete rows. if firstElem == 1 the 
                    else 
                    {
                            if (firstElem == 1 || (firstElem > 1 && i > firstRow) )
                            {
                                    current = readData[std::slice(vectorSize*(ii-firstRow)+
                                                               elementsInFirstRow,vectorSize,1)];
			            ++ii;
			            ++i;
                                    countRead += vectorSize;   
                            }   
                            else
                            { 
                                    if (i == firstRow)
                                    {
                                            std::copy(&readData[0],&readData[0]+elementsInFirstRow,
                                                                            &current[firstElem]);
                                            countRead += elementsInFirstRow;
                                            ++i;
                                            ++ii;
                                    }
                            }
                    }
            }
    }

#ifndef SPEC_TEMPLATE_DECL_DEFECT
template <>
void ColumnVectorData<complex<float> >::setDataLimits (complex<float>* limits)
        {
                m_minLegalValue = limits[0];
                m_maxLegalValue = limits[1];
                m_minDataValue =  limits[2];
                m_maxDataValue =  limits[3];
        }
template <>
void ColumnVectorData<complex<double> >::setDataLimits (complex<double>* limits)
        {
                m_minLegalValue = limits[0];
                m_maxLegalValue = limits[1];
                m_minDataValue =  limits[2];
                m_maxDataValue =  limits[3];
        }
#endif

    template <>
    void ColumnVectorData<complex<double> >::readColumnData (long firstRow, 
              long nelements,long firstElem, 
              complex<double>* nullValue)
    {

        // duplicated for each complex type to work around imagined or
        // actual compiler deficiencies.
            int   status=0;
            double nulval (0);
            FITSUtil::auto_array_ptr<double> pArray(new double[2*nelements]); 
            double*     array = pArray.get();
            int    anynul(0);

            if (fits_read_col_dblcmp(fitsPointer(),index(),firstRow, firstElem,
                            nelements,nulval,array,&anynul,&status) ) throw FitsError(status);

            if (m_data.size() != static_cast<size_t>(rows())) m_data.resize(rows());

            std::valarray<std::complex<double> > readData(nelements);
            for (long j = 0; j < nelements; ++j)
            {
                    readData[j] = std::complex<double>(array[2*j],array[2*j+1]);
            }
            size_t countRead = 0;
            const size_t ONE = 1;

            if (m_data.size() != static_cast<size_t>(rows())) m_data.resize(rows());
            size_t vectorSize(0);
            if (!varLength())
            {
                 vectorSize = std::max(repeat(),ONE); // safety check.
            }
            else
            {
                 // assume that the user specified the correct length for 
                 // variable columns. This should be ok since readVariableColumns
                 // uses fits_read_descripts to return this information from the
                 // fits pointer, and this is passed as nelements here.
                 vectorSize = nelements;       
            }
            size_t n = nelements; 

            int i = firstRow;
            int ii = i - 1;
            while ( countRead < n)
            {
                    std::valarray<std::complex<double> >& current = m_data[ii];
                    if (current.size() != vectorSize) current.resize(vectorSize,0.);
                    int elementsInFirstRow = vectorSize-firstElem + 1;
                    bool lastRow = ( (nelements - countRead) < vectorSize);
                    if (lastRow)
                    {
                            int elementsInLastRow = nelements - countRead;
                            std::copy(&readData[countRead],&readData[0]+nelements,&current[0]);
                            countRead += elementsInLastRow;
                    }             
                    // what to do with complete rows. if firstElem == 1 the 
                    else 
                    {
                            if (firstElem == 1 || (firstElem > 1 && i > firstRow) )
                            {
                                    current = readData[std::slice(vectorSize*(ii-firstRow)+
                                                               elementsInFirstRow,vectorSize,1)];
			            ++ii;
			            ++i;
                                    countRead += vectorSize;   
                            }   
                            else
                            { 
                                    if (i == firstRow)
                                    {
                                            std::copy(&readData[0],&readData[0]+elementsInFirstRow,
                                                                            &current[firstElem]);
                                            countRead += elementsInFirstRow;
                                            ++i;
                                            ++ii;
                                    }
                            }
                    }
            }
    }

        template <>
        void ColumnVectorData<complex<float> >::writeFixedArray 
                        (complex<float>* data, long nElements, long nRows, long firstRow, 
                         complex<float>* nullValue)
        {

                int status(0);

    // check for sanity of inputs, then write to file.
    // this function writes only complete rows to a table with
    // fixed width rows.


                if ( nElements < nRows*static_cast<long>(repeat()) )
                {
#ifdef SSTREAM_DEFECT
                        std::ostrstream msgStr;
#else
                        std::ostringstream msgStr;
#endif
                        msgStr << " input array size: " << nElements 
                                        << " required " << nRows*repeat();
#ifdef SSTREAM_DEFECT
                        msgStr << std::ends;
#endif


                        String msg(msgStr.str());

                        throw Column::InsufficientElements(msg);
                } 

                FITSUtil::auto_array_ptr<float> realData(new float[2*nElements]);

                for (int j = 0; j < nElements; ++j)
                {
                        realData[2*j] = data[j].real();
                        realData[2*j+1] = data[j].imag();       
                }



                if (fits_write_col_cmp(fitsPointer(),index(),firstRow,
                        1,nElements,realData.get(),&status)) throw FitsError(status);

                parent()->updateRows();
        }

        template <>
        void ColumnVectorData<complex<double> >::writeFixedArray 
                        (complex<double>* data, long nElements, long nRows, long firstRow, 
                         complex<double>* nullValue)
        {
                int status(0);

    // check for sanity of inputs, then write to file.
    // this function writes only complete rows to a table with
    // fixed width rows.


                if ( nElements < nRows*static_cast<long>(repeat()) )
                {
#ifdef SSTREAM_DEFECT
                        std::ostrstream msgStr;
#else
                        std::ostringstream msgStr;
#endif
                        msgStr << " input array size: " << nElements 
                                        << " required " << nRows*repeat();
                        String msg(msgStr.str());

                        throw Column::InsufficientElements(msg);
                } 

                FITSUtil::auto_array_ptr<double> realData(new double[2*nElements]);

                for (int j = 0; j < nElements; ++j)
                {
                        realData[2*j] = data[j].real();
                        realData[2*j+1] = data[j].imag();       
                }



                if (fits_write_col_dblcmp(fitsPointer(),index(),firstRow,
                        1,nElements,realData.get(),&status)) throw FitsError(status);

                parent()->updateRows();

        }
#endif
#endif

#ifndef SPEC_TEMPLATE_DECL_DEFECT
        template <>
        void  
        ColumnVectorData<std::complex<float> >::doWrite 
                        (std::complex<float>* data, long row, long rowSize, long firstElem, std::complex<float>* nullValue )
        {
                int status(0);
                FITSUtil::auto_array_ptr<float> carray( new float[2*rowSize]); 
                for ( long j = 0 ; j < rowSize; ++ j)
                {
                        carray[2*j] = data[j].real();
                        carray[2*j + 1] = data[j].imag();
                }
                if (fits_write_col_cmp(fitsPointer(),index(),row,firstElem,rowSize,
                        carray.get(),&status)) throw FitsError(status);
        }


        template <>
        void  
        ColumnVectorData<std::complex<double> >::doWrite
                        (std::complex<double>* data, long row, long rowSize, long firstElem, std::complex<double>* nullValue )
        {
                int status(0);
                FITSUtil::auto_array_ptr<double> carray( new double[2*rowSize]); 
                for ( long j = 0 ; j < rowSize; ++ j)
                {
                        carray[2*j] = data[j].real();
                        carray[2*j + 1] = data[j].imag();
                }
                if (fits_write_col_dblcmp(fitsPointer(),index(),row,firstElem,rowSize,
                        carray.get(),&status)) throw FitsError(status);

        }
#endif

}
