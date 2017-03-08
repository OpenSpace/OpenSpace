// cookbook CCfits demonstration program
//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman


// The CCfits headers are expected to be installed in a subdirectory of
// the include path.

// The <CCfits> header file contains all that is necessary to use both the CCfits
// library and the cfitsio library (for example, it includes fitsio.h) thus making
// all of cfitsio's symbolic names available.

#ifdef _MSC_VER
#include "MSconfig.h" // for truncation warning
#endif

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

// this includes 12 of the CCfits headers and will support all CCfits operations.
// the installed location of the library headers is $(ROOT)/include/CCfits

// to use the library either add -I$(ROOT)/include/CCfits or #include <CCfits/CCfits>
// in the compilation target.


#include <CCfits>
#include <cmath>
    // The library is enclosed in a namespace.
        
    using namespace CCfits;
    using std::valarray;



int main();
int writeImage();
int writeAscii();
int writeBinary();
int copyHDU();
int selectRows();
int readHeader(); 
int readImage();
int readTable();
int readExtendedSyntax();

int main()
{


     FITS::setVerboseMode(true);

     try
                     
     {

        if (!writeImage()) std::cerr << " writeImage() \n";
        if (!writeAscii()) std::cerr << " writeAscii() \n";
        if (!writeBinary()) std::cerr << " writeBinary()  \n";
        if (!copyHDU()) std::cerr << " copyHDU() \n";
        if (!readHeader()) std::cerr << " readHeader() \n";
        if (!readImage()) std::cerr << " readImage() \n";
        if (!readTable()) std::cerr << " readTable() \n";
        if (!readExtendedSyntax()) std::cerr << " readExtendedSyntax() \n";
        if (!selectRows()) std::cerr << " selectRows() \n";

     }
     catch (FitsException&) 
     // will catch all exceptions thrown by CCfits, including errors
     // found by cfitsio (status != 0). 
     {
             
        std::cerr << " Fits Exception Thrown by test function \n";       
             
     }
    return 0;
}

int writeImage()
{

    // Create a FITS primary array containing a 2-D image               
    // declare axis arrays.    
    long naxis    =   2;      
    long naxes[2] = { 300, 200 };   
    
    // declare auto-pointer to FITS at function scope. Ensures no resources
    // leaked if something fails in dynamic allocation.
    std::auto_ptr<FITS> pFits(0);
      
    try
    {                
        // overwrite existing file if the file already exists.
            
        const std::string fileName("!atestfil.fit");            
        
        // Create a new FITS object, specifying the data type and axes for the primary
        // image. Simultaneously create the corresponding file.
        
        // this image is unsigned short data, demonstrating the cfitsio extension
        // to the FITS standard.
        
        pFits.reset( new FITS(fileName , USHORT_IMG , naxis , naxes ) );
    }
    catch (FITS::CantCreate)
    {
          // ... or not, as the case may be.
          return -1;       
    }
    
    // references for clarity.
    
    long& vectorLength = naxes[0];
    long& numberOfRows = naxes[1];
    long nelements(1); 
    
    
    // Find the total size of the array. 
    // this is a little fancier than necessary ( It's only
    // calculating naxes[0]*naxes[1]) but it demonstrates  use of the 
    // C++ standard library accumulate algorithm.
    
    nelements = std::accumulate(&naxes[0],&naxes[naxis],1,std::multiplies<long>());
           
    // create a new image extension with a 300x300 array containing float data.
    
    std::vector<long> extAx(2,300);
    string newName ("NEW-EXTENSION");
    ExtHDU* imageExt = pFits->addImage(newName,FLOAT_IMG,extAx);
    
    // create a dummy row with a ramp. Create an array and copy the row to 
    // row-sized slices. [also demonstrates the use of valarray slices].   
    // also demonstrate implicit type conversion when writing to the image:
    // input array will be of type float.
    

    std::valarray<int> row(vectorLength);
    for (long j = 0; j < vectorLength; ++j) row[j] = j;
    std::valarray<int> array(nelements);
    for (int i = 0; i < numberOfRows; ++i)
    {
        array[std::slice(vectorLength*static_cast<int>(i),vectorLength,1)] = row + i;     
    }

#ifdef VALARRAY_DEFECT
    const double PI ( std::atan(1.)*4. );
#else
    const double PI (std::atan(1.)*4.);
#endif
    // create some data for the image extension.
    long extElements = std::accumulate(extAx.begin(),extAx.end(),1,std::multiplies<long>()); 
    std::valarray<float> ranData(extElements);
    const float PIBY = static_cast < float > (PI/150.);
    for ( int jj = 0 ; jj < extElements ; ++jj) 
    {
            float arg = static_cast < float > ( PIBY*jj );
#ifdef VALARRAY_DEFECT
			float val = std::cos( arg );
			ranData[jj] = val;
#else
            ranData[jj] = static_cast < float > ( std::cos(arg) );
#endif
    }
 
    long  fpixel(1);
    
    // write the image extension data: also demonstrates switching between
    // HDUs.
    imageExt->write(fpixel,extElements,ranData);
    
    //add two keys to the primary header, one long, one complex.
    
    long exposure(1500);
#ifdef VALARRAY_DEFECT
	double re = std::cos( 2*PI/3.0 );
	double im = std::sin( 2*PI/3.0 );
    std::complex<float> omega( re, im );
#else
	float re = static_cast < float > ( std::cos(2*PI/3.) );
	float im = static_cast < float > ( std::sin(2*PI/3.) );
    std::complex<float> omega( re, im );
#endif
    pFits->pHDU().addKey("EXPOSURE", exposure,"Total Exposure Time"); 
    pFits->pHDU().addKey("OMEGA",omega," Complex cube root of 1 ");  

    
    // The function PHDU& FITS::pHDU() returns a reference to the object representing 
    // the primary HDU; PHDU::write( <args> ) is then used to write the data.
    
    pFits->pHDU().write(fpixel,nelements,array);
    
    
    // PHDU's friend ostream operator. Doesn't print the entire array, just the
    // required & user keywords, and is provided largely for testing purposes [see 
    // readImage() for an example of how to output the image array to a stream].
    
    std::cout << pFits->pHDU() << std::endl;

    return 0;
}

int writeAscii ()

    //******************************************************************
    // Create an ASCII Table extension containing 3 columns and 6 rows *
    //******************************************************************
{
    // declare auto-pointer to FITS at function scope. Ensures no resources
    // leaked if something fails in dynamic allocation.
    std::auto_ptr<FITS> pFits(0);
      
    try
    {                
    
            
        const std::string fileName("atestfil.fit");
        
        // append the new extension to file created in previous function call.   
        // CCfits writing constructor. 
        
        // if this had been a new file, then the following code would create
        // a dummy primary array with BITPIX=8 and NAXIS=0.

 
        pFits.reset( new FITS(fileName,Write) );
    }
    catch (CCfits::FITS::CantOpen)
    {
          // ... or not, as the case may be.
          return -1;       
    }
        
    unsigned long rows(6); 
    string hduName("PLANETS_ASCII");  
    std::vector<string> colName(3,"");
    std::vector<string> colForm(3,"");
    std::vector<string> colUnit(3,"");
    
    /* define the name, datatype, and physical units for the 3 columns */    
    colName[0] = "Planet";
    colName[1] = "Diameter";
    colName[2] = "Density";

    colForm[0] = "a8";
    colForm[1] = "i6";
    colForm[2] = "f4.2";

    colUnit[0] = "";
    colUnit[1] = "km";
    colUnit[2] = "g/cm^-3";    

    std::vector<string> planets(rows);
    
    const char *planet[] = {"Mercury", "Venus", "Earth", 
			    "Mars","Jupiter","Saturn"};
    const char *mnemoy[] = {"Many", "Volcanoes", "Erupt", 
			    "Mulberry","Jam","Sandwiches","Under",
                                "Normal","Pressure"};
                    
    long diameter[] = {  4880,    12112,      12742,   6800,    143000,   121000};
    float density[]  = { 5.1f,     5.3f,      5.52f,   3.94f,    1.33f,    0.69f};
    
    // append a new ASCII table to the fits file. Note that the user
    // cannot call the Ascii or Bin Table constructors directly as they
    // are protected.
    
    Table* newTable = pFits->addTable(hduName,rows,colName,colForm,colUnit,AsciiTbl);
	size_t j = 0;    
    for ( ; j < rows; ++j) planets[j] = string(planet[j]);    
    
    // Table::column(const std::string& name) returns a reference to a Column object
    
    try
    {                
    
        newTable->column(colName[0]).write(planets,1);  
        newTable->column(colName[1]).write(diameter,rows,1);
        newTable->column(colName[2]).write(density,rows,1);
    
    }
    catch (FitsException&)
    {
         // ExtHDU::column could in principle throw a NoSuchColumn exception,
         // or some other fits error may ensue.
         std::cerr << " Error in writing to columns - check e.g. that columns of specified name "
                        << " exist in the extension \n";
                               
    }
    
    //  FITSUtil::auto_array_ptr<T> is provided to counter resource leaks that
    //  may arise from C-arrays. It is a std::auto_ptr<T> analog that calls
    //  delete[] instead of delete.
    
    FITSUtil::auto_array_ptr<long>  pDiameter(new long[rows]);
    FITSUtil::auto_array_ptr<float>  pDensity(new float[rows]);
    long* Cdiameter = pDiameter.get();
    float*  Cdensity = pDensity.get();
    
    Cdiameter[0] = 4880; Cdiameter[1] = 12112; Cdiameter[2] = 12742; Cdiameter[3] = 6800;
    Cdiameter[4] = 143000; Cdiameter[5] = 121000;
    
    Cdensity[0] = 5.1f;  Cdensity[1] = 5.3f;  Cdensity[2] = 5.52f;
	Cdensity[3] = 3.94f; Cdensity[4] = 1.33f; Cdensity[5] = 0.69;
    
    // this << operator outputs everything that has been read.
    
    std::cout << *newTable << std::endl;
    
    pFits->pHDU().addKey("NEWVALUE",42," Test of adding keyword to different extension");
    
	pFits->pHDU().addKey("STRING",std::string(" Rope "),"trailing blank test 1 "); 
    
	pFits->pHDU().addKey("STRING2",std::string("Cord"),"trailing blank test 2 "); 
    // demonstrate increaing number of rows and null values.
    long ignoreVal(12112); 
    long nullNumber(-999);    
    try
    {      
        // add a TNULLn value to column 2. 
        newTable->column(colName[1]).addNullValue(nullNumber);    
        // test that writing new data properly expands the number of rows
        // in both the file]).write(planets,rows-3);  
        newTable->column(colName[2]).write(density,rows,rows-3);    
        // test the undefined value functionality. Undefineds are replaced on
        // disk but not in the memory copy.
        newTable->column(colName[1]).write(diameter,rows,rows-3,&ignoreVal);
    }
    catch (FitsException&) 
    { 
            // this time we're going to ignore problems in these operations 
            
    }

    // output header information to check that everything we did so far
    // hasn't corrupted the file.    
            
    std::cout << pFits->pHDU() << std::endl;
    
    
    std::vector<string> mnemon(9);
    for ( j = 0; j < 9; ++j) mnemon[j] = string(mnemoy[j]);
    
    // Add a new column of string type to the Table.
    // type,  columnName, width, units. [optional - decimals, column number]
    // decimals is only relevant for floatingpoint data in ascii columns.
    newTable->addColumn(Tstring,"Mnemonic",10," words ");
    newTable->column("Mnemonic").write(mnemon,1);  
    
    // write the data string.
    newTable->writeDate();
   
    // and see if it all worked right.
    std::cout << *newTable << std::endl;
    
    return 0;
}
int writeBinary ()

    //*********************************************************************
    // Create a BINARY table extension and write and manipulate vector rows 
    //*********************************************************************
{
    std::auto_ptr<FITS> pFits(0);
      
    try
    {                
            
        const std::string fileName("atestfil.fit");        
        pFits.reset( new FITS(fileName,Write) );
    }
    catch (CCfits::FITS::CantOpen)
    {
          return -1;       
    }
    
        
    unsigned long rows(3);     
    string hduName("TABLE_BINARY");          
    std::vector<string> colName(7,"");
    std::vector<string> colForm(7,"");
    std::vector<string> colUnit(7,"");
    
    
    colName[0] = "numbers";
    colName[1] = "sequences";
    colName[2] = "powers";
    colName[3] = "big-integers";
    colName[4] = "dcomplex-roots";
    colName[5] = "fcomplex-roots";
    colName[6] = "scalar-complex";

    colForm[0] = "8A";
    colForm[1] = "20J";
    colForm[2] = "20D";
    colForm[3] = "20V";
    colForm[4] = "20M";
    colForm[5] = "20C";
    colForm[6] = "1M";

    colUnit[0] = "magnets";
    colUnit[1] = "bulbs";
    colUnit[2] = "batteries";  
    colUnit[3] = "mulberries";  
    colUnit[4] = "";  
    colUnit[5] = "";
    colUnit[6] = "pico boo";
    
    std::vector<string> numbers(rows);
    
    const string num("NUMBER-");
    for ( size_t j = 0; j < rows; ++j)
    {
#ifdef SSTREAM_DEFECT
        std::ostrstream pStr;
#else
     	std::ostringstream pStr;
#endif
	pStr << num << j+1;
#ifdef SSTREAM_DEFECT
	pStr << std::ends;
#endif
	numbers[j] = string(pStr.str());
        
        
    }
                    
    const size_t OFFSET(20);
    
    // write operations take in data as valarray<T>, vector<T> , and 
    // vector<valarray<T> >, and T* C-arrays. Create arrays to exercise the C++
    // containers. Check complex I/O for both float and double complex types.
   
    std::valarray<long> sequence(60);
    std::vector<long> sequenceVector(60);
    std::vector<std::valarray<long> > sequenceVV(3);
    
    
    for ( size_t j = 0; j < rows; ++j)
    {
	
    	sequence[OFFSET*j] = 1 + j;
        sequence[OFFSET*j+1] = 1 + j;
    	sequenceVector[OFFSET*j] = sequence[OFFSET*j];
    	sequenceVector[OFFSET*j+1] = sequence[OFFSET*j+1];
        // generate Fibonacci numbers.
	for (size_t i = 2; i < OFFSET; ++i)
	{
                size_t elt (OFFSET*j +i);
		sequence[elt] = sequence[elt-1]	+ sequence[elt - 2];
                sequenceVector[elt] = sequence[elt] ;
	}
        sequenceVV[j].resize(OFFSET);
        sequenceVV[j] = sequence[std::slice(OFFSET*j,OFFSET,1)];
         
    }
    
        
    std::valarray<unsigned int> unsignedData(60);
    unsigned int base (1 << 31);
    std::valarray<double> powers(60);
    std::vector<double> powerVector(60);
    std::vector<std::valarray<double> > powerVV(3);
    std::valarray<std::complex<double> > croots(60);
    std::valarray<std::complex<float> > fcroots(60);
    std::vector<std::complex<float> > fcroots_vector(60);
    std::vector<std::valarray<std::complex<float> > > fcrootv(3);
    const double PI (std::atan(1.)*4.);
    // create complex data as 60th roots of unity.
    double PIBY = PI/60.;
    
    for ( size_t j = 0; j < rows; ++j)
    {
	for (size_t i = 0; i < OFFSET; ++i)
	{
                size_t elt (OFFSET*j+i);
                unsignedData[elt] = sequence[elt];
#ifdef VALARRAY_DEFECT
				double re = std::cos(PIBY*elt);
				double im = std::sin(PIBY*elt);

                croots[elt] = std::complex<double>( re, im );
#else
                croots[elt] = std::complex<double>(std::cos(PIBY*elt),std::sin(PIBY*elt));
#endif
                fcroots[elt] = std::complex<float>(croots[elt].real(),croots[elt].imag());
                double x = i+1;
		powers[elt] = pow(x,(int)j+1);
		powerVector[elt] = powers[elt];
	}
        powerVV[j].resize(OFFSET);
        powerVV[j] = powers[std::slice(OFFSET*j,OFFSET,1)];
    }
#ifdef TEMPLATE_AMBIG7_DEFECT
	std::slice s ( 0, 20, 1 );
	std::valarray<std::complex<float> > fcroots_sliced ( fcroots[s] );
    FITSUtil::fillMSva(fcroots_vector, fcroots_sliced );
#else
    FITSUtil::fill(fcroots_vector,fcroots[std::slice(0,20,1)]);
#endif
    unsignedData += base;
    // syntax identical to Binary Table
    
    Table* newTable = pFits->addTable(hduName,rows,colName,colForm,colUnit);
    
    // numbers is a scalar column
    
    newTable->column(colName[0]).write(numbers,1);  
    
    // write valarrays to vector column: note signature change
    newTable->column(colName[1]).write(sequence,rows,1);
    newTable->column(colName[2]).write(powers,rows,1);
    newTable->column(colName[3]).write(unsignedData,rows,1);    
    newTable->column(colName[4]).write(croots,rows,1);    
    newTable->column(colName[5]).write(fcroots,rows,3);    
    newTable->column(colName[6]).write(fcroots_vector,1);    
    // write vectors to column: note signature change
    
    newTable->column(colName[1]).write(sequenceVector,rows,4);
    newTable->column(colName[2]).write(powerVector,rows,4);
    
    std::cout << *newTable << std::endl;
   
    for (size_t j = 0; j < 3 ;  ++j)
    {
            fcrootv[j].resize(20);
            fcrootv[j] = fcroots[std::slice(20*j,20,1)];
    } 

    // write vector<valarray> object to column.
    
    newTable->column(colName[1]).writeArrays(sequenceVV,7);
    newTable->column(colName[2]).writeArrays(powerVV,7);

 
    
    // create a new vector column in the Table


    newTable->addColumn(Tfloat,"powerSeq",20,"none");

    // add data entries to it.

    newTable->column("powerSeq").writeArrays(powerVV,1);
    newTable->column("powerSeq").write(powerVector,rows,4);
    newTable->column("dcomplex-roots").write(croots,rows,4);
    newTable->column("powerSeq").write(sequenceVector,rows,7);
    

    std::cout << *newTable << std::endl;
    
    // delete one of the original columns.

 

    newTable->deleteColumn(colName[2]);


    // add a new set of rows starting after row 3. So we'll have 14 with
    // rows 4,5,6,7,8 blank

    newTable->insertRows(3,5);
    
    // now, in the new column, write 3 rows (sequenceVV.size() = 3). This
    // will place data in rows 3,4,5 of this column,overwriting them.
    
    newTable->column("powerSeq").writeArrays(sequenceVV,3);
    newTable->column("fcomplex-roots").writeArrays(fcrootv,3);

    // delete 3 rows starting with row 2. A Table:: method, so the same
    // code is called for all Table objects. We should now have 11 rows.
    
    newTable->deleteRows(2,3);
    
    //add a history string. This function call is in HDU:: so is identical
    //for all HDUs

    string hist("This file was created for testing CCfits write functionality");
    hist += " it serves no other useful purpose. This particular part of the file was ";
    hist += " constructed to test the writeHistory() and writeComment() functionality" ;

 
    newTable->writeHistory(hist);
    
    // add a comment string. Use std::string method to change the text in the message
    // and write the previous junk as a comment.

    hist.insert(0, " COMMENT TEST ");

    newTable->writeComment(hist);

    // ... print the result.
    
    std::cout << *newTable << std::endl;
    
    return 0;
}

int copyHDU()
{
    //*******************************************************************/
    // copy the 1st and 3rd HDUs from the input file to a new FITS file */
    //*******************************************************************/


    const string inFileName("atestfil.fit");  
    const string outFileName("btestfil.fit");  

    remove(outFileName.c_str());            /* Delete old file if it already exists */

    // open the existing FITS file (Read is the default anyway)
    FITS inFile(inFileName,Read);
    
    // custom constructor FITS::FITS(const string&, const FITS&) for
    // this particular task.
    
    FITS outFile(outFileName,inFile);

    // copy extension by number...
    outFile.copy(inFile.extension(2));
    
    // copy extension by name...
    outFile.copy(inFile.extension("TABLE_BINARY"));
    
   return 0;

 }



 int readHeader()
 {

         
         const string SPECTRUM("SPECTRUM");
         
         // read a particular HDU within the file. This call reads just the header 
         // information from SPECTRUM
         
         std::auto_ptr<FITS> pInfile(new FITS("file1.pha",Read,SPECTRUM));
         
         // define a reference for clarity.
         
         ExtHDU& table = pInfile->extension(SPECTRUM);
         
         // read all the keywords, excluding those associated with columns.
         
         table.readAllKeys();
         // print the result.
         
         
         std::cout << table << std::endl;
         
         
        return 0;       
 }

 int readImage()
 {
        std::auto_ptr<FITS> pInfile(new FITS("atestfil.fit",Read,true));
        
        PHDU& image = pInfile->pHDU(); 
        
        std::valarray<unsigned long>  contents;
        
        // read all user-specifed, coordinate, and checksum keys in the image
        image.readAllKeys();
        
        image.read(contents);
        
        // this doesn't print the data, just header info.
        std::cout << image << std::endl;
        
        long ax1(image.axis(0));
        long ax2(image.axis(1));
        
        for (long j = 0; j < ax2; j+=10)
        {
                std::ostream_iterator<short> c(std::cout,"\t");
                std::copy(&contents[0]+j*ax1,&contents[0]+(j+1)*ax1,c);
                std::cout << '\n';       
        }
           
        std::cout << std::endl;
        return 0;   
 }

 int readTable()
 {
        // read a table and explicitly read selected columns. To read all the
        // data on construction, set the last argument of the FITS constructor
        // call to 'true'. This has been tested.
        std::vector<String> hdus(2);
        hdus[0] = "PLANETS_ASCII";
        hdus[1] = "TABLE_BINARY";
        
        
        std::auto_ptr<FITS> pInfile(new FITS("atestfil.fit",Read,hdus,false));
        
        ExtHDU& table = pInfile->extension(hdus[1]);
        
        
        
        std::vector < valarray <int > > pp;
        table.column("powerSeq").readArrays( pp, 1,3 );
                        
        std::vector < valarray <std::complex<double> > > cc;
        table.column("dcomplex-roots").readArrays( cc, 1,3 );
        
        std::valarray < std::complex<float> > ff;
        table.column("fcomplex-roots").read( ff, 7 );
        
        std::vector<std::complex<float> > sf;
        table.column("scalar-complex").read(sf, 10, 15);
        
        std::cout << pInfile->extension(hdus[0]) << std::endl;
        
        std::cout << pInfile->extension(hdus[1]) << std::endl;
         
        return 0;       
 }

 int readExtendedSyntax()
 {
    // Current extension will be set to PLANETS_ASCII after construction:
    std::auto_ptr<FITS> pInfile(new FITS("btestfil.fit[PLANETS_ASCII][Density > 5.2]"));
    std::cout << "\nCurrent extension: " 
        << pInfile->currentExtensionName() << std::endl;
        
    Column& col = pInfile->currentExtension().column("Density");
    std::vector<double> densities;
    
    // nRows should only include rows with density column vals > 5.2.
    const int nRows = col.rows();
    col.read(densities, 1, nRows);
    for (int i=0; i<nRows; ++i)
       std::cout << densities[i] << "  ";
    std::cout << std::endl;
    
    return 0;
 }

 int selectRows()
 {
         const string inFile("atestfil.fit");
         const string outFile("btestfil.fit");
         const string newFile("ctestfil.fit");
         remove(newFile.c_str());
       
         // test 1: write to a new file
         std::auto_ptr<FITS> pInfile(new FITS(inFile,Write,string("PLANETS_ASCII")));
         FITS* infile = pInfile.get();
         std::auto_ptr<FITS> pNewfile(new FITS(newFile,Write));
         ExtHDU& source = infile->extension("PLANETS_ASCII");
         const string expression("DENSITY > 3.0");
         
         
         Table& sink1 = pNewfile.get()->filter(expression,source,false,true);

                
         std::cout << sink1 << std::endl;
         
         // test 2: write a new HDU to the current file, overwrite false, read true.
         // AS OF 7/2/01 does not work because of a bug in cfitsio, but does not
         // crash, simply writes a new header to the file without also writing the
         // selected data.
         Table& sink2 = infile->filter(expression,source,false,true);
         
         std::cout << sink2 << std::endl;
         
         // reset the source file back to the extension in question.
         source = infile->extension("PLANETS_ASCII");
          
         // test 3: overwrite the current HDU with filtered data.
                    
         Table& sink3 = infile->filter(expression,source,true,true);
         
         std::cout << sink3 << std::endl;
          
        
         return 0;       
 }
