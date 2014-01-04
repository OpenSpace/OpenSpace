/* m2bodtrn.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure      M2BODTRN ( Body name and code translation ) */
/* Subroutine */ int m2bodtrn_0_(int n__, char *name__, integer *code, 
	logical *found, ftnlen name_len)
{
    /* Initialized data */

    static integer codes[415] = { 199,299,399,499,599,699,799,899,999,301,401,
	    402,501,502,503,504,505,506,507,508,509,510,511,512,513,514,514,
	    515,515,516,516,601,602,603,604,605,606,607,608,609,610,610,611,
	    611,612,612,613,613,614,614,615,615,616,616,617,617,701,702,703,
	    704,705,706,706,707,707,708,708,709,709,710,710,711,711,712,712,
	    713,713,714,714,715,715,801,802,803,804,805,806,807,808,901,901,
	    -12,-12,-12,-18,-18,-27,-27,-30,-30,-31,-31,-32,-32,-46,-46,-47,
	    -47,-58,-66,-67,-77,-77,-78,-94,-94,-112,0,0,1,2,3,3,3,3,4,5,6,7,
	    8,9,10,9511010,2431010,1000001,1000002,1000003,1000004,1000005,
	    1000006,1000007,1000008,1000009,1000010,1000011,1000012,1000013,
	    1000014,1000015,1000016,1000017,1000018,1000019,1000020,1000021,
	    1000022,1000023,1000024,1000025,1000026,1000027,1000028,1000029,
	    1000030,1000031,1000032,1000033,1000034,1000035,1000036,1000037,
	    1000038,1000039,1000040,1000041,1000042,1000043,1000044,1000045,
	    1000046,1000047,1000048,1000049,1000050,1000051,1000052,1000053,
	    1000054,1000055,1000056,1000057,1000058,1000059,1000060,1000061,
	    1000062,1000063,1000064,1000065,1000066,1000067,1000068,1000069,
	    1000070,1000071,1000072,1000073,1000074,1000075,1000076,1000077,
	    1000078,1000079,1000080,1000081,1000082,1000083,1000084,1000085,
	    1000086,1000087,1000088,1000089,1000090,1000091,1000092,1000093,
	    1000094,1000095,1000096,1000097,1000098,1000099,1000100,1000101,
	    1000102,1000103,1000104,1000105,1000106,1000107,1000108,1000109,
	    1000110,1000111,1000112,1000113,1000114,1000115,1000116,1000117,
	    1000118,1000119,1000120,1000121,1000122,1000123,1000124,1000125,
	    1000126,1000127,1000128,1000129,1000130,50000001,50000002,
	    50000003,50000004,50000005,50000006,50000007,50000008,50000009,
	    50000010,50000011,50000012,50000013,50000014,50000015,50000016,
	    50000017,50000018,50000019,50000020,50000021,50000022,50000023,
	    -40,-344,-344,2000433,2000253,618,-59,-53,-53,-93,-93,-82,-82,
	    -150,-55,399001,399002,399003,399004,1000131,1000132,-550,-550,
	    -550,-550,-90,-95,-81 };
    static char names[32*415] = "MERCURY                         " "VENUS   "
	    "                        " "EARTH                           " 
	    "MARS                            " "JUPITER                     "
	    "    " "SATURN                          " "URANUS                "
	    "          " "NEPTUNE                         " "PLUTO           "
	    "                " "MOON                            " "PHOBOS    "
	    "                      " "DEIMOS                          " "IO  "
	    "                            " "EUROPA                          " 
	    "GANYMEDE                        " "CALLISTO                    "
	    "    " "AMALTHEA                        " "HIMALIA               "
	    "          " "ELARA                           " "PASIPHAE        "
	    "                " "SINOPE                          " "LYSITHEA  "
	    "                      " "CARME                           " "ANAN"
	    "KE                          " "LEDA                            " 
	    "1979J2                          " "THEBE                       "
	    "    " "1979J1                          " "ADRASTEA              "
	    "          " "1979J3                          " "METIS           "
	    "                " "MIMAS                           " "ENCELADUS "
	    "                      " "TETHYS                          " "DION"
	    "E                           " "RHEA                            " 
	    "TITAN                           " "HYPERION                    "
	    "    " "IAPETUS                         " "PHOEBE                "
	    "          " "1980S1                          " "JANUS           "
	    "                " "1980S3                          " "EPIMETHEUS"
	    "                      " "1980S6                          " "HELE"
	    "NE                          " "1980S13                         " 
	    "TELESTO                         " "1980S25                     "
	    "    " "CALYPSO                         " "1980S28               "
	    "          " "ATLAS                           " "1980S27         "
	    "                " "PROMETHEUS                      " "1980S26   "
	    "                      " "PANDORA                         " "ARIE"
	    "L                           " "UMBRIEL                         " 
	    "TITANIA                         " "OBERON                      "
	    "    " "MIRANDA                         " "1986U7                "
	    "          " "CORDELIA                        " "1986U8          "
	    "                " "OPHELIA                         " "1986U9    "
	    "                      " "BIANCA                          " "1986"
	    "U4                          " "CRESSIDA                        " 
	    "1986U6                          " "DESDEMONA                   "
	    "    " "1986U3                          " "JULIET                "
	    "          " "1986U1                          " "PORTIA          "
	    "                " "1986U2                          " "ROSALIND  "
	    "                      " "1986U5                          " "BELI"
	    "NDA                         " "1985U1                          " 
	    "PUCK                            " "TRITON                      "
	    "    " "NEREID                          " "NAIAD                 "
	    "          " "THALASSA                        " "DESPINA         "
	    "                " "GALATEA                         " "LARISSA   "
	    "                      " "PROTEUS                         " "1978"
	    "P1                          " "CHARON                          " 
	    "VENUS ORBITER                   " "P12                         "
	    "    " "PIONEER 12                      " "MGN                   "
	    "          " "MAGELLAN                        " "VK1             "
	    "                " "VIKING 1 ORBITER                " "VK2       "
	    "                      " "VIKING 2 ORBITER                " "VG1 "
	    "                            " "VOYAGER 1                       " 
	    "VG2                             " "VOYAGER 2                   "
	    "    " "MS-T5                           " "SAKIGAKE              "
	    "          " "PLANET-A                        " "SUISEI          "
	    "                " "VSOP                            " "VEGA 1    "
	    "                      " "VEGA 2                          " "GLL "
	    "                            " "GALILEO ORBITER                 " 
	    "GIOTTO                          " "MGS                         "
	    "    " "MARS GLOBAL SURVEYOR            " "ICE                   "
	    "          " "SSB                             " "SOLAR SYSTEM BAR"
	    "YCENTER         " "MERCURY BARYCENTER              " "VENUS BARY"
	    "CENTER                " "EMB                             " "EART"
	    "H MOON BARYCENTER           " "EARTH-MOON BARYCENTER           " 
	    "EARTH BARYCENTER                " "MARS BARYCENTER             "
	    "    " "JUPITER BARYCENTER              " "SATURN BARYCENTER     "
	    "          " "URANUS BARYCENTER               " "NEPTUNE BARYCENT"
	    "ER              " "PLUTO BARYCENTER                " "SUN       "
	    "                      " "GASPRA                          " "IDA "
	    "                            " "AREND                           " 
	    "AREND-RIGAUX                    " "ASHBROOK-JACKSON            "
	    "    " "BOETHIN                         " "BORRELLY              "
	    "          " "BOWELL-SKIFF                    " "BRADFIELD       "
	    "                " "BROOKS 2                        " "BRORSEN-ME"
	    "TCALF                 " "BUS                             " "CHER"
	    "NYKH                        " "CHURYUMOV-GERASIMENKO           " 
	    "CIFFREO                         " "CLARK                       "
	    "    " "COMAS SOLA                      " "CROMMELIN             "
	    "          " "D'ARREST                        " "DANIEL          "
	    "                " "DE VICO-SWIFT                   " "DENNING-FU"
	    "JIKAWA                " "DU TOIT 1                       " "DU T"
	    "OIT-HARTLEY                 " "DUTOIT-NEUJMIN-DELPORTE         " 
	    "DUBIAGO                         " "ENCKE                       "
	    "    " "FAYE                            " "FINLAY                "
	    "          " "FORBES                          " "GEHRELS 1       "
	    "                " "GEHRELS 2                       " "GEHRELS 3 "
	    "                      " "GIACOBINI-ZINNER                " "GICL"
	    "AS                          " "GRIGG-SKJELLERUP                " 
	    "GUNN                            " "HALLEY                      "
	    "    " "HANEDA-CAMPOS                   " "HARRINGTON            "
	    "          " "HARRINGTON-ABELL                " "HARTLEY 1       "
	    "                " "HARTLEY 2                       " "HARTLEY-IR"
	    "AS                    " "HERSCHEL-RIGOLLET               " "HOLM"
	    "ES                          " "HONDA-MRKOS-PAJDUSAKOVA         " 
	    "HOWELL                          " "IRAS                        "
	    "    " "JACKSON-NEUJMIN                 " "JOHNSON               "
	    "          " "KEARNS-KWEE                     " "KLEMOLA         "
	    "                " "KOHOUTEK                        " "KOJIMA    "
	    "                      " "KOPFF                           " "KOWA"
	    "L 1                         " "KOWAL 2                         " 
	    "KOWAL-MRKOS                     " "KOWAL-VAVROVA               "
	    "    " "LONGMORE                        " "LOVAS 1               "
	    "          " "MACHHOLZ                        " "MAURY           "
	    "                " "NEUJMIN 1                       " "NEUJMIN 2 "
	    "                      " "NEUJMIN 3                       " "OLBE"
	    "RS                          " "PETERS-HARTLEY                  " 
	    "PONS-BROOKS                     " "PONS-WINNECKE               "
	    "    " "REINMUTH 1                      " "REINMUTH 2            "
	    "          " "RUSSELL 1                       " "RUSSELL 2       "
	    "                " "RUSSELL 3                       " "RUSSELL 4 "
	    "                      " "SANGUIN                         " "SCHA"
	    "UMASSE                      " "SCHUSTER                        " 
	    "SCHWASSMANN-WACHMANN 1          " "SCHWASSMANN-WACHMANN 2      "
	    "    " "SCHWASSMANN-WACHMANN 3          " "SHAJN-SCHALDACH       "
	    "          " "SHOEMAKER 1                     " "SHOEMAKER 2     "
	    "                " "SHOEMAKER 3                     " "SINGER-BRE"
	    "WSTER                 " "SLAUGHTER-BURNHAM               " "SMIR"
	    "NOVA-CHERNYKH               " "STEPHAN-OTERMA                  " 
	    "SWIFT-GEHRELS                   " "TAKAMIZAWA                  "
	    "    " "TAYLOR                          " "TEMPEL 1              "
	    "          " "TEMPEL 2                        " "TEMPEL-TUTTLE   "
	    "                " "TRITTON                         " "TSUCHINSHA"
	    "N 1                   " "TSUCHINSHAN 2                   " "TUTT"
	    "LE                          " "TUTTLE-GIACOBINI-KRESAK         " 
	    "VAISALA 1                       " "VAN BIESBROECK              "
	    "    " "VAN HOUTEN                      " "WEST-KOHOUTEK-IKEMURA "
	    "          " "WHIPPLE                         " "WILD 1          "
	    "                " "WILD 2                          " "WILD 3    "
	    "                      " "WIRTANEN                        " "WOLF"
	    "                            " "WOLF-HARRINGTON                 " 
	    "LOVAS 2                         " "URATA-NIIJIMA               "
	    "    " "WISEMAN-SKIFF                   " "HELIN                 "
	    "          " "MUELLER                         " "SHOEMAKER-HOLT 1"
	    "                " "HELIN-ROMAN-CROCKETT            " "HARTLEY 3 "
	    "                      " "PARKER-HARTLEY                  " "HELI"
	    "N-ROMAN-ALU 1               " "WILD 4                          " 
	    "MUELLER 2                       " "MUELLER 3                   "
	    "    " "SHOEMAKER-LEVY 1                " "SHOEMAKER-LEVY 2      "
	    "          " "HOLT-OLMSTEAD                   " "METCALF-BREWINGT"
	    "ON              " "LEVY                            " "SHOEMAKER-"
	    "LEVY 9                " "SHOEMAKER-LEVY 9-W              " "SHOE"
	    "MAKER-LEVY 9-V              " "SHOEMAKER-LEVY 9-U              " 
	    "SHOEMAKER-LEVY 9-T              " "SHOEMAKER-LEVY 9-S          "
	    "    " "SHOEMAKER-LEVY 9-R              " "SHOEMAKER-LEVY 9-Q    "
	    "          " "SHOEMAKER-LEVY 9-P              " "SHOEMAKER-LEVY 9"
	    "-N              " "SHOEMAKER-LEVY 9-M              " "SHOEMAKER-"
	    "LEVY 9-L              " "SHOEMAKER-LEVY 9-K              " "SHOE"
	    "MAKER-LEVY 9-J              " "SHOEMAKER-LEVY 9-H              " 
	    "SHOEMAKER-LEVY 9-G              " "SHOEMAKER-LEVY 9-F          "
	    "    " "SHOEMAKER-LEVY 9-E              " "SHOEMAKER-LEVY 9-D    "
	    "          " "SHOEMAKER-LEVY 9-C              " "SHOEMAKER-LEVY 9"
	    "-B              " "SHOEMAKER-LEVY 9-A              " "SHOEMAKER-"
	    "LEVY 9-Q1             " "SHOEMAKER-LEVY 9-P2             " "CLEM"
	    "ENTINE                      " "GLL PROBE                       " 
	    "GALILEO PROBE                   " "EROS                        "
	    "    " "MATHILDE                        " "PAN                   "
	    "          " "RADIOASTRON                     " "MARS PATHFINDER "
	    "                " "MPF                             " "NEAR      "
	    "                      " "NEAR EARTH ASTEROID RENDEZVOUS  " "CASS"
	    "INI                         " "CAS                             " 
	    "CASSINI HUYGENS PROBE           " "ULYSSES                     "
	    "    " "GOLDSTONE                       " "CANBERRA              "
	    "          " "MADRID                          " "USUDA           "
	    "                " "HYAKUTAKE                       " "HALE-BOPP "
	    "                      " "MARS-96                         " "M96 "
	    "                            " "MARS 96                         " 
	    "MARS96                          " "CASSINI SIMULATION          "
	    "    " "MGS SIMULATION                  " "CASSINI ITL           "
	    "          ";
    static logical init = TRUE_;
    static integer nnam = 315;

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer ncod, i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen), 
	    bodn2c_(char *, integer *, logical *, ftnlen);
    extern integer bschoc_(char *, integer *, char *, integer *, ftnlen, 
	    ftnlen), bschoi_(integer *, integer *, integer *, integer *);
    static integer ordcod[415], ordnam[415];
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    static char tmpnam[32];
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen), cmprss_(char *, 
	    integer *, char *, char *, ftnlen, ftnlen, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int m2bodini_(char *, integer *, integer *, 
	    integer *, integer *, integer *, ftnlen);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This is the umbrella routine that contains entry points for */
/*     translating between body names and NAIF integer codes and */
/*     for defining new name/code pairs. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     NAIF_IDS */

/* $ Keywords */

/*     BODY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     NAME       I   M2BODN2C and M2BODDEF */
/*                O   M2BODC2N */
/*     CODE       I   M2BODC2N and M2BODDEF */
/*                O   M2BODN2C */
/*     FOUND      O   M2BODN2C and M2BODC2N */
/*     MAXL       P   (All) */
/*     MAXP       P   M2BODDEF */

/* $ Detailed_Input */

/*     See the entry points for a discussion of their arguments. */

/* $ Detailed_Output */

/*     See the entry points for a discussion of their arguments. */

/* $ Parameters */

/*     MAXL        is the maximum length of a name.  MAXL should only */
/*                 be increased if names longer than the current value */
/*                 need to be supported.  If MAXL is decreased the */
/*                 default names may be truncated. */

/*     MAXP        is the maximum number of name/code pairs that can */
/*                 be defined via M2BODDEF.  It is the limit */
/*                 on the number of definitions over and above the */
/*                 number of default definitions.  The user may alter */
/*                 the the value of MAXP, however, it must remain a */
/*                 positive integer. */

/* $ Exceptions */

/*     1) If M2BODTRN is called directly, the error SPICE(BOGUSENTRY) is */
/*        signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     M2BODTRN should never be called directly, but should instead be */
/*     accessed through its entry points: */

/*        M2BODN2C      Body name to code */

/*        M2BODC2N      Body code to name */

/*        M2BODDEF      Body name/code definition */

/*     M2BODN2C and M2BODC2N perform translations between body names */
/*     and their corresponding integer codes which are used */
/*     in SPK and PCK files and routines.  A set of name/code */
/*     pairs are automatically defined during the first call to */
/*     one of these entry points.  Additional name/code pairs may */
/*     be defined via M2BODDEF for two purposes: */

/*        1.  to associate another, perhaps more familiar or */
/*            abbreviated, name with a particular body integer */
/*            code that has already been defined, or */

/*        2.  to define a new body integer code and name, */

/*     Each body has a unique integer code, but may have several */
/*     names.  Thus you may associate more than one name with */
/*     a particular integer code.  However, associating more */
/*     than one integer code with a particular name creates ambiguity. */
/*     Therefore, once a name has been defined, it may not be redefined */
/*     with a different integer code. */

/*     For example, Europa is the name of the second satellite of */
/*     Jupiter, and has the NAIF integer code 502.  Thus (EUROPA, 502) */
/*     is one of the default definitions.  Europa is also the name */
/*     of an asteroid.  Suppose you were able to associate the asteroid */
/*     integer code with the name EUROPA.  Then when you call M2BODN2C to */
/*     translate the name EUROPA, which code should be returned?  That */
/*     of the asteroid or 502? */

/*     M2BODDEF prevents this ambiguity by signalling an error if the */
/*     specified name has already been defined with a different code. */
/*     In the case of EUROPA, you may want to use the name ASTEROID */
/*     EUROPA.  The set of default definitions are listed in DATA */
/*     statements in the umbrella routine M2BODTRN for easy reference. */

/* $ Examples */

/*     1.  In the following code fragment, SPKEZ computes the state */
/*     (position and velocity) of Jupiter as seen from the Galileo */
/*     Orbiter.  It requires the NAIF integer codes of the target and */
/*     observer, so we use M2BODN2C to convert names to integer codes */
/*     for those bodies. */

/*       CALL M2BODN2C( 'JUPITER',         TARGET, FOUND ) */

/*       CALL M2BODN2C( 'GALILEO ORBITER', OBSRVR, FOUND ) */

/*       CALL SPKEZ   ( TARGET, EPOCH, FRAME, ABCORR, OBSRVR, STATE, LT) */


/*     2.  In this example, we assume that M2BODDEF has not been called. */
/*         Thus, only the set of default name/code pairs has been */
/*         defined. */

/*     Given these names, M2BODN2C will return the following codes: */

/*        Name                         Code    Found? */
/*        ------------------------   ------    ------ */
/*        'EARTH'                       399    Yes */
/*        '  Earth '                    399    Yes */
/*        'EMB'                           3    Yes */
/*        'Solar System Barycenter'       0    Yes */
/*        'SolarSystemBarycenter'         -    No */
/*        'SSB'                           0    Yes */
/*        'Voyager 2'                   -32    Yes */
/*        'U.S.S. Enterprise'             -    No */
/*        ' '                             -    No */
/*        'Halley's Comet'                -    No */


/*     and, given these codes, M2BODC2N will return the following names: */

/*        Code        Name                        Found? */
/*        -------     -------------------         ------ */
/*        399         'EARTH'                     Yes */
/*          0         'SOLAR SYSTEM BARYCENTER'   Yes */
/*          3         'EARTH BARYCENTER'          Yes */
/*        -77         'GALILEO ORBITER'           Yes */
/*         11          -                          No */
/*         -1          -                          No */


/*     3.  This example shows how to define a name/code pair. */
/*     You may associate a new name with a particular code that */
/*     has already been defined: */

/*            CALL M2BODDEF ( 'JB', 5 ) */

/*     You may also define the name and integer code for a new body: */

/*            CALL M2BODDEF ( 'Asteroid Frank', 20103456 ) */

/*     After these calls to M2BODDEF, M2BODN2C would return the following */
/*     translations: */

/*        Name                         Code    Found? */
/*        ------------------------   ------    ------ */
/*        'JB'                            5    Yes */
/*        'Jupiter Barycenter'            5    Yes */
/*        'ASTEROID FRANK'         20103456    Yes */
/*        'ASTEROIDFRANK'                 -    No */
/*        'Frank'                         -    No */

/*     and M2BODC2N will return these translations: */

/*        Code        Name                     Found? */
/*        -------     -------------------      ------ */
/*               5    'JB'                     Yes */
/*        20103456    'Asteroid Frank'         Yes */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J.E. McLean    (JPL) */
/*     H.A. Neilan    (JPL) */
/*     B.V. Semenov   (JPL) */
/*     M.J. Spencer   (JPL) */
/*     W.L. Taber     (JPL) */
/*     K.S. Zukor     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 22-MAY-1996 (WLT) */

/*        Added the id-code for Comet Hyakutake, Comet Hale-Bopp, */
/*        Mars 96, Cassini Simulation, MGS Simulation. */

/* -    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS) */

/*        Renamed umbrella subroutine and entry points to */
/*        correspond private routine convention (M2...). Added IDs for */
/*        tracking stations Goldstone (399001), Canberra (399002), */
/*        Madrid (399003), Usuda (399004). */

/* -    Beta Version 2.2.0, 01-AUG-1995 (HAN) */

/*        Added the IDs for Near Earth Asteroid Rendezvous (-93), */
/*        Mars Pathfinder (-53), Ulysses (-55), VSOP (-58), */
/*        Radioastron (-59), Cassini spacecraft (-82), and Cassini */
/*        Huygens probe (-150). */
/*        Mars Observer (-94) was replaced with Mars Global */
/*        Surveyor (-94). */

/* -    Beta Version 2.1.0, 15-MAR-1995 (KSZ) (HAN) */

/*        Two Shoemaker Levy 9 fragments were added, Q1 and P2 */
/*        (IDs 50000022 and 50000023). Two asteroids were added, */
/*        Eros and Mathilde (IDs 2000433 and 2000253). The */
/*        Saturnian satellite Pan (ID 618) was added. */

/* -    Beta Version 2.0.0, 03-FEB-1995 (NJB) */

/*        The Galileo probe (ID -344) has been added to the permanent */
/*        collection. */

/* -    Beta Version 1.0.0, 29-APR-1994 (MJS) */

/*        SPICELIB symbol tables are no longer used. Instead, two order */
/*        vectors are used to index the NAMES and CODES arrays. Also, */
/*        this version does not support reading body name ID pairs from a */
/*        file. */

/* -    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT) */

/*       The body id's for the Uranian satellites discovered by Voyager */
/*       were modified to conform to those established by the IAU */
/*       nomenclature committee.  In addition the id's for Gaspra and */
/*       Ida were added. */

/* -    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT) */

/*       Some items that were previously considered errors were removed */
/*       and some minor modifications were made to improve the */
/*       robustness of the routines. */

/* -    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM) */


/* -& */

/*     SPICELIB functions */


/*     Functions */


/*     The parameters here are for ease in maintaining the */
/*     large collection of automatic names that are stored */
/*     in data statements.  To insert a name/code pair in the */
/*     block from BEGx to ENDx, redefine ENDx to be */
/*     one larger than its current definition.  Recompiling */
/*     will automatically modify all the other parameters. */


/*     Local variables */


/*     Introducing the permanent collection. */

    switch(n__) {
	case 1: goto L_m2bodn2c;
	case 2: goto L_m2bodc2n;
	case 3: goto L_m2boddef;
	}


/*     The 851, 852, ... codes are temporary codes for the newly- */
/*     discovered satellites of Neptune.  These will go away when */
/*     the official codes are assigned.  The codes listed above */
/*     do not include these temporary assignments. */

/*     The proposed names are the following: */

/*        1989N1 = Proteus */
/*        1989N2 = Larissa */
/*        1989N3 = Despina */
/*        1989N4 = Galatea */
/*        1989N5 = Thalassa */
/*        1989N6 = Naiad */


/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("M2BODTRN", (ftnlen)8);
    }

/*     This routine should never be called. If it is called, */
/*     an error is signalled. */

    setmsg_("M2BODTRN: You have called an entry which performs no run-time f"
	    "unction. This may indicate a bug. Please check the documentation"
	    " for the subroutine M2BODTRN.", (ftnlen)156);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("M2BODTRN", (ftnlen)8);
    return 0;
/* $Procedure M2BODN2C ( Body name to code ) */

L_m2bodn2c:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Translate the name of a body into the integer code for */
/*     that body. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     NAIF_IDS */

/* $ Keywords */

/*     BODY */
/*     CONVERSION */

/* $ Declarations */

/*     CHARACTER*(*)         NAME */
/*     INTEGER               CODE */
/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     NAME       I   Body name to be translated. */
/*     CODE       O   Integer code for that body. */
/*     FOUND      O   True if translated, otherwise false. */
/*     MAXL       P   Max name length. */

/* $ Detailed_Input */

/*     NAME        is an arbitrary name of a body which could be */
/*                 a planet, satellite, barycenter, spacecraft, */
/*                 asteroid, comet, or other ephemeris object. */

/*                 Case and leading and trailing blanks in a name */
/*                 are not significant.  However when a name is made */
/*                 up of more than one word, they must be separated by */
/*                 at least one blank.  That is, all of the following */
/*                 strings are equivalent names: */

/*                         'JUPITER BARYCENTER' */
/*                         'Jupiter Barycenter' */
/*                         'JUPITER BARYCENTER   ' */
/*                         'JUPITER    BARYCENTER' */
/*                         '   JUPITER BARYCENTER' */

/*                 However, 'JUPITERBARYCENTER' is not equivalent to */
/*                 the names above. */

/*                 When ignoring trailing blanks, NAME must have fewer */
/*                 than MAXL characters. */

/* $ Detailed_Output */

/*     CODE        is the NAIF or user-defined integer code for the */
/*                 named body.  CODE will have at most MAXL digits */
/*                 including a minus sign if CODE is negative. */

/*     FOUND       is true if NAME has a translation.  Otherwise, FOUND */
/*                 is false. */

/* $ Parameters */

/*     MAXL        is the maximum length of a name.  MAXL should only */
/*                 be increased if names longer than the current value */
/*                 need to be supported.  If MAXL is decreased the */
/*                 default names may be truncated. */

/* $ Exceptions */

/*     NONE */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     M2BODN2C is one of three related entry points, */

/*        M2BODN2C      Body name to code */

/*        M2BODC2N      Body code to name */

/*        M2BODDEF      Body name/code definition */

/*     M2BODN2C and M2BODC2N perform translations between body names */
/*     and their corresponding integer codes which are used */
/*     in SPK and PCK files and routines.  A set of name/code */
/*     pairs are automatically defined during the first call to */
/*     one of these entry points.  Additional name/code pairs may */
/*     be defined via M2BODDEF. */

/* $ Examples */

/*     1.  In the following code fragment, SPKEZ computes the state */
/*     (position and velocity) of Jupiter as seen from the Galileo */
/*     Orbiter.  It requires the NAIF integer codes of the target and */
/*     observer, so we use M2BODN2C to convert names to integer codes */
/*     for those bodies. */

/*       CALL M2BODN2C( 'JUPITER',         TARGET, FOUND ) */

/*       CALL M2BODN2C( 'GALILEO ORBITER', OBSRVR, FOUND ) */

/*       CALL SPKEZ   ( TARGET, EPOCH, FRAME, ABCORR, OBSRVR, STATE, LT ) */


/*     2.  In this example, we assume that neither M2BODDEF has not been */
/*         called.  Thus, only the set of default name/code pairs has */
/*         been defined. */

/*     Given these names, M2BODN2C will return the following codes: */

/*        Name                         Code    Found? */
/*        ------------------------   ------    ------ */
/*        'EARTH'                       399    Yes */
/*        '  Earth '                    399    Yes */
/*        'EMB'                           3    Yes */
/*        'Solar System Barycenter'       0    Yes */
/*        'SolarSystemBarycenter'         -    No */
/*        'SSB'                           0    Yes */
/*        'Voyager 2'                   -32    Yes */
/*        'U.S.S. Enterprise'             -    No */
/*        ' '                             -    No */
/*        'Halley's Comet'                -    No */

/*     and, given these codes, M2BODC2N will return the following names: */

/*        Code        Name                        Found? */
/*        -------     -------------------         ------ */
/*        399         'EARTH'                     Yes */
/*          0         'SOLAR SYSTEM BARYCENTER'   Yes */
/*          3         'EARTH BARYCENTER'          Yes */
/*        -77         'GALILEO ORBITER'           Yes */
/*         11          -                          No */
/*         -1          -                          No */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J.E. McLean    (JPL) */
/*     B.V. Semenov   (JPL) */
/*     M.J. Spencer   (JPL) */
/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 29-FEB-1996 (WLT) */

/*        Added the id-code for Comet Hyakutake, Comet Hale-Bopp. */

/* -    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS) */

/*        Renamed to M2BODN2C (BVS) */

/* -    Beta Version 1.0.0, 29-APR-1994 (MJS) */

/*        SPICELIB symbol tables are no longer used. Instead, two order */
/*        vectors are used to index the NAMES and CODES arrays. */

/* -    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT) */

/*       The body id's for the Uranian satellites discovered by Voyager */
/*       were modified to conform to those established by the IAU */
/*       nomenclature committee.  In addition the id's for Gaspra and */
/*       Ida were added. */

/* -    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT) */

/*       Items that were previously considered errors were downgraded */
/*       to simply be exceptions.  Any NAME is a legitimate input now. */
/*       If its not in the table, the FOUND flag is just set to .FALSE. */

/* -    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM) */


/* -& */
/* $ Index_Entries */

/*     body name to code */

/* -& */

/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("M2BODN2C", (ftnlen)8);
    }
    *found = FALSE_;
    bodn2c_(name__, code, found, name_len);
    if (*found) {
	chkout_("M2BODN2C", (ftnlen)8);
	return 0;
    }

/*     Get the order vectors for the names and codes. */

    if (init) {
	init = FALSE_;
	m2bodini_(names, &nnam, codes, &ncod, ordnam, ordcod, (ftnlen)32);
    }

/*     Return the CODE associated with the name. */

    ljust_(name__, tmpnam, name_len, (ftnlen)32);
    ucase_(tmpnam, tmpnam, (ftnlen)32, (ftnlen)32);
    cmprss_(" ", &c__1, tmpnam, tmpnam, (ftnlen)1, (ftnlen)32, (ftnlen)32);
    i__ = bschoc_(tmpnam, &nnam, names, ordnam, (ftnlen)32, (ftnlen)32);
    if (i__ != 0) {
	*code = codes[(i__1 = i__ - 1) < 415 && 0 <= i__1 ? i__1 : s_rnge(
		"codes", i__1, "m2bodtrn_", (ftnlen)1264)];
	*found = TRUE_;
    } else {
	i__1 = nnam;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (eqstr_(tmpnam, names + (((i__2 = i__ - 1) < 415 && 0 <= i__2 ?
		     i__2 : s_rnge("names", i__2, "m2bodtrn_", (ftnlen)1271)) 
		    << 5), (ftnlen)32, (ftnlen)32)) {
		*code = codes[(i__2 = i__ - 1) < 415 && 0 <= i__2 ? i__2 : 
			s_rnge("codes", i__2, "m2bodtrn_", (ftnlen)1272)];
		*found = TRUE_;
		chkout_("M2BODN2C", (ftnlen)8);
		return 0;
	    }
	}
    }
    chkout_("M2BODN2C", (ftnlen)8);
    return 0;
/* $Procedure M2BODC2N ( Body code to name ) */

L_m2bodc2n:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Translate the integer code of a body into a common name for */
/*     that body. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     NAIF_IDS */

/* $ Keywords */

/*     BODY */
/*     CONVERSION */

/* $ Declarations */

/*     INTEGER               CODE */
/*     CHARACTER*(*)         NAME */
/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     CODE       I   Integer code to be translated. */
/*     NAME       O   Common name for the body identified by CODE. */
/*     FOUND      O   True if translated, otherwise false. */
/*     MAXL       P   Max name length. */

/* $ Detailed_Input */

/*     CODE        is an integer code for a body --- */
/*                 a planet, satellite, barycenter, spacecraft, */
/*                 asteroid, comet, or other ephemeris object. */

/* $ Detailed_Output */

/*     NAME        is the common name of the body identified by CODE. */
/*                 If CODE has more than one translation, then the */
/*                 most recently defined NAME corresponding to CODE */
/*                 is returned.  NAME will have the exact format (case */
/*                 and blanks) as when the name/code pair was defined. */

/*     FOUND       is true if CODE has a translation.  Otherwise, FOUND */
/*                 is false. */

/* $ Parameters */

/*     MAXL        is the maximum length of a name.  MAXL should only */
/*                 be increased if names longer than the current value */
/*                 need to be supported.  If MAXL is decreased the */
/*                 default names may be truncated. */

/* $ Exceptions */

/*     NONE */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     M2BODC2N is one of three related entry points, */

/*        M2BODN2C      Body name to code */

/*        M2BODC2N      Body code to name */

/*        M2BODDEF      Body name/code definition */

/*     M2BODN2C and M2BODC2N perform translations between body names */
/*     and their corresponding integer codes which are used */
/*     in SPK and PCK files and routines.  A set of name/code */
/*     pairs are automatically defined during the first call to */
/*     one of these entry points.  Additional name/code pairs may */
/*     be defined via M2BODDEF. */

/* $ Examples */

/*     1.  Suppose you ran the utility program SPACIT to summarize */
/*     an SPK ephemeris file and the following data was output */
/*     to the terminal screen. */

/*         ---------------------------------------------------------- */
/*         Segment identifier: JPL archive 21354 */
/*         Body        : -77                         Center     : 399 */
/*         From        : 1990 DEC 08 18:00:00.000 */
/*         To          : 1990 DEC 10 21:10:00.000 */
/*         Reference   : DE-200                      SPK Type    :1 */
/*         ---------------------------------------------------------- */

/*     You could write a program to translate the body codes */
/*     shown in the SPACIT output: */

/*        CALL M2BODC2N ( -77, BODY,   FOUND ) */
/*        CALL M2BODC2N ( 399, CENTER, FOUND ) */

/*        IF ( FOUND ) THEN */

/*           WRITE ( *,* ) 'BODY:    -77 = ', BODY */
/*           WRITE ( *,* ) 'CENTER:  399 = ', CENTER */

/*        END IF */

/*     You could also read the body and center codes directly from */
/*     the SPK files, using the appropriate DAF routines, and then */
/*     translate them, as above. */


/*     2.  In this example, we assume that neither M2BODDEF has not been */
/*         called.  Thus, only the set of default name/code pairs has */
/*         been defined. */

/*     Given these names, M2BODN2C will return the following codes: */

/*        Name                         Code    Found? */
/*        ------------------------   ------    ------ */
/*        'EARTH'                       399    Yes */
/*        '  Earth '                    399    Yes */
/*        'EMB'                           3    Yes */
/*        'Solar System Barycenter'       0    Yes */
/*        'SolarSystemBarycenter'         -    No */
/*        'SSB'                           0    Yes */
/*        'Voyager 2'                   -32    Yes */
/*        'U.S.S. Enterprise'             -    No */
/*        ' '                             -    No */
/*        'Halley's Comet'                -    No */


/*     and, given these codes, M2BODC2N will return the following names: */

/*        Code        Name                        Found? */
/*        -------     -------------------         ------ */
/*        399         'EARTH'                     Yes */
/*          0         'SOLAR SYSTEM BARYCENTER'   Yes */
/*          3         'EARTH BARYCENTER'          Yes */
/*        -77         'GALILEO ORBITER'           Yes */
/*         11          -                          No */
/*         -1          -                          No */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J.E. McLean    (JPL) */
/*     B.V. Semenov   (JPL) */
/*     M.J. Spencer   (JPL) */
/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 29-FEB-1996 (WLT) */

/*        Added the id-code for Comet Hyakutake, Comet Hale-Bopp. */

/* -    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS) */

/*        Renamed to M2BODC2N (BVS) */

/* -    Beta Version 1.0.0, 29-APR-1994 (MJS) */

/*        SPICELIB symbol tables are no longer used. Instead, two order */
/*        vectors are used to index the NAMES and CODES arrays. */

/* -    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT) */

/*       The body id's for the Uranian satellites discovered by Voyager */
/*       were modified to conform to those established by the IAU */
/*       nomenclature committee.  In addition the id's for Gaspra and */
/*       Ida were added. */

/* -    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT) */

/*       Checks to see that the input integer code can be represented */
/*       as a character string were removed along with the exceptions */
/*       associated with these checks.  It is now the responsibility */
/*       of a maintenance programmer to make sure that MAXL is large */
/*       enough to allow any integer to be converted to a string */
/*       representation. */

/* -    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM) */


/* -& */
/* $ Index_Entries */

/*     body code to name */

/* -& */

/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("M2BODC2N", (ftnlen)8);
    }
    *found = FALSE_;

/*     Get the order vectors for the names and codes. */

    if (init) {
	init = FALSE_;
	m2bodini_(names, &nnam, codes, &ncod, ordnam, ordcod, (ftnlen)32);
    }

/*     Return the name associated with the CODE. */

    i__ = bschoi_(code, &ncod, codes, ordcod);
    if (i__ != 0) {
	s_copy(name__, names + (((i__1 = i__ - 1) < 415 && 0 <= i__1 ? i__1 : 
		s_rnge("names", i__1, "m2bodtrn_", (ftnlen)1551)) << 5), 
		name_len, (ftnlen)32);
	*found = TRUE_;
    }
    chkout_("M2BODC2N", (ftnlen)8);
    return 0;
/* $Procedure M2BODDEF ( Body name/code definition ) */

L_m2boddef:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Define a body name/code pair for later translation by */
/*     M2BODN2C or M2BODC2N. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     NAIF_IDS */

/* $ Keywords */

/*     BODY */
/*     CONVERSION */

/* $ Declarations */

/*     CHARACTER*(*)         NAME */
/*     INTEGER               CODE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     NAME       I   Common name of some body. */
/*     CODE       I   Integer code for that body. */
/*     MAXL       P   Max name length and max number of digits in code. */
/*     MAXP       P   Maximum number of name/code pair definitions. */

/* $ Detailed_Input */

/*     NAME        is an arbitrary name of a body which could be */
/*                 a planet, satellite, barycenter, spacecraft, */
/*                 asteroid, comet, or other ephemeris object. */

/*                 NAME must uniquely identify a body, so NAME must */
/*                 be distinct from all other names that have been */
/*                 defined.  (The list of default definitions are */
/*                 in DATA statements in M2BODTRN for easy reference.) */

/*                 Case and leading and trailing blanks in a name */
/*                 are not significant.  However when a name is made */
/*                 up of more than one word, they must be separated by */
/*                 at least one blank.  That is, all of the following */
/*                 strings are equivalent names: */

/*                         'JUPITER BARYCENTER' */
/*                         'Jupiter Barycenter' */
/*                         'JUPITER BARYCENTER   ' */
/*                         'JUPITER    BARYCENTER' */
/*                         '   JUPITER BARYCENTER' */

/*                 However, 'JUPITERBARYCENTER' is distinct from */
/*                 the names above. */

/*                 When ignoring trailing blanks, NAME must have fewer */
/*                 than MAXL characters. */

/*     CODE        is the integer code for the named body. */

/*                 CODE may already have a name as defined by a */
/*                 previous call to M2BODDEF or as part of the set of */
/*                 default definitions.  That previous definition will */
/*                 remain, and a translation of that name will still */
/*                 give the same CODE.  However, future translations */
/*                 of CODE will give the new NAME instead of the */
/*                 previous one.  This feature is useful for assigning */
/*                 a more familiar or abbreviated name to a body. */
/*                 For example, in addition to the default name for */
/*                 body 5, 'JUPITER BARYCENTER', you could define the */
/*                 abbreviation 'JB' to mean 5. */

/*                 CODE must have at most MAXL digits, where the */
/*                 minus sign is counted as a digit if CODE is negative. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     MAXL        is the maximum length of a name.  MAXL should only */
/*                 be increased if names longer than the current value */
/*                 need to be supported.  If MAXL is decreased the */
/*                 default names may be truncated. */

/*     MAXP        is the maximum number of name/code pairs that can */
/*                 be defined via M2BODDEF.  It is the limit */
/*                 on the number of definitions over and above the */
/*                 number of default definitions.  The user may alter */
/*                 the the value of MAXP, however, it must remain a */
/*                 positive integer. */

/* $ Exceptions */

/*     1) If NAME has already been associated with a different CODE, */
/*        the error SPICE(NAMENOTUNIQUE) is signalled. */

/*     2) If the maximum number of definitions is exceeded, a the */
/*        error SPICE(TOOMANYPAIRS) is signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     M2BODDEF is one of three related entry points, */

/*        M2BODN2C      Body name to code */

/*        M2BODC2N      Body code to name */

/*        M2BODDEF      Body name/code definition */

/*     M2BODN2C and M2BODC2N perform translations between body names */
/*     and their corresponding integer codes which are used */
/*     in SPK and PCK files and routines.  A set of name/code */
/*     pairs are automatically defined during the first call to */
/*     one of these entry points.  Additional name/code pairs may */
/*     be defined via M2BODDEF for two purposes: */

/*        1.  to associate another, perhaps more familiar or */
/*            abbreviated, name with a particular body integer */
/*            code that has already been defined, or */

/*        2.  to define a new body integer code and name, */

/*     Each body has a unique integer code, but may have several */
/*     names.  Thus you may associate more than one name with */
/*     a particular integer code.  However, associating more */
/*     than one integer code with a particular name creates ambiguity. */
/*     Therefore, once a name has been defined, it may not be redefined */
/*     with a different integer code. */

/*     For example, Europa is the name of the second satellite of */
/*     Jupiter, and has the NAIF integer code 502.  Thus (EUROPA, 502) */
/*     is one of the default definitions.  Europa is also the name */
/*     of an asteroid.  Suppose you were able to associate the asteroid */
/*     integer code with the name EUROPA.  Then when you call M2BODN2C to */
/*     translate the name EUROPA, which code should be returned?  That */
/*     of the asteroid or 502? */

/*     M2BODDEF prevent this ambiguity by signalling an error */
/*     if the specified name has already been defined with a */
/*     different code.  In the case of EUROPA, you may want to use the */
/*     name ASTEROID EUROPA.  The set of default definitions are listed */
/*     in DATA statements in the umbrella routine M2BODTRN for easy */
/*     reference. */

/* $ Examples */

/*     You may associate a new name with a particular code that */
/*     has already been defined: */

/*            CALL M2BODDEF ( 'JB', 5 ) */

/*     You may also define the name and integer code for a new body: */

/*            CALL M2BODDEF ( 'Asteroid Frank', 20103456 ) */

/*     After these calls to M2BODDEF, M2BODN2C would return the following */
/*     translations: */

/*        Name                         Code    Found? */
/*        ------------------------   ------    ------ */
/*        'JB'                            5    Yes */
/*        'Jupiter Barycenter'            5    Yes */
/*        'ASTEROID FRANK'         20103456    Yes */
/*        'ASTEROIDFRANK'                 -    No */
/*        'Frank'                         -    No */

/*     and M2BODC2N will return these translations: */

/*        Code        Name                     Found? */
/*        -------     -------------------      ------ */
/*               5    'JB'                     Yes */
/*        20103456    'Asteroid Frank'         Yes */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J.E. McLean    (JPL) */
/*     B.V. Semenov   (JPL) */
/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 29-FEB-1996 (WLT) */

/*        Added the id-code for Comet Hyakutake, Comet Hale-Bopp. */

/* -    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS) */

/*        Renamed to M2BODDEF (BVS). More careful checking for overflow */
/*        of the recognized names is now performed. */

/* -    Beta Version 1.0.0, 29-APR-1994 (MJS) */

/*        SPICELIB symbol tables are no longer used. Instead, two order */
/*        vectors are used to index the NAMES and CODES arrays. */

/* -    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT) */

/*       The body id's for the Uranian satellites discovered by Voyager */
/*       were modified to conform to those established by the IAU */
/*       nomenclature committee.  In addition the id's for Gaspra and */
/*       Ida were added. */

/* -    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT) */

/*       Checks to see that an integer code can be represented */
/*       as a character string were removed along with the exceptions */
/*       associated with these checks.  It is now the responsibility */
/*       of a maintenance programmer to make sure that MAXL is large */
/*       enough to allow any integer to be converted to a string */
/*       representation. */

/* -    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM) */

/* -& */
/* $ Index_Entries */

/*     body name/code definition */

/* -& */

/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("M2BODDEF", (ftnlen)8);
    }

/*     Initialize the order vectors if we haven't already. */

    if (init) {
	init = FALSE_;
	m2bodini_(names, &nnam, codes, &ncod, ordnam, ordcod, (ftnlen)32);
    }

/*     Make sure the name has not already been used. */

    ljust_(name__, tmpnam, name_len, (ftnlen)32);
    ucase_(tmpnam, tmpnam, (ftnlen)32, (ftnlen)32);
    cmprss_(" ", &c__1, tmpnam, tmpnam, (ftnlen)1, (ftnlen)32, (ftnlen)32);
    i__ = bschoc_(tmpnam, &nnam, names, ordnam, (ftnlen)32, (ftnlen)32);
    if (i__ != 0) {
	setmsg_("The name, '#', has already been used for body having id-cod"
		"e #.", (ftnlen)63);
	errch_("#", name__, (ftnlen)1, name_len);
	errint_("#", &codes[(i__1 = i__ - 1) < 415 && 0 <= i__1 ? i__1 : 
		s_rnge("codes", i__1, "m2bodtrn_", (ftnlen)1866)], (ftnlen)1);
	sigerr_("SPICE(NAMENOTUNIQUE)", (ftnlen)20);
	chkout_("M2BODDEF", (ftnlen)8);
	return 0;
    }

/*     Do we have room for another name/code pair? */

    if (nnam < 415) {
	++nnam;
    } else {
	setmsg_("There is no room available for adding '#'  to the list of n"
		"ame/code pairs. The number of names that can be supported is"
		" #.  This number has been reached. ", (ftnlen)154);
	errch_("#", name__, (ftnlen)1, name_len);
	errint_("#", &nnam, (ftnlen)1);
	sigerr_("SPICE(TOOMANYPAIRS)", (ftnlen)19);
	chkout_("M2BODDEF", (ftnlen)8);
	return 0;
    }

/*     Add NAME and CODE and reorder the vectors. */

    s_copy(names + (((i__1 = nnam - 1) < 415 && 0 <= i__1 ? i__1 : s_rnge(
	    "names", i__1, "m2bodtrn_", (ftnlen)1900)) << 5), tmpnam, (ftnlen)
	    32, (ftnlen)32);
    codes[(i__1 = nnam - 1) < 415 && 0 <= i__1 ? i__1 : s_rnge("codes", i__1, 
	    "m2bodtrn_", (ftnlen)1901)] = *code;
    m2bodini_(names, &nnam, codes, &ncod, ordnam, ordcod, (ftnlen)32);
    chkout_("M2BODDEF", (ftnlen)8);
    return 0;
} /* m2bodtrn_ */

/* Subroutine */ int m2bodtrn_(char *name__, integer *code, logical *found, 
	ftnlen name_len)
{
    return m2bodtrn_0_(0, name__, code, found, name_len);
    }

/* Subroutine */ int m2bodn2c_(char *name__, integer *code, logical *found, 
	ftnlen name_len)
{
    return m2bodtrn_0_(1, name__, code, found, name_len);
    }

/* Subroutine */ int m2bodc2n_(integer *code, char *name__, logical *found, 
	ftnlen name_len)
{
    return m2bodtrn_0_(2, name__, code, found, name_len);
    }

/* Subroutine */ int m2boddef_(char *name__, integer *code, ftnlen name_len)
{
    return m2bodtrn_0_(3, name__, code, (logical *)0, name_len);
    }

