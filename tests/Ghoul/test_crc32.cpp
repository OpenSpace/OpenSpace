/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include <catch2/catch_test_macros.hpp>

#include <ghoul/misc/crc32.h>
#include <cstring>
#include <random>

namespace {
struct Data {
    const char* string = nullptr;
    unsigned int hash = 0;
};

// Just a list of random strings to test
constexpr Data TestStrings[] = {
    { "Ghoul",  2275704949 }, { "String", 2568140703 }, { "TestString", 1470758616 },
    { "Hashing" , 107888401 }, { "HashingString", 2912793972 }, { "C++", 1464987298 },
    { "C66Pkzdzpi", 2356802799 }, { "xEkpgXmTJu", 1718269938 }, { "51ljrJvP5K", 3387307192 },
    { "7IAhEwMYax", 2058753453 }, { "ijU5EcCeZE", 3773846330 }, { "qakUu5AySX", 4092950936 },
    { "BwuihWvbcj", 2679982581 }, { "XkUbodmAW1", 531660383 },  { "N245fEc4R9", 187038018 },
    { "vC8S3WPT3l", 2749218524 }, { "hiuJSlJlpR", 475500008 },  { "gZ5Fb7MSeP", 4284724501 },
    { "zw76Rj6gVd", 2126183020 }, { "RozKnT5y2Y", 978468321 },  { "JgJbY0dyTn", 3883767462 },
    { "V2MMkw1Nb0", 2856546409 }, { "vPeJUSXYDc", 477494521 },  { "Z4kYUIUe27", 3943652345 },
    { "ZyNIXYd7y0", 1688382846 }, { "sk5Z6t5HEb", 603287316 },  { "8Xo6Rz9aLT", 2508503521 },
    { "IT5l6iIH3O", 3812371612 }, { "APESt0Ic0q", 4139489531 }, { "2Dis9UZOYP", 305438034 },
    { "ec5NG2c8Sl", 2004244237 }, { "eosmQrVa7N", 2112882518 }, { "qnzuCE6omi", 2252990143 },
    { "vdq5njozpO", 2448455704 }, { "qVd5vJ13wH", 3964196402 }, { "wv9iYII0TL", 3986819427 },
    { "piDD49A4rl", 2458754099 }, { "X6myqhoEAY", 2709645954 }, { "G64aUFJvF1", 911845262 },
    { "OlqdOOQyqk", 3373728589 }, { "QBQZSw56k4", 3663166158 }, { "8MJO88cCm9", 269227342 },
    { "zcrQNy6hOo", 4095758316 }, { "B3dvrZsDRg", 3563819599 }, { "S3jlMboqdt", 2610163939 },
    { "krbERsTEBS", 1828866126 }, { "HC4bq0YLcU", 1868423130 }, { "QyFs2D4g2r", 1581604687 },
    { "zcKibcmziV", 811946608 },  { "bbsE7BRBjB", 1584912542 }, { "TM9aAM8Dp0", 3831913514 },
    { "FKqeQiXkzb", 4198116589 }, { "Ag2YQYayHa", 4079694195 }, { "LDjF0VUhFZ", 2787191236 },
    { "NfqnCN5v5i", 2552928300 }, { "ljZBtxeUAE", 639944708 },  { "ngiVendsao", 2787381362 },
    { "Xhq3O90Gu3", 1843023544 }, { "qfpE2GJYvO", 3300440152 }, { "JtCUukcHDw", 2848022269 },
    { "KIE16m2EoF", 2057369746 }, { "1n4f0rdz2Y", 2356575215 }, { "9TNcZYOcm6", 3498208275 },
    { "fp2nQEgcBB", 848103359 },  { "jQzfkc0v0b", 4120671677 }, { "ScX4NeNyCU", 2325141283 },
    { "hxPVFiEtea", 4023138454 }, { "UDkksjkmqm", 3435314723 }, { "MpiLUnPtVf", 2665882327 },
    { "vFUxmSrjAV", 1406239025 }, { "1Ar1Xsx5jT", 1822623602 }, { "AobLZzig1S", 565396345 },
    { "WmyW2yieRb", 3731907790 }, { "rRUBDzGGsT", 3211344348 }, { "wCgf3FOSKq", 3994537508 },
    { "9vBRDnpIZX", 2826747208 }, { "bqMIlTE6gE", 2697483075 }, { "W3XlU5fvDZ", 20178902 },
    { "FYwPmuiWV4", 1280271458 }, { "XT4GFQc5b8", 1314154983 }, { "9BKusNgZ1M", 774900790 },
    { "iXR6XaSBZi", 1095860694 }, { "8xqyQ6VUCr", 3257067782 }, { "E86RudK4AG", 2699563460 },
    { "GlGkBtaf4o", 1752666198 }, { "SzT2At2ZLI", 2586290310 }, { "NBCKX5K7kw", 3737540575 },
    { "1wCO03fRuj", 2307489277 }, { "FBw6VVjgeb", 584955940 },  { "rL7NeGuIzV", 1949565036 },
    { "RPhOhQ2RMq", 728699057 },  { "bHf89Iz7KR", 453125771 },  { "g1DTlXFDxc", 508692631 },
    { "iXNalb8ifL", 3031710043 }, { "ugjIANiskG", 113209632 },  { "DgsXceLmp5", 1238341000 },
    { "jktr3gwNFs", 3802895210 }, { "pkQRKzj3e2", 2451929436 }, { "XAGSqGnsqM", 678426830 },
    { "lZy6isZesG", 537821493 },  { "Z8R5Wez7S5", 2547506008 }, { "QxNmEjE6Nio", 2209223120 },
    { "4qFahyrE3I", 4212172072 }, { "xKAx03OXHp", 1273309782 }, { "yV7zVSSvnF", 1743741706 },
    { "mdSeuvk9f8", 897512390 },  { "WRFJyp9tsz", 1418265756 }, { "Ak3usChrHS", 135678948 },
    { "RmsMQum6GZ", 3750279427 }, { "zUp315r4rD", 670306073 },  { "KTFoFbAgXF", 2172415845 },
    { "Fjym4eiAEw", 1513950738 }, { "ffl8Mgq0ie", 3927535377 }, { "mP65hsBV4E", 2992498107 },
    { "oPRsvbaMvR", 1122427484 }, { "1FSoPPF7Rv", 4115262403 }, { "EEmIBRzdbr", 935226087 },
    { "WSuYUNh4pa", 3533340304 }, { "FwcirSgAIv", 3532328506 }, { "bGgfYwU6Ca", 3817098450 },
    { "NL4Iunw6Cf", 1596804777 }, { "LbeZEYQiwo", 3915711401 }, { "7HMydtu4ns", 356606690 },
    { "CSRV0R4uaC", 1916245463 }, { "m3nkJ5NqMV", 3000649515 }, { "eI1Cq4gnOz", 765922371 },
    { "0PMDbDduPo", 2456852164 }, { "wdjfpwhvWC", 2882323002 }, { "EZieADG58x", 1847235298 },
    { "5ZLZQqUaMd", 3390395162 }, { "IIcIFjviK0", 1248217506 }, { "1TE5gajNw6", 3772480364 },
    { "jDIIlrWGY7", 1302320234 }, { "mnbkSk8jhO", 388832586 },  { "yQU2emmpg8", 140375469 },
    { "qVchWUyyuf", 1199740437 }, { "0TOSOhlYQ1", 4058541249 }, { "D7vmvahywO", 3830667431 },
    { "J2UXV8PQuA", 3645891076 }, { "mcQFTzNI3I", 3351010822 }, { "jt0KhskNYm", 3746373872 },
    { "ibgP0Z01OG", 2031390403 }, { "EnnVlIRa0d", 495030325 },  { "4VhTX0okea", 323632070 },
    { "RmuqwjVJGJ", 571959082 },  { "Bc1iqTfu5c", 2087853824 }, { "7OJiNekDkQ", 2880241603 },
    { "9qYGgKaOi9", 1925571023 }, { "D7IstRYwnX", 2127299497 }, { "2jrujLlp5u", 2245502642 },
    { "aIHr32jFzV", 4896627 },    { "9XqWhcfNfg", 2254689464 }, { "JN9JFC8vyr", 3858536448 },
    { "MUCfg3WTlD", 2308551023 }, { "mDyU6goud8", 1740827944 }, { "XJDIdok3L1", 833510854 },
    { "mT0rMdhhQi", 3875233041 }, { "XdwIZ1ujLw", 2241369640 }, { "QY5IFEgKAX", 1760373711 },
    { "JpNTIwSkBw", 2207058783 }, { "QVKY5qyeMS", 154486681 },  { "g5qMFJITPg", 3736594104 },
    { "WcRNXTBjqG", 1494488044 }, { "mnuU3qlsCv", 3309702757 }, { "GLnmAArIaA", 3936175768 },
    { "0Vbfun9yLO", 3789086985 }, { "sDDdRbhGzs", 2592856197 }, { "vxCXjHbOWp", 1786048212 },
    { "q4SvlIGB0o", 2156822579 }, { "RoAhZPV9dx", 2404027665 }, { "jyFHo4rTrS", 162672573 },
    { "an5kSPAcN3", 126411814 },  { "UKD5LSbzLP", 1329267389 }, { "sntByJ1xpg", 1093255201 },
    { "IY29A4tG8x", 3987417412 }, { "TKxvEOKU7S", 1049525232 }, { "7gYLzjin7t", 576118851 },
    { "sH8RJsk0H7", 557580577 },  { "I4cIT4RL5I", 184852120 },  { "DedGfm2zzf", 2053650935 },
    { "tDC6OcCF1o", 898466311 },  { "3m3BUQParj", 2129806792 }, { "vd9ZMk88Me", 1052323001 },
    { "Aood9kYERo", 1062849625 }, { "rRydUKo1g2", 1452507744 }, { "6vG2GedSEW", 4165603994 },
    { "ZvRQjgaaAn", 1463703067 }, { "89IKqmwRfM", 1764022809 }, { "5wuW3yLlyB", 2069628836 },
    { "TErEbiY1LV", 2738897941 }, { "Gq3IjT9LYh", 447368898 },  { "PNPmpVM3tP", 546564557 },
    { "T9yvh0JCn5", 2206100252 }, { "YdiSJ7rbke", 4142612632 }, { "Mt1Og5anNO", 3957284894 },
    { "BJw6scVoVL", 840787819 },  { "c70P28l6PZ", 2380439826 }, { "Fd8QY7HdyH", 221200488 },
    { "CHjfP3ZCZF", 3112761315 }, { "7kEQacpzjI", 3963430565 }, { "LqqkbTbrVx", 2505414383 },
    { "41scXti33m", 1757219246 }, { "dJMKpb307a", 3791251 },    { "MV6LnmRjU8", 3512327403 },
    { "g02wOkNNm8", 4191539549 }, { "JDKTsIHTyA", 1483193947 }, { "kf48TtamhN", 459772078 },
    { "WmCYHY9FRK", 1116618482 }, { "nEUreXLXhp", 3230020768 }, { "vr2P2sM1sz", 761285284 },
    { "MzSRffVsHC", 635149420 },  { "srZS7sM7of", 1603914848 }, { "xqLgptFEee", 1004787316 },
    { "LbDKycnLVL", 1024432519 }, { "0M1tAZfltZ", 3694016997 }, { "H9GrytclCc", 4078178262 },
    { "6oMFX0YyU0", 3711105877 }, { "WebWXe8tvg", 2815494589 }, { "gUXFcaMp7D", 3211212146 },
    { "ud6mca1d7O", 12165849 },   { "VYjdHv6KqF", 573458929 },  { "9TR1p068K6", 3368627942 },
    { "Ik3YJHuD1b", 1792011113 }, { "3tygofH0oU", 1550861175 }, { "mYktPGRKls", 1997960645 },
    { "g7ga5UGixH", 393103429 },  { "ok43OcOhnG", 867516279 },  { "XSy1ZxXp7v", 2818788507 },
    { "dekqeiDfis", 65563808 },   { "pqHpprxPz9", 618214098 },  { "SnUnOG5C0f", 958672997 },
    { "rtpBAq7Q2S", 3701185244 }, { "TiQEhVNChF", 1148354300 }, { "CPyvULymZM", 129231251 },
    { "xQhUYYz6Vu", 3887519503 }, { "zfTOnnKYcp", 1810258424 }, { "pSeVkmEExU", 2751406027 },
    { "lIP8sAaYlw", 2603707100 }, { "fgkWnamPCT", 3901498218 }, { "vpXN4iJ842", 3698083498 },
    { "HOIsciKQvw", 145963288 },  { "ZHncNU6185", 598556860 },  { "IzVmm1Hb50", 615565831 },
    { "9MsPSe9fDO", 830213837 },  { "VMXTJW1RVQ", 3671497605 }, { "ugvwg3Q8gu", 678618621 },
    { "zDDSCvzCjk", 4060558624 }, { "A0s6QLv9K1", 683875216 },  { "4ljRisATEx", 3353363358 },
    { "QJ0Wt3Swa0", 961851791 },  { "BWKwNNaZI4", 48305543 },   { "821gDEq87f", 1458105261 },
    { "sHZzRwlEDv", 925260578 },  { "g87YKJoxLx", 812940872 },  { "YyphPdzKUk", 3948923146 },
    { "Rd8b5xP7Z7", 2586847961 }, { "erGQ5adFHn", 251114865 },  { "f9WOF2xFHr", 244726540 },
    { "419aYyn4Do", 1650160023 }, { "nQpKL11iBi", 3199891874 }, { "0ATB6069pf", 1182727337 },
    { "bQbegLt7pt", 757968685 },  { "ktjq6X2vMm", 3392932379 }, { "UfRGVC0BUE", 2192184938 },
    { "LKm6pApvsh", 2837396633 }, { "JmI8b6KcR2", 1206094971 }, { "DmCt62bpSY", 704587552 },
    { "D4rfRTVUPt", 3825346019 }, { "DelPm0JNju", 3794136331 }, { "3FfQlUMePb", 4013964142 },
    { "bR54CeiJnK", 40953794 },   { "Uj1YYIWeiL", 656494917 },  { "jMZcS41AFq", 310318316 },
    { "Xt9vKBTAiW", 3264678761 }, { "S8ywUXZe8W", 3800879110 }, { "kHPQCenC4x", 3951828612 },
    { "AYrnZTcclj", 508858739 },  { "Z90STlPrxm", 3684574467 }, { "o3XpcKGQEh", 3135803713 },
    { "Do2dybltXr", 1856932464 }, { "MsXUaY9gyh", 1028261131 }, { "ODuc9vbhL5", 3654847980 },
    { "QizqHTIWtr", 1715242578 }, { "dcIZ7raffw", 1198822866 }, { "vqzkmKgsbc", 2740685520 },
    { "K6ckvV9PeZ", 397277210 },  { "q8Cx72kZyx", 4174152574 }, { "HSJtuO02nQ", 886621554 },
    { "0ZQdvtyHkg", 1714972450 }, { "2Ru7jnwxgK", 2995353102 }, { "olBg7YXD3Q", 3788074604 },
    { "cOJtUiQmgW", 1550300213 }, { "U71Uaiixfe", 2337541318 }, { "8c897dhPyJ", 428130919 },
    { "4m6nPOd5IC", 959587747 },  { "JknlbcGW4l", 920867207 },  { "VoIRvES2ir", 4022966781 },
    { "4AAXCxQ9ko", 4246180706 }, { "uOar8jOsBD", 2296534811 }, { "98OwSqPgwE", 2499978917 },
    { "fonH8DikW4", 2643723140 }, { "JAW6hUCQaN", 715311685 },  { "0KfM6BHdGv", 64371565 },
    { "1jIDyv4m7W", 730929254 },  { "BCzazxgsZF", 3258812225 }, { "2edEmwHbNA", 1587566157 },
    { "mHUCVOXDzH", 511027730 },  { "lV8KbopKVH", 3832898111 }, { "WohUkwxRCz", 3398300451 },
    { "3lvHRVBQ7r", 1136162678 }, { "iurvfYiBXy", 2426670260 }, { "yCCLLekxUU", 1764774991 },
    { "1vcIn6Z5Lf", 174555912 },  { "4NySlmSHF6", 3856438979 }, { "4famp4aP4i", 1680521280 },
    { "SLbJkQLpnN", 1943245910 }, { "UlnPnHDU4U", 1677919467 }, { "b3A2XgGIzh", 3742480702 },
    { "GyTlh5WUdP", 2242331738 }, { "vR5QirjZBE", 1364624103 }, { "gRC7mr05om", 1918261793 },
    { "q3jVIk3n8C", 1924864629 }, { "XwchShlhQF", 331725130 },  { "IIORjiuHaI", 3676360603 },
    { "T8VEXn4krr", 502304756 },  { "a2IyHhPHwR", 2291932140 }, { "X0J51U6ZwT", 2730023100 },
    { "hvSTDg4qr4", 120376923 },  { "v63HAxgnOx", 4184980194 }, { "11jx6kF1fZ", 3696437695 },
    { "aLrw5gB3z6", 1347190202 }, { "pdBXRbMsOu", 2772259399 }, { "7CPZ6XiKjL", 1558232910 },
    { "384dNpiUCn", 442403182 },  { "9UmE17AWk4", 599120675 },  { "GUudvKWKW9", 1322391217 },
    { "2X7pUFdFba", 62631474 },   { "ITAYKqstyp", 3628496348 }, { "KgRPkDD16y", 3613293680 },
    { "QUt8j4ZvKp", 1954929610 }, { "Q7gWUY4MFw", 3310952199 }, { "zdkJkLKxYP", 2269192377 },
    { "o4dONthTMV", 1034009758 }, { "vBmMt2nACD", 3449638210 }, { "EBSIq8FOHq", 3933417220 },
    { "JEjKVfGHEV", 1121566429 }, { "3elDYzHfGT", 412965638 },  { "rtcv0Aofyr", 1671764351 },
    { "BgYHtfrO7L", 2937471530 }, { "syVSsAKIgj", 2132818123 }, { "1Nwj7O7C32", 469298555 },
    { "aP3CUbt7zb", 3724055947 }, { "ivvxeTN6jL", 3599119504 }, { "mQZC4AkAHf", 3905523902 },
    { "reyn7SIbZF", 2067570615 }, { "c4qDictO3U", 4221692109 }, { "uagZozAHCZ", 2725424891 },
    { "Bx2bWmP6d3", 3439068524 }, { "AQUoH5h9Dd", 382402871 },  { "bVrDLeGBBk", 760995095 },
    { "CMUOmaPxOR", 1695342486 }, { "PqwIp4Mhak", 486505026 },  { "13R60w1ZBh", 1218826889 },
    { "nbVdlj4seJ", 3108522366 }, { "zLDNqboipD", 3672465099 }, { "xhLXkKFH1X", 2712432120 },
    { "dOkDiTtYHo", 2275230084 }, { "nIquagzubt", 1050584714 }, { "gL18nYLZMu", 3897558066 },
    { "qXF6xZUlmQ", 4080656166 }, { "d5c9QwncNA", 1322651505 }, { "876MjjCa0F", 1310756512 },
    { "SqWNxne2c2", 1908764667 }, { "p9cuVeA690", 3614690056 }, { "d5vKOx2vAi", 2481807538 },
    { "7YX0ijC0S1", 135227051 },  { "2DICvOfRBs", 841850910 },  { "0EwQJ9yVIm", 1376219596 },
    { "M5MCCC0BCA", 3828764599 }, { "8UzYp7k2fc", 524377047 },  { "0jUGCJjhkJ", 1200551958 },
    { "j8ZTVOmX0B", 1339678276 }, { "a9be1AemYF", 117001534 },  { "Pb59g3RYox", 2508151616 },
    { "m3lINa6cfu", 2615226504 }, { "Go2x9EiqWu", 1907852548 }, { "kBG69qChdp", 2872363554 },
    { "rORr9rrurg", 2170358869 }, { "0VRcnjIPoy", 2265773632 }, { "b0CdJXJPlh", 1960191801 },
    { "F8CQRvWHKS", 958372646 },  { "ErwvSzNvME", 1036980181 }, { "rAuOQspMgv", 1999333080 },
    { "lZObqhi2ZX", 245419370 },  { "EDkAyKnBrB", 1504240574 }, { "Nxvd3yEbEY", 860413093 },
    { "FS0dZfJ0xs", 2223668836 }, { "PLUNIjwM1Z", 1398202381 }, { "qKvBdrFOxj", 2149756682 },
    { "WEo0XloBxC", 697887155 },  { "j7f9gNTdls", 3181854343 }, { "m1Z4j0TDmw", 1133741390 },
    { "Zla1wWGUiJ", 925423769 },  { "kfGnOmp4vc", 1728748436 }, { "OZTZIeVVAu", 189918820 },
    { "tT8RcX5sXV", 920643905 },  { "muVoWjqqWv", 2603952838 }, { "wdzssxFEwv", 1134741058 },
    { "LD1Vk94tcj", 3960760331 }, { "kC924jWcql", 2867747311 }, { "wkth179ciE", 276194107 },
    { "XJcsrH6yc8", 1204453799 }, { "Prq2jAVHhq", 234377694 },  { "dlZI23yjp2", 2399702030 },
    { "ZGGUr9HvN7", 2545600596 }, { "DZqC9K9sdv", 3918257691 }, { "F4dyVh5eIL", 2316278335 },
    { "31j7LrVLdU", 51770569 },   { "6wKktdiEvS", 2828682615 }, { "wNTsAVRXh3", 2410398386 },
    { "b3DbW6G634", 1556794557 }, { "eDsytkS6L6", 409752503 },  { "evlAX8Zjmy", 1530122939 },
    { "spCfJJa98L", 1031192370 }, { "erNxvzgXcX", 1755727136 }, { "fvGbUrm730", 1714459546 },
    { "EdQEmaSi20", 1342614523 }, { "mWTcArQeNf", 167955905 },  { "920G28WunK", 3048810282 },
    { "9693rsDvar", 1462839534 }, { "ZUKudR11OE", 1912700072 }, { "UPRdTKTuJt", 1435040540 },
    { "h7ZLOWZaDd", 1811266329 }, { "GpbZNmBV0P", 3285572374 }, { "rLppzFblcy", 2739532549 },
    { "Xj816oSWVs", 2229208543 }, { "0RWhbGfX9Y", 4284080854 }, { "ZXe7ByXkcg", 278285904 },
    { "6FQQi0TXIK", 1601084514 }, { "6jsyqPgq8d", 1122654064 }, { "lPTpQrZXtu", 2397315483 },
    { "hvvBDNxuaR", 1838189646 }, { "V73cFaHIyR", 2347545330 }, { "BWxMp4fWW4", 3387732355 },
    { "flt6n4ztKB", 3707185049 }, { "rBIRqVJHfT", 3244748520 }, { "d75nbaPLVp", 2289372034 },
    { "Q7uevvEWiL", 1740071537 }, { "KiGCoTHFV4", 4034419853 }, { "oKc64nGApD", 2592762618 },
    { "IRs4njzW92", 3912015855 }, { "5JNi9UYAPj", 131208553 },  { "T86sjgAqC4", 1454960367 },
    { "Qqaucfjtoq", 2860716815 }, { "Rz6IhSkiVU", 1845688880 }, { "PMi1Mnv3Yh", 1633576056 },
    { "VX0sDo4smv", 153564198 },  { "EYKySZffIe", 4263693081 }, { "nMROrZ8R1T", 1653377697 },
    { "ZUOCJHWyKm", 454234118 },  { "SMEn2KiD2c", 1248296100 }, { "kRW4NWOQEd", 1130360757 },
    { "LPAS9AVOdK", 3515708627 }, { "ItPqGqGkVq", 1813967370 }, { "mvmeXZUyUN", 3043999736 },
    { "ITSsYRYm0o", 899808512 },  { "XcE0vyCCZr", 3374479254 }, { "fXGzPyv6Ug", 526191231 },
    { "TDYOXVwgLS", 1785791492 }, { "BKtwiQCkmp", 1126849747 }, { "pG0eueDlRv", 2636038561 },
    { "espKtEMkBs", 3509180815 }, { "nkGekVvbpF", 1755534293 }, { "DAArqaCVh9", 1416599414 },
    { "xBcZIVTqHN", 137217533 },  { "auZCYGkMs7", 1057373573 }, { "0E5no1i1Wc", 788342337 },
    { "5NhHOtEDMe", 1591783093 }, { "2jsTkjxOds", 535115328 },  { "yOBnVEgkZs", 2032384554 },
    { "ad6gMSD5du", 181309609 },  { "xOx7IcBYpX", 4218684341 }, { "LIZd77DJ5D", 514289212 },
    { "NIfNHTzjqo", 2534119003 }, { "APs5MKuKkr", 3169696001 }, { "4XhzBHuOSj", 835402076 },
    { "eMEVA2F8fu", 1120639842 }, { "Q3bgZE9Y2i", 1953070987 }, { "8kQuiDaluU", 1758630886 },
    { "uWhSTLgNnS", 723489935 },  { "14bY7UnEvG", 3860638202 }, { "1DBpfqtuND", 3542522250 },
    { "VDCMEbr11w", 116696746 },  { "5XGPGEqqmC", 2670710164 }, { "c9jd2HH3ya", 4176377933 },
    { "prPWiszW2X", 217715637 },  { "TOc5F15OGi", 2219054376 }, { "zlrlK3GwkZ", 3271660485 },
    { "8FUAhF6nJK", 2233011526 }, { "h6lSUTHUut", 1238337316 }, { "C8U6ntriYu", 638650598 },
    { "WxeFIFoXPL", 2000045289 }, { "hr2LmF9FjL", 3241800974 }, { "AMv6Kg4gaJ", 2316368142 },
    { "T33FH0acrS", 203057196 },  { "v94eDwXkeq", 3905846404 }
};
} // namespace


TEST_CASE("HashFixedEquality", "[crc32]") {
    for (const Data& d : TestStrings) {
        const unsigned int stringHash = ghoul::hashCRC32(std::string(d.string));
        const unsigned int charHash = ghoul::hashCRC32(d.string);
        const unsigned int bufferHash = ghoul::hashCRC32(
            d.string,
            static_cast<unsigned int>(strlen(d.string))
        );
        CHECK(stringHash == d.hash);
        CHECK(stringHash == charHash);
        CHECK(stringHash == bufferHash);
    }
}

TEST_CASE("CRC32: StaticTest", "[crc32]") {
    // This stops compiling when hashCRC32 stops being constexpr
    static_assert(ghoul::hashCRC32("spCfJJa98L") == 1031192370, "spCfJJa98L");
    static_assert(ghoul::hashCRC32("erNxvzgXcX") == 1755727136, "erNxvzgXcX");
    static_assert(ghoul::hashCRC32("fvGbUrm730") == 1714459546, "fvGbUrm730");
    static_assert(ghoul::hashCRC32("WxeFIFoXPL") == 2000045289, "WxeFIFoXPL");
    static_assert(ghoul::hashCRC32("hr2LmF9FjL") == 3241800974, "hr2LmF9FjL");
    static_assert(ghoul::hashCRC32("AMv6Kg4gaJ") == 2316368142, "AMv6Kg4gaJ");
}

TEST_CASE("CRC32: HashRandomEquality", "[crc32]") {
    static const std::vector<char> alphanum = {
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
        'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
        'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l',
        'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
    };

    constexpr int nTests = 100;

    std::random_device r;
    std::default_random_engine e(r());
    std::uniform_int_distribution<int> dist(0, static_cast<int>(alphanum.size() - 1));

    // Create nTests number of random strings to test
    for (int i = 0; i < nTests; i++) {
        for (int j = 0; j < nTests; j++) {
            std::vector<char> data(j);

            for (int k = 0; k < j; k++) {
                data[k] = alphanum[dist(e)];
            }

            std::string string(data.begin(), data.end());
            const unsigned int stringHash = ghoul::hashCRC32(string);
            const unsigned int charHash = ghoul::hashCRC32(string.data());
            const unsigned int bufferHash = ghoul::hashCRC32(data.data(), j);
            CHECK(stringHash == charHash);
            CHECK(stringHash == bufferHash);
        }
    }
}
