//
// Test Suite for C-API GEOSRelatePatternMatch

#include <tut/tut.hpp>
// geos
#include <geos_c.h>
// std
#include <cstdarg>
#include <cstdio>
#include <cstdlib>

#include "capi_test_utils.h"

namespace tut {
//
// Test Group
//

// Common data used in test cases.
struct test_capigeosrelatepatternmatch_data : public capitest::utility {};

typedef test_group<test_capigeosrelatepatternmatch_data> group;
typedef group::object object;

group test_capigeosrelatepatternmatch_group("capi::GEOSRelatePatternMatch");

//
// Test Cases
//

template<>
template<>
void object::test<1>
()
{
    char ret = GEOSRelatePatternMatch("000000000", "*********");
    ensure_equals(ret, char(1));
}

template<>
template<>
void object::test<2>
()
{
    char ret = GEOSRelatePatternMatch("000000000", "TTTTTTTTT");
    ensure_equals(ret, char(1));
}

template<>
template<>
void object::test<3>
()
{
    char ret = GEOSRelatePatternMatch("000000000", "000000000");
    ensure_equals(ret, char(1));
}

template<>
template<>
void object::test<4>
()
{
    char ret = GEOSRelatePatternMatch("000000000", "FFFFFFFFF");
    ensure_equals(ret, char(0));
}

template<>
template<>
void object::test<5>
()
{
    const char* mat = "012TF012F";

    char ret = GEOSRelatePatternMatch(mat, "TTTTFTTTF");
    ensure_equals(ret, char(1));

    ret = GEOSRelatePatternMatch(mat, "TT1TFTTTF");
    ensure_equals(ret, char(0));
}

} // namespace tut

