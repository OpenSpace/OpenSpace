/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

namespace {

struct [[codegen::Dictionary(SpiceEllipse)]] Ellipse {
    glm::dvec3 center;
    glm::dvec3 semiMajor;
    glm::dvec3 semiMinor;
};

enum class [[codegen::enum]] CK05Subtype {
    C05TP0,
    C05TP1,
    C05TP2,
    C05TP3
};

// Helper functions
struct StringBuffer {
    std::vector<char> buffer;
    int ndim;
    int arrlen;
};

StringBuffer arrayToBuffer(std::vector<std::string> array) {
    int arrlen = 0;
    for (const std::string_view& arr : array) {
        arrlen = std::max(arrlen, static_cast<int>(arr.size()));
    }
    // For the \0 character
    arrlen += 1;
    int ndim = static_cast<int>(array.size());

    std::vector<SpiceChar> buffer;
    buffer.resize(ndim * arrlen);
    std::fill(buffer.begin(), buffer.end(), '\0');
    size_t pos = 0;
    for (const std::string_view& arr : array) {
        std::copy(arr.begin(), arr.end(), buffer.begin() + pos);
        pos += 1; // Skip the \0 character
    }

    return {
        .buffer = buffer,
        .ndim = ndim,
        .arrlen = arrlen
    };
}

// Start the SPICE functions

[[codegen::luawrap]] void appndc(std::string item, SpiceCell* cell) {
    appndc_c(item.c_str(), cell);
}

[[codegen::luawrap]] void appndd(double item, SpiceCell* cell) {
    appndd_c(item, cell);
}

[[codegen::luawrap]] void appndi(int item, SpiceCell* cell) {
    appndi_c(item, cell);
}

[[codegen::luawrap]] glm::dmat3 axisar(glm::dvec3 axis, double angle) {
    SpiceDouble res[3][3];
    axisar_c(glm::value_ptr(axis), angle, res);
    return glm::dmat3(
        res[0][0], res[0][1], res[0][2],
        res[1][0], res[1][1], res[1][2],
        res[2][0], res[2][1], res[2][2]
    );
}

[[codegen::luawrap]] std::tuple<std::array<double, 6>, double>
azlcpo(std::string method, std::string target, double et, std::string abcorr, bool azccw,
    bool elplsz, glm::dvec3 obspos, std::string obsctr, std::string obsref)
{
    std::array<SpiceDouble, 6> azlsta;
    double lt;
    azlcpo_c(
        method.c_str(),
        target.c_str(),
        et,
        abcorr.c_str(),
        azccw,
        elplsz,
        glm::value_ptr(obspos),
        obsctr.c_str(),
        obsref.c_str(),
        azlsta.data(),
        &lt
    );

    return { azlsta, lt };
}

[[codegen::luawrap]] glm::dvec3 azlrec(double range, double az, double ez, bool azccw,
                                       bool elplsz)
{
    glm::dvec3 rectan;
    azlrec_c(range, az, ez, azccw, elplsz, glm::value_ptr(rectan));
    return rectan;
}

[[codegen::luawrap]] bool badkpv(std::string caller, std::string name, std::string comp,
                                 int size, int divby, std::string type)
{
    return badkpv_c(caller.c_str(), name.c_str(), comp.c_str(), size, divby, type[0]);
}

/**
 * idset is out parameter
 */
[[codegen::luawrap]] void bltfrm(int frmcls, SpiceCell* idset) {
    bltfrm_c(frmcls, idset);
}

[[codegen::luawrap]] std::optional<std::string> bodc2n(int code) {
    std::array<SpiceChar, 256> buffer = {};
    SpiceBoolean found;
    bodc2n_c(code, 256, buffer.data(), &found);
    if (found) {
        return std::string(buffer.begin(), buffer.end());
    }
    else {
        return std::nullopt;
    }
}

[[codegen::luawrap]] std::string bodc2s(int code) {
    std::array<SpiceChar, 256> buffer = {};
    bodc2s_c(code, 256, buffer.data());
    return std::string(buffer.begin(), buffer.end());
}

[[codegen::luawrap]] void boddef(std::string name, int code) {
    boddef_c(name.c_str(), code);
}

[[codegen::luawrap]] bool bodfnd(int body, std::string item) {
    return bodfnd_c(body, item.c_str());
}

[[codegen::luawrap]] std::optional<int> bodn2c(std::string name) {
    SpiceInt code;
    SpiceBoolean found;
    bodn2c_c(name.c_str(), &code, &found);
    if (found) {
        return code;
    }
    else {
        return std::nullopt;
    }
}

[[codegen::luawrap]] std::optional<int> bods2c(std::string name) {
    SpiceInt code;
    SpiceBoolean found;
    bods2c_c(name.c_str(), &code, &found);
    if (found) {
        return code;
    }
    else {
        return std::nullopt;
    }
}

[[codegen::luawrap]] std::vector<double> bodvar(int body, std::string item) {
    SpiceInt dim;
    std::vector<double> values;
    bodvar_c(body, item.c_str(), &dim, values.data());
    values.resize(dim);
    return values;
}

[[codegen::luawrap]] std::vector<double> bodvcd(int body, std::string item, int maxn) {
    SpiceInt dim;
    std::vector<double> values;
    values.resize(maxn);
    bodvcd_c(body, item.c_str(), maxn, &dim, values.data());
    values.resize(dim);
    return values;
}

[[codegen::luawrap]] std::vector<double> bodvrd(std::string bodynm, std::string item,
                                                int maxn)
{
    SpiceInt dim;
    std::vector<double> values;
    values.resize(maxn);
    bodvrd_c(bodynm.c_str(), item.c_str(), maxn, &dim, values.data());
    values.resize(dim);
    return values;
}

[[codegen::luawrap]] double brcktd(double number, double end1, double end2) {
    return brcktd_c(number, end1, end2);
}

[[codegen::luawrap]] int brckti(int number, int end1, int end2) {
    return brckti_c(number, end1, end2);
}

/**
 * ndim <- array.size()
 * arrlen <-  max_i(array[i].size())
 */
[[codegen::luawrap]] int bschoc(std::string value, std::vector<std::string> array,
                                std::vector<int> order)
{
    StringBuffer buf = arrayToBuffer(std::move(array));
    return bschoc_c(value.c_str(), buf.ndim, buf.arrlen, buf.buffer.data(), order.data());
}

[[codegen::luawrap]] int bschoi(int value, std::vector<int> array, std::vector<int> order)
{
    return bschoi_c(value, static_cast<int>(array.size()), array.data(), order.data());
}

[[codegen::luawrap]] int bsrchc(std::string value, std::vector<std::string> array) {
    StringBuffer buf = arrayToBuffer(std::move(array));
    return bsrchc_c(value.c_str(), buf.ndim, buf.arrlen, buf.buffer.data());
}

[[codegen::luawrap]] int bsrchd(double value, std::vector<double> array) {
    return bsrchd_c(value, static_cast<int>(array.size()), array.data());
}

[[codegen::luawrap]] int bsrchi(int value, std::vector<int> array) {
    return bsrchi_c(value, static_cast<int>(array.size()), array.data());
}

[[codegen::luawrap]] double b1900() {
    return b1900_c();
}

[[codegen::luawrap]] double b1950() {
    return b1950_c();
}

[[codegen::luawrap]] int card(SpiceCell* cell) {
    return card_c(cell);
}

[[codegen::luawrap]] std::optional<std::tuple<int, std::string, int>> ccifrm(int frclss,
                                                                             int clssid)
{
    SpiceInt frcode;
    std::array<SpiceChar, 256> frname = {};
    SpiceInt cent;
    SpiceBoolean found;
    ccifrm_c(frclss, clssid, 256, &frcode, frname.data(), &cent, &found);
    if (found) {
        std::tuple<int, std::string, int> ret = {
            frcode,
            std::string(frname.begin(), frname.end()),
            cent
        };
        return ret;
    }
    else {
        return std::nullopt;
    }
}

[[codegen::luawrap]] Ellipse cgv2el(glm::dvec3 center, glm::dvec3 vec1,
                                         glm::dvec3 vec2)
{
    SpiceEllipse ellips;
    cgv2el_c(glm::value_ptr(center), glm::value_ptr(vec1), glm::value_ptr(vec2), &ellips);
    return {
        glm::dvec3(ellips.center[0], ellips.center[1], ellips.center[2]),
        glm::dvec3(ellips.semiMajor[0], ellips.semiMajor[1], ellips.semiMajor[2]),
        glm::dvec3(ellips.semiMinor[0], ellips.semiMinor[1], ellips.semiMinor[2])
    };
}

[[codegen::luawrap]] std::vector<double> chbder(std::vector<double> cp, int depg,
                                                glm::dvec2 x2s, double x, int nderiv)
{
    std::vector<SpiceDouble> dp;
    dp.resize(3 * (nderiv + 1));
    std::fill(dp.begin(), dp.end(), 0.0);

    std::vector<SpiceDouble> dpdxs;
    dpdxs.resize(nderiv + 1);
    chbder_c(cp.data(), depg, glm::value_ptr(x2s), x, nderiv, dp.data(), dpdxs.data());
    return dpdxs;
}

[[codegen::luawrap]] std::tuple<double, double> chbigr(int degp, std::vector<double> cp,
                                                       glm::dvec2 x2s, double x)
{
    SpiceDouble p;
    SpiceDouble itgrlp;
    chbigr_c(degp, cp.data(), glm::value_ptr(x2s), x, &p, &itgrlp);
    return { p, itgrlp };
}

[[codegen::luawrap]] std::tuple<double, double> chbint(std::vector<double> cp, int degp,
                                                       glm::dvec2 x2s, double x)
{
    SpiceDouble p;
    SpiceDouble dpdx;
    chbint_c(cp.data(), degp, glm::value_ptr(x2s), x, &p, &dpdx);
    return { p, dpdx };
}

[[codegen::luawrap]] double chbval(std::vector<double> cp, int degp, glm::dvec2 x2s,
                                   double x)
{
    SpiceDouble p;
    chbval_c(cp.data(), degp, glm::value_ptr(x2s), x, &p);
    return p;
}

[[codegen::luawrap]] void chkin(std::string module) {
    chkin_c(module.c_str());
}

[[codegen::luawrap]] void chkout(std::string module) {
    chkout_c(module.c_str());
}

[[codegen::luawrap]] std::optional<std::tuple<int, std::string>> cidfrm(int cent) {
    SpiceInt frcode;
    std::array<SpiceChar, 256> frname = {};
    SpiceBoolean found;
    cidfrm_c(cent, 256, &frcode, frname.data(), &found);
    if (found) {
        std::tuple<int, std::string> ret = {
            frcode,
            std::string(frname.begin(), frname.end())
        };
        return ret;
    }
    else {
        return std::nullopt;
    }
}

[[codegen::luawrap]] std::optional<std::tuple<glm::dmat3, int>> ckfrot(int inst,
                                                                       double et)
{
    SpiceDouble rotate[3][3];
    SpiceInt ref;
    SpiceBoolean found;
    ckfrot_c(inst, et, rotate, &ref, &found);
    if (found) {
        glm::dmat3 rot = {
            rotate[0][0], rotate[0][1], rotate[0][2],
            rotate[1][0], rotate[1][1], rotate[1][2],
            rotate[2][0], rotate[2][1], rotate[2][2]
        };
        std::tuple<glm::dmat3, int> ret = { rot, ref };
        return ret;
    }
    else {
        return std::nullopt;
    }
}

[[codegen::luawrap]] std::optional<std::tuple<std::array<double, 36>, int>> ckfxfm(
                                                                                 int inst,
                                                                                double et)
{
    SpiceDouble xform[6][6];
    SpiceInt ref;
    SpiceBoolean found;
    ckfxfm_c(inst, et, xform, &ref, &found);
    if (found) {
        std::array<double, 36> xf;
        int c = 0;
        for (int i = 0; i < 6; i++) {
            for (int j = 0; j < 6; j++) {
                xf[c] = xform[i][j];
                c++;
            }
        }
        std::tuple<std::array<double, 36>, int> ret = { xf, ref };
        return ret;
    }
    else {
        return std::nullopt;
    }
}

[[codegen::luawrap]] std::array<double, 10> ckgr02(int handle, std::vector<double> descr,
                                                   int recno)
{
    std::array<double, 10> record;
    ckgr02_c(handle, descr.data(), recno, record.data());
    return record;
}

[[codegen::luawrap]] void ckcls(int handle) {
    ckcls_c(handle);
}

[[codegen::luawrap]] void ckcov(std::string ckfnm, int idcode, bool needav,
                                std::string level, double tol, std::string timsys,
                                SpiceCell* cover)
{
    ckcov_c(ckfnm.c_str(), idcode, needav, level.c_str(), tol, timsys.c_str(), cover);
}

[[codegen::luawrap]] std::array<double, 8> ckgr03(int handle, std::vector<double> descr,
                                                  int recno)
{
    std::array<double, 8> record = {};
    ckgr03_c(handle, descr.data(), recno, record.data());
    return record;
}

[[codegen::luawrap]] void ckobj(std::string ck, SpiceCell* ids) {
    ckobj_c(ck.c_str(), ids);
}

[[codegen::luawrap]] std::optional<std::tuple<glm::dmat3, double>> ckgp(int inst,
                                                                        double sclkdp,
                                                                        double tol,
                                                                        std::string ref)
{
    SpiceDouble cmat[3][3];
    SpiceDouble clkout;
    SpiceBoolean found;
    ckgp_c(inst, sclkdp, tol, ref.c_str(), cmat, &clkout, &found);
    if (found) {
        glm::dmat3 rot = {
            cmat[0][0], cmat[0][1], cmat[0][2],
            cmat[1][0], cmat[1][1], cmat[1][2],
            cmat[2][0], cmat[2][1], cmat[2][2]
        };
        std::tuple<glm::dmat3, double> ret = { rot, clkout };
        return ret;
    }
    else {
        return std::nullopt;
    }
}

[[codegen::luawrap]] std::optional<std::tuple<glm::dmat3, glm::dvec3, double>> ckgpav(
                                                                                 int inst,
                                                                            double sclkdp,
                                                                               double tol,
                                                                          std::string ref)
{
    SpiceDouble cmat[3][3];
    glm::dvec3 av;
    SpiceDouble clkout;
    SpiceBoolean found;
    ckgpav_c(inst, sclkdp, tol, ref.c_str(), cmat, glm::value_ptr(av), &clkout, &found);
    if (found) {
        glm::dmat3 rot = {
            cmat[0][0], cmat[0][1], cmat[0][2],
            cmat[1][0], cmat[1][1], cmat[1][2],
            cmat[2][0], cmat[2][1], cmat[2][2]
        };
        std::tuple<glm::dmat3, glm::dvec3, double> ret = { rot, av, clkout };
        return ret;
    }
    else {
        return std::nullopt;
    }
}

[[codegen::luawrap]] int ckmeta(int ckid, std::string meta) {
    SpiceInt idcode;
    ckmeta_c(ckid, meta.c_str(), &idcode);
    return idcode;
}

[[codegen::luawrap]] int cknr02(int handle, std::vector<double> descr) {
    SpiceInt nrec;
    cknr02_c(handle, descr.data(), &nrec);
    return nrec;
}

[[codegen::luawrap]] int cknr03(int handle, std::vector<double> descr) {
    SpiceInt nrec;
    cknr03_c(handle, descr.data(), &nrec);
    return nrec;
}

[[codegen::luawrap]] void clearc(int ndim, int arrlen, void* array) {
    clearc_c(ndim, arrlen, array);
}

[[codegen::luawrap]] int cklpf(std::string fname) {
    SpiceInt handle;
    cklpf_c(fname.c_str(), &handle);
    return handle;
}

[[codegen::luawrap]] int ckopn(std::string fname, std::string ifname, int ncomch) {
    SpiceInt handle;
    ckopn_c(fname.c_str(), ifname.c_str(), ncomch, &handle);
    return handle;
}

[[codegen::luawrap]] void ckupf(int handle) {
    ckupf_c(handle);
}

[[codegen::luawrap]] void ckw01(int handle, double begtim, double endtim, int inst,
                                std::string ref, bool avflag, std::string segid, int nrec,
                                std::vector<double> sclkdp, std::vector<glm::dvec4> quats,
                                std::vector<glm::dvec3> avvs)
{
    ckw01_c(
        handle,
        begtim,
        endtim,
        inst,
        ref.c_str(),
        avflag,
        segid.c_str(),
        nrec,
        sclkdp.data(),
        quats.data(),
        avvs.data()
    );
}

[[codegen::luawrap]] void ckw02(int handle, double begtim, double endtim, int inst,
                                std::string ref, std::string segid, int nrec,
                                std::vector<double> start, std::vector<double> stop,
                                std::vector<glm::dvec4> quats,
                                std::vector<glm::dvec3> avvs,
                                std::vector<double> rates)
{
    return ckw02_c(
        handle,
        begtim,
        endtim,
        inst,
        ref.c_str(),
        segid.c_str(),
        nrec,
        start.data(),
        stop.data(),
        quats.data(),
        avvs.data(),
        rates.data()
    );
}

[[codegen::luawrap]] void ckw03(int handle, double begtim, double endtim, int inst,
                                std::string ref, bool avflag, std::string segid, int nrec,
                                std::vector<double> sclkdp, std::vector<glm::dvec4> quats,
                                std::vector<glm::dvec3> avvs, int nints,
                                std::vector<double> starts)
{
    ckw03_c(
        handle,
        begtim,
        endtim,
        inst,
        ref.c_str(),
        avflag,
        segid.c_str(),
        nrec,
        sclkdp.data(),
        quats.data(),
        avvs.data(),
        nints,
        starts.data()
    );
}

[[codegen::luawrap]] void ckw05(int handle, CK05Subtype subtyp, int degree, double begtim,
                                double endtim, int inst, std::string ref, bool avflag,
                                std::string segid, int n, std::vector<double> sclkdp,
                                void* packts, double rate, int nints,
                                std::vector<double> starts)
{
    ckw05_c(
        handle,
        static_cast<SpiceCK05Subtype>(subtyp),
        degree,
        begtim,
        endtim,
        inst,
        ref.c_str(),
        avflag,
        segid.c_str(),
        n,
        sclkdp.data(),
        packts,
        rate,
        nints,
        starts.data()
    );
}

[[codegen::luawrap]] std::vector<double> cleard(int ndim, std::vector<double> array) {
    cleard_c(ndim, array.data());
    return array;
}

[[codegen::luawrap]] std::vector<int> cleari(int ndim, std::vector<int> array) {
    std::vector<SpiceInt> arr;
    arr.reserve(array.size());
    for (int i : array) {
        arr.push_back(i);
    }

    cleari_c(ndim, arr.data());

    array.clear();
    array.reserve(ndim);
    for (long l : arr) {
        array.push_back(static_cast<int>(l));
    }
    return array;
}

[[codegen::luawrap]] double clight() {
    return clight_c();
}


[[codegen::luawrap]] void clpool() {
    clpool_c();
}

[[codegen::luawrap]] std::string cmprss(std::string delim, int n, std::string input) {
    std::array<SpiceChar, 256> output = {};
    cmprss_c(delim[0], n, input.c_str(), 256, output.data());
    return std::string(output.begin(), output.end());
}

[[codegen::luawrap]] std::optional<std::tuple<int, std::string>> cnmfrm(std::string cname)
{
    SpiceInt frcode;
    std::array<SpiceChar, 256> frname = {};
    SpiceBoolean found;
    cnmfrm_c(cname.c_str(), 256, &frcode, frname.data(), &found);
    if (found) {
        std::tuple<int, std::string> tuple = {
            static_cast<int>(frcode),
            std::string(frname.begin(), frname.end())
        };
        return tuple;
    }
    else {
        return std::nullopt;
    }
}

[[codegen::luawrap]] std::array<double, 6> conics(std::array<double, 8> elts, double et) {
    std::array<double, 6> state;
    conics_c(elts.data(), et, state.data());
    return state;
}

[[codegen::luawrap]] double convrt(double x, std::string in, std::string out) {
    SpiceDouble y;
    convrt_c(x, in.c_str(), out.c_str(), &y);
    return y;
}

[[codegen::luawrap]] void copy(SpiceCell* cell, SpiceCell* copy) {
    copy_c(cell, copy);
}

[[codegen::luawrap]] int cpos(std::string str, std::string chars, int start) {
    return cpos_c(str.c_str(), chars.c_str(), start);
}

[[codegen::luawrap]] int cposr(std::string str, std::string chars, int start) {
    return cposr_c(str.c_str(), chars.c_str(), start);
}

[[codegen::luawrap]] bool cvpool(std::string agent) {
    SpiceBoolean update;
    cvpool_c(agent.c_str(), &update);
    return update;
}

[[codegen::luawrap]] std::tuple<double, double, double> cyllat(double r, double clon,
                                                              double z)
{
    SpiceDouble radius;
    SpiceDouble lon;
    SpiceDouble lat;
    cyllat_c(r, clon, z, &radius, &lon, &lat);
    return { radius, lon, lat };
}

[[codegen::luawrap]] glm::dvec3 cylrec(double r, double clon, double z) {
    glm::dvec3 rectan;
    cylrec_c(r, clon, z, glm::value_ptr(rectan));
    return rectan;
}

[[codegen::luawrap]] std::tuple<double, double, double> cylsph(double r, double clon,
                                                               double z)
{
    SpiceDouble radius;
    SpiceDouble colat;
    SpiceDouble slon;
    cylsph_c(r, clon, z, &radius, &colat, &slon);
    return { radius, colat, slon };
}

[[codegen::luawrap]] void dafac(int handle, int n, int buflen, std::string buffer) {
    if (buffer.size() != n * buflen) {
        throw ghoul::lua::LuaRuntimeException(fmt::format(
            "Buffer size '{}' must be equal to n*buflen ({}*{})", buffer.size(), n, buflen
        ));
    }

    dafac_c(handle, n, buflen, buffer.data());
}

[[codegen::luawrap]] void dafbbs(int handle) {
    dafbbs_c(handle);
}

[[codegen::luawrap]] void dafbfs(int handle) {
    dafbfs_c(handle);
}

[[codegen::luawrap]] void dafcls(int handle) {
    dafcls_c(handle);
}

[[codegen::luawrap]] void dafcs(int handle) {
    dafcs_c(handle);
}

[[codegen::luawrap]] void dafdc(int handle) {
    dafdc_c(handle);
}

[[codegen::luawrap]] std::tuple<int, std::string, bool> dafec(int handle, int bufsiz,
                                                              int buffln)
{
    SpiceInt n;
    std::vector<SpiceChar> buffer;
    buffer.resize(bufsiz * buffln);
    SpiceBoolean done;
    dafec_c(handle, bufsiz, buffln, &n, buffer.data(), &done);
    return { static_cast<int>(n), std::string(buffer.begin(), buffer.end()), done };
}

[[codegen::luawrap]] bool daffna() {
    SpiceBoolean found;
    daffna_c(&found);
    return found;
}

[[codegen::luawrap]] bool daffpa() {
    SpiceBoolean found;
    daffpa_c(&found);
    return found;
}

[[codegen::luawrap]] std::vector<double> dafgda(int handle, int baddr, int eaddr) {
    std::vector<double> data;
    data.resize(eaddr - baddr);
    dafgda_c(handle, baddr, eaddr, data.data());
    return data;
}

[[codegen::luawrap]] int dafgh() {
    SpiceInt handle;
    dafgh_c(&handle);
    return handle;
}

[[codegen::luawrap]] std::string dafgn() {
    std::array<SpiceChar, 256> name;
    dafgn_c(256, name.data());
    return std::string(name.begin(), name.end());
}

[[codegen::luawrap]] std::vector<double> dafgs() {
    std::vector<SpiceDouble> sum;
    dafgs_c(sum.data());
    return sum;
}

[[codegen::luawrap]] std::optional<std::vector<double>> dafgsr(int handle, int recno,
                                                               int begin, int end)
{
    std::vector<double> data;
    data.resize(end - begin);
    SpiceBoolean found;
    dafgsr_c(handle, recno, begin, end, data.data(), &found);
    if (found) {
        return data;
    }
    else {
        return std::nullopt;
    }
}

[[codegen::luawrap]] std::tuple<int, int> dafhsf(int handle) {
    SpiceInt nd;
    SpiceInt ni;
    dafhsf_c(handle, &nd, &ni);
    return { nd, ni };
}

[[codegen::luawrap]] int dafopr(std::string fname) {
    SpiceInt handle;
    dafopr_c(fname.c_str(), &handle);
    return handle;
}

[[codegen::luawrap]] int dafopw(std::string fname) {
    SpiceInt handle;
    dafopw_c(fname.c_str(), &handle);
    return handle;
}

[[codegen::luawrap]] std::vector<double> dafps(std::vector<double> dc,
                                                 std::vector<int> ic)
{
    int nd = static_cast<int>(dc.size());
    int ni = static_cast<int>(ic.size());

    std::vector<double> sum;
    sum.resize(nd + (ni - 1) / 2 + 1);
    dafps_c(nd, ni, dc.data(), ic.data(), sum.data());
    return sum;
}

[[codegen::luawrap]] std::vector<double> dafrda(int handle, int begin, int end) {
    std::vector<double> data;
    data.resize(end - begin);
    dafrda_c(handle, begin, end, data.data());
    return data;
}

[[codegen::luawrap]] std::tuple<int, int, std::string, int, int, int> dafrfr(int handle) {
    SpiceInt nd;
    SpiceInt ni;
    std::array<SpiceChar, 256> ifname = {};
    SpiceInt fward;
    SpiceInt bward;
    SpiceInt free;
    dafrfr_c(handle, 256, &nd, &ni, ifname.data(), &fward, &bward, &free);
    return {
        static_cast<int>(nd),
        static_cast<int>(ni),
        std::string(ifname.begin(), ifname.end()),
        static_cast<int>(fward),
        static_cast<int>(bward),
        static_cast<int>(free)
    };
}

[[codegen::luawrap]] void dasadc(int handle, int n, int bpos, int epos, int datlen,
                                 void* data)
{
    dasadc_c(handle, n, bpos, epos, datlen, data);
}

[[codegen::luawrap]] void dasadd(int handle, std::vector<double> data) {
    dasadd_c(handle, static_cast<int>(data.size()), data.data());
}

[[codegen::luawrap]] void dasadi(int handle, std::vector<int> data) {
    std::vector<SpiceInt> buffer;
    buffer.reserve(data.size());
    for (int i : data) {
        buffer.push_back(i);
    }
    dasadi_c(handle, static_cast<int>(buffer.size()), buffer.data());
}

[[codegen::luawrap]] void dafrs(std::vector<double> sum) {
    dafrs_c(sum.data());
}

[[codegen::luawrap]] std::tuple<std::vector<double>, std::vector<int>> dafus(
                                                                  std::vector<double> sum,
                                                                                   int nd,
                                                                                   int ni)
{
    std::vector<double> dc;
    dc.resize(nd);
    std::vector<SpiceInt> ic;
    ic.resize(ni);
    dafus_c(sum.data(), nd, ni, dc.data(), ic.data());

    std::vector<int> icRet;
    icRet.reserve(ic.size());
    for (SpiceInt i : ic) {
        icRet.push_back(static_cast<int>(i));
    }
    return { dc, icRet };
}

[[codegen::luawrap]] void dasac(int handle, int n, int buflen, void* buffer) {
    dasac_c(handle, n, buflen, buffer);
}

[[codegen::luawrap]] void dascls(int handle) {
    dascls_c(handle);
}

[[codegen::luawrap]] void dasdc(int handle) {
    dasdc_c(handle);
}

[[codegen::luawrap]] std::tuple<int, std::string, bool> dasec(int handle, int bufsiz,
                                                              int buffln)
{
    SpiceInt n;
    std::string buffer;
    buffer.resize(bufsiz * buffln);
    SpiceBoolean done;
    dasec_c(handle, bufsiz, buffln, &n, buffer.data(), &done);
    return { static_cast<int>(n), buffer, done };
}

[[codegen::luawrap]] std::string dashfn(int handle) {
    std::string fname;
    fname.resize(256);
    dashfn_c(handle, 256, fname.data());
    return fname;
}

[[codegen::luawrap]]
std::tuple<int, int, int, int, int, glm::ivec3, glm::ivec3, glm::ivec3>
dashfs(int handle)
{
    SpiceInt nresvr;
    SpiceInt nresvc;
    SpiceInt ncomr;
    SpiceInt ncomc;
    SpiceInt free;
    std::array<SpiceInt, 3> lastla;
    std::array<SpiceInt, 3> lastrc;
    std::array<SpiceInt, 3> lastwd;
    dashfs_c(
        handle,
        &nresvr,
        &nresvc,
        &ncomr,
        &ncomc,
        &free,
        lastla.data(),
        lastrc.data(),
        lastwd.data()
    );
    return {
        static_cast<int>(nresvr),
        static_cast<int>(nresvc),
        static_cast<int>(ncomr),
        static_cast<int>(ncomc),
        static_cast<int>(free),
        glm::ivec3(lastla[0], lastla[1], lastla[2]),
        glm::ivec3(lastrc[0], lastrc[1], lastrc[2]),
        glm::ivec3(lastwd[0], lastwd[1], lastwd[2])
    };
}

[[codegen::luawrap]] std::tuple<int, int, int> daslla(int handle) {
    SpiceInt lastc;
    SpiceInt lastd;
    SpiceInt lasti;
    daslla_c(handle, &lastc, &lastd, &lasti);
    return { static_cast<int>(lastc), static_cast<int>(lastd), static_cast<int>(lasti) };
}

[[codegen::luawrap]] void dasllc(int handle) {
    return dasllc_c(handle);
}

[[codegen::luawrap]] int dasonw(std::string fname, std::string ftype, std::string ifname,
                                int ncomr)
{
    SpiceInt handle;
    dasonw_c(fname.c_str(), ftype.c_str(), ifname.c_str(), ncomr, &handle);
    return static_cast<int>(handle);
}

[[codegen::luawrap]] int dasopr(std::string fname) {
    SpiceInt handle;
    dasopr_c(fname.c_str(), &handle);
    return handle;
}

[[codegen::luawrap]] int dasops() {
    SpiceInt handle;
    dasops_c(&handle);
    return handle;
}

[[codegen::luawrap]] int dasopw(std::string fname) {
    SpiceInt handle;
    dasopw_c(fname.c_str(), &handle);
    return handle;
}

[[codegen::luawrap]] void* dasrdc(int handle, int first, int last, int bpos, int epos,
                                  int datlen)
{
    void* data;
    dasrdc_c(handle, first, last, bpos, epos, datlen, data);
    return data;
}

[[codegen::luawrap]] std::vector<double> dasrdd(int handle, int first, int last) {
    std::vector<double> data;
    data.resize(last - first);
    dasrdd_c(handle, first, last, data.data());
    return data;
}

[[codegen::luawrap]] std::vector<int> dasrdi(int handle, int first, int last) {
    std::vector<SpiceInt> data;
    data.resize(last - first);
    dasrdi_c(handle, first, last, data.data());

    std::vector<int> ret;
    ret.reserve(last - first);
    for (SpiceInt i : data) {
        ret.push_back(static_cast<int>(i));
    }
    return ret;
}

[[codegen::luawrap]] std::tuple<std::string, std::string, int, int, int, int> dasrfr(
                                                                               int handle)
{
    std::array<char, 256> idword = {};
    std::array<char, 256> ifname = {};
    SpiceInt nresvr;
    SpiceInt nresvc;
    SpiceInt ncomr;
    SpiceInt ncomc;
    dasrfr_c(
        handle,
        256,
        256,
        idword.data(),
        ifname.data(),
        &nresvr,
        &nresvc,
        &ncomr,
        &ncomc
    );
    return {
        std::string(idword.begin(), idword.end()),
        std::string(ifname.begin(), ifname.end()),
        nresvr,
        nresvc,
        ncomr,
        ncomc
    };
}

[[codegen::luawrap]] void dasudc(int handle, int first, int last, int bpos, int epos,
                                 int datlen, void* data)
{
    dasudc_c(handle, first, last, bpos, epos, datlen, data);
}

[[codegen::luawrap]] void dasudd(int handle, int first, int last,
                                 std::vector<double> data)
{
    dasudd_c(handle, first, last, data.data());
}

[[codegen::luawrap]] void dasudi(int handle, int first, int last, std::vector<int> data) {
    std::vector<SpiceInt> d;
    d.reserve(data.size());
    for (int i : data) {
        d.push_back(i);
    }
    dasudi_c(handle, first, last, d.data());
}

[[codegen::luawrap]] void daswbr(int handle) {
    daswbr_c(handle);
}

[[codegen::luawrap]] glm::dmat3 dazldr(double x, double y, double z, bool azccw,
                                       bool elplsz)
{
    SpiceDouble jacobi[3][3];
    dazldr_c(x, y, z, azccw, elplsz, jacobi);
    return glm::dmat3(
        jacobi[0][0], jacobi[0][1], jacobi[0][2],
        jacobi[1][0], jacobi[1][1], jacobi[1][2],
        jacobi[2][0], jacobi[2][1], jacobi[2][2]
    );
}

[[codegen::luawrap]] glm::dmat3 dcyldr(double x, double y, double z) {
    SpiceDouble jacobi[3][3];
    dcyldr_c(x, y, z, jacobi);
    return glm::dmat3(
        jacobi[0][0], jacobi[0][1], jacobi[0][2],
        jacobi[1][0], jacobi[1][1], jacobi[1][2],
        jacobi[2][0], jacobi[2][1], jacobi[2][2]
    );
}

[[codegen::luawrap]] double deltet(double epoch, std::string eptype) {
    SpiceDouble delta;
    deltet_c(epoch, eptype.c_str(), &delta);
    return delta;
}

[[codegen::luawrap]] double det(glm::dmat3 m1) {
    return det_c(glm::value_ptr(m1));
}

[[codegen::luawrap]] std::tuple<glm::dmat2, glm::dmat2> diags2(glm::dmat2 symmat) {
    SpiceDouble diag[2][2];
    SpiceDouble rotate[2][2];
    diags2_c(glm::value_ptr(symmat), diag, rotate);
    return {
        glm::dmat2(diag[0][0], diag[0][1], diag[1][0], diag[1][1]),
        glm::dmat2(rotate[0][0], rotate[0][1], rotate[1][0], rotate[1][1])
    };
}

[[codegen::luawrap]] void diff(SpiceCell* a, SpiceCell* b, SpiceCell* c) {
    diff_c(a, b, c);
}

[[codegen::luawrap]] glm::dmat3 dgeodr(double x, double y, double z, double re, double f)
{
    SpiceDouble jacobi[3][3];
    dgeodr_c(x, y, z, re, f, jacobi);
    return glm::dmat3(
        jacobi[0][0], jacobi[0][1], jacobi[0][2],
        jacobi[1][0], jacobi[1][1], jacobi[1][2],
        jacobi[2][0], jacobi[2][1], jacobi[2][2]
    );
}

[[codegen::luawrap]] std::optional<SpiceDLADescr*> dlabbs(int handle) {
    SpiceDLADescr* dladsc = new SpiceDLADescr;
    SpiceBoolean found;
    dlabbs_c(handle, dladsc, &found);
    if (found) {
        return dladsc;
    }
    else {
        return std::nullopt;
    }
}

[[codegen::luawrap]] std::optional<SpiceDLADescr*> dlabfs(int handle) {
    SpiceDLADescr* dladsc = new SpiceDLADescr;
    SpiceBoolean found;
    dlabfs_c(handle, dladsc, &found);
    if (found) {
        return dladsc;
    }
    else {
        return std::nullopt;
    }
}

[[codegen::luawrap]] void dlabns(int handle) {
    dlabns_c(handle);
}

[[codegen::luawrap]] void dlaens(int handle) {
    dlaens_c(handle);
}

[[codegen::luawrap]] std::optional<SpiceDLADescr*> dlafns(int handle,
                                                          SpiceDLADescr* dladsc)
{
    SpiceDLADescr* nxtdsc = new SpiceDLADescr;
    SpiceBoolean found;
    dlafns_c(handle, dladsc, nxtdsc, &found);
    if (found) {
        return nxtdsc;
    }
    else {
        return std::nullopt;
    }
}

[[codegen::luawrap]] std::optional<SpiceDLADescr*>dlafps(int handle,
                                                         SpiceDLADescr* dladsc)
{
    SpiceDLADescr* prvdsc = new SpiceDLADescr;
    SpiceBoolean found;
    dlafps_c(handle, dladsc, prvdsc, &found);
    if (found) {
        return prvdsc;
    }
    else {
        return std::nullopt;
    }
}

[[codegen::luawrap]] int dlaopn(std::string fname, std::string ftype, std::string ifname,
                                int ncomch)
{
    SpiceInt handle;
    dlaopn_c(fname.c_str(), ftype.c_str(), ifname.c_str(), ncomch, &handle);
    return static_cast<int>(handle);
}

[[codegen::luawrap]] glm::dmat3 dlatdr(double x, double y, double z) {
    SpiceDouble jacobi[3][3];
    dlatdr_c(x, y, z, jacobi);
    return glm::dmat3(
        jacobi[0][0], jacobi[0][1], jacobi[0][2],
        jacobi[1][0], jacobi[1][1], jacobi[1][2],
        jacobi[2][0], jacobi[2][1], jacobi[2][2]
    );
}

[[codegen::luawrap]]
std::optional<std::tuple<std::array<double, 6>, std::array<double, 2>>>
dnearp(std::array<double, 6> state, double a, double b, double c) {
    std::array<double, 6> dnear;
    std::array<double, 2> dalt;
    SpiceBoolean found;
    dnearp_c(state.data(), a, b, c, dnear.data(), dalt.data(), &found);
    if (found) {
        std::tuple t = { dnear, dalt };
        return t;
    }
    else {
        return std::nullopt;
    }
}

[[codegen::luawrap]] std::string dp2hx(double number) {
    std::string hxstr;
    hxstr.resize(256);
    SpiceInt hxssiz;
    dp2hx_c(number, 256, hxstr.data(), &hxssiz);
    hxstr.resize(hxssiz);
    return hxstr;
}

[[codegen::luawrap]] glm::dmat3 dpgrdr(std::string body, double x, double y, double z,
                                       double re, double f)
{
    SpiceDouble jacobi[3][3];
    dpgrdr_c(body.c_str(), x, y, z, re, f, jacobi);
    return glm::dmat3(
        jacobi[0][0], jacobi[0][1], jacobi[0][2],
        jacobi[1][0], jacobi[1][1], jacobi[1][2],
        jacobi[2][0], jacobi[2][1], jacobi[2][2]
    );
}

[[codegen::luawrap]] glm::dmat4 drdazl(double range, double az, double el, bool azccw,
                                       bool elplsz)
{
    SpiceDouble jacobi[3][3];
    drdazl_c(range, az, el, azccw, elplsz, jacobi);
    return glm::dmat3(
        jacobi[0][0], jacobi[0][1], jacobi[0][2],
        jacobi[1][0], jacobi[1][1], jacobi[1][2],
        jacobi[2][0], jacobi[2][1], jacobi[2][2]
    );
}

[[codegen::luawrap]] double dpmax() {
    return dpmax_c();
}

[[codegen::luawrap]] double dpmin() {
    return dpmin_c();
}

[[codegen::luawrap]] double dpr() {
    return dpr_c();
}

[[codegen::luawrap]] glm::dmat3 drdcyl(double r, double clon, double z) {
    SpiceDouble jacobi[3][3];
    drdcyl_c(r, clon, z, jacobi);
    return glm::dmat3(
        jacobi[0][0], jacobi[0][1], jacobi[0][2],
        jacobi[1][0], jacobi[1][1], jacobi[1][2],
        jacobi[2][0], jacobi[2][1], jacobi[2][2]
    );
}

[[codegen::luawrap]] glm::dmat3 drdgeo(double lon, double lat, double alt, double re,
                                       double f)
{
    SpiceDouble jacobi[3][3];
    drdgeo_c(lon, lat, alt, re, f, jacobi);
    return glm::dmat3(
        jacobi[0][0], jacobi[0][1], jacobi[0][2],
        jacobi[1][0], jacobi[1][1], jacobi[1][2],
        jacobi[2][0], jacobi[2][1], jacobi[2][2]
    );
}

[[codegen::luawrap]] glm::dmat3 drdlat(double r, double lon, double lat) {
    SpiceDouble jacobi[3][3];
    drdlat_c(r, lon, lat, jacobi);
    return glm::dmat3(
        jacobi[0][0], jacobi[0][1], jacobi[0][2],
        jacobi[1][0], jacobi[1][1], jacobi[1][2],
        jacobi[2][0], jacobi[2][1], jacobi[2][2]
    );
}

[[codegen::luawrap]] glm::dmat3 drdpgr(std::string body, double lon, double lat,
                                       double alt, double re, double f)
{
    SpiceDouble jacobi[3][3];
    drdpgr_c(body.c_str(), lon, lat, alt, re, f, jacobi);
    return glm::dmat3(
        jacobi[0][0], jacobi[0][1], jacobi[0][2],
        jacobi[1][0], jacobi[1][1], jacobi[1][2],
        jacobi[2][0], jacobi[2][1], jacobi[2][2]
    );
}

[[codegen::luawrap]] glm::dmat3 drdsph(double r, double colat, double slon) {
    SpiceDouble jacobi[3][3];
    drdsph_c(r, colat, slon, jacobi);
    return glm::dmat3(
        jacobi[0][0], jacobi[0][1], jacobi[0][2],
        jacobi[1][0], jacobi[1][1], jacobi[1][2],
        jacobi[2][0], jacobi[2][1], jacobi[2][2]
    );
}

[[codegen::luawrap]]
std::tuple<
    int, int, int, std::array<double, 6>, double, glm::dvec3, glm::ivec3, int, int, int,
    int
>
dskb02(int handle, SpiceDLADescr* dladsc) {
    SpiceInt nv;
    SpiceInt np;
    SpiceInt nvxtot;
    SpiceDouble vtxbds[3][2];
    double voxsiz;
    glm::dvec3 voxori;
    SpiceInt vgrext[3];
    SpiceInt cgscal;
    SpiceInt vtxnpl;
    SpiceInt voxnpt;
    SpiceInt voxnpl;
    dskb02_c(
        handle,
        dladsc,
        &nv,
        &np,
        &nvxtot,
        vtxbds,
        &voxsiz,
        glm::value_ptr(voxori),
        vgrext,
        &cgscal,
        &vtxnpl,
        &voxnpt,
        &voxnpl
    );

    std::tuple t = {
        static_cast<int>(nv),
        static_cast<int>(np),
        static_cast<int>(nvxtot),
        std::array<double, 6> {
            vtxbds[0][0], vtxbds[0][1],
            vtxbds[1][0], vtxbds[1][1],
            vtxbds[2][0], vtxbds[2][1]
        },
        voxsiz,
        voxori,
        glm::ivec3(vgrext[0], vgrext[1], vgrext[2]),
        static_cast<int>(cgscal),
        static_cast<int>(vtxnpl),
        static_cast<int>(voxnpt),
        static_cast<int>(voxnpl)
    };
    return t;
}

[[codegen::luawrap]] void dskcls(int handle, bool optmiz) {
    dskcls_c(handle, optmiz);
}

[[codegen::luawrap]] std::vector<double> dskd02(int handle, SpiceDLADescr* dladsc,
                                                int item, int start, int room)
{
    std::vector<double> values;
    values.resize(room);
    SpiceInt n;
    dskd02_c(handle, dladsc, item, start, room, &n, values.data());
    values.resize(n);
    return values;
}

[[codegen::luawrap]] SpiceDSKDescr* dskgd(int handle, SpiceDLADescr* dladsc) {
    SpiceDSKDescr* dskdsc = new SpiceDSKDescr;
    dskgd_c(handle, dladsc, dskdsc);
    return dskdsc;
}

[[codegen::luawrap]] double dskgtl(int keywrd) {
    SpiceDouble dpval;
    dskgtl_c(keywrd, &dpval);
    return dpval;
}

[[codegen::luawrap]] std::vector<int> dski02(int handle, SpiceDLADescr* dladsc,
                                             int item, int start, int room)
{
    std::vector<SpiceInt> values;
    values.resize(room);
    SpiceInt n;
    dski02_c(handle, dladsc, item, start, room, &n, values.data());
    std::vector<int> val;
    val.resize(n);
    for (SpiceInt i : values) {
        val.push_back(static_cast<int>(i));
    }
    return val;
}

[[codegen::luawrap]] void dskobj(std::string dskfnm, SpiceCell* bodids) {
    dskobj_c(dskfnm.c_str(), bodids);
}

[[codegen::luawrap]] int dskopn(std::string fname, std::string ifname, int ncomch) {
    SpiceInt handle;
    dskopn_c(fname.c_str(), ifname.c_str(), ncomch, &handle);
    return static_cast<int>(handle);
}

[[codegen::luawrap]] glm::dvec3 dskn02(int handle, SpiceDLADescr* dladsc, int plid) {
    glm::dvec3 normal;
    dskn02_c(handle, dladsc, plid, glm::value_ptr(normal));
    return normal;
}


#include "spicemanager_spice_lua_codegen.cpp"

} // namespace
