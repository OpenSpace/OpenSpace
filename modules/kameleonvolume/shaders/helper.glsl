#define KAMELEON_PI      3.14159265358979323846  /* pi */
#define KAMELEON_SQRT1_3 0.57735026919           /* 1/sqrt(3) */

vec3 kameleon_cartesianToSpherical(vec3 zeroToOneCoords) {
    // Put cartesian in [-1..1] range first
    vec3 cartesian = vec3(-1.0,-1.0,-1.0) + zeroToOneCoords * 2.0f;

    float r = length(cartesian);
    float theta, phi;

    if (r == 0.0) {
        theta = phi = 0.0;
    } else {
        theta = acos(cartesian.z/r) / KAMELEON_PI;
        phi = (KAMELEON_PI + atan(cartesian.y, cartesian.x)) / (2.0*KAMELEON_PI );
    }
    r *= KAMELEON_SQRT1_3;
    return vec3(r, theta, phi);
}