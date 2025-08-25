bool rayIntersectsEllipsoid(vec3 rayOrigin, vec3 rayDir, vec3 ellipsoidCenter, vec3 ellipsoidRadii) {
    // Translate ray to ellipsoid's local coordinate system
    vec3 oc = rayOrigin - ellipsoidCenter;
    
    // Normalize by ellipsoid radii to convert to unit sphere problem
    vec3 ocNorm = oc / ellipsoidRadii;
    vec3 dirNorm = rayDir / ellipsoidRadii;
    
    // Quadratic equation coefficients: At² + Bt + C = 0
    float a = dot(dirNorm, dirNorm);
    float b = dot(ocNorm, dirNorm); // Note: factor of 2 moved to discriminant calc
    float c = dot(ocNorm, ocNorm) - 1.0;
    
    // Calculate discriminant (optimized: b² - ac since we factored out the 2)
    float discriminant = b * b - a * c;
    
    // Early exit if no intersection
    if (discriminant < 0.0) {
        return false;
    }
    
    // Check if at least one intersection is in front of ray origin
    // For quadratic At² + 2Bt + C = 0, if we want to check if any t >= 0:
    // If C <= 0, ray origin is inside ellipsoid, so definitely intersects
    if (c <= 0.0) {
        return true;
    }
    
    // If both intersections exist and C > 0, check if the smaller root t1 >= 0
    // t1 = (-b - sqrt(discriminant)) / a
    // Since we need t1 >= 0: -b - sqrt(discriminant) >= 0
    // This means: -b >= sqrt(discriminant), so b <= -sqrt(discriminant)
    // Since sqrt(discriminant) >= 0, this means b <= 0
    return b <= 0.0;
}