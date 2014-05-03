
#ifdef CL_VERSION_1_2
    #define RC_TF_TYPE image1d_t
    #define RC_TF_MAP(intensity) intensity
#else
    #define RC_TF_TYPE image2d_t
    #define RC_TF_MAP(intensity) (float2)(intensity,0.0f)
#endif


#define EARLY_RAY_TERMINATION_OPACITY 0.95

void raySetup(float3 first, float3 last, float3 dimension, float3* rayDirection, float* tIncr, float* tEnd);
bool earlyRayTermination(float4* color, float maxOpacity);


#define RC_DEFINE_TEXTUE_COORDINATES(coords)       \
    int2 coords = (int2)(get_global_id(0), get_global_id(1))

#define RC_DEFINE_VOLUME3D_DIMENSIONS(dimension, volume)    \
    float3 dimension;                                          \
    {                                                       \
        int4 idim = get_image_dim(volume);                  \
        dimension = (float3)(idim.x,idim.y,idim.z);         \
    }


//#define RC_DEFINE_VARIABLES(dimension, intCoords)

/***
 * Calculates the direction of the ray and returns the number
 * of steps and the direction.
 ***/
void raySetup(float3 first, float3 last, float3 dimension, float3* rayDirection, float* tIncr, float* tEnd) {
    float samplingRate_ = 1.0f;
    *rayDirection = last - first;
    *tEnd = length(*rayDirection);
    *rayDirection = normalize(*rayDirection);
    *tIncr = 1.0/(samplingRate_*length((*rayDirection)*dimension));

}

/***
 * Applies early ray termination. The current opacity is compared to
 * the maximum opacity. In case it is greater, the opacity is set to
 * 1.0 and true is returned, otherwise false is returned.
 ***/
bool earlyRayTermination(float4* color, float maxOpacity) {
    if ((*color).w >= maxOpacity) {
        (*color).w = 1.0f;
        return true;
    } else {
        return false;
    }
}

#define RC_EARLY_RAY_TERMINATION(opacity, maxOpacity, finished)     \
    finished = earlyRayTermination(&opacity, maxOpacity)             

#define RC_BEGIN_LOOP_FOR                                              \
  for (int loop=0; !finished && loop<RAYCASTING_LOOP_COUNT; ++loop) {
#define RC_END_LOOP_BRACES }

/***
 * The beginning of a typical raycasting loop.
 */
#define RC_BEGIN_LOOP                                         \
    bool finished = false;                                    \
    float t = 0.0f;                                             \
    int RAYCASTING_LOOP_COUNT = tEnd / tIncr;               \
    RC_BEGIN_LOOP_FOR


/***
 * The end of a typical raycasting loop. If adaptive sampling
 * is used for rendering bricked volumes, t is increased by a
 * multiple of tIncr, thereby skipping several samples.
 */
#ifdef ADAPTIVE_SAMPLING
#define RC_END_LOOP(result)                                        \
            RC_EARLY_RAY_TERMINATION(result, EARLY_RAY_TERMINATION_OPACITY, finished);    \
            t += (tIncr * float(numberOfSkippedSamples));          \
            finished = finished || (t > tEnd);                     \
    RC_END_LOOP_BRACES                                             
#else
#define RC_END_LOOP(result)                                        \
            RC_EARLY_RAY_TERMINATION(result, EARLY_RAY_TERMINATION_OPACITY, finished);    \
            t += tIncr;                                            \
            finished = finished || (t > tEnd);                     \
    RC_END_LOOP_BRACES                                             

#endif

/**
* In order to keep the shaders as free as possible from dealing
* with bricking and adaptive sampling, these defines can be placed
* before and after the compositing function calls to enable adaptive
* sampling when bricking is used. For normal datasets these defines
* will have no impact at all.
*/
#ifdef ADAPTIVE_SAMPLING
#define RC_BEGIN_COMPOSITING \
    for (int i=0; i<numberOfSkippedSamples; i++) {
#else
    #define RC_BEGIN_COMPOSITING
#endif

#ifdef ADAPTIVE_SAMPLING
#define RC_END_COMPOSITING \
    }
#else
#define RC_END_COMPOSITING
#endif
