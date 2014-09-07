#version 440
uniform sampler2D texture1;
uniform vec3 Color;

in vec4 vs_position;
in vec2 texCoord;

layout(location = 2) in vec3 ge_brightness;

out vec4 diffuse;

in float scale; 

const float k = 10.0;

//---------------------------------------------------------------------------
vec4 bv2rgb(float bv)    // RGB <0,1> <- BV <-0.4,+2.0> [-]
{
    float t; 
	vec4 c;
	if (t<-0.4) t=-0.4; if (t> 2.0) t= 2.0;
         if ((bv>=-0.40)&&(bv<0.00)) { t=(bv+0.40)/(0.00+0.40); c.r=0.61+(0.11*t)+(0.1*t*t); }
    else if ((bv>= 0.00)&&(bv<0.40)) { t=(bv-0.00)/(0.40-0.00); c.r=0.83+(0.17*t)          ; }
    else if ((bv>= 0.40)&&(bv<2.10)) { t=(bv-0.40)/(2.10-0.40); c.r=1.00                   ; }
         if ((bv>=-0.40)&&(bv<0.00)) { t=(bv+0.40)/(0.00+0.40); c.g=0.70+(0.07*t)+(0.1*t*t); }
    else if ((bv>= 0.00)&&(bv<0.40)) { t=(bv-0.00)/(0.40-0.00); c.g=0.87+(0.11*t)          ; }
    else if ((bv>= 0.40)&&(bv<1.60)) { t=(bv-0.40)/(1.60-0.40); c.g=0.98-(0.16*t)          ; }
    else if ((bv>= 1.60)&&(bv<2.00)) { t=(bv-1.60)/(2.00-1.60); c.g=0.82         -(0.5*t*t); }
         if ((bv>=-0.40)&&(bv<0.40)) { t=(bv+0.40)/(0.40+0.40); c.b=1.00                   ; }
    else if ((bv>= 0.40)&&(bv<1.50)) { t=(bv-0.40)/(1.50-0.40); c.b=1.00-(0.47*t)+(0.1*t*t); }
    else if ((bv>= 1.50)&&(bv<1.94)) { t=(bv-1.50)/(1.94-1.50); c.b=0.63         -(0.6*t*t); }
	
	return c;
}
//---------------------------------------------------------------------------

vec4 psc_normlization(vec4 invec) {
	
	float xymax = max(invec.x,invec.y);

	if(invec.z > 0.0f || invec.z < 0.0f) {
		return invec / abs(invec.z);
	} else if (xymax != 0.0f) {
		return invec / xymax;
	} else {
		return invec / -.0;
	}
}

void main(void)
{
	// Observable universe is 10^27m, setting the far value to extremely high, aka 30!! ERMAHGERD!
	float s_far			= 27.0; //= gl_DepthRange.far;	// 40
	float s_farcutoff	= 12.0;
	float s_nearcutoff	= 7.0;
	float s_near		= 0.0f;// gl_DepthRange.near;	// 0.1
	float depth;

	// the value can be normalized to 1
	
	vec4 p = vs_position;
	if(vs_position.w <= 0.5) {
		//depth = abs(vs_position.z * pow(10, vs_position.w)) / pow(k,s_far);
		depth = (vs_position.w+log(abs(vs_position.z)))/pow(k, vs_position.w);
	} else if(vs_position.w < 3.0) {
		depth = vs_position.w+log(abs(vs_position.z))/pow(k, vs_position.w);
	} else {
		depth = vs_position.w+log(abs(vs_position.z));
	}
	
	// DEBUG
	float depth_orig = depth;
	float x = 0.0f;
	float cutoffs = 0.0;
	float orig_z = vs_position.z;
	
	// calculate a normalized depth [0.0 1.0]
	if((depth > s_near && depth <= s_nearcutoff) || (depth > s_farcutoff && depth < s_far)) {

		// completely linear interpolation [s_near .. depth .. s_far]
		depth = (depth - s_near) / (s_far - s_near);

	} else if(depth > s_nearcutoff && depth < s_farcutoff) {

		// DEBUG
		cutoffs = 1.0;
		// interpolate [10^s_nearcutoff .. 10^depth .. 10^s_farcutoff]
		// calculate between 0..1 where the depth is
		x = (pow(10,depth) - pow(10, s_nearcutoff)) / (pow(10,s_farcutoff) - pow(10, s_nearcutoff));

		// remap the depth to the 0..1 depth buffer
		depth = s_nearcutoff + x * (s_farcutoff - s_nearcutoff);
		depth = (depth - s_near) / (s_far - s_near);

	} else {
		// where am I?
		// do I need to be discarded?
		// discard;
	}
	
	// set the depth
	gl_FragDepth = depth;

	
	//float a = ge_brightness[2]/500;
	float b = scale;
	vec4 color = bv2rgb(ge_brightness[0]*b)/250;
	//color*=b;
	
	// GL_SMOOTH_POINTS decrepated in core profile. 
	if(dot(gl_PointCoord-0.5,gl_PointCoord-0.5)>0.25) 
		discard;
	else
		//diffuse = vec4(color.xyz, 1);//*b;
		diffuse = vec4(1)*b;
		
   //diffuse = vec4(Color, 1.0);
    
}