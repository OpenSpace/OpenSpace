#version 440
uniform sampler2D texture1;
uniform vec3 Color;

in vec4 vs_position;
in vec2 texCoord;

layout(location = 2) in vec3 ge_brightness;


out vec4 diffuse;


#include "ABuffer/abufferStruct.hglsl"
#include "ABuffer/abufferAddToBuffer.hglsl"
#include "PowerScaling/powerScaling_fs.hglsl"

//---------------------------------------------------------------------------
vec4 bv2rgb(float bv)    // RGB <0,1> <- BV <-0.4,+2.0> [-]
{
    float t; 
	vec4 c;

	// TODO CHECK: Isn't t uninitialized here?
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


void main(void)
{

	// Something in the color calculations need to be changed because before it was dependent
	// on the gl blend functions since the abuffer was not involved
	//glDisable(GL_DEPTH_TEST);
	//glEnable(GL_BLEND);
	//glBlendFunc(GL_ONE_MINUS_DST_COLOR, GL_ONE); 
	vec4 color = bv2rgb(ge_brightness[0])/1.1;
	//color = 1/ color;
	//color.a = 1-color.a;
    diffuse = texture2D(texture1, texCoord)*color;

    
  // diffuse = vec4(Color, 1.0);

   	vec4 position = vs_position;
	float depth = pscDepth(position);

	//ABufferStruct_t frag = createGeometryFragment(vec4(1,0,0,1), position, depth);
	ABufferStruct_t frag = createGeometryFragment(diffuse, position, depth);
	addToBuffer(frag);

	//discard;
    
}