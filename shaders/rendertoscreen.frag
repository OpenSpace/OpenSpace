#version 400 core

uniform sampler2D texBack, texFront;
uniform sampler3D texVolume;
uniform int screenWidth, screenHeight;

in vec3 vPosition;
in vec2 texCoords;
out vec4 fragColor;
 
void main() {; 
	fragColor = vec4(texCoords, 1.0, 1.0);

	vec3 front  = texture(texFront, texCoords).xyz;
	vec3 back 	= texture(texBack, texCoords).xyz;
	vec3 direction = back-front;
	direction = normalize(direction);
	vec3 position = front;
	float stepSize = 0.05;
	vec4 color = vec4(0);
	vec4 tmp;
	

	for (int i = 0; i < 100; ++i) {
		tmp = texture(texVolume, position);
		if (tmp.r > color.r)
			color = tmp;

		position = position + direction * stepSize;
	}

	fragColor	= vec4(color.rrr,1.0);
}