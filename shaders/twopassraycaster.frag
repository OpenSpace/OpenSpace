#version 400 core

uniform sampler2D texBack, texFront;
uniform sampler3D texVolume;
uniform float stepSize;

in vec3 vPosition;
in vec2 texCoords;
out vec4 fragColor;
 
void main() {; 
	fragColor = vec4(texCoords, 1.0, 1.0);

	vec3 front  = texture(texFront, texCoords).xyz;
	vec3 back 	= texture(texBack, texCoords).xyz;
	vec3 direction = back-front;
	float directionLength = length(direction);
	direction = normalize(direction);
	vec3 position = front;
	vec4 tmp, color = vec4(0);

	while (length(position-front) < directionLength && color.r != 1.0) {
		tmp = texture(texVolume, position);
		color = max(color, tmp); // MIP
		position = position + direction * stepSize;
	}

	fragColor	= vec4(color.rrr,1.0);
}