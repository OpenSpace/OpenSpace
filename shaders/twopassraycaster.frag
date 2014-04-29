#version 400 core

uniform sampler2D texBack, texFront;
uniform sampler3D texVolume;
uniform float stepSize;

in vec3 vPosition;
in vec2 texCoords;
out vec4 fragColor;
 
void main() {
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

	fragColor = vec4(color.rrr, 1.0);
	
	// // DEBUG DEBUG DEBUG
	// fragColor = vec4(front, 1.0);
	// if (front.x < 0.1)
	//     fragColor = vec4(1.0);

	// if (front.y < 0.1)
	//     fragColor = vec4(1.0);

	// if (front.x > 0.9)
	//     fragColor = vec4(1.0);

	// if (front.y > 0.9)
	//     fragColor = vec4(1.0);

	// if (front.x > 0.45 && front.x < 0.55)
	//     fragColor = vec4(0.0);

	// if (front.y > 0.45 && front.y < 0.55)
	//     fragColor = vec4(0.0);
}