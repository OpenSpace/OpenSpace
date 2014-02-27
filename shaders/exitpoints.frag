#version 400 core

// uniform sampler2D texUnit;

in vec3 vPosition;
in vec2 texCoords;
out vec4 fragColor;
 
void main() {
	fragColor = vec4(vPosition+0.5, 1.0);
	// if (fragColor.x <= 0.55 && fragColor.x >= 0.45)
	// 	fragColor = vec4(1.0);
	// else if (fragColor.y <= 0.55 && fragColor.y >= 0.45)
	// 	fragColor = vec4(1.0);

	// fragColor = vec4(texCoords.x, texCoords.y, 0.0, 1.0);

	// fragColor = texture(texUnit, -texCoords+1);
}