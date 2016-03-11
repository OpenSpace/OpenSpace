vec4 sampleEnlil(inout vec4 finalColor, vec3 position) {
	vec3 p = CartesianToSpherical(position);
	float intensity = texture(EnlilVolume, p).x;
	// float intensity = texture(EnlilVolume, position).x;
	// intensity *= 0.001f;
	return texture(EnlilTF, intensity);
	// color = vec4(1.0,0,0,1);
	//blendStep(finalColor, color, stepSize);
}