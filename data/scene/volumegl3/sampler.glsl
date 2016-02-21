vec4 sampleVolume3(inout vec4 finalColor, vec3 position) {
	float intensity = texture(volume3, position).x;
	return texture(transferFunction3, 1-intensity);
	//blendStep(finalColor, color, stepSize);
}