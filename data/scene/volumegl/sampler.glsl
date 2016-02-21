vec4 sampleVolume1(inout vec4 finalColor, vec3 position) {
	float intensity = texture(volume1, position).x;
	return texture(transferFunction1, intensity);
	//blendStep(finalColor, color, stepSize);
}