vec4 sampleVolume2(inout vec4 finalColor, vec3 position) {
	float intensity = texture(volume2, position).x;
	return texture(transferFunction2, intensity);
	//blendStep(finalColor, color, stepSize);
}