vec4 pSampler(inout vec4 finalColor, vec3 position) {
	float intensity = texture(BatsrusPVolume, position).x;
	return texture(BatsrusPTF, intensity);
	//blendStep(finalColor, color, stepSize);
}