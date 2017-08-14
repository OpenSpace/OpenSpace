vec4 rhoSampler(inout vec4 finalColor, vec3 position) {
	float intensity = texture(BatsrusRhoVolume, position).x;
	return texture(BatsrusRhoTF, intensity);
	//blendStep(finalColor, color, stepSize);
}