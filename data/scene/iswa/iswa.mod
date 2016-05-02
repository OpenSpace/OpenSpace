return {
	{
		Name = "iSWA",
		Parent = "SolarSystem",
		Renderable = {
			Type = "ISWAContainer",
			TextureCygnets = "[]",
			DataCygnets = "[-2]",
			Frame = "GALACTIC"
		},
		Ephemeris = {
			Type = "Spice",
			Body = "Sun",
			Observer = "Earth",
			Kernels = {
			 	"${SPICE}/iSWAKernels/heliospheric.tf",
			 	"${SPICE}/iSWAKernels/GSE.ti",
			 	"${SPICE}/iSWAKernels/GSM.ti",
			 }
		}
	}
}