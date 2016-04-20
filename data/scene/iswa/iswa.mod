return {
	{
		Name = "iSWA",
		Parent = "SolarSystem",
		Renderable = {
			Type = "ISWAContainer",
			TextureCygnets = "[-1, -3, -2]",
			DataCygnets = "[]",
			Frame = "GALACTIC"
		},
		Ephemeris = {
			Type = "Spice",
			Body = "Sun",
			Observer = "Earth",
			Kernels = {
			 	"${SPICE}/GSM.ti",
			 	"${SPICE}/GSE.ti"
			 }
		}
	}
}