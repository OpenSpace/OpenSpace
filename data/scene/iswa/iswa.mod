return {
	{
		Name = "iSWA",
		Parent = "SolarSystem",
		Renderable = {
			Type = "ISWAContainer",
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