return {
	{
		Name = "DataSurfaces",
		Parent = "SolarSystem",
		Renderable = {
			Type = "ISWAManager",
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