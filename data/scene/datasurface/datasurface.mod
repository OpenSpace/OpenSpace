return {
	{
		Name = "DataSurfaces",
		Parent = "Root",
		Renderable = {
			Type = "DataSurfaceContainer"
		},
		Ephemeris = {
			Type = "Spice",
			Body = "Earth",
			Observer = "Sun",
			Kernels = {
			 	"${SPICE}/GSM.ti"
			 }
		}
	}
}