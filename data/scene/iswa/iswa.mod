return {
	{
		Name = "iSWA",
		Parent = "Root",
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