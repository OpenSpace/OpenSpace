FOR %%N in (reduced, front, default) DO (
	FOR %%T IN (shen, sand, opacity, leaf, child) DO (
		START /W bin\openspace\Release\OpenSpace.exe -c mrv-%1-%%T-%%N.cfg
	)
)
