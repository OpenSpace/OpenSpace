return {
    {
        Name = "CircularDependency1",
        Parent = "Root",
        Dependencies = {"CircularDependency2"}
    },
    {
        Name = "CircularDependency2",
        Parent = "CircularDependency1"
    }
}
