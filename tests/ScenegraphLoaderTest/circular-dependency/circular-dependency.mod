return {
    {
        Name = "CircularDependency1",
        Parent = "CircularDependency2"
    },
    {
        Name = "CircularDependency2",
        Parent = "CircularDependency1"
    }
}
