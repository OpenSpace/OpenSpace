--[[  OpenSpace startup script ]]--
-- This Lua script get executed once at the start of the application

openspace.iswa.addCygnet("-1,Data,1");
openspace.iswa.addCygnet("-2,Data,1");
openspace.iswa.addCygnet("-3,Data,1");

openspace.registerScreenSpaceRenderable(
{
    Type = "ScreenSpaceCygnet",
    CygnetId = 7,
    Position = {-0.8, 0.3},
    FlatScreen = true,
    Scale = 0.25,
});

openspace.registerScreenSpaceRenderable(
{
    Type = "ScreenSpaceImage",
    TexturePath = "${OPENSPACE_DATA}/test2.jpg",
    Position = {0.8, -0.3},
    FlatScreen = true,
    Scale = 0.25,
});