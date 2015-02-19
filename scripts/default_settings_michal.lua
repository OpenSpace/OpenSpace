openspace.printInfo("Setting default values");
openspace.setPropertyValue("Constellation Bounds.renderable.enabled", false);

openspace.setPropertyValue("Stars.renderable.magnitudeClamp", {0.941, 3.824});
openspace.setPropertyValue("Stars.renderable.exponentialOffset", 6.180);
openspace.setPropertyValue("Stars.renderable.exponentialDampening", 0.838);
openspace.setPropertyValue("Stars.renderable.scaleFactor", 0.563);

openspace.setPropertyValue("MilkyWay.renderable.transparency", 0.65);
openspace.setPropertyValue("MilkyWay.renderable.segments", 50);

openspace.printInfo("Done setting default values");