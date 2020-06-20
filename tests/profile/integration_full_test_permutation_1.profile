#Version
22.21

#MarkNodes
Earth
Mars
Moon
Sun

#Time
relative	-1d

#Camera
goToGeo	"Earth"	58.5877	16.1924	2.0e+07

#Property
setPropertyValue	{earth_satellites}.Renderable.Enabled	false
setPropertyValue	property_name_1	property_value_1
setPropertyValue	property_name_2	property_value_2
setPropertyValue	property_name_3	property_value_3
setPropertyValueSingle	property_name_4	property_value_5
setPropertyValueSingle	property_name_4	property_value_5
setPropertyValueSingle	property_name_4	property_value_5

#Asset
scene/solarsystem/planets/earth/earth	require	
scene/solarsystem/planets/earth/satellites/satellites	require	
folder1/folder2/asset	require	
folder3/folder4/asset2	require	variable
folder5/folder6/asset3	request	

#Keybinding
T	T documentation	T name	T Gui-Path	true	T script
U	U documentation	U name	U Gui-Path	false	U script
CTRL+V	CTRL+V documentation	CTRL+V name	CTRL+V Gui-Path	false	CTRL+V script

#Module
abc-module		
def-module		
ghi-module		

