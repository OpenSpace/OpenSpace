#Version
22.21

#Meta
Name	name
Version	version
Description	description
Author	author
URL	url
License	license

#Module
abc-module		
def-module		
ghi-module		

#Asset
scene/solarsystem/planets/earth/earth	
scene/solarsystem/planets/earth/satellites/satellites	
folder1/folder2/asset	
folder3/folder4/asset2	variable
folder5/folder6/asset3	

#Property
setPropertyValue	{earth_satellites}.Renderable.Enabled	false
setPropertyValue	property_name_1	property_value_1
setPropertyValue	property_name_2	property_value_2
setPropertyValue	property_name_3	property_value_3
setPropertyValueSingle	property_name_4	property_value_5
setPropertyValueSingle	property_name_4	property_value_5
setPropertyValueSingle	property_name_4	property_value_5

#Keybinding
T	T documentation	T name	T Gui-Path	true	T script
U	U documentation	U name	U Gui-Path	false	U script
CTRL+V	CTRL+V documentation	CTRL+V name	CTRL+V Gui-Path	false	CTRL+V script

#Time
relative	-1d

#Camera
goToGeo	"Earth"	58.5877	16.1924	2.0e+07

#MarkNodes
Earth
Mars
Moon
Sun

#AdditionalScripts
script-1
script-2
script-3
