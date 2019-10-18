local parser = require("_parser")
local makeAsset = require("_makeAsset")

profileOut = ProfileFilename:match("^.+%.").."asset"

local resultTable = parser.parseProfile(ProfileFilename)
makeAsset.generateAsset(resultTable, profileOut)
