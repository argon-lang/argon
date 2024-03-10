
local ArgonRuntime = require "ArgonRuntime"
local m = require "out"

local f = m[""]["main"][ArgonRuntime:intern_signature {parameters = {{type = "tuple",elements = {},},},result = {type = "tuple",elements = {},},}]

f()
