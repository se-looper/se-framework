fol = {
	basic = require "BasicPackage",
};

function SetPackagePath(apath)
	package.path = package.path .. ";".. apath .. "?.lua";
end

function Start()
	fol.start = require "module_start";
	fol.start:init("init modules");
end

fol.basic:ShowMsg("from lua msg...");