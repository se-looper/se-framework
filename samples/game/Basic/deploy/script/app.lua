fol = {
	uipackage = require "UIPackage",
};

function InitRunEnvironment(aScriptPath, aLibPath)
	package.path  = package.path .. ";".. aScriptPath .. "?.lua";
	package.cpath = package.cpath.. ";".. aLibPath;
	
	fol.json       = require "lib_json";
	fol.msgcode    = require "common_msgcode";
	fol.netmsgcode = require "common_msgcode_net";
	fol.types      = require "common_types";
	fol.dialog     = require "common_dialog";
	fol.listen     = require "common_listen";
	fol.netcmd     = require "common_netcommand";
	fol.start      = require "module_start";
	fol.login      = require "module_login";
	fol.rank       = require "module_rank";
end

function Start()
	fol.start:init();
end