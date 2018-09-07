fol = {
	uipackage = require "UIPackage",
};

function InitRunEnvironment(aPath)
	package.path = package.path .. ";".. aPath .. "?.lua";
	
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
	
	local str="{\"id\": 199, \"name\": \"testjson\"}";
	local jsonData = fol.json.decode(str);
	fol.uipackage:ShowMsg("from lua msg: "..jsonData.name);
end