local NETCOMMAND = {}

function NETCOMMAND:start_login(AUserName, APassword)
	local v = {username = AUserName, password = APassword};
	fol.netpkg:request(fol.netmsgcode.MSG_Start_Login, fol.json.encode(v));
end;

function NETCOMMAND:login_login(AUserName, APassword)
	local v = {username = AUserName, password = APassword};
	fol.netpkg:request(fol.netmsgcode.MSG_Login_Login, fol.json.encode(v));
end;

function NETCOMMAND:register(APhone, AVerCode)
	local v = {phone = APhone, vercode = AVerCode};
	fol.netpkg:request(fol.netmsgcode.MSG_Register, fol.json.encode(v));
end;

return NETCOMMAND;