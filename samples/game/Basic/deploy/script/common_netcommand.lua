local NETCOMMAND = {}

function NETCOMMAND:loginByGuest()
	fol.net:LoginByGuest(netmsgcode.Start_Login_r);
end;

function NETCOMMAND:login(AUserName, APassword)
	fol.net:Login(netmsgcode.Login_Login_r, AUserName, APassword);
end;

function NETCOMMAND:sendVerCode(APhone)
	fol.net:SendVerCode(netmsgcode.Register_SendVerCode_r, APhone);
end;

function NETCOMMAND:register(APhone, AVerCode)
	fol.net:Register(netmsgcode.Register_Register_r, APhone, AVerCode);
end;

function NETCOMMAND:getRoleList(AAccountID)
	local v = {accountid = AAccountID};
	fol.net:Request(netmsgcode.RoleSelect_GetRoleList, json.encode(v));
end;

function NETCOMMAND:createRole(AAccountID, AName, ARace, ASex)
	local v = {accountid = AAccountID, name = AName, race = ARace, sex = ASex};
	fol.net:Request(netmsgcode.RoleSelect_CreateRole, json.encode(v));
end;

function NETCOMMAND:deleteRole(ARoleID)
	local v = {roleid = ARoleID};
	fol.net:Request(netmsgcode.RoleSelect_DeleteRole, json.encode(v));
end;

function NETCOMMAND:startGame(ARoleID)
	local v = {roleid = ARoleID};
	fol.net:Request(netmsgcode.RoleSelect_StartGame, json.encode(v));
end;

return NETCOMMAND;