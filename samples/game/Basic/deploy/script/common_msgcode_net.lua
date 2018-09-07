local NETMSGCODE = {
	--Start_Login                   = 100001,
	Start_Login_r                 = 100002,

	--Login_Login                   = 101101,
	Login_Login_r                 = 101102,
	
	--Register_Register             = 102101,
	Register_Register_r           = 102102,
	--Register_SendVerCode          = 102103,
	Register_SendVerCode_r        = 102104,
	
	RoleSelect_GetRoleList        = 103101,
	RoleSelect_GetRoleList_r      = 103102,
	RoleSelect_DeleteRole         = 103103,
	RoleSelect_DeleteRole_r       = 103104,
	RoleSelect_CreateRole         = 103105,
	RoleSelect_CreateRole_r       = 103106,
	RoleSelect_StartGame          = 103107,	
	RoleSelect_StartGame_r        = 103108,
}

return NETMSGCODE;