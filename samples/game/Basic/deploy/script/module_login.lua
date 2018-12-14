local module_login = {};

function module_login:showme()	
	--注册lua消息
	fol.listen:register(self, fol.msgcode.Login_OnClickClose,     fol.login.doClose);
	fol.listen:register(self, fol.msgcode.Login_OnClickLogin,     fol.login.doLogin);
	fol.listen:register(self, fol.msgcode.Login_OnClickRegister,  fol.login.doRegister);
	
	fol.listen:register(self, fol.netmsgcode.MSG_Login_Login_r,   fol.login.doLogin_r);
	
	--注册点击事件
	fol.uipkg:registerClickEvent("frmLogin", "btnClose",          fol.msgcode.Login_OnClickClose);
	fol.uipkg:registerClickEvent("frmLogin", "btnLogin",          fol.msgcode.Login_OnClickLogin);
	fol.uipkg:registerClickEvent("frmLogin", "btnRegister",       fol.msgcode.Login_OnClickRegister);
	
    --显示登录窗口
	fol.uipkg:showWindow("frmLogin");
end

function module_login:doClose()
	--关闭登录窗口
	fol.uipkg:closeWindow("frmLogin");
end

function module_login:doLogin()	
	--发送登录请求
	fol.netcmd:login_login("test", "654321");	
end

function module_login:doRegister()
	--跳转到注册模块
	fol.register:showme();
end

function module_login:doLogin_r(data)
	--登录请求的响应
	if data.result == true then
		fol.login:doClose();
	else
		OutputDebugString("login fail...");
	end
	--
	
	OutputDebugString(data.data);
end

return module_login;