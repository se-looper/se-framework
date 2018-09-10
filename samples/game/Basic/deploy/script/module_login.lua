local module_login = {};

function module_login:showme()	
	--注册lua消息
	fol.listen:register(self, fol.msgcode.Login_OnLogin,  fol.login.doLogin);
	--注册点击事件
	fol.uipkg:registerClickEvent("frmLogin", "btnLogin",  fol.msgcode.Login_OnLogin);
    --显示
	fol.uipkg:showWindow("frmLogin");
end

function module_login:doLogin()
	--关闭
	fol.uipkg:closeWindow("frmLogin");
end

return module_login;