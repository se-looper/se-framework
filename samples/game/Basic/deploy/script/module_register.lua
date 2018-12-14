local module_register = {};

function module_register:showme()	
	--注册lua消息
	fol.listen:register(self, fol.msgcode.Register_OnClickClose,     fol.register.doClose);
	fol.listen:register(self, fol.msgcode.Register_OnClickRegister,  fol.register.doRegister);
	
	fol.listen:register(self, fol.netmsgcode.MSG_Register_Register_r,fol.start.doRegister_r);
	
	--注册点击事件
	fol.uipkg:registerClickEvent("frmRegister", "btnClose",          fol.msgcode.Register_OnClickClose);
	fol.uipkg:registerClickEvent("frmRegister", "btnRegister",       fol.msgcode.Register_OnClickRegister);
	
    --显示注册窗口
	fol.uipkg:showWindow("frmRegister");
end

function module_register:doClose()
	--关闭注册窗口
	fol.uipkg:closeWindow("frmRegister");
end

function module_register:doRegister()
	--发送注册请求
	fol.netcmd:register("13800591505", "123456");
end

function module_register:doRegister_r(data)
	--注册请求的响应
	if data.result == true then
		OutputDebugString("register success...");
	else
		OutputDebugString("register fail...");
	end
	--
	OutputDebugString(data.data);
end

return module_register;