local module_start = {};

function module_start:init()
	--注册lua消息
	fol.listen:register(self, fol.msgcode.Start_OnClickShowLogin,     fol.start.showLogin);
	fol.listen:register(self, fol.msgcode.Start_OnClickShowRank,      fol.start.showRank);
	--注册点击事件
	fol.uipackage:registerClickEvent("MainForm", "btnShowLogin",      fol.msgcode.Start_OnClickShowLogin);
	fol.uipackage:registerClickEvent("MainForm", "btnShowRank",       fol.msgcode.Start_OnClickShowRank);
end

function module_start:showLogin()
	--显示登录窗口
	fol.login:showme();
end

function module_start:showRank()
	--显示排行窗口
	fol.rank:showme();
end

return module_start;