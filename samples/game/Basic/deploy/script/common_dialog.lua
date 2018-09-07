local DIALOG = {
	mCallbackEvent = nil,
}

function DIALOG:showmessage(ATitle, ACaption, AKind, ACallbackEvent)
	--注册lua消息
	listen:register(self, msgcode.Common_Dialog_OnClickClose,    dialog.doCloseMessageBox);
	listen:register(self, msgcode.Common_Dialog_OnClickYes,      dialog.doMessageBoxYes);
	--绑定按钮事件
	fol.scene:BindClickEvent("frmMessageBox", "btnClose",        msgcode.Common_Dialog_OnClickClose);
	fol.scene:BindClickEvent("frmMessageBox", "btnOK",           msgcode.Common_Dialog_OnClickYes);
	--设置title和caption
	fol.scene:SetCaption("frmMessageBox", "lblTitle", ATitle);
	fol.scene:SetCaption("frmMessageBox", "lblCaption", ACaption);
	--隐藏cancel按钮, 显示ok按钮
	fol.scene:SetVisible("frmMessageBox", "btnOK", true);
	fol.scene:SetVisible("frmMessageBox", "btnCancel", false);
	--根据提示类型显示相应的图标
	if AKind == types.TMessageBoxKind.mkWarnning then
		fol.scene:SetImage("frmMessageBox", "imgKind", "common_img_warn.png");
	elseif AKind == types.TMessageBoxKind.mkError then
		fol.scene:SetImage("frmMessageBox", "imgKind", "common_img_error.png");
	else
		fol.scene:SetImage("frmMessageBox", "imgKind", "common_img_info.png");
	end;
	--模态显示窗口
	fol.scene:ShowModal("frmMessageBox");
	--回调函数
	mCallbackEvent = ACallbackEvent;
end

function DIALOG:messagebox(ATitle, ACaption, AKind, ACallbackEvent)
	--注册lua消息
	listen:register(self, msgcode.Common_Dialog_OnClickClose,    dialog.doCloseMessageBox);
	listen:register(self, msgcode.Common_Dialog_OnClickYes,      dialog.doMessageBoxYes);
	listen:register(self, msgcode.Common_Dialog_OnClickNo,       dialog.doMessageBoxNo);
	--绑定按钮事件
	fol.scene:BindClickEvent("frmMessageBox", "btnClose",        msgcode.Common_Dialog_OnClickClose);
	fol.scene:BindClickEvent("frmMessageBox", "btnOK",           msgcode.Common_Dialog_OnClickYes);
	fol.scene:BindClickEvent("frmMessageBox", "btnCancel",       msgcode.Common_Dialog_OnClickNo);
	--设置title和caption
	fol.scene:SetCaption("frmMessageBox", "lblTitle", ATitle);
	fol.scene:SetCaption("frmMessageBox", "lblCaption", ACaption);
	--显示ok按钮和cancel按钮
	fol.scene:SetVisible("frmMessageBox", "btnOK", true);
	fol.scene:SetVisible("frmMessageBox", "btnCancel", true);
	--根据提示类型显示相应的图标
	if AKind == types.TMessageBoxKind.mkWarnning then
		fol.scene:SetImage("frmMessageBox", "imgKind", "common_img_warn.png");
	elseif AKind == types.TMessageBoxKind.mkError then
		fol.scene:SetImage("frmMessageBox", "imgKind", "common_img_error.png");
	else
		fol.scene:SetImage("frmMessageBox", "imgKind", "common_img_info.png");
	end;
	--模态显示窗口
	fol.scene:ShowModal("frmMessageBox");
	--回调函数
	mCallbackEvent = ACallbackEvent;
end

function DIALOG:doCloseMessageBox()
	listen:unRegister(msgcode.Common_Dialog_OnClickClose,        dialog.doCloseMessageBox);
	listen:unRegister(msgcode.Common_Dialog_OnClickYes,          dialog.doMessageBoxYes);
	listen:unRegister(msgcode.Common_Dialog_OnClickNo,           dialog.doMessageBoxNo);
	
	fol.scene:Close("frmMessageBox");
	mCallbackEvent = nil;
end

function DIALOG:doMessageBoxYes()
	if mCallbackEvent ~= nil then
		mCallbackEvent(types.TMessageBoxID.mbYes);
	end;
	dialog.doCloseMessageBox();
end

function DIALOG:doMessageBoxNo()
	if mCallbackEvent ~= nil then
		mCallbackEvent(types.TMessageBoxID.mbNo);
	end;
	dialog.doCloseMessageBox();
end

function DIALOG:inputbox(ATitle, ACaption, ADefault, ACallbackEvent)
	--注册lua消息
	listen:register(self, msgcode.Common_Dialog_OnClickClose,    dialog.doCloseInputBox);
	listen:register(self, msgcode.Common_Dialog_OnClickYes,      dialog.doInputBoxYes);
	listen:register(self, msgcode.Common_Dialog_OnClickNo,       dialog.doInputBoxNo);
	--绑定按钮事件
	fol.scene:BindClickEvent("frmInputBox", "btnClose",          msgcode.Common_Dialog_OnClickClose);
	fol.scene:BindClickEvent("frmInputBox", "btnOK",             msgcode.Common_Dialog_OnClickYes);
	fol.scene:BindClickEvent("frmInputBox", "btnCancel",         msgcode.Common_Dialog_OnClickNo);	
	--设置title,caption,默认文本
	fol.scene:SetCaption("frmInputBox", "lblTitle", ATitle);
	fol.scene:SetCaption("frmInputBox", "lblCaption", ACaption);
	fol.scene:SetEditText("frmInputBox", "edtInput", ADefault);
	--模态显示窗口
	fol.scene:ShowModal("frmInputBox");
	--回调函数
	mCallbackEvent = ACallbackEvent;
end

function DIALOG:doCloseInputBox()
	listen:unRegister(msgcode.Common_Dialog_OnClickClose,        dialog.doCloseInputBox);
	listen:unRegister(msgcode.Common_Dialog_OnClickYes,          dialog.doInputBoxYes);
	listen:unRegister(msgcode.Common_Dialog_OnClickNo,           dialog.doInputBoxNo);
	
	fol.scene:Close("frmInputBox");
	mCallbackEvent = nil;
end

function DIALOG:doInputBoxYes()
	local LText = fol.scene:GetEditText("frmInputBox", "edtInput");
	if mCallbackEvent ~= nil then
		mCallbackEvent(LText);
	end;
	dialog.doCloseInputBox();
end

function DIALOG:doInputBoxNo()
	dialog.doCloseInputBox();
end

return DIALOG;