local LISTEN = {}

local mCallbackTable = {}

function execClick(msgcode)
	local LTable = mCallbackTable[msgcode];
	if LTable ~= nil then
		for k,v in pairs(LTable) do
			v.callback(v.m_this);
		end;
	end
end

function execNetMsg(msgcode, data)
	local LTable = mCallbackTable[msgcode];
	if LTable ~= nil then
		local LData = json.decode(data);
		for k,v in pairs(LTable) do
			v.callback(v.m_this, LData);
		end;
	end
end

function LISTEN:register(this, msgcode, event)
	if mCallbackTable[msgcode] == nil then
		mCallbackTable[msgcode] = {};
		local LTable = mCallbackTable[msgcode];
	end
	local LTable = mCallbackTable[msgcode];
	table.insert(LTable, {m_this=this, callback=event});
end;

function LISTEN:unRegister(msgcode, event)
	if mCallbackTable[msgcode] ~= nil then
		local LTable = mCallbackTable[msgcode];
        for I=1, #(LTable) do  
            if LTable[i].callback == event then  
                LTable[index] = nil;
                table.remove(LTable, I); 
                break;
            end
        end
	else
		print("has not register");
	end
end;

return LISTEN;