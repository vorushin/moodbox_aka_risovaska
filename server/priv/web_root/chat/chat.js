/*

Specs
-----

AuthTicketResult getAuthTicket(login, password)
	gets auth ticket to use in all other requests
	

Array<MessageInfo> getChatRoomMessages(chatRoomId, [lastGotMessageId])
    gets next N messages info starting from lastGotMessageId. If lastGotMessageId is out of scope or is null, gets N last items
    

Array<> getChatRoomUsers(chatRoomId)
	gets users currently in the room
	

ChatRoomEnteringResult enterChatRoom(chatRoomId)
	enters chat room. Result contains data to get room events.

void leaveChatRoom(chatRoomId)
	leaves chat room
	
SendMessageResult sendMessage(message


*/

(function()
{

if(!window.MoodBox)
{
	window.MoodBox = {};
}

window.MoodBox.Tools = function()
{
};

var proto = window.MoodBox.Tools.prototype;

proto.byId = function(id)
{
	return window.document.getElementById(id);
};

proto.addDomHandler = function(elementId, eventName, handlerObject, handlerFunction)
{
	return this.addHandler(this.byId(elementId), eventName, handlerObject, handlerFunction);
};

proto.addHandler = function(object, eventName, handlerObject, handlerFunction)
{
	var handler = function(event){ handlerFunction.apply(handlerObject, [object, event]) };

	if(object.addEventListener)
		object.addEventListener(eventName, handler, false);
	else if(object.attachEvent)
		object.attachEvent('on' + eventName, handler);
	else
		throw "Incompatible browser";

	return handler;
};

proto.removeHandler = function(object, eventName, handler)
{
	if(object.removeEventListener)
		object.removeEventListener(eventName, handler, false);
	else if(object.detachEvent)
		object.detachEvent('on' + eventName, handler);
	else
		throw "Incompatible browser";
};

window.MoodBox.Tools.instance = new window.MoodBox.Tools();

})();


(function()
{

if(!window.MoodBox)
{
	window.MoodBox = {};
}

window.MoodBox.Inet = function(url)
{
	this.url = url;
};

var proto = window.MoodBox.Inet.prototype;

proto.send = function(data)
{
	var req = new XMLHttpRequest();
	
	req.onreadystatechange = function()
		{
			try
			{
				if(req.readyState == 4)
				{
					if(req.status == 200)
					{
						alert(this.responseText);
						document.getElementById('debugConsole').value = this.responseText;
					}
					else
					{
						var statusText = '?';
						
						try
						{
							statusText = req.statusText;
						}
						catch(e)
						{
						}
						
						alert("Error requesting data:\n" + statusText);
					}
				}
			}
			catch(e)
			{
				alert('Error requesting data Exception: ' + e.description);
			}
		};

	req.open("POST", this.url, true);
	alert(data);
	req.send(data);
};

//window.MoodBox.Inet.instance = new window.MoodBox.Inet();

})();


(function()
{

if(!window.MoodBox)
{
	window.MoodBox = {};
}

window.MoodBox.Server = function()
{
	// TODO change to JSON ?
	this.inet = new window.MoodBox.Inet("/json");
};

var proto = window.MoodBox.Server.prototype;

proto.signIn = function(login, password)
{
	var template =
	'<?xml version="1.0" encoding="utf-8"?>' +
	'<envelope>' +
	'	<body>' +
	'		<get_auth_ticket>' +
	'			<login>test1</login>' +
	'			<password>test1</password>' +
	'		</get_auth_ticket>' +
	'	</body>' +
	'</envelope>';

//	this.sendRequest(template, function(){});
	this.inet.send(template);
};

window.MoodBox.Server.instance = new window.MoodBox.Server();

})();


(function()
{

if(!window.MoodBox)
{
	window.MoodBox = {};
}

window.MoodBox.Chat = function()
{
/*
	authTicket
	roomId
	eventsKey
*/
};

var proto = window.MoodBox.Chat.prototype;

proto.init = function(event)
{
	this.tools = window.MoodBox.Tools.instance;
	this.server = window.MoodBox.Server.instance;

	this.tools.addDomHandler('signInButton', 'click', this, this.onSignInButtonPressed);
	
	this.roomId = this.getRoomId();
	this.authTicket = this.getSavedAuthTicket();
	
	if(!this.authTicket)
	{
		this.logon();
	}
	else
	{
		//signUpForEvents();
	}
};

proto.getRoomId = function()
{
	return 1; // TODO
};

proto.getSavedAuthTicket = function()
{
	return null; // TODO
};

proto.logon = function()
{
	this.showLogonDialog();
};

proto.showLogonDialog = function()
{
	this.tools.byId('mainScreen').style.display = 'none';
	this.tools.byId('signInScreen').style.display = '';
};

proto.onSignInButtonPressed = function(target, event)
{
	var login = this.tools.byId('loginField').value;
	var password = this.tools.byId('passwordField').value;

	this.server.signIn(login, password);
};

window.MoodBox.Chat.instance = new window.MoodBox.Chat();

})();