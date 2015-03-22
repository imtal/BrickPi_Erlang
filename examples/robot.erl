-module(robot).
-compile(export_all).
-include_lib("wx/include/wx.hrl").

start() ->
	Wx=wx:new(),
	Frame=wxFrame:new(Wx, -1, "Robot example!"),
	setup(Frame),
	wxFrame:show(Frame),
	loop(Frame),
    wx:destroy().

setup(Frame) ->
	wxFrame:createStatusBar(Frame),
	wxFrame:setStatusText(Frame, "Starting ..."),
	Text = wxTextCtrl:new(Frame,1),
	wxTextCtrl:setEditable(Text,false),
	wxTextCtrl:setValue(Text,["<b>Usage:</b>",13,10,"- Q: quit",13,10]),
	KeyX = wxAcceleratorEntry:new([{keyCode,?WXK_SPACE}]),
	KeyTab = wxAcceleratorTable:new(1,[KeyX]),
	wxWindow:setAcceleratorTable(Frame,KeyTab),
    wxFrame:connect(Frame, close_window),
    wxFrame:connect(Text, key_down).


loop(Frame) ->
	wxFrame:setStatusText(Frame, ""),
	receive
		#wx{event=#wxClose{}} ->
			io:format("Closed.~n");
		Other ->
			io:format("Unexpected: ~p~n",[Other]),
			loop(Frame)
	end.
