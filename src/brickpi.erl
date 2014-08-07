%%% @author Tjeerd van der Laan <imtal@yolt.nl>
%%% @copyright (C) 2014, Tjeerd van der Laan
%%%
%%% @doc
%%% BrickPi port driver
%%%
%%% Created: December 27, 2013
%%% Updated: July 12, 2014
%%%
%%% You may use this code as you wish, provided you give credit where it's due.
%%%
%%% This program is specifically to be used with the BrickPi.
%%%
%%% This is an Erlang port driver for the BrickPi.
%%%
%%% @end

-module(brickpi).
-vsn('0.8').

-include("brickpi.hrl").

-compile(export_all).

-export([start/0,stop/0,sleep/1]).
-export([change_address/2,set_timeout/1]).
-export([set_sensor_type/2,set_sensor_settings/2,get_sensor_value/1,get_sensor_ext/2]).
-export([set_sensor_i2c_devices/2,set_sensor_i2c_speed/2,set_sensor_i2c_address/3,set_sensor_i2c_out/3,get_sensor_i2c_in/2]).
-export([set_motor_enable/2,set_motor_speed/2,set_motor_offset/2,get_motor_encoder/1]).
-export([setup/0,update/0,update/1,halt/0]).

-record(state, {
            parent = undefined,
            port = undefined,
            timeout = 20000,
            interval = infinity
        }).

%%--------------------------------------------------------------------
%% @doc
%% Starts the BrickPi port driver
%% @end
%%--------------------------------------------------------------------
-spec brickpi:start() -> ok | {error,Reason::atom()}.
start() ->
    Pid = spawn(?MODULE,init,[self()]),
    receive ok -> {ok,Pid} end.

%%--------------------------------------------------------------------
%% @doc
%% Stops the BrickPi port driver
%% @end
%%--------------------------------------------------------------------
-spec brickpi:stop() -> ok | {error,Reason::atom()}.
stop() ->
    brickpi_driver ! stop.

%%--------------------------------------------------------------------
%% @doc
%% Sleep for the given time in milliseconds
%% @end
%%--------------------------------------------------------------------
-spec brickpi:sleep(Time::unsigned()) -> ok.
sleep(Time) ->
    receive after Time -> ok end.

%%--------------------------------------------------------------------
%% @doc
%% Change the address of one of the Arduinos on the BrickPi
%% @end
%%--------------------------------------------------------------------
-spec brickpi:change_address(From::unsigned(),To::unsigned()) -> ok | {error,Reason::atom()}.
change_address(From,To) ->
    call({?M_CHANGE_ADDRESS,From,To}).

%%--------------------------------------------------------------------
%% @doc
%% Sets the timeout on the BrickPi
%% @end
%%--------------------------------------------------------------------
-spec brickpi:set_timeout(Value::unsigned()) -> ok | {error,Reason::atom()}.
set_timeout(Value) ->
    call({?M_SET_TIMEOUT,Value}).

%%--------------------------------------------------------------------
%% @doc
%% Sets the sensor type on one of the ports
%% @end
%%--------------------------------------------------------------------
-spec brickpi:set_sensor_type(Port::unsigned(),Value::unsigned()) -> ok | {error,Reason::atom()}.
set_sensor_type(Port,Value) ->
    call({?M_SET_SENSOR_TYPE,Port,Value}).

%%--------------------------------------------------------------------
%% @doc
%% Sets the sensor settings for the specfied port. The settings are given
%% as a binary with a maximum length of 8 bytes.
%% @end
%%--------------------------------------------------------------------
-spec brickpi:set_sensor_settings(Port::unsigned(),Value::binary()) -> ok | {error,Reason::atom()}.
set_sensor_settings(Port,Value) ->
    call({?M_SET_SENSOR_SETTINGS,Port,Value}).

%%--------------------------------------------------------------------
%% @doc
%% Get the sensor value of the specified port.
%% @end
%%--------------------------------------------------------------------
-spec brickpi:get_sensor_value(Port::unsigned()) -> ok | {error,Reason::atom()}.
get_sensor_value(Port) ->
    call({?M_GET_SENSOR_VALUE,Port}).

%%--------------------------------------------------------------------
%% @doc
%% Get one of the sensor array values of the specified port.
%% @end
%%--------------------------------------------------------------------
-spec brickpi:get_sensor_ext(Port::unsigned(),Index::unsigned()) -> ok | {error,Reason::atom()}.
get_sensor_ext(Port,Index) ->
    call({?M_GET_SENSOR_EXT,Port,Index}).

%%--------------------------------------------------------------------
%% @doc
%% Sets the number of I2C devices on the specified sensor port.
%% @end
%%--------------------------------------------------------------------
-spec brickpi:set_sensor_i2c_devices(Port::unsigned(),Number::unsigned()) -> ok | {error,Reason::atom()}.
set_sensor_i2c_devices(Port,Number) ->
    call({?M_SET_SENSOR_I2C_DEVICES,Port,Number}).

%%--------------------------------------------------------------------
%% @doc
%% Sets the I2C speed on the specified sensor port.
%% @end
%%--------------------------------------------------------------------
-spec brickpi:set_sensor_i2c_speed(Port::unsigned(),Speed::unsigned()) -> ok | {error,Reason::atom()}.
set_sensor_i2c_speed(Port,Speed) ->
    call({?M_SET_SENSOR_I2C_SPEED,Port,Speed}).

%%--------------------------------------------------------------------
%% @doc
%% Sets the address of the specified I2C device on the specified sensor port.
%% @end
%%--------------------------------------------------------------------
-spec brickpi:set_sensor_i2c_address(Port::unsigned(),Device::unsigned(),Address::unsigned()) -> ok | {error,Reason::atom()}.
set_sensor_i2c_address(Port,Device,Address) ->
    call({?M_SET_SENSOR_I2C_ADDRESS,Port,Device,Address}).

%%--------------------------------------------------------------------
%% @doc
%% Sends the data to the specified I2C device on the specified sensor port.
%% @end
%%--------------------------------------------------------------------
-spec brickpi:set_sensor_i2c_out(Port::unsigned(),Device::unsigned(),Output::binary()) -> ok | {error,Reason::atom()}.
set_sensor_i2c_out(Port,Device,Output) ->
    call({?M_SET_SENSOR_I2C_OUT,Port,Device,Output}).

%%--------------------------------------------------------------------
%% @doc
%% Sets the address of the specified I2C device on the specified sensor port.
%% @end
%%--------------------------------------------------------------------
-spec brickpi:get_sensor_i2c_in(Port::unsigned(),Device::unsigned()) -> ok | {error,Reason::atom()}.
get_sensor_i2c_in(Port,Device) ->
    call({?M_GET_SENSOR_I2C_IN,Port,Device}).

%%--------------------------------------------------------------------
%% @doc
%% Enable the motor on the specified port.
%% @end
%%--------------------------------------------------------------------
-spec brickpi:set_motor_enable(Port::unsigned(),Value::boolean()) -> ok | {error,Reason::atom()}.
set_motor_enable(Port,Value) ->
    call({?M_SET_MOTOR_ENABLE,Port,Value}).

%%--------------------------------------------------------------------
%% @doc
%% Set the motor speed on the specified port.
%% @end
%%--------------------------------------------------------------------
-spec brickpi:set_motor_speed(Port::unsigned(),Value::unsigned()) -> ok | {error,Reason::atom()}.
set_motor_speed(Port,Value) ->
    call({?M_SET_MOTOR_SPEED,Port,Value}).

%%--------------------------------------------------------------------
%% @doc
%% Set the motor encoder offset on the specified port.
%% @end
%%--------------------------------------------------------------------
-spec brickpi:set_motor_offset(Port::unsigned(),Value::unsigned()) -> ok | {error,Reason::atom()}.
set_motor_offset(Port,Value) ->
    call({?M_SET_MOTOR_OFFSET,Port,Value}).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve the motor encoder value for the specified port
%% @end
%%--------------------------------------------------------------------
-spec brickpi:get_motor_encoder(Port::unsigned()) -> {ok,Value::unsigned()} | {error,Reason::atom()}.
get_motor_encoder(Port) ->
    call({?M_GET_MOTOR_ENCODER,Port}).

%%--------------------------------------------------------------------
%% @doc
%% Initialize the sensors with the given values.
%% @end
%%--------------------------------------------------------------------
-spec brickpi:setup() -> ok | {error,Reason::atom()}.
setup() ->
    call({?M_SETUP}).

%%--------------------------------------------------------------------
%% @doc
%% Update the values of the BrickPi. This sends all settings to the
%% hardware and gets new values from the sensors.
%% @end
%%--------------------------------------------------------------------
-spec brickpi:update() -> ok | {error,Reason::atom()}.
update() ->
    call({?M_UPDATE}).

%%--------------------------------------------------------------------
%% @doc
%% Starts or stops the automatic update the values of the BrickPi.
%% An interval higher of 0 stops the automatic update, a value higher than
%% zero starts in in steps of 10ms.
%% @end
%%--------------------------------------------------------------------
-spec brickpi:update(Interval::unsigned()) -> ok | {error,Reason::atom()}.
update(Interval) ->
    I = case Interval of
            0 -> infinity;
            _Other -> Interval * 10
        end,
    call({interval,I}).

%%--------------------------------------------------------------------
%% @doc
%% Stops all activity of the motors
%% @end
%%--------------------------------------------------------------------
-spec brickpi:halt() -> ok | {error,Reason::atom()}.
halt() ->
    call({interval,infinity}),
    call({?M_HALT}).



%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------
%% @private
call(Msg) ->
    brickpi_driver ! {call, self(), Msg},
    receive
        {brickpi_driver, Result} -> Result
    after
        20000 -> {error,timeout}
    end.

init(Parent) ->
    register(brickpi_driver, self()),
    process_flag(trap_exit, true),
    Loaded = code:which(?MODULE),
    Path = filename:dirname(Loaded),
    Port = open_port({spawn, Path++"/../priv/bin/brickpi_driver"},[use_stdio,stream,binary]),
    S = #state{ parent = Parent, port=Port },
    Parent ! ok,
    loop(S).


loop(S) ->
    receive
        {call, Caller, {interval,Interval}} ->
            Caller ! {brickpi_driver, ok},
            loop(S#state{ interval=Interval });
        {call, Caller, Msg} ->
            Response = call_driver(S#state.port,Msg,S#state.timeout),
            Caller ! {brickpi_driver, Response},
            loop(S);
        stop ->
            Port = S#state.port,
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', _Port, Reason} ->
            exit({terminated,Reason})
    after
        S#state.interval ->
            call_driver(S#state.port,{?M_UPDATE},S#state.timeout),
            loop(S)
    end.

call_driver(Port,Msg,Timeout) ->
    Port ! {self(), {command, encode(Msg)}},
    receive
        {Port, {data,Data}} -> decode(Data)
    after
        Timeout -> {error,timeout}
    end.

    
% -------------------
encode({Msg}) ->
    <<$s,Msg:8,$e>>;
%
encode({Msg,Param1,Param2})         when Msg == ?M_SET_MOTOR_SPEED ->
    <<$s,Msg:8,Param1:16,Param2:16,$e>>;
%
encode({Msg,Param1,Param2})         when Msg == ?M_SET_MOTOR_ENABLE ->
    <<$s,Msg:8,Param1:16,Param2:16,$e>>;
%
encode({Msg,Param1,Param2})         when Msg == ?M_SET_MOTOR_OFFSET ->
    <<$s,Msg:8,Param1:16,Param2:32,$e>>;
%
encode({Msg,Param1})                when Msg == ?M_GET_MOTOR_ENCODER ->
    <<$s,Msg:8,Param1:16,$e>>;
%
encode({Msg,Param1,Param2})         when Msg == ?M_SET_SENSOR_TYPE ->
    <<$s,Msg:8,Param1:16,Param2:16,$e>>;
%
encode({Msg,Param1,Param2})         when Msg == ?M_SET_SENSOR_SETTINGS ->
    % extend/restrict Param2 to 8 bytes
    L = bit_size(Param2),
    P = if  
            L<64 -> <<Param2/bitstring,0:(64-L)>>;
            true -> binary:part(Param2,0,8)
        end,
    <<$s,Msg:8,Param1:16,P/bitstring,$e>>;
%
encode({Msg,Param1})                when Msg == ?M_GET_SENSOR_VALUE ->
    <<$s,Msg:8,Param1:16,$e>>;
%
encode({Msg,Param1,Param2})         when Msg == ?M_GET_SENSOR_EXT ->
    <<$s,Msg:8,Param1:16,Param2:16,$e>>;
%
encode({Msg,Param1})                when Msg == ?M_SET_TIMEOUT ->
    <<$s,Msg:8,Param1:32,$e>>;
%
encode({Msg,Param1,Param2})         when Msg == ?M_SET_SENSOR_I2C_DEVICES ->
    <<$s,Msg:8,Param1:16,Param2:16,$e>>;
%
encode({Msg,Param1,Param2})         when Msg == ?M_SET_SENSOR_I2C_SPEED ->
    <<$s,Msg:8,Param1:16,Param2:16,$e>>;
%
encode({Msg,Param1,Param2,Param3})  when Msg == ?M_SET_SENSOR_I2C_ADDRESS ->
    <<$s,Msg:8,Param1:16,Param2:16,Param3:16,$e>>;
%
encode({Msg,Param1,Param2,Param3})  when Msg == ?M_SET_SENSOR_I2C_OUT ->
    % extend/restrict Param3 to 16 bytes
    L = bit_size(Param3),
    P = if  
            L<128 -> <<Param3/bitstring,0:(128-L)>>;
            true  -> binary:part(Param3,0,16)
        end,
    <<$s,Msg:8,Param1:16,Param2:16,P/bitstring,$e>>;
%
encode({Msg,Param1,Param2})         when Msg == ?M_GET_SENSOR_I2C_IN ->
    <<$s,Msg:8,Param1:16,Param2:16,$e>>;
%
encode({Msg,Param1,Param2})         when Msg == ?M_CHANGE_ADDRESS ->
    <<$s,Msg:8,Param1:16,Param2:16,$e>>;
%
encode(_Other) ->
    exit(wrong_message_format).


decode(<<?MSG_START:8,?R_OKAY:8,?MSG_END:8>>) ->
    ok;
decode(<<?MSG_START:8,?R_SHORT:8,Value:16,?MSG_END:8>>) ->
    {ok,Value};
decode(<<?MSG_START:8,?R_LONG:8,Value:32,?MSG_END:8>>) ->
    {ok,Value};
decode(<<?MSG_START:8,?R_DATA_8:8,Value:64,?MSG_END:8>>) ->
    {ok,Value};
decode(<<?MSG_START:8,?R_DATA_16:8,Value:128,?MSG_END:8>>) ->
    {ok,Value};
decode(<<?MSG_START:8,?R_ERROR:8,Code:8,?MSG_END:8>>) ->
    case Code of
        ?E_LOAD               -> {error,not_loaded};
        ?E_SETUP              -> {error,setup_sensors};
        ?E_UPDATE             -> {error,update_values};
        ?E_ADDRESS            -> {error,change_address};
        ?E_INVALID_COMMAND    -> {error,invalid_command};
        ?E_PROTOCOL_ERROR     -> {error,protocol_error};
        ?E_BUFFER_OVERFLOW    -> {error,buffer_overflow};
        ?E_PARAMETER_OVERFLOW -> {error,parameter_overflow};
        Else                  -> {error,Else}
    end;
decode(_Other) ->
    {error,bad_response}.
