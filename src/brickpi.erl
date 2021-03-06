%%% @author Tjeerd van der Laan <tjeerd@imtal.nl>
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
-vsn('0.81').

-include("brickpi.hrl").

-compile(export_all).

-export([start/0,stop/0,sleep/1]).
-export([change_address/2,set_timeout/1]).
-export([set_sensor_type/2,set_sensor_i2c_settings/3,get_sensor_value/1,get_sensor_ext/2]).
-export([i2c_setup/0,set_sensor_i2c_devices/2,set_sensor_i2c_speed/2,set_sensor_i2c_address/3,set_sensor_i2c_write/3,set_sensor_i2c_read/3,set_sensor_i2c_out/3,get_sensor_i2c_in/2]).
-export([set_motor_enable/2,set_motor_speed/2,set_motor_offset/2,get_motor_encoder/1]).
-export([setup/0,update/0,update/1,halt/0]).

-record(state, {
            parent = undefined,
            port = undefined,
            timeout = 20000,
            interval = infinity,
            timer = none,
            monitors = []
        }).

-record(monitor,{
            type = undefined,
            port = undefined,
            message = undefined,
            handler = undefined
        }).

%%--------------------------------------------------------------------
%% @doc
%% Starts the BrickPi port driver
%% @end
%%--------------------------------------------------------------------
-spec brickpi:start() -> {ok,Pid::pid()} | {error,Reason::atom()}.
start() ->
    case whereis(brickpi_driver) of
        undefined ->
            Pid = spawn(?MODULE,init,[self()]),
            receive ok -> {ok,Pid} end;
        _Other ->
            {error,already_running}
    end.
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
%% Sets the sensor settings for the specfied I2C device on the specified port.
%% The settings should be given as an unsigned of which only 8 bits are used.
%% @end
%%--------------------------------------------------------------------
-spec brickpi:set_sensor_i2c_settings(Port::unsigned(),Device::unsigned(),Value::unsigned()) -> ok | {error,Reason::atom()}.
set_sensor_i2c_settings(Port,Device,Value) ->
    call({?M_SET_SENSOR_SETTINGS,Port,Device,Value}).

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
%% Sets automatic monitoring of a sensor value
%% and sends messages when this value changes. Without passing a handler
%% pid the messages are sent tio the calling process.
%% @end
%%--------------------------------------------------------------------
-spec brickpi:set_sensor_monitor(Port::unsigned()) -> ok | {error,Reason::atom()}.
set_sensor_monitor(Port) ->
    call({monitor,sensor_value,Port,{?M_GET_SENSOR_VALUE,Port},self()}).

-spec brickpi:set_sensor_monitor(Port::unsigned(),Handler::pid()) -> ok | {error,Reason::atom()}.
set_sensor_monitor(Port,Handler) ->
    call({monitor,sensor_value,Port,{?M_GET_SENSOR_VALUE,Port},Handler}).

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
%% Sets the amount of bytes that are sent to
%% the specified I2C device on the specified sensor port.
%% @end
%%--------------------------------------------------------------------
-spec brickpi:set_sensor_i2c_write(Port::unsigned(),Device::unsigned(),Count::unsigned()) -> ok | {error,Reason::atom()}.
set_sensor_i2c_write(Port,Device,Count) ->
    call({?M_SET_SENSOR_I2C_WRITE,Port,Device,Count}).

%%--------------------------------------------------------------------
%% @doc
%% Sets the amount of bytesthat are read from
%% the specified I2C device on the specified sensor port.
%% @end
%%--------------------------------------------------------------------
-spec brickpi:set_sensor_i2c_read(Port::unsigned(),Device::unsigned(),Count::unsigned()) -> ok | {error,Reason::atom()}.
set_sensor_i2c_read(Port,Device,Count) ->
    call({?M_SET_SENSOR_I2C_READ,Port,Device,Count}).

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
%% Reads the data from the specified I2C device on the specified sensor port.
%% @end
%%--------------------------------------------------------------------
-spec brickpi:get_sensor_i2c_in(Port::unsigned(),Device::unsigned()) -> {ok,Value::binary()} | {error,Reason::atom()}.
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
%% Sets automatic monitoring of a motor encoder value
%% and sends messages when this value changes. Without passing a handler
%% pid the messages are sent tio the calling process.
%% @end
%%--------------------------------------------------------------------
-spec brickpi:set_motor_monitor(Port::unsigned()) -> ok | {error,Reason::atom()}.
set_motor_monitor(Port) ->
    call({monitor,motor_encoder,Port,{?M_GET_MOTOR_ENCODER,Port},self()}).

-spec brickpi:set_motor_monitor(Port::unsigned(),Handler::pid()) -> ok | {error,Reason::atom()}.
set_motor_monitor(Port,Handler) ->
    call({monitor,motor_encoder,Port,{?M_GET_MOTOR_ENCODER,Port},Handler}).

%%--------------------------------------------------------------------
%% @doc
%% Setup I2C communication on port 5
%% @end
%%--------------------------------------------------------------------
-spec brickpi:i2c_setup() -> ok | {error,Reason::atom()}.
i2c_setup() ->
    call({?M_I2C_SETUP}).

%%--------------------------------------------------------------------
%% @doc
%% Read from I2C device on port 5
%% @end
%%--------------------------------------------------------------------
-spec brickpi:i2c_read(Address::unsigned()) -> {ok,Value::unsigned()} | {error,Reason::atom()}.
i2c_read(Address) ->
    call({?M_I2C_READ,Address}).


%%--------------------------------------------------------------------
%% @doc
%% Write to I2C device on port 5
%% @end
%%--------------------------------------------------------------------
-spec brickpi:i2c_write(Address::unsigned(), Data::unsigned()) -> ok | {error,Reason::atom()}.
i2c_write(Address,Data) ->
    call({?M_I2C_WRITE,Address,Data}).


%%--------------------------------------------------------------------
%% @doc
%% Read byte from I2C device register on port 5
%% @end
%%--------------------------------------------------------------------
-spec brickpi:i2c_read_reg_8(Address::unsigned(), Reg::unsigned()) -> {ok,Value::unsigned()} | {error,Reason::atom()}.
i2c_read_reg_8(Address,Reg) ->
    call({?M_I2C_READ_REG_8,Address,Reg}).


%%--------------------------------------------------------------------
%% @doc
%% Write byte to I2C device register on port 5
%% @end
%%--------------------------------------------------------------------
-spec brickpi:i2c_write_reg_8(Address::unsigned(), Reg::unsigned(), Data::unsigned()) -> ok | {error,Reason::atom()}.
i2c_write_reg_8(Address,Reg,Data) ->
    call({?M_I2C_WRITE_REG_8,Address,Reg,Data}).


%%--------------------------------------------------------------------
%% @doc
%% Read word from I2C device register on port 5
%% @end
%%--------------------------------------------------------------------
-spec brickpi:i2c_read_reg_16(Address::unsigned(), Reg::unsigned()) -> {ok,Value::unsigned()} | {error,Reason::atom()}.
i2c_read_reg_16(Address,Reg) ->
    call({?M_I2C_READ_REG_16,Address,Reg}).


%%--------------------------------------------------------------------
%% @doc
%% Write word to I2C device register on port 5
%% @end
%%--------------------------------------------------------------------
-spec brickpi:i2c_write_reg_16(Address::unsigned(), Reg::unsigned(), Data::unsigned()) -> ok | {error,Reason::atom()}.
i2c_write_reg_16(Address,Reg,Data) ->
    call({?M_I2C_WRITE_REG_16,Address,Reg,Data}).


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
%% An interval of 0 stops the automatic update, a value higher than
%% zero starts automatic updates using this interval in milliseconds.
%% An interval smaller than 10ms generates an error
%% @end
%%--------------------------------------------------------------------
-spec brickpi:update(Interval::unsigned()) -> ok | {error,Reason::atom()}.
update(Interval) ->
    case Interval of
        0 ->
            call({interval,infinity});
        I when I<10 ->
            {error,interval_too_small};
        I ->
            call({interval,I})
    end.

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
        update ->
            call_driver(S#state.port,{?M_UPDATE},S#state.timeout),
            F = fun(M) ->
                    case call_driver(S#state.port,M#monitor.message,S#state.timeout) of
                        {ok,Value} ->
                            M#monitor.handler ! {M#monitor.type, M#monitor.port, Value};
                        _Other ->
                            M#monitor.handler ! {M#monitor.type, M#monitor.port, error}
                    end
                end,
            lists:foreach(F,S#state.monitors),
            loop(S);
        {call, Caller, {interval,infinity}} ->
            case timer:cancel(S) of
                {ok,cancel} ->
                    Caller ! {brickpi_driver, ok};
                {error,Reason} ->
                    Caller ! {brickpi_driver, {error,Reason}}
            end,
            loop(S#state{ timer=none, interval=infinity });
        {call, Caller, {interval,Interval}} ->
            case timer:send_interval(Interval,update) of
                {ok,Pid} ->
                    Caller ! {brickpi_driver, ok},
                    loop(S#state{ timer=Pid, interval=Interval });
                {error,Reason} ->
                    Caller ! {brickpi_driver, {error,Reason}},
                    loop(S)
            end;
        {call, Caller, {monitor,Type,Port,Msg,Handler}} ->
            M = #monitor{ type=Type, port=Port, message=Msg, handler=Handler },
            L = S#state.monitors,
            Caller ! {brickpi_driver, ok},
            loop(S#state{ monitors=[M|L] });
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
encode({Msg,Param1,Param2,Param3})  when Msg == ?M_SET_SENSOR_SETTINGS ->
    <<$s,Msg:8,Param1:16,Param2:16,Param3:16,$e>>;
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
encode({Msg,Param1,Param2,Param3})  when Msg == ?M_SET_SENSOR_I2C_WRITE ->
    <<$s,Msg:8,Param1:16,Param2:16,Param3:16,$e>>;
%
encode({Msg,Param1,Param2,Param3})  when Msg == ?M_SET_SENSOR_I2C_READ ->
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
encode({Msg,Param1})                when Msg == ?M_I2C_READ ->
    <<$s,Msg:8,Param1:8,$e>>;
%
encode({Msg,Param1,Param2})         when Msg == ?M_I2C_WRITE ->
    <<$s,Msg:8,Param1:8,Param2:8,$e>>;
%
encode({Msg,Param1,Param2})         when Msg == ?M_I2C_READ_REG_8 ->
    <<$s,Msg:8,Param1:8,Param2:8,$e>>;
%
encode({Msg,Param1,Param2})         when Msg == ?M_I2C_READ_REG_16 ->
    <<$s,Msg:8,Param1:8,Param2:8,$e>>;
%
encode({Msg,Param1,Param2,Param3})         when Msg == ?M_I2C_WRITE_REG_8 ->
    <<$s,Msg:8,Param1:8,Param2:8,Param3:8,$e>>;
%
encode({Msg,Param1,Param2,Param3})         when Msg == ?M_I2C_WRITE_REG_16 ->
    <<$s,Msg:8,Param1:8,Param2:8,Param3:16,$e>>;
%
encode(_Other) ->
    exit(wrong_message_format).


decode(<<?MSG_START:8,?R_OKAY:8,?MSG_END:8>>) ->
    ok;
decode(<<?MSG_START:8,?R_BYTE:8,Value:8,?MSG_END:8>>) ->
    {ok,Value};
decode(<<?MSG_START:8,?R_SHORT:8,Value:16,?MSG_END:8>>) ->
    {ok,Value};
decode(<<?MSG_START:8,?R_LONG:8,Value:32,?MSG_END:8>>) ->
    {ok,Value};
decode(<<?MSG_START:8,?R_DATA_8:8,Value:8/binary,?MSG_END:8>>) ->
    {ok,Value};
decode(<<?MSG_START:8,?R_DATA_16:8,Value:16/binary,?MSG_END:8>>) ->
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
        ?E_I2C_SETUP          -> {error,i2c_setup};
        ?E_I2C_COMMUNICATION  -> {error,i2c_communication};
        Else                  -> {error,Else}
    end;
decode(_Other) ->
    io:format("~p~n",[_Other]),
    {error,bad_response}.
