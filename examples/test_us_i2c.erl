-module(test_us_i2c).
-compile(export_all).
-include("brickpi.hrl").

% The Lego NXT Ultrasonic Sensor does not always return a correct values
% but sometimes the value differs with 128.
%
% On the BrickPi forum a participant with the name Frans posted some
% Python code that reads the Ultrasonic Sensor through I2C.
%
% This module reads the NXT Ultrasonic Sensor in this alternative way.

-define(I2C_PORT,       ?PORT_1).
-define(I2C_SPEED,            8). % default 7
-define(I2C_DEVICE_INDEX,     0).

-define(LEGO_US_I2C_ADDR, 16#02).

-define(LEGO_US_CMD_REG,  16#41).
-define(LEGO_US_CMD_OFF,  16#00).
-define(LEGO_US_CMD_SS,   16#01).
-define(LEGO_US_CMD_CONT, 16#02).
-define(LEGO_US_CMD_EVNT, 16#03).
-define(LEGO_US_CMD_RST,  16#04).

-define(LEGO_US_DATA_REG, 16#42).


start() ->
    Handler = spawn_link(?MODULE,init,[]),
    brickpi:sleep(10000), % run excercise for 10 seconds
    Handler ! stop,
    brickpi:sleep(1000). % wait another second

init() ->
    brickpi:start(),

    % setup sensor
    brickpi:set_sensor_type(?I2C_PORT, ?TYPE_SENSOR_I2C_9V),
    brickpi:set_sensor_i2c_speed(?I2C_PORT, ?I2C_SPEED),
    brickpi:set_sensor_i2c_devices(?I2C_PORT, 1),

    brickpi:set_sensor_i2c_settings(?I2C_PORT,?I2C_DEVICE_INDEX, ?BIT_I2C_MID bor ?BIT_I2C_SAME),
    brickpi:set_sensor_i2c_address(?I2C_PORT,?I2C_DEVICE_INDEX, ?LEGO_US_I2C_ADDR),

    brickpi:set_sensor_i2c_write(?I2C_PORT,?I2C_DEVICE_INDEX, 1),
    brickpi:set_sensor_i2c_read(?I2C_PORT,?I2C_DEVICE_INDEX, 1),
    brickpi:set_sensor_i2c_out(?I2C_PORT,?I2C_DEVICE_INDEX, <<?LEGO_US_DATA_REG,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>),

    ok = brickpi:setup(),
    brickpi:set_sensor_monitor(?I2C_PORT,self()),

    % run
    brickpi:update(1000), % update every 1000ms
    loop().

loop() ->
    receive
        {sensor_value, ?I2C_PORT, 1} -> % There is one byte available
            {ok,Value} = brickpi:get_sensor_i2c_in(?I2C_PORT,?I2C_DEVICE_INDEX),
            <<Distance,_Rest/binary>> = Value,
            io:format("distance: ~p~n",[Distance]),
            loop();
        stop ->
            brickpi:halt(),
            brickpi:stop();
        _Other ->
            io:format("~p~n",[_Other]),
            loop()
    end.
