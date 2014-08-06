-module(testset).
% ----------------------------------

-include("brickpi.hrl").

-export([start/0]).

start() ->
    {ok,_Pid} = brickpi:start(),
    ok = brickpi:set_sensor_type(?PORT_1,?TYPE_SENSOR_RAW),
    ok = brickpi:setup(),
    %{error,parameter_overfow} = brickpi:set_motor_enable(6,1), % there are four ports
    ok = brickpi:set_motor_enable(?PORT_A,1),
    ok = brickpi:set_motor_speed(?PORT_A,200),
    ok = brickpi:set_timeout(100000),
    ok = brickpi:update(),
    ok = brickpi:sleep(3000),
    ok = brickpi:set_motor_speed(?PORT_A,-200),
    ok = brickpi:update(),
    ok = brickpi:sleep(3000),
    {ok, Value} = brickpi:get_sensor_value(?PORT_2),
    io:format("Port ~p has value ~p~n",[?PORT_2,Value]),
    ok = brickpi:halt(),
    ok = brickpi:sleep(3000),
    ok = brickpi:update(),
    ok = brickpi:sleep(3000),
    brickpi:stop(),
    halt(0).

