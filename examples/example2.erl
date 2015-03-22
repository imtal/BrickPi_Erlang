-module(example2).
-compile(export_all).
-include("include/brickpi.hrl").

start() ->
    Handler = spawn_link(?MODULE,init,[]),
    brickpi:sleep(10000), % run excercise for 10 seconds
    Handler ! stop,
    brickpi:sleep(1000). % wait for another second

init() ->
    brickpi:start(),
    brickpi:set_timeout(3000), % motors run for 3 seconds max
    brickpi:set_motor_enable(?PORT_A,1),
    brickpi:update(),
    {ok,Offset} = brickpi:get_motor_encoder(?PORT_A),
    brickpi:set_motor_speed(?PORT_A,200),
    brickpi:set_motor_monitor(?PORT_A),
    brickpi:update(100), % update every 100ms
    loop(Offset).

loop(Offset) ->
    receive
        {motor_encoder,?PORT_A,Value} when Value-Offset>500 ->
            % reverse
            brickpi:set_motor_speed(?PORT_A,-200),
            loop(Offset);
        {motor_encoder,?PORT_A,Value} when Value-Offset<0 ->
            % forward
            brickpi:set_motor_speed(?PORT_A,200),
            loop(Offset);
        stop ->
            brickpi:halt(),
            brickpi:stop();
        _Other ->
            loop(Offset)
    end.
