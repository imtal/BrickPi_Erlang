-module(test_us).
-compile(export_all).
-include("brickpi.hrl").

% This module reads the NXT Ultrasonic Sensor straight away.

start() ->
    Handler = spawn_link(?MODULE,init,[]),
    brickpi:sleep(10000), % run excercise for 10 seconds
    Handler ! stop,
    brickpi:sleep(1000). % wait another second

init() ->
    brickpi:start(),
    brickpi:set_sensor_type(?PORT_1, ?TYPE_SENSOR_ULTRASONIC_CONT),
    ok = brickpi:setup(),
    brickpi:set_sensor_monitor(?PORT_1),
    brickpi:update(1000), % update every 100ms
    loop().

loop() ->
    receive
        {sensor_value,?PORT_1,Value} ->
            % reverse
            io:format("distance: ~p~n",[Value]),
            loop();
        stop ->
            brickpi:halt(),
            brickpi:stop();
        _Other ->
            io:format("~p~n",[_Other]),
            loop()
    end.
