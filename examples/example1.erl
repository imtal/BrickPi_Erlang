-module(example1).
-compile(export_all).
-include("include/brickpi.hrl").

start() ->
    brickpi:start(),
	brickpi:set_motor_enable(?PORT_A,1),
	brickpi:set_motor_enable(?PORT_B,1),
    brickpi:set_motor_speed(?PORT_A,-200),
    brickpi:set_motor_speed(?PORT_B,-200),
    brickpi:update(),
    brickpi:sleep(3000),
    brickpi:set_motor_speed(?PORT_A,200),
    brickpi:set_motor_speed(?PORT_B,200),
    brickpi:update(),
    brickpi:sleep(3000),
    brickpi:halt(),
    brickpi:stop().
