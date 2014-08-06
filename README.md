BrickPi
=======
This software offers a port driver for the BrickPi which can be used with Erlang. My first try was to use a serial port driver and format the messages within Erlang. The bit syntax is powerfull and seems suitable for the job. But the way the messages are compressed is quite complex (I really do not know why they choose to do it this way) and the chance to make mistakes is huge.

Therefore I made an alternative driver based on the C-sources for the BrickPi. Communication with the driver is based on short messages which can be used to alter the data structure that is updated. This way not the whole data structure needs to be exchanged between Erlang and the port driver. After the sensor and motor initialization parameters a setup message can be sent. After that subsequent update messages initiate the synchronization. Before and after each update the values can be set or read.

Message structure
=================
The driver allows simple messages to be passed like for setting a motor speed, setting a sensor mode, etc. The overall message format for messages and responses between the Raspberry and the BrickPi is like this:

    's' TYPE [VALUE [';' VALUE])*] 'e'

The message type is given by one byte, values are transferred as binaries. Depending on the message short or long integers and arrays of 8 or 16 bytes are transferred. Based on this message format the following several functions are defined:

    change_address(From,To)
    set_timeout()

    set_motor_speed(Port,Value)
    set_motor_enable(Port,Value)
    set_motor_offset(Port,Value)
    get_motor_encoder(Port)

    set_sensor_type(Port,Type)
    set_sensor_settings(Port,Settings)
    get_sensor_value(Port)
    get_sensor_ext(Port,Index)

    set_sensor_i2c_devices(Port)
    set_sensor_i2c_speed(Port)
    set_sensor_i2c_adress(Port,Device,Address)
    set_sensor_i2c_output(Port,Device,Data)
    get_sensor_i2c_input(Port,Device)

    setup_sensors()
    update()
    update(Interval)
    halt()

Using update with an interval enables automatic updates. Using an interval of zero disables it again. The halt() function immediately halts all motors and also stops automatic updates. 

Examples
========
The first example is a simple example where the instructions to the BrickPi are given in a sequential order.

    -module(example1).

    start() ->
        brickpi:start(),
        brickpi:motor_speed(?PORT_A,-200),
        brickpi:motor_speed(?PORT_B,-200),
        brickpi:sleep(3000),
        brickpi:motor_speed(?PORT_A,200),
        brickpi:motor_speed(?PORT_B,200),
        brickpi:sleep(3000),
        brickpi:halt(),
        brickpi:stop().

The second exmaple shows the async behaviour of the BrickPi and how to use it.

    -module(example2).

    start() ->
        brickpi:start(),
        ok = brickpi:motor_mode(?PORT_A,10),
        ok = brickpi:motor_speed(?PORT_A,20),
        loop().

    loop() ->
        receive
            {motor_encoder,Value} when Value>200 ->
                ok = brickpi:flush(),
                brickpi:stop().
            _Other ->
                loop()
        end.

The third example shows how to use multiple control loops to manage more complex situation.

    -module(example3).

    start() ->
        brickpi:start(),
        Motor = spawn(?MODULE,motor_loop,\[\]),
        brickpi:motor_handler(?PORT_A,Motor),
        brickpi:motor_mode(?PORT_A,10),
        brickpi:motor_speed(?PORT_A,200),
        Sensor = spawn(?MODULE,sensor_loop,\[\]).
        brickpi:sensor_handler(?PORT_1,Sensor),
        brickpi:sensor_mode(?PORT_1,10).

    motor_loop(N) ->
        receive
            {motor_encoder,Value} when Value>500 ->
                brickpi:motor_speed(?PORT_A,-200);
            {motor_encoder,Value} when Value<200 ->
                brickpi:motor_speed(?PORT_A,200);
            stop ->
                brickpi:flush(),
                brickpi:stop;
            _Other ->
                motor_loop()
        end.

    sensor_loop(Motor) ->
        receive
            {sensor_changed,Value} when Value>400 ->
                Motor ! stop;
            _Other ->
                sensor_loop()
        end.


I think the beauty of this approach is its modularity and how it supports to alter the program in an evolutionary way. It is quite easy to extend the behaviour of the robot while keeping earlier code in place. The parallelism of Erlang allows us to add a control loop (handler) for each sensor or motor and let them interact with messages. This way the whole system adapt to more complex situations step by step.


Language bindings
=====
Because the interface specification is quite simple it is easy to reuse this driver in different languages. The following language bindings are at this moment available for the BrickPiA driver. 

**Erlang** - This was what it was all about. A very interesting language for robotics and therefore the initial implementation. Uses a port driver to communicate with the BrickPi.

**Python** - Because Python is the most used language on the Raspberry, a Pyhton  binding to the drive is added. This binding is not yet complete.

See also
=====
[Dexter Industries] (http://www.dexterindustries.com/)
[BrickPi] (http://www.dexterindustries.com/BrickPi)
[Erlang Embedded] (http://erlang-embedded.com/)
[@yolt] (http://www.yolt.nl)
