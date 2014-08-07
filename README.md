BrickPi Erlang
========

Introduction
--------
This software offers a port driver for the BrickPi which can be used with Erlang. My first try was to use a serial port driver and format the messages within Erlang just like Python. The bit syntax is powerful and seems suitable for the job. But the way the messages are compressed is quite complex and the chance to make mistakes is huge.

Therefore I made an alternative driver based on the C-sources for the BrickPi. Communication with the driver is based on short messages which can be used to alter or read the data structure. The data structure is updated on demand. 

This way not the whole data structure needs to be exchanged between Erlang and the port driver. After the sensor and motor initialization parameters a setup message can be sent. After that subsequent update messages can be issued for  the synchronization. Before and after each update the values can be set or read.

Usage
-----
The BrickPi driver for Erlang defined the following functions, which alter parts of the data structure or are equivalents of the C functions. 

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

Using update with an interval enables automatic updates. Using an interval of zero disables it again. The `halt()` function immediately halts all motors and also stops automatic updates. 

Installation
--------
Get the sources and place them in a directory of choice. In order to compile and install issue the following commands:

    $ wget https://github.com/imtal/BrickPi_Erlang/archive/master.zip
    $ unzip master.zip
    $ mv BrickPi_Erlang-master BrickPi_Erlang
    $ cd BrickPi_Erlang
    BrickPi_Erlang $ make
    BrickPi_Erlang $ make
    BrickPi_Erlang $ sudo make install
    
The files are installed under `/usr/lib/erlang/lib`, so they are automatically found from within Erlang (use `code:which(brickpi)` to check from the `erl` command line). Additionally tests can be run and documentation can be generated:

    BrickPi_Erlang $ make tests
    BrickPi_Erlang $ make edoc

Examples
--------
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

(More exmaples to follow.)

Internals
--------
The port driver is implemented using the BrickPi_C library. The port driver is based on simple messages passing like for setting a motor speed, setting a sensor mode, etc. The overall message format for messages and responses between the Raspberry and the BrickPi is like this:

    's' CMD VALUE* 'e'

The message command is given by one byte, values are transferred as binaries. Depending on the message `short` or `long` integers and arrays of 8 or 16 bytes are transferred. 

Language bindings
--------
Because the interface specification is quite simple it is easy to reuse this driver in different languages. The following language bindings are at this moment available for the BrickPiA driver. 

**Erlang** - This was what it was all about. A very interesting language for robotics and therefore the initial implementation. Uses a port driver to communicate with the BrickPi.

**Python** - Because Python is the most used language on the Raspberry, a Pyhton  binding to the drive is added. This binding is not yet complete.

See also
--------
See for more information:
* [Dexter Industries] (http://www.dexterindustries.com/)
* [BrickPi] (http://www.dexterindustries.com/BrickPi)
* [Erlang Embedded] (http://erlang-embedded.com/)
* [@yolt] (http://www.yolt.nl)
