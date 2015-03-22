
-define(MOTOR_A,                        0).
-define(MOTOR_B,                        1).
-define(MOTOR_C,                        2).
-define(MOTOR_D,                        3).
% for backward readability
-define(PORT_A,                         0).
-define(PORT_B,                         1).
-define(PORT_C,                         2).
-define(PORT_D,                         3).

-define(SENSOR_1,                       0).
-define(SENSOR_2,                       1).
-define(SENSOR_3,                       2).
-define(SENSOR_4,                       3).
% for backward readability
-define(PORT_1,                         0).
-define(PORT_2,                         1).
-define(PORT_3,                         2).
-define(PORT_4,                         3).

-define(MASK_D0_M,                  16#01).
-define(MASK_D1_M,                  16#02).
-define(MASK_9V,                    16#04).
-define(MASK_D0_S,                  16#08).
-define(MASK_D1_S,                  16#10).

% protocol definitions
-define(MSG_START,                     $s).
-define(MSG_END,                       $e).

% message types
-define(M_CHANGE_ADDRESS,              $A).
-define(M_SET_TIMEOUT,                 $T).
% sensor
-define(M_SET_SENSOR_TYPE,             $S).
-define(M_SET_SENSOR_SETTINGS,         $C).
-define(M_GET_SENSOR_VALUE,            $V).
-define(M_GET_SENSOR_EXT,              $W).
% sensor i2c
-define(M_SET_SENSOR_I2C_DEVICES,      $D).
-define(M_SET_SENSOR_I2C_SPEED,        $B).
-define(M_SET_SENSOR_I2C_ADDRESS,      $N).
-define(M_SET_SENSOR_I2C_WRITE,        $w).
-define(M_SET_SENSOR_I2C_READ,         $r).
-define(M_SET_SENSOR_I2C_OUT,          $O).
-define(M_GET_SENSOR_I2C_IN,           $I).
% motor
-define(M_SET_MOTOR_ENABLE,            $M).
-define(M_SET_MOTOR_SPEED,             $G).
-define(M_SET_MOTOR_OFFSET,            $L).
-define(M_GET_MOTOR_ENCODER,           $E).
% generic
-define(M_SETUP,                       $Q).
-define(M_UPDATE,                      $Z).
-define(M_HALT,                        $X).

% response types
-define(R_OKAY,                        $O).
-define(R_ERROR,                       $E).
-define(R_LONG,                        $L).
-define(R_SHORT,                       $S).
-define(R_DATA_8,                      $7).
-define(R_DATA_16,                     $F).

% errors
-define(E_LOAD,                        $L).
-define(E_SETUP,                       $S).
-define(E_UPDATE,                      $U).
-define(E_ADDRESS,                     $A).
-define(E_TIMEOUT,                     $T).
-define(E_INVALID_COMMAND,             $i).
-define(E_PROTOCOL_ERROR,              $P).
-define(E_BUFFER_OVERFLOW,             $O).
-define(E_PARAMETER_OVERFLOW,          $o).

% Sensor setup (MSG_TYPE_SENSOR_TYPE)
-define(BYTE_SENSOR_1_TYPE,             1).
-define(BYTE_SENSOR_2_TYPE,             2).
  
% Timeout setup (MSG_TYPE_TIMEOUT_SETTINGS)
-define(BYTE_TIMEOUT, 1).

-define(TYPE_MOTOR_PWM,                 0).
-define(TYPE_MOTOR_SPEED,               1).
-define(TYPE_MOTOR_POSITION,            2).

-define(TYPE_SENSOR_RAW,                0). % - 31
-define(TYPE_SENSOR_LIGHT_OFF,          0).
-define(TYPE_SENSOR_LIGHT_ON,      (?MASK_D0_M bor ?MASK_D0_S)).
-define(TYPE_SENSOR_TOUCH,             32).
-define(TYPE_SENSOR_ULTRASONIC_CONT,   33).
-define(TYPE_SENSOR_ULTRASONIC_SS,     34).
-define(TYPE_SENSOR_RCX_LIGHT,         35). % tested minimally
-define(TYPE_SENSOR_COLOR_FULL,        36).
-define(TYPE_SENSOR_COLOR_RED,         37).
-define(TYPE_SENSOR_COLOR_GREEN,       38).
-define(TYPE_SENSOR_COLOR_BLUE,        39).
-define(TYPE_SENSOR_COLOR_NONE,        40).
-define(TYPE_SENSOR_I2C,               41).
-define(TYPE_SENSOR_I2C_9V,            42).

-define(BIT_I2C_MID,                16#01). % Do one of those funny clock pulses between writing and reading. defined for each device.
-define(BIT_I2C_SAME,               16#02). % The transmit data, and the number of bytes to read and write isn"t going to change. defined for each device.

-define(INDEX_RED,                      0).
-define(INDEX_GREEN,                    1).
-define(INDEX_BLUE,                     2).
-define(INDEX_BLANK,                    3).

-type unsigned() :: non_neg_integer().
