
#define VERSION "0.81"
//#define SHOW_LOG 1

typedef unsigned char byte;

// limits.h?
#define ULONG_MAX 4294967295

// protocol definitions
#define MSG_START 's'
#define MSG_END   'e'

// message types
#define M_CHANGE_ADDRESS              'A'
#define M_SET_TIMEOUT                 'T'
// sensor
#define M_SET_SENSOR_TYPE             'S'
#define M_SET_SENSOR_SETTINGS         'C'
#define M_GET_SENSOR_VALUE            'V'
#define M_GET_SENSOR_EXT              'W'
// sensor i2c
#define M_SET_SENSOR_I2C_DEVICES      'D'
#define M_SET_SENSOR_I2C_SPEED        'B'
#define M_SET_SENSOR_I2C_ADDRESS      'N'
#define M_SET_SENSOR_I2C_WRITE        'w'
#define M_SET_SENSOR_I2C_READ         'r'
#define M_SET_SENSOR_I2C_OUT          'O'
#define M_GET_SENSOR_I2C_IN           'I'
// motor
#define M_SET_MOTOR_ENABLE            'M'
#define M_SET_MOTOR_SPEED             'G'
#define M_SET_MOTOR_OFFSET            'L'
#define M_GET_MOTOR_ENCODER           'E'
// port 5
#define M_I2C_SETUP                   's'
#define M_I2C_READ                    'g'
#define M_I2C_WRITE                   'h'
#define M_I2C_READ_REG_8              'k'
#define M_I2C_WRITE_REG_8             'l'
#define M_I2C_READ_REG_16             'm'
#define M_I2C_WRITE_REG_16            'n'
// generic
#define M_SETUP                       'Q'
#define M_UPDATE                      'Z'
#define M_HALT                        'X'

// response types
#define R_OKAY                        'O' // R_OK is defined in io.h
#define R_ERROR                       'E'
#define R_LONG                        'L'
#define R_SHORT                       'S'
#define R_DATA_8                      '7'
#define R_DATA_16                     'F'

// errors
#define E_LOAD                        'L'
#define E_SETUP                       'S'
#define E_UPDATE                      'U'
#define E_ADDRESS                     'A'
#define E_TIMEOUT                     'T'
#define E_INVALID_COMMAND             'i'
#define E_PROTOCOL_ERROR              'P'
#define E_PARAMETER_COUNT             'p'
#define E_BUFFER_OVERFLOW             'O'
#define E_PARAMETER_OVERFLOW          'o'
#define E_I2C_SETUP                   's'
#define E_I2C_COMMUNICATION           'c'

// parser states
//#define S_IDLE    0
//#define S_COMMAND 1
//#define S_PARAM   2
//#define S_VALUE   3
//#define S_DO      99

// sizes and limits
#define MAX_PARAMETERS  4
#define MAX_PARAMSIZE  16
#define MAX_PORTS       2
#define MAX_OUTPUT     64

// sensor types
//#define TYPE_SENSOR_RAW                0 // - 31
#define TYPE_SENSOR_LIGHT_OFF          0
#define TYPE_SENSOR_LIGHT_ON           (MASK_D0_M | MASK_D0_S)
#define TYPE_SENSOR_TOUCH              32
#define TYPE_SENSOR_ULTRASONIC_CONT    33
#define TYPE_SENSOR_ULTRASONIC_SS      34
#define TYPE_SENSOR_RCX_LIGHT          35 // tested minimally
#define TYPE_SENSOR_COLOR_FULL         36
#define TYPE_SENSOR_COLOR_RED          37
#define TYPE_SENSOR_COLOR_GREEN        38
#define TYPE_SENSOR_COLOR_BLUE         39
#define TYPE_SENSOR_COLOR_NONE         40
#define TYPE_SENSOR_I2C                41
#define TYPE_SENSOR_I2C_9V             42

// functions
void htob(char* in, char* out, int max);

#ifdef SHOW_LOG
#define LOG(...)    fprintf(stderr,__VA_ARGS__)
#else
#define LOG(...)
#endif
