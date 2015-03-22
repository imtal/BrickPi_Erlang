/*
 *  imtal<at>yolt.nl
 *  http://www.yolt.nl/
 *  Created: December 27, 2013
 *  Updated: July 12, 2014
 *
 *  You may use this code as you wish, provided you give credit where it's due.
 *
 *  This program is specifically to be used with the BrickPi.
 *
 *  This is an Erlang port driver for the BrickPi.
 */


#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <unistd.h>
#include <stdint.h>

#include "tick.h"
#include "brickpi.h"
#include "brickpi_driver.h"

unsigned char buffer[MAX_OUTPUT];

unload(int i) {
    LOG("- Unloaded brickpi_driver (%s at %i)\n\r",strerror(errno),i);
    exit(0);
}

int read_start() {
    if (read(STDIN_FILENO,buffer,1)<=0) unload(1);
    return (buffer[0]==MSG_START);
}

int read_end() {
    if (read(STDIN_FILENO,buffer,1)<=0) unload(2);
    return (buffer[0]==MSG_END);
}

char read_command() {
    if (read(STDIN_FILENO,buffer,1)<=0) unload(3);
    return buffer[0];
}

short read_byte() {
    uint8_t value;
    if (read(STDIN_FILENO,buffer,sizeof(uint8_t))<=0) unload(4);
    memcpy(&value,buffer,sizeof(uint8_t));
    return ntohs(value);
}
short read_short() {
    uint16_t value;
    if (read(STDIN_FILENO,buffer,sizeof(uint16_t))<=0) unload(5);
    memcpy(&value,buffer,sizeof(uint16_t));
    return ntohs(value);
}

long read_long() {
    uint32_t value;
    if (read(STDIN_FILENO,buffer,sizeof(uint32_t))<=0) unload(6);
    memcpy(&value,buffer,sizeof(uint32_t));
    return ntohl(value);
}

void read_bytes(unsigned char* buf, int len) {
    if (read(STDIN_FILENO,buf,len)<=0) unload(7);
}

send_ok() {
    buffer[0] = MSG_START;
    buffer[1] = R_OKAY;
    buffer[2] = MSG_END;
    write(STDOUT_FILENO,buffer,3);
}

send_value_short(uint16_t value) {
    buffer[0] = MSG_START;
    buffer[1] = R_SHORT;
    value = htons(value);
    memcpy(buffer+2,&value,sizeof(short));
    buffer[2+sizeof(short)] = MSG_END;
    write(STDOUT_FILENO,buffer,3+sizeof(short));
}

send_value_long(uint32_t value) {
    buffer[0] = MSG_START;
    buffer[1] = R_LONG;
    value = htonl(value);
    memcpy(buffer+2,&value,sizeof(long));
    buffer[2+sizeof(long)] = MSG_END;
    write(STDOUT_FILENO,buffer,3+sizeof(long));
}

send_value_data_8(char* data) {
    int j;
    buffer[0] = MSG_START;
    buffer[1] = R_DATA_8;
    for(j = 0; j < 8; j++)
        buffer[2+j] = data[j];
    buffer[2+8] = MSG_END;
    write(STDOUT_FILENO,buffer,3+8);
}

send_value_data_16(char* data) {
    int j;
    buffer[0] = MSG_START;
    buffer[1] = R_DATA_16;
    for(j = 0; j < 16; j++)
        buffer[2+j] = data[j];
    buffer[2+16] = MSG_END;
    write(STDOUT_FILENO,buffer,3+16);
}

send_error(byte b) {
    buffer[0] = MSG_START;
    buffer[1] = R_ERROR;
    buffer[2] = b;
    buffer[3] = MSG_END;
    write(STDOUT_FILENO,buffer,4);
}

receive_command() {
    if (read_start()) {
        switch (read_command()) {
            case M_CHANGE_ADDRESS: {
                short from, to;
                from = read_short();
                to = read_short();
                if (read_end()) {
                    LOG("- Change address %i to %i\r\n",from,to);
                    if (BrickPiChangeAddress(from,to)) {
                        send_error(E_ADDRESS);
                    } else {
                        send_ok();
                    }
                } else {
                    send_error(E_PROTOCOL_ERROR);
                }
                break;
            }
            case M_SET_TIMEOUT: {
                long timeout;
                timeout = read_long();
                if (read_end()) {
                    LOG("- Set timeout to %u\r\n",timeout);
                    BrickPi.Timeout = timeout;
                    if (BrickPiSetTimeout()) {
                        send_error(E_TIMEOUT);
                    } else {
                        send_ok();
                    }
                } else {
                    send_error(E_PROTOCOL_ERROR);
                }
                break;
            }
            case M_SET_MOTOR_SPEED: {
                short port,speed;
                port = read_short();
                speed = read_short();
                if (read_end()) {
                    if (port<0 || port>3) { send_error(E_PARAMETER_OVERFLOW); break; }
                    LOG("- Set motor %i to speed %i\r\n",port,speed);
                    BrickPi.MotorSpeed[port] = speed;
                    send_ok();
                } else {
                    send_error(E_PROTOCOL_ERROR);
                }
                break;
            }
            case M_SET_MOTOR_ENABLE: {
                short port,enable;
                port = read_short();
                enable = read_short();
                if (read_end()) {
                    if (port<0 || port>3) { send_error(E_PARAMETER_OVERFLOW); break; }
                    LOG("- Set motor %i to enabled %i\r\n",port,enable);
                    BrickPi.MotorEnable[port] = enable;
                    send_ok();
                } else {
                    send_error(E_PROTOCOL_ERROR);
                }
                break;
            }
            case M_SET_MOTOR_OFFSET: {
                short port,offset;
                port = read_short();
                offset = read_long();
                if (read_end()) {
                    if (port<0 || port>3) { send_error(E_PARAMETER_OVERFLOW); break; }
                    LOG("- Set motor %i to offset %i\r\n",port,offset);
                    BrickPi.EncoderOffset[port] = offset;
                    send_ok();
                } else {
                    send_error(E_PROTOCOL_ERROR);
                }
                break;
            }
            case M_GET_MOTOR_ENCODER: {
                short port;
                port = read_short();
                if (read_end()) {
                    if (port<0 || port>3) { send_error(E_PARAMETER_OVERFLOW); break; }
                    LOG("- Get motor %i encoder value\r\n",port);
                    send_value_long(BrickPi.Encoder[port]);
                } else {
                    send_error(E_PROTOCOL_ERROR);
                }
                break;
            }
            case M_SET_SENSOR_TYPE: {
                short port,type;
                port = read_short();
                type = read_short();
                if (read_end()) {
                    if (port<0 || port>3) { send_error(E_PARAMETER_OVERFLOW); break; }
                    LOG("- Set sensor %i to type %i\r\n",port,type);
                    BrickPi.SensorType[port] = type;
                    send_ok();
                } else {
                    send_error(E_PROTOCOL_ERROR);
                }
                break;
            }
            case M_SET_SENSOR_SETTINGS: {
                short port,device,settings;
                port = read_short();
                device = read_short();
                settings = read_short();
                if (read_end()) {
                    if (port<0 || port>3) { send_error(E_PARAMETER_OVERFLOW); break; }
                    if (device<0 || device>7) { send_error(E_PARAMETER_OVERFLOW); break; }
                    LOG("- Set sensor port %i, I2C device %i to the settings %i\r\n",port,device,settings);
                    BrickPi.SensorSettings[port][device] = settings;
                    send_ok();
                } else {
                    send_error(E_PROTOCOL_ERROR);
                }
                break;
            }
            case M_GET_SENSOR_VALUE: {
                short port;
                port = read_short();
                if (read_end()) {
                    if (port<0 || port>3) { send_error(E_PARAMETER_OVERFLOW); break; }
                    LOG("- Get sensor value for port %i\r\n",port);
                    send_value_long(BrickPi.Sensor[port]);
                } else {
                    send_error(E_PROTOCOL_ERROR);
                }
                break;
            }
            case M_GET_SENSOR_EXT: {
                short port,i;
                port = read_short();
                i = read_short();
                if (read_end()) {
                    if (port<0 || port>3) { send_error(E_PARAMETER_OVERFLOW); break; }
                    if (i<0 || i>4) { send_error(E_PARAMETER_OVERFLOW); break; }
                    LOG("- Get sensor array value for port %i\r\n",port,i);
                    send_value_long(BrickPi.SensorArray[port][i]);
                } else {
                    send_error(E_PROTOCOL_ERROR);
                }
                break;
            }
            case M_SET_SENSOR_I2C_DEVICES: {
                short port,devices;
                port = read_short();
                devices = read_short();
                if (read_end()) {
                    if (port<0 || port>3) { send_error(E_PARAMETER_OVERFLOW); break; }
                    LOG("- Set sensor port %i to the number of I2C devices to %i\r\n",port,devices);
                    BrickPi.SensorI2CDevices[port] = devices;
                    send_ok();
                } else {
                    send_error(E_PROTOCOL_ERROR);
                }
                break;
            }
            case M_SET_SENSOR_I2C_SPEED: {
                short port,speed;
                port = read_short();
                speed = read_short();
                if (read_end()) {
                    if (port<0 || port>3) { send_error(E_PARAMETER_OVERFLOW); break; }
                    LOG("- Set sensor port %i to the I2C speed %i\r\n",port,speed);
                    BrickPi.SensorI2CSpeed[port] = speed;
                    send_ok();
                } else {
                    send_error(E_PROTOCOL_ERROR);
                }
                break;
            }
            case M_SET_SENSOR_I2C_ADDRESS: {
                short port,device,address;
                port = read_short();
                device = read_short();
                address = read_short();
                if (read_end()) {
                    if (port<0 || port>3) { send_error(E_PARAMETER_OVERFLOW); break; }
                    if (device<0 || device>7) { send_error(E_PARAMETER_OVERFLOW); break; }
                    LOG("- Set sensor port %i, I2C device %i to the address to %i\r\n",port,device,address);
                    BrickPi.SensorI2CAddr[port][device] = address;
                    send_ok();
                } else {
                    send_error(E_PROTOCOL_ERROR);
                }
                break;
            }
            case M_SET_SENSOR_I2C_WRITE: {
                short port,device,count;
                port = read_short();
                device = read_short();
                count = read_short();
                if (read_end()) {
                    if (port<0 || port>3) { send_error(E_PARAMETER_OVERFLOW); break; }
                    if (device<0 || device>7) { send_error(E_PARAMETER_OVERFLOW); break; }
                    if (count<0 || count>15) { send_error(E_PARAMETER_OVERFLOW); break; }
                    LOG("- Set sensor port %i, I2C device %i to the amount of bytes to write to %i\r\n",port,device,count);
					BrickPi.SensorI2CWrite[port][device]=count;
                    send_ok();
                } else {
                    send_error(E_PARAMETER_COUNT);
                }
                break;
            }
            case M_SET_SENSOR_I2C_READ: {
                short port,device,count;
                port = read_short();
                device = read_short();
                count = read_short();
                if (read_end()) {
                    if (port<0 || port>3) { send_error(E_PARAMETER_OVERFLOW); break; }
                    if (device<0 || device>7) { send_error(E_PARAMETER_OVERFLOW); break; }
                    if (count<0 || count>15) { send_error(E_PARAMETER_OVERFLOW); break; }
                    LOG("- Set sensor port %i, I2C device %i to the amount of bytes to read to %i\r\n",port,device,count);
					BrickPi.SensorI2CRead[port][device]=count;
                    send_ok();
                } else {
                    send_error(E_PROTOCOL_ERROR);
                }
                break;
            }
            case M_SET_SENSOR_I2C_OUT: {
                short port,device;
                unsigned char output[16];
                port = read_short();
                device = read_short();
                read_bytes(output,16);
                if (read_end()) {
                    if (port<0 || port>3) { send_error(E_PARAMETER_OVERFLOW); break; }
                    if (device<0 || device>7) { send_error(E_PARAMETER_OVERFLOW); break; }
                    LOG("- Set sensor port %i, I2C device %i to write to output \"%s\"\r\n",port,device,output);
                    memcpy(BrickPi.SensorI2COut[port][device],output,16);
                    send_ok();
                } else {
                    send_error(E_PARAMETER_COUNT);
                }
                break;
            }
            case M_GET_SENSOR_I2C_IN: {
                short port,device;
                port = read_short();
                device = read_short();
                if (read_end()) {
                    if (port<0 || port>3) { send_error(E_PARAMETER_OVERFLOW); break; }
                    if (device<0 || device>7) { send_error(E_PARAMETER_OVERFLOW); break; }
                    LOG("- Get sensor port %i, I2C device %i the input\r\n",port,device);
                    send_value_data_16(BrickPi.SensorI2COut[port][device]);
                } else {
                    send_error(E_PROTOCOL_ERROR);
                }
                break;
            }
            case M_SETUP: {
                int error;
                LOG("- Setup sensors\r\n");
                if (read_end()) {
                    error = BrickPiSetupSensors();
                    if (error) {
                        send_error(E_SETUP);
                    } else {
                        send_ok();
                    }
                } else {
                    send_error(E_PARAMETER_COUNT);
                }
                break;
            }
            case M_UPDATE: {
                int error;
                LOG("- Update values\r\n");
                if (read_end()) {
                    error = BrickPiUpdateValues();
                    if (error) {
                        send_error(E_UPDATE);
                    } else {
                        send_ok();
                    }
                } else {
                    send_error(E_PROTOCOL_ERROR);
                }
                break;
            }
            case M_HALT: {
                LOG("- Halt\r\n");
                if (read_end()) {
                    BrickPi.MotorEnable[PORT_A] = 0;
                    BrickPi.MotorEnable[PORT_B] = 0;
                    BrickPi.MotorEnable[PORT_C] = 0;
                    BrickPi.MotorEnable[PORT_D] = 0;
                    BrickPiUpdateValues();
                    send_ok();
                } else {
                    send_error(E_PROTOCOL_ERROR);
                }
                break;
            }
            default: {
                LOG("- Unknown command\r\n");
                send_error(E_PROTOCOL_ERROR);
            }
        }
    } else {
        LOG("- Received unexpected character\r\n");
        send_error(E_PROTOCOL_ERROR);
    }
}

// singal handler
void sig_handler(int signo) {
    LOG("\n\r - Someone killed brickpi_driver\r\n");
    exit(1);
}

// main loop

main() {
    unsigned long now;
    long value;
    int error=0;
    int i;
    signal(SIGTERM, sig_handler);
    signal(SIGKILL, sig_handler);
    error = BrickPiSetup();
    if (error) {
        LOG("\n\r- Error loading brickpi_driver: %d\n", error);
        send_error(E_LOAD);
    } else {
        LOG("\n\r- Loaded brickpi_driver version %s\n\r",VERSION);
        BrickPi.Address[0] = 1;
        BrickPi.Address[1] = 2;
        BrickPi.Timeout=5000; // motors run five seconds by default
        if (BrickPiSetTimeout()) {
            send_error(E_TIMEOUT);
        } else {
            while (1) {
                receive_command();
            }
        }
    }
}
