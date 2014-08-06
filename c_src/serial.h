#include <stdio.h> /* Standard input/output definitions */
#include <string.h> /* String function definitions */
#include <unistd.h> /* UNIX standard function definitions */
#include <fcntl.h> /* File control definitions */
#include <errno.h> /* Error number definitions */
#include <termios.h> /* POSIX terminal control definitions */
#include <sys/ioctl.h>


/*
* 'serial_open()' - Open serial port 1.
*
* Returns the file descriptor on success or -1 on error.
*/
int serial_open(char *device, unsigned long speed) {
    int status,fd; /* File descriptor for the port */
    fd = open(device, O_RDWR | O_NOCTTY | O_NDELAY);
    if (fd < 0) {
        fprintf(stderr,"- open_port(): unable to open %s\n",device);
    } else {
        fcntl(fd, F_SETFL, O_NONBLOCK);
        int portstatus = 0;
        struct termios options;
        // get the current options for the port...
        tcgetattr(fd, &options);
		cfmakeraw(&options) ;
        cfsetispeed(&options, speed);
        cfsetospeed(&options, speed);
        // enable the receiver and set local mode...
        options.c_cflag |= (CLOCAL | CREAD);
        // no parity (8N1)
        options.c_cflag &= ~PARENB;
        options.c_cflag &= ~CSTOPB;
        options.c_cflag &= ~CSIZE;
        options.c_cflag |= CS8;
        // no control flow
        options.c_cflag &= ~CRTSCTS;
        options.c_iflag &= ~(IXON|IXOFF|IXANY);
        options.c_lflag &= ~(ICANON|ECHO|ECHOE|ISIG);
        // raw output
        options.c_oflag &= ~OPOST;
        // buffer specifications
        options.c_cc[VMIN] = 0;
        options.c_cc[VTIME] = 10;
        // set the new options for the port ...
        if (tcsetattr(fd, TCSANOW|TCSAFLUSH, &options)==-1) {
            fprintf(stderr,"- open_port(): error initializing serial port\n");
            close(fd);
            fd = -1;
        } else {
			ioctl(fd, TIOCMGET, &status);
			status |= TIOCM_DTR ;
			status |= TIOCM_RTS ;
			ioctl(fd, TIOCMSET, &status);
            usleep(10000);
        }
    }
    return (fd);
}

void serial_flush(const int fd) {
    tcflush(fd, TCIOFLUSH);
}

void serial_close(const int fd) {
    close(fd);
}

void serial_putchar(const int fd, const unsigned char c) {
    write(fd, &c, 1);
}

void serial_putstring(const int fd, const char *s) {
    write(fd, s, strlen (s));
}

int serial_scan(const int fd) {
    int result ;
    if (ioctl(fd, FIONREAD, &result) == -1) return -1 ;
    return result ;
}

int serail_getchar(const int fd) {
    unsigned char x;
    if (read(fd,&x,1)!=1) return -1;
    return ((int)x) & 0xFF;
}
