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
	int portstatus = 0;
	struct termios options;
    int status = 0;
    int result = 0;
    int fd; /* File descriptor for the port */
    speed_t baud;
	switch (speed) {
		case      50: baud =      B50; break;
		case      75: baud =      B75; break;
		case     110: baud =     B110; break;
		case     134: baud =     B134; break;
		case     150: baud =     B150; break;
		case     200: baud =     B200; break;
		case     300: baud =     B300; break;
		case     600: baud =     B600; break;
		case    1200: baud =    B1200; break;
		case    1800: baud =    B1800; break;
		case    2400: baud =    B2400; break;
		case    4800: baud =    B4800; break;
		case    9600: baud =    B9600; break;
		case   19200: baud =   B19200; break;
		case   38400: baud =   B38400; break;
		case   57600: baud =   B57600; break;
		case  115200: baud =  B115200; break;
		case  230400: baud =  B230400; break;
		case  500000: baud =  B500000; break;
		case 1000000: baud = B1000000; break;
		case 2000000: baud = B2000000; break;
		default: return -2 ;
	}
    fd = open(device, O_RDWR | O_NOCTTY | O_NDELAY | O_NONBLOCK);
    if (fd < 0) {
        fprintf(stderr,"- open_port(): unable to open %s\n",device);
    } else {
        fcntl(fd, F_SETFL, O_RDWR);
        // get the current options for the port...
        tcgetattr(fd, &options);
        cfmakeraw(&options) ;
        cfsetispeed(&options, baud);
        cfsetospeed(&options, baud);
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
        options.c_iflag &= ~(INLCR|IGNCR|ICRNL|IGNBRK);
        options.c_lflag &= ~(ICANON|ECHO|ECHOE|ISIG);
        options.c_lflag &= ~(IEXTEN|ECHOK|ECHONL);
        // raw output
        options.c_oflag &= ~OPOST;
        // buffer specifications
        options.c_cc[VMIN] = 0;
        options.c_cc[VTIME] = 10;
        // set the new options for the port ...
        if (tcsetattr(fd, TCSANOW, &options)==-1) {
            fprintf(stderr,"- open_port(): error initializing serial port\n");
            close(fd);
            fd = -1;
        } else {
            write(fd,"\x02\x07\x03\x02\x00\x00",6);
			//tcflush(fd,TCIOFLUSH);
            ioctl(fd, TIOCMGET, &status);
            status |= TIOCM_DTR ;
            status |= TIOCM_RTS ;
            ioctl(fd, TIOCMSET, &status);
            usleep(10000);
			tcflush(fd,TCIOFLUSH);
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

int serial_getchar(const int fd) {
    unsigned char x;
    if (read(fd,&x,1)!=1) return -1;
    return ((int)x) & 0xFF;
}
