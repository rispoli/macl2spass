CC = /usr/bin/mzc

all: macl2spass macl2spass_dist

macl2spass: macl2spass.scm
	$(CC) --exe macl2spass macl2spass.scm

macl2spass_dist: macl2spass
	$(CC) --exe-dir macl2spass_dist macl2spass

clean:
	rm -r macl2spass macl2spass_dist
