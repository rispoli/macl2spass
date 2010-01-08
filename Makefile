CC = /usr/bin/mzc

all: fsl2spass fsl2spass_dist

fsl2spass: fsl2spass.scm
	$(CC) --exe fsl2spass fsl2spass.scm

fsl2spass_dist: fsl2spass
	$(CC) --exe-dir fsl2spass_dist fsl2spass

clean:
	rm -r fsl2spass fsl2spass_dist
