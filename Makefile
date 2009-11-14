all: fsl2spass fsl2spass_dist

fsl2spass: fsl2spass.scm
	mzc --exe fsl2spass fsl2spass.scm

fsl2spass_dist: fsl2spass
	mzc --exe-dir fsl2spass_dist fsl2spass

clean:
	rm -r fsl2spass fsl2spass_dist
