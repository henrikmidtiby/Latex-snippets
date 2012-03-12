library(signal)
# Load data from file
spectre <- read.table("LIA121.UXD")

# Extract variables and perform median filtering of the signal count
scatterangle <- spectre$V1
signal <- medfilt1(spectre$V2, n = 3)


# Plot the filtered signal
#pdf('exampleplot.pdf')
plot(signal ~ scatterangle, t='l')


# Perform a non linear fit of several gauss bells to the signal peaks
res <- nls( signal ~ bg + a*scatterangle 
	+ h1*exp(-((scatterangle - m1)/s1)^2) 
	+ h2*exp(-((scatterangle - m2)/s2)^2) 
	+ h3*exp(-((scatterangle - m3)/s3)^2)
	+ h4*exp(-((scatterangle - m4)/s4)^2)
	+ h5*exp(-((scatterangle - m5)/s5)^2)
	+ h6*exp(-((scatterangle - m6)/s6)^2)
	+ h7*exp(-((scatterangle - m7)/s7)^2)
	, 
	start=list( 
		h1 =  25, m1 = 22.26, s1 = 0.3, 
		h2 =  69, m2 = 11.07, s2 = 0.2, 
		h3 =   7, m3 = 34.43, s3 = 0.6, 
		h4 =   6, m4 = 38.55, s4 = 1.3, 
		h5 =   3, m5 = 45.53, s5 = 1.6, 
		h6 =  10, m6 = 60.29, s6 = 0.3, 
		h7 =  10, m7 = 61.46, s7 = 0.3, 
		bg=2, a = -0.1))

# Show the values of the fit
print(summary(res))

# Draw the fitted function on top of the original data.
lines(scatterangle, predict(res, data.frame(scatterangle)), col='red')

#dev.off()

