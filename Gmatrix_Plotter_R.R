
#Make sure you install "shape"
#install.packages("shape")

#set working directory and load data
#change these commands to match your files
#setwd("C:/Users/Adam/Documents/R/GmatrixRanimation")
data <- read.csv("jones_table1_row1_run_1.csv")

#Set parameters
draw_width <- 12 #The width of the drawing area
draw_interval <- 20 #Draw G-matrix every ___ generations
sleep_time <- 0.2   #Pause this long on each G-matrix

library(shape)

#Figure out the size of the plot area
#And create it
trait0max <- max(data[,"zbar0"])
trait1max <- max(data[,"zbar1"])
G00max <- max(data[,"G00"])
G11max <- max(data[,"G11"])
trait0min <- min(data[,"zbar0"])
trait1min <- min(data[,"zbar1"])
G00min <- min(data[,"G00"])
G11min <- min(data[,"G11"])

xmin <- trait0min - sqrt(G00min)*3
xmax <- trait0max + sqrt(G00max)*3
ymin <- trait1min - sqrt(G11min)*3
ymax <- trait1max + sqrt(G11max)*3

#If the G-matrix is oddly proportioned, change the "+2" and "+1.5" in dev.new until it looks about right
dev.new(width=draw_width+1.5,height=draw_width*(ymax-ymin)/(xmax-xmin)+2)
plot(NULL, xlim=c(xmin,xmax), ylim=c(ymin,ymax), xlab="Trait 0", ylab="Trait 1")

number_gens <- length(data[,"Gen"]) - 1

#Draw an ellipse each generation
for (iii in 1:as.integer(number_gens/draw_interval)){
	gen <- as.integer(iii*draw_interval)
	BivariateMean <- c(data[gen,"zbar0"],data[gen,"zbar1"])
	Gmatrix <- matrix(c(data[gen,"G00"],data[gen,"G01"],data[gen,"G01"],data[gen,"G11"]),nrow=2,ncol=2)
	Optimum <- c(data[gen,"Opt0"],data[gen,"Opt1"])

	segments(Optimum[1]-0.2,Optimum[2],Optimum[1]+0.2,Optimum[2], lwd=4, col="green")
	segments(Optimum[1],Optimum[2]-0.2,Optimum[1],Optimum[2]+0.2, lwd=4, col="green")

	#Draw the crosshairs
	CHlength <- 1.96*sqrt(data[gen,"Lambda1"])
	CHwidth <- 1.96*sqrt(data[gen,"Lambda2"])
	Xcoord1 <- cos(data[gen,"Angle"]*pi/180)*CHlength
	Ycoord1 <- sin(data[gen,"Angle"]*pi/180)*CHlength
	Xcoord2 <- cos((data[gen,"Angle"]-90)*pi/180)*CHwidth
	Ycoord2 <- sin((data[gen,"Angle"]-90)*pi/180)*CHwidth

	plotellipse(rx = CHlength, ry = CHwidth, mid=BivariateMean, angle=data[gen,"Angle"], lcol="navy", lwd=2)
	segments(BivariateMean[1],BivariateMean[2],Xcoord1+BivariateMean[1],Ycoord1+BivariateMean[2],lwd=3, col="navy")
	segments(BivariateMean[1],BivariateMean[2],BivariateMean[1]-Xcoord1,BivariateMean[2]-Ycoord1,lwd=3, col="navy")
	segments(BivariateMean[1],BivariateMean[2],Xcoord2+BivariateMean[1],Ycoord2+BivariateMean[2],lwd=3, col="navy")
	segments(BivariateMean[1],BivariateMean[2],BivariateMean[1]-Xcoord2,BivariateMean[2]-Ycoord2,lwd=3, col="navy")

	Sys.sleep(sleep_time)
	plotellipse(rx = CHlength, ry = CHwidth, mid=BivariateMean, angle=data[gen,"Angle"], lcol="white", lwd=2)
	segments(BivariateMean[1],BivariateMean[2],Xcoord1+BivariateMean[1],Ycoord1+BivariateMean[2],lwd=3, col="white")
	segments(BivariateMean[1],BivariateMean[2],BivariateMean[1]-Xcoord1,BivariateMean[2]-Ycoord1,lwd=3, col="white")
	segments(BivariateMean[1],BivariateMean[2],Xcoord2+BivariateMean[1],Ycoord2+BivariateMean[2],lwd=3, col="white")
	segments(BivariateMean[1],BivariateMean[2],BivariateMean[1]-Xcoord2,BivariateMean[2]-Ycoord2,lwd=3, col="white")

	segments(Optimum[1]-0.2,Optimum[2],Optimum[1]+0.2,Optimum[2], lwd=4, col="white")
	segments(Optimum[1],Optimum[2]-0.2,Optimum[1],Optimum[2]+0.2, lwd=4, col="white")
	
}

	#redraw the final ellipse
	segments(Optimum[1]-0.2,Optimum[2],Optimum[1]+0.2,Optimum[2], lwd=4, col="green")
	segments(Optimum[1],Optimum[2]-0.2,Optimum[1],Optimum[2]+0.2, lwd=4, col="green")

	#Draw the crosshairs
	CHlength <- 1.96*sqrt(data[gen,"Lambda1"])
	CHwidth <- 1.96*sqrt(data[gen,"Lambda2"])
	Xcoord1 <- cos(data[gen,"Angle"]*pi/180)*CHlength
	Ycoord1 <- sin(data[gen,"Angle"]*pi/180)*CHlength
	Xcoord2 <- cos((data[gen,"Angle"]-90)*pi/180)*CHwidth
	Ycoord2 <- sin((data[gen,"Angle"]-90)*pi/180)*CHwidth

	plotellipse(rx = CHlength, ry = CHwidth, mid=BivariateMean, angle=data[gen,"Angle"], lcol="navy", lwd=2)
	segments(BivariateMean[1],BivariateMean[2],Xcoord1+BivariateMean[1],Ycoord1+BivariateMean[2],lwd=3, col="navy")
	segments(BivariateMean[1],BivariateMean[2],BivariateMean[1]-Xcoord1,BivariateMean[2]-Ycoord1,lwd=3, col="navy")
	segments(BivariateMean[1],BivariateMean[2],Xcoord2+BivariateMean[1],Ycoord2+BivariateMean[2],lwd=3, col="navy")
	segments(BivariateMean[1],BivariateMean[2],BivariateMean[1]-Xcoord2,BivariateMean[2]-Ycoord2,lwd=3, col="navy")


