

#data folder (relative to current)
srcdir <- "C:/Users/MEHAK BAL/Downloads/Mehak/R/"

#data files
srcfile1 <- "mitbih_test.csv"
srcfile2 <- "mitbih_train.csv"
srcfile3 <- "ptbdb_abnormal.csv"
srcfile4 <- "ptbdb_normal.csv"

#read data
srcfile <- srcfile1
sdata <- read.csv(paste0(srcdir, srcfile),header = F)

C0 = which(sdata[,188]==0)
C1 = which(sdata[,188]==1)
C2 = which(sdata[,188]==2)
C3 = which(sdata[,188]==3)
C4 = which(sdata[,188]==4)

#plot data, if needed
#color <- "red"
#plot(x = t,y = amplitude)
len <- ncol(sdata) - 1
t <- seq(from = 0.25, length.out = len, by = 3/1000)

plot(x = t, y = sdata[C0[1],1:len], col = "red", type = "o", 
     main = "One ECG beat for every Hearbeat class", xlab = "time [ms]", ylab = "amplitude")
lines(x = t, y = sdata[C1[1],1:len], col = "yellow", type = "o", main = srcfile)
lines(x = t, y = sdata[C2[1],1:len], col = "black", type = "o", main = srcfile)
lines(x = t, y = sdata[20123,1:len], col = "green", type = "o", main = srcfile)
lines(x = t, y = sdata[20286,1:len], col = "blue", type = "o", main = srcfile)
legend("topright", legend = c("0", "1", "2"), pch = "ooo", col = c("red", "yellow", "black"))

#plots for means
plot(x = t, y = colMeans(sdata[C0,1:len]), col = "red", type = "o", 
     main = "Hearbeat classes", xlab = "time [ms]", ylab = "amplitude")
lines(x = t, y = colMeans(sdata[C1,1:len]), col = "yellow", type = "o", main = srcfile)
lines(x = t, y = colMeans(sdata[C2,1:len]), col = "black", type = "o", main = srcfile)
lines(x = t, y = colMeans(sdata[C3,1:len]), col = "green", type = "o", main = srcfile)
lines(x = t, y = colMeans(sdata[C4,1:len]), col = "blue", type = "o", main = srcfile)
legend("topright", legend = c("Cat'0'= N", "Cat'1'= S", "Cat'2'= V","Cat'3'= F","Cat'4'= Q"), pch = "ooooo", col = c("red", "yellow", "black","green","blue"))

hist(colMeans(sdata[C0,1:len]))
hist(colMeans(sdata[C1,1:len]))
hist(colMeans(sdata[C2,1:len]))
hist(colMeans(sdata[C3,1:len]))
hist(colMeans(sdata[C4,1:len]))
