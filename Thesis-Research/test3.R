

#data folder (relative to current)
srcdir <- "C:/Users/MEHAK BAL/Downloads/Mehak/R/"

#data files
srcfile1 <- "mitbih_test.csv"
srcfile2 <- "mitbih_train.csv"
srcfile3 <- "ptbdb_abnormal.csv"
srcfile4 <- "ptbdb_normal.csv"

#read data
srcfile <- srcfile3
sdata <- read.csv(paste0(srcdir, srcfile),header = F)

#plot data, if needed
#color <- "red"
#plot(x = t,y = amplitude)
len <- ncol(sdata) - 1
t <- seq(from = 0, length.out = len, by = 8/1000)

plot(x = t, y = sdata[400,1:len], col = "red", type = "o", main = srcfile3)

srcfile <- srcfile4
sdata <- read.csv(paste0(srcdir, srcfile),header = F)

#plot data, if needed
#color <- "red"
#plot(x = t,y = amplitude)
len <- ncol(sdata) - 1
t <- seq(from = 0, length.out = len, by = 8/1000)


lines(x = t, y = sdata[400,1:len], col = "black", type = "o", main = srcfile4)

