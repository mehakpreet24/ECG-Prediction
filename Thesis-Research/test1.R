##############

#data folder (relative to current)
srcdir <- "C:/Users/MEHAK BAL/Downloads/Mehak/R/"

#data files
srcfile1 <- "mitbih_test.csv"
srcfile2 <- "mitbih_train.csv"
srcfile3 <- "ptbdb_abnormal.csv"
srcfile4 <- "ptbdb_normal.csv"

#read data
srcfile <- srcfile3
sdata <- read.csv(paste0( srcfile),header = F)


#plot data, if needed
color <- "red"
len <- ncol(sdata) - 1
t <- seq(from = 0, length.out = len, by = 8/1000)
plot(x = t, y = sdata[1,1:len], col = color, type = "o", main = srcfile)
plot(x = t, y = sdata[2,1:len], col = color, type = "o", main = srcfile)
plot(x = t, y = sdata[3,1:len], col = color, type = "o", main = srcfile)
plot(x = t, y = sdata[4,1:len], col = color, type = "o", main = srcfile)
plot(x = t, y = sdata[21500,1:len], col = color, type = "o", main = srcfile)

#lines(x = t, y = sdata[1,1:len], col = color, type = "o", main = srcfile)
#lines(x = t, y = sdata[2,1:len], col = "yellow", type = "o", main = srcfile)
#lines(x = t, y = sdata[3,1:len], col = "black", type = "o", main = srcfile)
#lines(x = t, y = sdata[4,1:len], col = "green", type = "o", main = srcfile)
#lines(x = t, y = sdata[168,1:len], col = "blue", type = "o", main = srcfile)
