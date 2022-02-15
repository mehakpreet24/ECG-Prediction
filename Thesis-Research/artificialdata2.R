###########################################
#
# Artificial data set 1: six petals, one in the middle; some degree of overlapping
##########################################

discNorm <- function(N, C, R) {
  #rho <- rnorm(N, sd = sqrt(R))^2
  rho <- runif(N, 0, R)
  theta <- runif(N, 0, 2) * pi
  x <- rho * cos(theta) + C[1]
  y <- rho * sin(theta) + C[2]
  
  l <- data.frame(x,y)
  return (l)
}

N <- 500
R <- 1

centers <- matrix(c(5,2.1, 3.2,3.2, 4.8,4.1, 3.5,5.3, 5.4,5.9, 6.7,4.0),nrow = 6,ncol = 2,byrow = T)
cols <- c("red", "blue", "green", "black", "orange", "maroon")
classs <- c(1, 2, 3, 4, 5, 6)

set.seed(123)
df <- data.frame()
for (row in 1:nrow(centers)) {
  C <- centers[row,]
  g <- discNorm(N, C, R)
  col <- rep(cols[row], N)
  class <- rep(classs[row], N)
  g <- cbind(g, col, class)
  df <- rbind(df,g)
}

df.inicol <- df$col
plot(df$x, df$y, xlab = 'x1', ylab = 'x2', col = as.vector(df.inicol))
legend(x = "topright", NULL,legend = classs, 
       lty = rep(1, length(classs)), 
       lwd = rep(2, length(classs)),
       col=as.vector(cols))



df$col <- NULL
df$class <- factor(df$class)

#randomly divide into p% for training, (1-p)% for testing
set.seed(2017)
p <- 0.9
train <- sample(nrow(df), p*nrow(df))
#store each fragment of data 
df.train <- df[train,]
df.test <- df[-train,]
