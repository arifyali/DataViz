#
# Make a square pie chart
#

# x- and y-coordinates of rows and columns
x_row <- 1:10
y_col <- 1:10

# Put together full coordinate vectors
x <- rep(x_row, 10)
y <- rep(y_col, each=10)

# Square grid
plot(0, 0, type="n", xlab="", ylab="", main="", xlim=c(0,11), ylim=c(0,11), asp=1)
symbols(x, y, asp=1, squares=rep(.25, 100), inches=FALSE)

# Make the squares bigger to fill space
plot(0, 0, type="n", xlab="", ylab="", main="", xlim=c(0,11), ylim=c(0,11), asp=1, bty="n", axes=FALSE)
symbols(x, y, asp=1, squares=rep(1, 100), inches=FALSE, add=TRUE)

# Adjust the color based on percentage value.
pct <- 55
col <- "#000000"
fill_col <- c(rep(col, pct), rep("#ffffff", 100-pct))
plot(0, 0, type="n", xlab="", ylab="", main="", xlim=c(0,11), ylim=c(0,11), asp=1, bty="n", axes=FALSE)
symbols(x, y, asp=1, squares=rep(1, 100), inches=FALSE, add=TRUE, bg=fill_col)

# Minor adjustments
pct <- 55
col <- "#000000"
fill_col <- c(rep(col, pct), rep("#ffffff", 100-pct))
par(mar=c(0,0,3,0), oma=c(1,0,2,0))
plot(0, 0, type="n", xlab="", ylab="", main="Percent in a Square Pie", xlim=c(0,11), ylim=c(0,10.5), asp=1, bty="n", axes=FALSE)
symbols(x, y, asp=1, squares=rep(1, 100), inches=FALSE, add=TRUE, bg=fill_col, fg="#e0e0e0", lwd=0.5)
rect(.5, .5, 10.5, 10.5, lwd=2)





# Convert to a function
squarePie <- function(pct, col="black", col.grid="#e0e0e0", col.border="black", main="Hello, Square Pie") {
  
  if (pct > 100) {
    pct <- 100
    warning("Percentage value, pct, should be an integer that is between 0 and 100.")
  } else if (pct < 0) {
    pct <- 0
    warning("Percentage value, pct, should be an integer that is between 0 and 100.")
  }
  
  # Round to nearest integer
  pct <- round(pct)
  
  # x- and y-coordinates of rows and columns
  x_row <- 1:10
  y_col <- 1:10
  
  # Put together full coordinate vectors
  x <- rep(x_row, 10)
  y <- rep(y_col, each=10)
  
  # Colors
  fill_col <- c(rep(col, pct), rep("#ffffff", 100-pct))
  
  # Plot
  plot(0, 0, type="n", xlab="", ylab="", main=main, xlim=c(0,11), ylim=c(0,10.5), asp=1, bty="n", axes=FALSE)
  symbols(x, y, asp=1, squares=rep(1, 100), inches=FALSE, add=TRUE, bg=fill_col, fg=col.grid, lwd=0.5)
  rect(.5, .5, 10.5, 10.5, lwd=2, border=col.border)
  
}


# Example usage
squarePie(12, col="#87d9c6", col.grid="#000000")
squarePie(52, col="#d041fc", col.border="#f4930d")


# Small multiples example
par(mfrow=c(3,4), mar=c(2,1,4,1))

for (i in 1:12) {
  rand_pct <- sample(0:100, size = 1)
  rand_col <- colors()[sample(1:256, size=1)]
  squarePie(rand_pct, main=paste("Square Pie", i), col=rand_col)
}






# Multiple proportions
squarePie <- function(values, col=terrain.colors(length(values)), col.grid="#e0e0e0", col.border="black", main="Hello, Square Pie") {
  
  # Convert pcts to parts of a whole
  total <- sum(values)
  pcts <- round(100 * values / total)
  
  # Naive way to handle rounding
  pcts[length(pcts)] <- 100 - sum(pcts[-length(pcts)])
  
  # x- and y-coordinates of rows and columns
  x_row <- 1:10
  y_col <- 1:10
  
  # Put together full coordinate vectors
  x <- rep(x_row, 10)
  y <- rep(y_col, each=10)
  
  # Colors
  fill_col <- c()
  for (i in 1:length(pcts)) {
    fill_col <- c(fill_col, rep(col[i], pcts[i]))
  }
  
  # Plot
  plot(0, 0, type="n", xlab="", ylab="", main=main, xlim=c(0,11), ylim=c(0,10.5), asp=1, bty="n", axes=FALSE)
  symbols(x, y, asp=1, squares=rep(1, 100), inches=FALSE, add=TRUE, bg=fill_col, fg=col.grid, lwd=0.5)
  rect(.5, .5, 10.5, 10.5, lwd=2, border=col.border)
  
}



# Example usage
squarePie(c(30,10,10,2), col=c("#a89cd1", "#d97d8b", "#4de6a1", "#e4d696"))

# Example small multiples
par(mfrow=c(3,4), mar=c(2,1,4,1))
for (i in 1:12) {
  rand_vals <- sample(0:100, size = 4)
  rand_cols <- colors()[sample(1:256, size=4)]
  squarePie(rand_vals, main=paste("Square Pie", i), col=rand_cols)
}


