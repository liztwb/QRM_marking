# sidebar version

# clean up records file
recfile <- "C:/Users/liztwb/Documents/QRM/Marking/2024/record.rds"
oldrec <- readRDS(recfile)
has_part <- sapply(oldrec, function(x) !is.na(x$participant))
newrec <- oldrec[has_part]
saveRDS(newrec, recfile)


getrc <- function(x, y) {
  col <- ifelse(x > 200, 3,
                ifelse(x > 100, 2, 1))
  row <- ifelse(y > 500, 1,
                ifelse(y > 400, 2,
                       ifelse(y > 300, 3,
                              ifelse(
                                y > 200, 4,
                                ifelse(y > 100, 5, 6)
                              ))))
  return(c(row, col))
}

getrect <- function(r, c) {
  xb <- switch(c,
               c(0, 100),
               c(100, 200),
               c(200, 300))
  yb <- switch(r,
               c(500, 600),
               c(400, 500),
               c(300, 400),
               c(200, 300),
               c(100, 200),
               c(0, 100))
  
  
  #    cat(file  = stderr(), "xb = ", xb, " yb = ", yb, "\n")
  return(c(xb[1], yb[1], xb[2], yb[2]))
}



plotmat <- function(mat) {
  plot(
    c(-100, 300),
    c(-100, 630),
    type = "n",
    axes = FALSE,
    xlab = "",
    ylab = ""
  )
  #rect(0, 0, c(100,200,300), 100, col = "blue", border = "black", lwd=2) # transparent
  text(-2,550, "Presentation", pos = 2)
  text(-2,450, "Comprehension of statistics", pos =2)
  text(-2,350, "Comprehension of risk man", pos = 2)
  text(-2,250, "Comprehension of computation", pos =2)
  text(-2,150, "Clarity of expression", pos=2)
  text(-2,50, "Quality of analysis", pos=2)
  text(50,625, "Poor", adj = 0.5)
  text(150,625, "Satisfactory", adj = 0.5)
  text(250,625, "Very good", adj = 0.5)
  
  rect(0, seq(0, 600, 100), 100, 200, lwd = 2)
  rect(100, seq(0, 600, 100), 200, 200, lwd = 2)
  rect(200, seq(0, 600, 100), 300, 200, lwd = 2)
  if (any(mat)) {
    r <- row(mat)[which(mat)]
    c <-  col(mat)[which(mat)]
    tc <- cbind(r, c)
    lst <- list()
    for (i in 1:dim(tc)[1]) {
      lst[[i]] <- getrect(tc[i, 1], tc[i, 2])
    }
    sapply(lst, function(x)
      rect(x[1], x[2], x[3], x[4], lwd = 2, col = "lightblue"))
  }
}
# note this could be a list?

# com1 etc are the actual comments to go in the feedback
# scom1 etc are the short versions for the input selectors
# library(googlesheets4)
# comsheet <- read_sheet("https://docs.google.com/spreadsheets/d/1BuN1QsssXn8YNmmvp9ByesLNngk71wJxBF_rLul2zN8/edit?usp=sharing")
coms <- readxl::read_xlsx("comments.xlsx")
c1 <- coms[,1, drop = TRUE]
c2 <- coms[,2, drop = TRUE]
coms <- data.frame(c1,c2)
