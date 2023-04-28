#function that makes modifies images
#image is the image to be changed
#P is the percent of pixels I get to change
#white box function
random1 <- function (x, P) {
  x_size <- length(x[,1,1])
  y_size <- length(x[1,,1])
  #loop through image
  count <- 0
  for (i in 1:x_size) {
    for (j in 1:y_size) {
      if ((count < P) && (i>50) && (j>50) && (j<75) && (i<75)) {  #condition for what pixels you want to change
        #RGB values should be 0 to 1
        count <- count + 1
        x[i,j,1] <- 1 #modifies red value
        x[i,j,2] <- 1 #modifies green value
        x[i,j,3] <- 1 #modifies blue value
      }
    }
  }
  return(x)
}

#green at top of image function
random2 <- function (x, P) {
  x_size <- length(x[,1,1])
  y_size <- length(x[1,,1])
  count <- 0
  #loop through image
  for (i in 1:x_size) {
    for (j in 1:y_size) {
      if (count < P) {  #condition for what pixels you want to change
        #RGB values should be 0 to 1
        x[i,j,1] <- 0 #modifies red value
        x[i,j,2] <- 1 #modifies green value
        x[i,j,3] <- 0 #modifies blue value
      }
    }
  }
  return(x)
}


random3 <- function (x, P) {
  x_size <- length(x[,1,1])
  y_size <- length(x[1,,1])
  #loop through image
  count <- 0
  for (i in 1:x_size) {
    for (j in 1:y_size) {
      if ((count < P) && ((((j-1) * x_size) + i) %% 10 == 0)) {  #condition for what pixels you want to change
        #RGB values should be 0 to 1
        x[i,j,1] <- 0 #modifies red value
        x[i,j,2] <- 0 #modifies green value
        x[i,j,3] <- 1 #modifies blue value
      }
    }
  }
  return(x)
}

random4 <- function (x, P) {
  x_size <- length(x[,1,1])
  y_size <- length(x[1,,1])
  #loop through image
  count <- 0
  for (i in 1:x_size) {
    for (j in 1:y_size) {
      if ((count < P) && ((((1.0 * x[i,j,1]) > x[i,j,3]) &&  ((1.0 * x[i,j,2]) > x[i,j,3])))) {  #condition for what pixels you want to change
        #RGB values should be 0 to 1
        count <- count + 1
        x[i,j,1] <- 1 #modifies red value
        x[i,j,2] <- 1 #modifies green value
        x[i,j,3] <- 0 #modifies blue value
      }
    }
  }
  return(x)
}

#yellow box
random5 <- function (x, P) {
  x_size <- length(x[,1,1])
  y_size <- length(x[1,,1])
  #loop through image
  count <- 0
  for (i in 1:x_size) {
    for (j in 1:y_size) {
      if ((count < P) && (j > 10 && j < 200 && i > 10 && i < 200)) {  #condition for what pixels you want to change
        #RGB values should be 0 to 1
        count <- count + 1
        x[i,j,1] <- 1 #modifies red value
        x[i,j,2] <- 1 #modifies green value
        x[i,j,3] <- 0 #modifies blue value
      }
    }
  }
  return(x)
}

#replace green with yellow
random6 <- function (x, P) {
  x_size <- length(x[,1,1])
  y_size <- length(x[1,,1])
  #loop through image
  count <- 0
  for (i in 1:x_size) {
    for (j in 1:y_size) {
      if ((count < P) && (x[i,j,2] > 0.8) && (x[i,j,1] < 0.8) && (x[i,j,3] < 0.8)) {  #condition for what pixels you want to change
        #RGB values should be 0 to 1
        count <- count + 1
        x[i,j,1] <- 1 #modifies red value
        x[i,j,2] <- 1 #modifies green value
        x[i,j,3] <- 0 #modifies blue value
      }
    }
  }
  return(x)
}
