
#load packages
library(tidyverse)
library(keras)
library(tensorflow)
library(reticulate)
library(jpeg)

#majority voting classifier that puts the other five algorithms together
#img is the image to be modified stored as an array
#b is the pixel budget
#imgtype is 1 for dandelion and 2 for grass
main <- function(img, b, imgtype = 1) {
  
  #get image type from folder name
  if (imgtype == 1) {
    target_type <- 2
  } else {
    target_type <- 1
  }

  #print("made it to main")
  #call 5 sub algorithms and save resulting image matrices
  img1 <- random1(img, b)
  img2 <- random2(img, b)
  img3 <- random3(img, b)
  img4 <- random4(img, b)
  img5 <- random5(img, b)
  
  #print(img[50:55,50:55,1])
  #print(img1[50:55,50:55,1])
  #print(img2[50:55,50:55,1])
  #print(img3[50:55,50:55,1])
  #print(img4[50:55,50:55,1])
  #print(img5[50:55,50:55,1])
  
  #print("functions ran")
  #compare image to original image matrix and make matrix of 1's and 0's
  #where 1's are changed pixels, 0's are unmodified pixels
  x_size <- length(img[,1,1]) #width of image
  y_size <- length(img[1,,1]) #height of image
  matrix1 <- matrix(data = 0, nrow = x_size, ncol = y_size)
  matrix2 <- matrix(data = 0, nrow = x_size, ncol = y_size)
  matrix3 <- matrix(data = 0, nrow = x_size, ncol = y_size)
  matrix4 <- matrix(data = 0, nrow = x_size, ncol = y_size)
  matrix5 <- matrix(data = 0, nrow = x_size, ncol = y_size)
  
  #loop through image
  for (i in 1:x_size) {
    for (j in 1:y_size) {
      if ((abs(img1[i,j,1] - img[i,j,1]) > 0.005) || (abs(img1[i,j,2] - img[i,j,2]) > 0.005) 
          ||(abs(img1[i,j,3] - img[i,j,3]) > 0.005)) {
        matrix1[i,j] <- 1
      }
      if ((abs(img2[i,j,1] - img[i,j,1]) > 0.005) || (abs(img2[i,j,2] - img[i,j,2]) > 0.005) 
          ||(abs(img2[i,j,3] - img[i,j,3]) > 0.005)) {
        matrix2[i,j] <- 1
      }
      if ((abs(img3[i,j,1] - img[i,j,1]) > 0.005) || (abs(img3[i,j,2] - img[i,j,2]) > 0.005) 
          ||(abs(img3[i,j,3] - img[i,j,3]) > 0.005)) {
        matrix3[i,j] <- 1
      }
      if ((abs(img4[i,j,1] - img[i,j,1]) > 0.005) || (abs(img4[i,j,2] - img[i,j,2]) > 0.005) 
          ||(abs(img4[i,j,3] - img[i,j,3]) > 0.005)) {
        matrix4[i,j] <- 1
      }
      if ((abs(img5[i,j,1] - img[i,j,1]) > 0.005) || (abs(img5[i,j,2] - img[i,j,2]) > 0.005) 
          ||(abs(img5[i,j,3] - img[i,j,3]) > 0.005)) {
        matrix5[i,j] <- 1
      }
    }
  }
  
  #print(matrix1[50:55,50:55])
  #print(matrix2[50:55,50:55])
  #print(matrix3[50:55,50:55])
  #print(matrix4[50:55,50:55])
  #print(matrix5[50:55,50:55])
  #print("made matrices")
  
  #get random weights
  weights <- matrix(runif(5,0,1), ncol=5)
  weights <- weights/sum(weights)
  
  
  #multiply the matrices by the weights
  matrix1 <- weights[1] * matrix1
  matrix2 <- weights[2] * matrix2
  matrix3 <- weights[3] * matrix3
  matrix4 <- weights[4] * matrix4
  matrix5 <- weights[5] * matrix5
  
  #sum all the matrices
  sumMatrix <- matrix1 + matrix2 + matrix3 + matrix4 + matrix5
  
  #print(sumMatrix[50:55,50:55])
  #print("summed matrix")
  
  #find the location of the max value in the matrix 
  #and change the corresponding pixel in the image
  for (i in 1:b) {
    index <- which(sumMatrix == max(sumMatrix),arr.ind = TRUE)
    maxVal <- max(matrix1[index[1,1],index[1,2]],matrix2[index[1,1],index[1,2]],
                  matrix3[index[1,1],index[1,2]],matrix4[index[1,1],index[1,2]],
                  matrix5[index[1,1],index[1,2]])
    sumMatrix[index[1,1], index[1,2]] <- 0
    #print(c(index[1,1],index[1,2]))
    #find which matrix had the max value and set the img pixel value to the 
    #pixel values from the corresponding image
    if (matrix1[index[1],index[2]] == maxVal) {
      img[index[1],index[2],] <- img1[index[1],index[2],]
    } else if (matrix2[index[1],index[2]] == maxVal) {
      img[index[1],index[2],] <- img2[index[1],index[2],]
    } else if (matrix3[index[1],index[2]] == maxVal) {
      img[index[1],index[2],] <- img3[index[1],index[2],]
    } else if (matrix4[index[1],index[2]] == maxVal) {
      img[index[1],index[2],] <- img4[index[1],index[2],]
    } else if (matrix5[index[1],index[2]] == maxVal) {
      img[index[1],index[2],] <- img5[index[1],index[2],]
    }
  }
  #print(img[50:55,50:55,1])
  #print("close to return!")
  #return the image
  return(img)
}

#set working directory
setwd("C:/Users/sheat/OneDrive/Documents/IE332Project/Project2")
getwd()

target_size <- c(224, 224)

#code to load model
model <- load_model_tf("./dandelion_model")

f <- list.files("./grass")
for (i in f){
  #print(paste("./grass/",f[1],sep=""))
  #x <- readJPEG(paste("./grass/",f[1],sep=""))
  test_image <- image_load(paste("./grass/",i,sep=""),
                           target_size = target_size)
  x <- image_to_array(test_image)
  x <- x/ 255
  x_size <- length(x[,1,1]) #width of image
  y_size <- length(x[1,,1]) #height of image
  b <- (x_size * y_size) / 100
  img <- main(x, b)
  writeJPEG(img, paste("./modified_grass/",i,sep=""))
  
  #test with model
  img <- array_reshape(img, c(1, dim(img)))
  pred <- model %>% predict(img)
  if (pred[1,2] < 0.5) {
    print(i)
    print(pred)
  }
  #x <- array_reshape(x, c(1, dim(x)))
  #og <- model %>% predict(x)
  #print(og)
}


f <- list.files("./dandelions")
#i <- f[1]
for (i in f){
  test_image <- image_load(paste("./dandelions/",i,sep=""),
                           target_size = target_size)
  x <- image_to_array(test_image)
  x <- x/ 255
  x_size <- length(x[,1,1]) #width of image
  y_size <- length(x[1,,1]) #height of image
  b <- (x_size * y_size) / 100
  img <- main(x, b)
  writeJPEG(img, paste("./modified_dandelions/",i,sep=""))
  
  #test with model
  img <- array_reshape(img, c(1, dim(img)))
  pred <- model %>% predict(img)
  if (pred[1,1] < 0.5) {
    print(i)
    print(pred)
  }
}
