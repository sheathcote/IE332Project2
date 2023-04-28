
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

  
  #call 5 sub algorithms and save resulting image matrices
  img1 <- generate_jsma(model, img, target_type, epochs = b)
  img2 <- FGSMFunction(img, b)
  img3 <- PGDFunction(img, b)
  img4 <- SVMFunction(img, b)
  img5 <- LastFunction(img, b)
  
  #compare image to original image matrix and make matrix of 1's and 0's
  #where 1's are changed pixels, 0's are unmodified pixels
  x_size <- length(img[,1,1])
  y_size <- length(img[1,,1])
  b < (x_size*y_size)/100
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
  sumMatrix <- sum(matrix1, matrix2, matrix3, matrix4, matrix5)
  
  
  #find the location of the max value in the matrix 
  #and change the corresponding pixel in the image
  for (i in 1:b) {
    index <- which(sumMatrix == max(sumMatrix), arr.ind = TRUE)
    maxVal <- max(matrix1[index[1],index[2]],matrix2[index[1],index[2]],
                  matrix3[index[1],index[2]],matrix4[index[1],index[2]],
                  matrix5[index[1],index[2]])
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
  
  #return the image
  return(img)
}
