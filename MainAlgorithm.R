
#majority voting classifier that puts the other five algorithms together
#img is the image to be modified
#b is the pixel budget
main <- function(img, b) {
  
  #load the image as an array
  
  #get image type from foldername
  imgtype <- "dandelion"
  
  #call 5 sub algorithms and save resulting images
  img1 <- generate_jsma(model, img, imgtype, epochs = b)
  img2 <- FGSMFunction(img, b)
  img3 <- PGDFunction(img, b)
  img4 <- SVMFunction(img, b)
  img5 <- LastFunction(img, b)
  
  #compare image to original image and make matrix of 1's and 0's
  #where 1's are changed pixels, 0's are unmodified pixels
  
  
  #get random weights
  
  
  #multiply the matrices by the weights
  
  
  #sum all the matrices
  
  
  #find the location of the max value in the matrix 
  #and change the corresponding pixel in the image
  
  
}