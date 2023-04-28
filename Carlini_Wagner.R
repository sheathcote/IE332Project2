#loads libraries
library(adversarial)
library(keras)
library(tidyverse)
library(tensorflow)
library(reticulate)
library(jpeg)

#load directory
setwd("C:/Users/glent/Downloads/IE332_P2")
getwd()

# Load trained model
dandelion_model <- load_model_tf("./dandelion_model")
summary(model)


#function that makes modifies images
#image is the image to be changed
#P is the percent of pixels I get to change
random <- function (x, P) {
  
  x_size <- length(x[,1,1])
  y_size <- length(x[1,,1])
  #loop through image
  for (i in 1:x_size) {
    for (j in 1:y_size) {
      if ((((j-1) * x_size) + i) %% P == 0) {  #condition for what pixels you want to change
        #RGB values should be 0 to 1
        x[i,j,1] <- 1 #modifies red value
        x[i,j,2] <- 1 #modifies green value
        x[i,j,3] <- 0 #modifies blue value
      }
    }
  }
  
  # Convert the modified x array back to an image
  writeJPEG(x, "./grass/modified_grass.jpg")
  
  # Read the modified image file
  modified_image <- jpeg::readJPEG("./grass/modified_grass.jpg")
  

  # Display the modified image
  graphics::plot(1, type="n", xlim=c(0, 1), ylim=c(0, 1), xlab="", ylab="")
  graphics::rasterImage(modified_image, 0, 0, 1, 1)
}



# Define the Carlini-Wagner attack parameters
cw_params <- cw_params(
  binary_search_steps = 10,
  learning_rate = 0.01,
  max_iterations = 1000,
  abort_early = TRUE,
  initial_const = 0.1,
  confidence = 0.5
)


# Generate adversarial example using Carlini-Wagner attack
adv_example <- cw(dandelion_model, image, cw_params)

# Display original image and adversarial example
par(mfrow=c(1,2))
plot(image, main='Original Image')
plot(adv_example$adv_image, main='Adversarial Example')


###################################################

