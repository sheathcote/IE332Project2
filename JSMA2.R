library(tensorflow)

saliency_map <- function(X, dtdx, dodx, eps, cmin, cmax) {
  c1 <- tf.logical_or(eps < 0, X < cmax)
  c2 <- tf.logical_or(eps > 0, X > cmin)
  c3 <- (dtdx >= 0)
  c4 <- (dodx <= 0)
  cond <- tf.cast(tf.reduce_all(c(c1, c2, c3, c4), axis=0), dtype=tf.float32)
  score <- cond * (dtdx * tf.abs(dodx))
  score <- tf.reshape(score, shape=c(1, -1))
  return(score)
}

jacobian_matrix <- function(y, x, n_class) {
  j <- NULL
  for (i in 1:n_class) {
    if (i == 1) {
      j <- tf$gradients(y[[i]], x)
    } else {
      j <- tf$concat(c(j, tf$gradients(y[[i]], x)), axis=0)
    }
  }
  return(j)
}

jsma <- function(X_adv, target_y, model, eps, cmin=0.0, cmax=1.0) {
  X_adv <- tf$convert_to_tensor(X_adv, dtype=tf$float32)
  X_adv <- tf$reshape(X_adv, shape=as.integer(c(1, 224, 224, 3)))
  logits_probs <- model(X_adv)
  logits <- logits_probs[[1]]
  probs <- logits_probs[[2]]
  y_ind <- tf$argmax(probs[1,])
  jacobian <- jacobian_matrix(logits_probs, X_adv, 10)
  grad_input <- jacobian[y_ind, , , ]
  grad_target <- jacobian[target_y, , , ]
  grad_other <- grad_input - grad_target
  score <- saliency_map(X_adv, grad_target, grad_other, eps, cmin, cmax)
  idx <- tf$argmax(score, axis=2)
  pert <- tf$one_hot_v2(idx, 224*224*3, on_value=eps, off_value=0.0, dtype=tf$float32)
  pert <- tf$reshape(pert, shape=tf$shape(X_adv))
  X_adv <- tf$clip_by_value(X_adv + pert, cmin, cmax)
  return(list(X_adv = X_adv, pert = pert))
}

generate_jsma <- function(model, X, target, eps=1.0, epochs=50) {
  
  tf$random$set_seed(42)
  
  # Placeholder for single image.
  #X_p <- array_reshape(X, dim = c(1, 224, 224, 3))
  X_p <- tf$constant(X, dtype=tf$float32)
  #print(X_p)
  
  # Op for one iteration of jsma.
  adv_op <- jsma(X_p, target_y=target, model=model, eps=eps)
  
  sess <- tf$Session()
  with(sess, {
    tf$train$Saver()$restore(sess, "model.ckpt")
    for (i in 1:epochs) {
      digit <- sess$run(adv_op)
      X_p <- tf$constant(digit, dtype=tf$float32)
    }
  })
  sess$close()
  
  digit <- tf$reshape(digit, shape = c(224, 224, 3))
  digit <- sess$run(digit)
  pert <- digit - X
  
  return(list(digit = digit, pert = pert))
}

setwd("C:/Users/sheat/OneDrive/Documents/IE332Project/Project2")

# Load pre-trained model and test image
model <- load_model_tf("./dandelion_model")
gu=list.files("./grass")
target_size <- c(224, 224)
img <- image_load(paste("./grass/",gu[5],sep=""), target_size = target_size)
img <- image_to_array(img)
img <- array_reshape(img, c(1, dim(img)))
img <- img/255
#img <- imagenet_preprocess_input(img)


# Choose target class
target_class <- 1

# Generate adversarial example
adv <- generate_jsma(model, img, target_class)

# Plot original image, adversarial example, and perturbation
plot_image(img, title = "Original Image")
plot_image(adv$digit, title = "Adversarial Example")
plot_image(adv$pert, title = "Perturbation")

# Classify original and adversarial images
orig_pred <-  model %>% predict(img)
adv_pred <-  model %>% predict(adv$digit)

# Print predicted classes
cat("Original image predicted class:", orig_pred[1], "\n")
cat("Adversarial image predicted class:", adv_pred[1], "\n")
