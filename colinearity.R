vif(meta3)

#so it looked dodgy that east and west were sig pos
#so i delved a little 
#and it looks like ive got a multicolinearity problem
#lets see what we can do

library(glmnet)

X <- model.matrix(~ first_collection_scaled + length_of_collection_scaled + 
                    location + relativeby, data = data)
y <- data$hedgesg

ridge_model <- cv.glmnet(X, y, alpha = 0)

best_lambda <- ridge_model$lambda.min

print(best_lambda)
coef(ridge_model, s = best_lambda)

#releveled in meta_analysis to make plains (largest sample size) ref
#this whacked most multicolinearity down ~ 5 or below as was no longer comparing to unstable desert estimate
#still seeing issues in first and length of collection

variables <- data.frame(first_collection = data$first_collection_scaled,
                        length_of_collection = data$length_of_collection_scaled)

pca_result <- prcomp(variables, center = TRUE, scale. = TRUE)

summary(pca_result)

pca_result$rotation

#so looks like they're essentially cancelling each other
#going to try fit this into neta3

data$pca1 <- pca_result$x[, 1]

vif(meta3)
#huzah! 