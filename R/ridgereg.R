#' Ridge Regression
#' 
#' Running a Ridge Reregression Model.
#' 
#' @param formula Takes a formula of the form Y~X.
#' @param data Takes a dataset in the form of a data.frame.
#' @param lambda Is the constraint on the parameters.
#' 
#' @return Returns an object of the class 'ridgereg'. This object can be manipulated.
#' 
#' @export ridgereg
#' @exportClass ridgereg
ridgereg = setRefClass(Class = "ridgereg",
                       
                       fields = list(
                         
                         formula = "formula",
                         data = "data.frame",
                         data_name = "character",
                         formula_name = "character",
                         beta_hat = "matrix",
                         X = "matrix",
                         Y = "matrix",
                         lambda = "numeric",
                         y_hat = "matrix"
                       ),
                       
                       methods = list(
                         
                         initialize = function(formula, data, lambda){
                           formula_name <<- deparse(formula)
                           data_name <<- deparse(substitute(data))
                           
                           lambda <<- lambda
                           
                           # normalize X
                           X <<- scale(model.matrix(formula, data = data)[,-1])
                           Y <<- as.matrix(data[all.vars(formula)[1]])
                           
                           L = diag(x = sqrt(lambda), nrow = ncol(X), ncol = ncol(X))
                           
                           X_new = rbind(X, L)
                           Y_new = rbind(Y, matrix(0, nrow = ncol(X), ncol = 1))
                           
                           # use QR decomposition to matrix X_new to find estimates of parameters
                           decomp = qr(X_new)
                           Q = qr.Q(decomp)
                           R = qr.R(decomp)
                           beta_hat <<- solve(R) %*% t(Q) %*% Y_new
                           
                           # calculate the fitted values
                           y_hat <<- X %*% beta_hat
                           
                         },
                         
                         
                         predict = function(){
                           return(y_hat)
                         },
                         
                         coef = function(){
                           return(as.vector(beta_hat))
                         },
                         
                         print = function(){
                           
                           beta_vector = as.vector(beta_hat)
                           names(beta_vector) = rownames(beta_hat)
                           cat("Call:\n")
                           cat(paste("linreg(formula = ", formula_name, ", data = ",data_name,")", sep = ""),"\n\n")
                           cat("Coefficients:\n")
                           print_inside(beta_vector)
                         }
                       ))


print_inside = function(x){
  print(x)
}
