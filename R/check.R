#' Validity of functions' arguments.
#'
#' Internal functions to be called from within another functions (see details).
#'
#' \code{.check_classes} tests if the arguments passed to functions comply with their expected classes.
#'
#' \code{.check_labels} tests if the character arguments passed to functions comply with their expected content.
#'
#' \code{.check_value} tests if the numeric arguments passed to functions comply with their expected value.
#'
#' \code{.check_length} tests if the arguments passed to functions comply with their expected length.
#'
#' \code{.check_additional_arguments} tests if the additional arguments passed to functions match acceptable arguments.
#' This function only checks if the name of the argument is expected. It does not check if it is valid.
#'
#' @keywords internal
#'
#' @param x One or more aceptable conditions
#' @param ...  arguments to be checked.
#' @param add Acceptable additional arguments.
#' @param operator  character. Logical operator ("==", "!=", "<", ">", "<=", or ">=" ) for comparison to value or length
#' @param message (optional) character. Message to print out if the test fails. If missing, a general message is used.
#'
#' @return
#' Error message if the test fails.
#'
#' @examples
#' ### .check_classes
#' argcheck:::.check_classes("character", "a")                 # no error found
#' argcheck:::.check_classes(c("character","list"), "a", "a")  # no error found
#' argcheck:::.check_classes(c("character","list"), list("a")) # no error found
#' \dontrun{
#' argcheck:::.check_classes("character", list("a"))           # error (not a character)
#' }
#'
#' fun<-function(x){
#'   argcheck:::.check_classes("numeric",x)
#'   print(x+1)
#' }
#' fun(1)      # no error found
#' \dontrun{
#' fun("a")    # error (not numeric)
#' }
#'
#'
#' ### .check_labels
#' a<-c("a","b","c")
#' b<-c("a","b","d")
#' argcheck:::.check_labels(a,a)
#' argcheck:::.check_labels(a,a[1:2])
#' argcheck:::.check_labels(a,a,a)
#' \dontrun{
#' argcheck:::.check_labels(a,b)
#' }
#'
#'
#' ### .check_length
#' argcheck:::.check_length(1,"a")
#' argcheck:::.check_length(1,2)
#' argcheck:::.check_length(2,c("a","b"))
#' argcheck:::.check_length(2,c("a","b"), operator="<=")
#' \dontrun{
#' argcheck:::.check_length(3,c("a","b"))
#' argcheck:::.check_length(3,c("a","b"), operator=">", message="the length should be larger than 3")
#' }
#'
#' ### .check_additional_arguments
#' f<-function(...){
#'   argcheck:::.check_additional_arguments(..., "add")
#' }
#' f(add="whatever")
#' \dontrun{
#' f(wrong_argument="whatever")
#' }
#'
#' @name internal.check
NULL

#' @rdname internal.check
.check_classes<-function(x, ..., message){
  if(missing(message)) message<-"Check the class of the arguments."
  classes.expected<-as.character(x)
  arguments<-list(...)
  classes.objects<-sapply(arguments,class)
  # fazer no futuro a seguinte linha.
  # if(length(classes.expected)>1 & length(classes.objects)>1) stop("Error in .check_classes: when length(x)>1, length(...) must be 1.")
  # depois, na linha abaixo, escrever !any em vez de !all
  if(!all(sapply(classes.objects,'%in%',classes.expected))) stop(message, call. = FALSE)
}


#' @rdname internal.check
.check_labels<-function(x, ..., message){
  if(missing(message)) message<-"Some argument expects label(s) "
  .check_classes("character", x)
  x<-unique(x)
  arguments=sapply(list(...),unique)
  if(!all(sapply(arguments,'%in%',x))){
    stop(message, paste(x,collapse = ", "), call. = FALSE)
  }
}

#' @rdname internal.check
.check_value<-function(x, ..., operator="==", message){
  argcheck:::.check_classes(c("numeric", "integer"), x)
  argcheck:::.check_classes("character", operator)
  if(length(x)>1) stop("Only one value can be tested.", call. = FALSE)
  argcheck:::.check_length(1, x, message = "Only one value can be tested.")
  argcheck:::.check_labels(c("==", "!=", "<", ">", "<=", ">="), operator, message = "Argument 'operator' must be one of the following characters: ")

  if(missing(message)){
    if(operator=="==") relation<-"equal to"
    if(operator=="<")  relation<-"smaller than"
    if(operator=="<=") relation<-"equal to or smaller than"
    if(operator==">")  relation<-"larger than"
    if(operator==">=") relation<-"equal to or larger than"
    if(operator=="!=") relation<-"different from"
    message<- sprintf("Some argument expects value(s) %s ", relation)
  }
  x<-unique(x)
  arguments=sapply(list(...),unique)
  if(!all(sapply(arguments, function(a) { # based on here: https://stackoverflow.com/questions/13339157/pass-character-argument-and-evaluate
    expr <- c(operator, x)
    Comp <- match.fun(expr[1])
    Comp(a, as.numeric(expr[2]))
  }))){
    stop(message, x, call. = FALSE)
  }
}


#' @rdname internal.check
.check_length<-function(x, ..., operator="==", message){
  stopifnot(length(x)==1)
  argcheck:::.check_classes(c("numeric", "integer"), x)
  argcheck:::.check_classes("character", operator)
  if(length(x)>1) stop("Only one length can be tested.", call. = FALSE)
  if(length(x)>1) stop("Only positive length can be tested.", call. = FALSE)
  argcheck:::.check_labels(c("==", "!=", "<", ">", "<=", ">="), operator, message = "Argument 'operator' must be one of the following characters: ")

  if(missing(message)) message<-"Check the length of the arguments."
  arguments<-list(...)
  if(!all(sapply(arguments, function(a) { # based on here: https://stackoverflow.com/questions/13339157/pass-character-argument-and-evaluate
    expr <- c(operator, x)
    Comp <- match.fun(expr[1])
    Comp(length(a), as.numeric(expr[2]))
  }))){
    stop(message, call. = FALSE)
  }
}


#' @rdname internal.check
.check_additional_arguments<-function(..., add, message){
  if(missing(message)) message<-"Additional argument(s) not consistent with settings; see function declareSettings"
  args<-list(...)
  if(length(args)>0 & !all(names(args)%in%add)){
    stop(message, call. = FALSE)
  }
}
