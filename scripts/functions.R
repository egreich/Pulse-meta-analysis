### commonly-used functions to read-in


library(purrr)
# Create a "not in" function
`%nin%` <- negate(`%in%`)



# function that finds the index of variables to remove
get_remove_index <- function(to_keep, list, type){
  
  if(type %nin% c("rjags", "jagUI")){
    paste("Please indicate whether this is a rjags or jagsUI samples object")
  }
    
  if(type == "rjags"){
    list <- list[list != "deviance"] # remove deviance
    list <- sort(list, method = "radix")
    out_list <- c()
    for(j in c(1:length(list))){
      if(list[j] %in% to_keep){
        out_list[j] = NA
      } else{
        out_list[j] = j
      }
    }
    out_list <- out_list[!is.na(out_list)]
    out_list
  }
  
  if(type == "jagsUI"){
    list <- list[list != "deviance"] # remove deviance
    out_list <- c()
    for(j in c(1:length(list))){
      if(list[j] %in% to_keep){
        out_list[j] = NA
      } else{
        out_list[j] = j
      }
    }
    out_list <- out_list[!is.na(out_list)]
    out_list
  }

}

# function that finds the index of each parameter in a jagsUI coda output
# will put into a single list for convenient dataframe creation
get_index <- function(jagsUI_object){
  list_out <- list()
  for(i in c(1:length(jagsUI_object$mean))){
    l <- c(1:length(jagsUI_object$mean[[i]]))
    list_out[[i]] <- l
  }
  out <- do.call(c, list_out)
  out
}


