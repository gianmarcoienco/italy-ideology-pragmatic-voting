# TARGET: create toy examples of user-written functions

#define function
#NOTE: definitions live in global envir once they have been run from 00_PROGRAMS.R
	   #upon defining(!) ordering does not matter, i.e., DEMOFUNCTION can be included into other function definitions before DEMOFUNCTION is defined itself 
	   #upon running(!) a function all its inputs have to live in the global envir already
DEMOFUNCTION <- function(x){
	result <- x * 10
	print(result)
	return(result)
}

# #MWE:
# # generic set-up of functions:
# # myfunction <- function(arg1, arg2, ... ){
# # statements
# # return(object)
# # }
# #run function
# input <- 385
# anyrhing <- DEMOFUNCTION(x=input)
