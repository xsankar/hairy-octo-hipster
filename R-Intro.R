#
# Intro to R
#
# Based on excellent R workshops
#
#
aString <- "A String"
aNumber <- 12
class(aString)
class(aNumber)
aVector <- c(1,2,3,4)
class(aVector)
aVector * 2
sqrt(aVector)
#
require(dplyr)
x1 <- c("A","B","C")
x2 <- c(1,2,3)
x3 <- c(T,F,T)
#
a <- data_frame(x1,x2)
b <- data_frame(x1,x3)
b[3,1] <- "D"
#
a
b
left_join(a,b,by="x1")
right_join(a,b,by="x1")
inner_join(a,b,by="x1")
full_join(a,b,by="x1")
semi_join(a,b,by="x1")
anti_join(a,b,by="x1")
#




