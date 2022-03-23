library(lattice)
library(MASS)
histogram(crabs$CW)
histogram(~CW|sex, data = crabs,col='lightblue')

data = crabs$CW
datatotal = crabs


nortest2 <- with(crabs, tapply(CW, sex,shapiro.test))
nortest2


library(readr)
ohi <- read_csv("IDI_last version.csv")
dfB = ohi$B

nortest1<-shapiro.test(ohi$B)
nortest1
