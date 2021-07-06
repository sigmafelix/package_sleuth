# Last revised 07/05/2021
if (!require(pacman)) {install.packages('pacman')}
library(pacman)
p_load(RandomFields)


model1 <- RMmatern(proj = "space", nu = 2.5, scale = 15, var = 2) * RMexp(proj = "time", scale = 10, var = 8)
model2 <- RMmatern(proj = "space", nu = 2, scale = 3, var = 4) * RMexp(proj = "time", scale = 7, var = 4)

xT <- seq(1, 50, 1)
T <- seq(1, 24, 1)
z1 <- RFsimulate(model1, x=xT, y=xT, T = T)
z2 <- RFsimulate(model2, x=xT, y=xT, T = T)


z12 <- z1
z12@data$variable2 <- z2@data$variable1

z1.pnt <- as(z1, 'RFspatialPointsDataFrame')
# error in RandomFields:::extract.names
z1.pnt.vg <- RFvariogram(data=z1.pnt, deltaT=c(12, 1))

# after commentizing line #243 of R/Methods-aux.R
z1.orig.vg <- RFvariogram(data=z1, deltaT=c(12, 1), distances = 10)
z1.pnt.vg <- RFvariogram(data=z1.pnt, deltaT=c(12, 1))

z2.orig.vc <- RFcov(data=z2, deltaT=c(12, 1))
z2.orig.vg <- RFvariogram(data=z2, deltaT=c(12, 1))

model <- RMbiwm(nudiag=c(1, 2), nured=1, rhored=1, cdiag=c(1, 5), 
                s=c(1, 1, 2))
n <- 30
x <- seq(0, 20, 0.1)
z <- RFsimulate(model, x=x, y=x, n=n)


model <- RMexp()
x <- seq(0, 10, 0.02)
n <- 2
z <- RFsimulate(model, x=x, n=n)
emp.vario <- RFvariogram(data=z)
plot(emp.vario, model=model)
