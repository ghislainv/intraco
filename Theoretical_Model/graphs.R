#!/usr/bin/R

## ==============================================================================
## author          :Ghislain Vieilledent
## email           :ghislain.vieilledent@cirad.fr, ghislainv@gmail.com
## web             :https://ghislainv.github.io
## license         :GPLv3
## ==============================================================================

# Libraries
library(gstat)
library(sp)
library(latticeExtra)

# Grid dimensions
gridDim <- 10
xy <- expand.grid(x=1:gridDim, y=1:gridDim)

# Variogram model, with defined sill and range
varioMod <- vgm(psill=0.05, range=10, model='Exp')

# Set up an additional variable from simple kriging
zDummy <- gstat(formula=z~1, locations = ~x+y, dummy=TRUE, 
                beta=1, model=varioMod, nmax=20)

# Generate 2 randomly autocorrelated predictor data fields
set.seed(3)
xyz <- predict(zDummy, newdata=xy, nsim=2)

# Random points
set.seed(1234)
sp1 <- cbind(runif(20, 0.5, 9.5), runif(20, 0.5, 9.5))
sp2 <- cbind(runif(20, 0.5, 9.5), runif(20, 0.5, 9.5))

# Grid data and plot
test <- xyz
gridded(test)=~x+y
p <- spplot(test[1]) +
  layer(panel.points(sp1[,1], sp1[,2], col="black", pch=1, cex=2)) +
  layer(panel.points(sp2[,1], sp2[,2], col="black", pch=3, cex=2))
pdf(file="Theoretical_Model/figs/env.pdf")
plot(p)
dev.off()
  
# EOF