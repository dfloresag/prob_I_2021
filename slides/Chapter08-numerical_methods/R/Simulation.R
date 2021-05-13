#######################################################################
# R script for "Simulation"
#######################################################################
# Reference: "UniGE.ProbaI.2016/Cours/Simulation"
#######################################################################
# Last modified: 09.05.2016
#######################################################################
# Warranty: none.
#######################################################################
# Contact: Prof. Dr. Diego Kuonen, CStat PStat CSci, Statoo Consulting,
#          CH - www.statoo.info - kuonen@statoo.com
#######################################################################

require(animation)

source("SimulationFcts.R")

#---------------------------------------------------------------------

set.seed(2)
rbinom(1, 10, 0.5)
rbinom(1, 10, 0.5)
rbinom(1, 10, 0.5)

set.seed(3)
rbinom(5, 10, 0.5)
rbinom(5, 10, 0.5)

set.seed(4)
runif(1, 0, 1)

set.seed(5)
runif(5, 0, 1)

#---------------------------------------------------------------------
# UniGE/Teaching/UniGE.PC.MBA.HEC.2008
#----------------------------------------

particule
args(particule)

particule(nb.depl=6, nb.chem=1)

particule(nb.depl=8, nb.chem=10, interval=0.5)

particule(nb.depl=8, nb.chem=10, interval=0.5, histo=TRUE)

# particule(nb.depl=6, nb.chem=100)

particule(nb.depl=6, nb.chem=100, histo=TRUE)

# particule(nb.depl=6, nb.chem=100, histo=TRUE, theo=TRUE)

particule(nb.depl=8, nb.chem=100, histo=TRUE, theo=TRUE, interval=0.1)

# pdf("../PDF/simulation_particule.pdf", version="1.4", width=8, height=8)
# particule(nb.depl=8, nb.chem=100, histo=TRUE, theo=TRUE, interval=0)
# dev.off()

#---------------------------------------------------------------------

ani.options(interval = 0.25, nmax = 100)

#flip.coin(faces = c("Face", "Pile"),
#          type = "n",
#		  prob = c(0.5, 0.5),
#		  col = c("blue", "orange")
#		  )

flip.coin.DK(faces = c("Face", "Pile"), type = "n",
		         prob = c(0.5, 0.5), col = c("blue", "orange"))

flip.coin.DK(faces = c("Face", "Pile"), type = "n",
		         prob = c(0.5, 0.5), col = c("blue", "orange"))

ani.options(interval = 0, nmax = 1000)
flip.coin.DK(faces = c("Face", "Pile"), type = "n",
		         prob = c(0.5, 0.5), col = c("blue", "orange"))

#--------------------

ani.options(interval = 0.25, nmax = 200)
flip.coin.DK(faces = c("Face", "Pile"), type = "n",
		         prob = c(0.2, 0.8), col = c("blue", "orange"))

#--------------------

flip.coin.DK(faces = c("Face", "Debout", "Pile"), type = "n",
		         prob = c(0.45, 0.1, 0.45), col = c("black", "blue", "orange"))

#---------------------------------------------------------------------

ani.options(interval = 0.001, nmax=2000)
lln.ani.DK(FUN = function(n, mu) rbinom(n, 1, mu), mu = 0.5, np = 1, cex = 1.2,
           ylab = "Proportion de pile",
           col.pts="blue", col.mu="orange")

#---------------------------------------------------------------------

ani.options(nmax = ifelse(interactive(), 200 + 15 - 2, 2), interval = 0.25)
quincunx.DK(balls = 200, col.balls = rainbow(200))

ani.options(nmax = ifelse(interactive(), 1000 + 15 - 2, 2), interval = 0.01)
quincunx.DK(balls = 1000, col.balls = rainbow(1000))

#--------------------

ani.options(nmax = ifelse(interactive(), 200 + 15 - 2, 2), interval = 0.25)
quincunx2.DK(balls = 200, col.balls = rainbow(200))

ani.options(nmax = ifelse(interactive(), 1000 + 15 - 2, 2), interval = 0.01)
quincunx2.DK(balls = 1000, col.balls = rainbow(1000))

#---------------------------------------------------------------------
