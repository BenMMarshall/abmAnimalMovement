
library(abmAnimalMovement)

# Noise only --------------------------------------------------------------

envNoiseTest <- genLandscape_noise(1000, 1000)

quick_plot_matrix(envNoiseTest)

# Basic gradient ----------------------------------------------------------

envGradMat <- genLandscape_gradient(1000, 1000)

quick_plot_matrix(envGradMat)




