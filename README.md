color-categorization-games
==========================

List of files and directories and their contents:

sim_max_signaling.py                Python script that runs the simulations. Adapted from the code provided on blackboard for simmilarity-maximization games.
perceptual-space-with-sphere.csv    CSV file containing the perceptual space, including WCS coordinates, Munsell coordinates, CIELAB coordinates and coordinates to be used for the perfect sphere scenario (see report).
data-analysis-functions.R           R script with functions used in the analysis of the data. Includes reading simulation results, plotting, etc.
data-analysis.R                     R script that does all the analysis in one go. Includes reading the WCS data, calculating mode maps, classifying languages, reading all the simulation results, calculating matches, etc. WARNING: Please be aware that doing all the calculations is computationally intensive, so running this script might take a while...
WCS-Data-20110316                   Directory containing all the WCS data as available online.
results                             Directory containing results of the experiment.
results/simulation-<n>.csv          CSV file with the mode maps of all the simulations with <n> terms in the normal scenario.
results/sphere-<n>.csv              CSV file with the mode maps of all the simulations with <n> terms in the perfect sphere scenario.
results/simulation-<n>.csv          CSV file with the mode maps of all the simulations with <n> terms in the random scenario.
results/plots/simulations           Directory containing plots of the mode maps of the simulations in the normal scenario.
results/plots/wcs                   Directory containing plots of the mode maps of all the languages in the WCS.
