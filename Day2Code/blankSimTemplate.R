# ~~~~~~~~~~~~~~ #
# Run Simulation #
# ~~~~~~~~~~~~~~ #
library(snowfall)

# (1) Initialize numer of parallel processors to use
## Option 1: For running on local computer with multiple cores. Warning: This will use the maximum number of cores available on the computer if possible!
library(parallel)
sfInit(parallel = T, cpus = detectCores(), type = "SOCK")

## Option 2: For running on cores distributed across multiple computers/nodes
# sfSetMaxCPUs(number = 1000)
# sfInit(parallel = T, cpus = 250, type = "MPI")

## (2) Create experimental conditions to run. To make the call to use parallel processing easier, conds contains as many rows as there are teams to be simulated
## nTeams PER condition to run

## Design matrix

# (3) Load additional model scripts; make sure model code is in the same directory in which this simulation code is run

# (4) Export all data and packages necessary to run the model
sfLibrary()
sfExportAll()
sfClusterSetupRNG()

# (5) Run simulation

# (6) Stop cluster and return R to running on single-core
sfStop()

### (7) Save raw data

## (8) Save formatted data