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
nTeams = 20
## H is group heterogeneity in domains of expertise [0,1]; H = 1 means agents have distinct domains of expertise (heterogenous), H = 0 means agents have no domain expertise (homogenous)
## K is number of members in the in-group network; K = 2:n-1
## M is level of mutual interest [0,1]; M = 0 means agents determine whether to update confidence in speaker based only on its evaluation of opinion, H = 1 means agents determine whether to update confidence in speaker based on avg rsp of other listeners
conds = expand.grid(H = 0:1, M = 0:1, K = 2:10)
conds = conds[rep(seq(nrow(conds)), nTeams),]
conds = conds[order(conds$H, conds$M, conds$K),]
rownames(conds) <- 1:nrow(conds)
condsList <- split(conds, seq(nrow(conds))) # Turns condition data into list so that it can be used for parallel processing

# (3) Load additional model scripts; make sure model code is in the same directory in which this simulation code is run
source("MMConvergeFuncs.R") # Loads in model functions
source("MMConvergeModel.R") # Loads in process model script

# (4) Export all data and packages necessary to run the model
#sfLibrary(igraph)
sfExportAll()
sfClusterSetupRNG()

# (5) Run simulation
Start_time <- Sys.time()
condsDat <- sfClusterApplyLB(condsList, function(x) mmconverge(as.numeric(x[1]), as.numeric(x[2]), as.numeric(x[3])))
Run_time <- Sys.time() - Start_time
Run_time

# (6) Stop cluster and return R to running on single-core
sfStop()

### (7) Save raw data
save(conds, file = "conds.RData")
save(condsDat, file = "condsDat.RData")

## (8) Save formatted data
mmStatsDat = lapply(condsDat, function(x) x$mmStats)
mmStatsDat = lapply(mmStatsDat, function(x) {
  accScaled = x$acc - x$acc[1]
  confScaled = x$confDist - x$confDist[1]
  return(data.frame(x, accScaled, confScaled))
})
finalmmStats = do.call("rbind", mmStatsDat)
finalmmStats = cbind(finalmmStats, conds[rep(seq_len(nrow(conds)), each = max(finalmmStats$time)+1), ])
finalmmStats$teamID = rep(1:nrow(conds), each = max(finalmmStats$time)+1)
row.names(finalmmStats) = 1:nrow(finalmmStats)
save(finalmmStats, file = "finalmmStats.RData")


# Plots, etc.
## Compare to Dionne et al. Figure 4
matplot(matrix(finalmmStats$confScaled[finalmmStats$H == 1 & finalmmStats$M == 0  & finalmmStats$K == 10], nrow = 501), 
        type = "l", lty = 1)

## Compare to Dionne et al. Figure 6
matplot(matrix(finalmmStats$accScaled[finalmmStats$H == 0 & finalmmStats$M == 1  & finalmmStats$K == 10], nrow = 501), 
  type = "l", lty = 1)

## Create graphs to compare to Dionne et al. Figure 5 & 7
finalTimePt = finalmmStats[finalmmStats$time == 500,]
finalTimePtConf = aggregate(finalTimePt$confScaled, by = list(finalTimePt$H, finalTimePt$M, finalTimePt$K), mean)
finalTimePtAcc = aggregate(finalTimePt$accScaled, by = list(finalTimePt$H, finalTimePt$M, finalTimePt$K), mean)
names(finalTimePtConf) = c("H", "M", "K", "confScaled")
names(finalTimePtAcc) = c("H", "M", "K", "accScaled")
finalTimePtConf = finalTimePtConf[order(finalTimePtConf$H, finalTimePtConf$M),]
finalTimePtAcc = finalTimePtAcc[order(finalTimePtAcc$H, finalTimePtAcc$M),]

## Compare to Figure 5
matplot(t(matrix(finalTimePtConf$confScaled, nrow = 4, byrow = T)), type = "l", lty = 1, col = 1:4)
## Compare to Figure 7
matplot(t(matrix(finalTimePtAcc$accScaled, nrow = 4, byrow = T)), type = "l", lty = 1, col = 1:4)