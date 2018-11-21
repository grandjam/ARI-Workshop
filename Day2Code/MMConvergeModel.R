mmconverge = function(H, M, K) {
####################
# MODEL PARAMETERS #
####################
n = 10 # number of members in a team
nIter = 500 # number of time points to let simulation run
lambda = .001 # learning rate; factor by which differences between an agent's IPF changes to match a speaker's IPF

netType = 1 # parameter for creating different network configurations; 1 = Dionne et al specificaiton, 2 = small world network; 3 = scale-free/preferential atatchment; 4 = random based on expected density
domSize = 100 # size of problem domain (must be integer value). NOTE: IN THEORY THIS VALUE CAN BE MANIPULATED, BUT CURRENT CODE IS NOT SET UP TO DO SO VERY EASILY. WRITTEN HERE SO THAT USERS CAN EASILY SEE THIS PARAMETER SETTING
rndmDom = domSize*.5 # number of random points to select, between which values will be interpolated to construct problem domain
domExptMean = 30 # size of expertise domain for each agent; higher numbers = more likely to be an expert on more things
domExptSD = 5 # variability in expertise domain size for each agent; higher numbers = agents more likely to vary in the number of things on which they are expert
ldrNdx = sample(1:n, 1) # randomly sample leader
confHeight = .1 # height of triangular function for changing confidence evaluations
confWidth = 10 # width of triangular function for changing confidence evaluations
confChg = c(head((0:(confWidth*10/2))*(confHeight/(confWidth*10/2)), -1), ((confWidth*10/2):0)*(confHeight/(confWidth*10/2))) # vector of values from a triangle function with height = confHeight & width = confWidth indicating

##########################################
# INITIALIZE INPUT DATA AND MODEL OUTPUT #
##########################################
# Initialize the true problem function (TPF)
TPFObj = createTPF(rndmDom = rndmDom, domSize = domSize)
TPF = TPFObj$tpf
# Initialize agents' individual problem functions (IPF); IPF is a list of dataframes where each list element corresponds to an agent
IPF = lapply(1:n, function(x) {createIPF(rndmDom = rndmDom, H = H, tpf = TPFObj, domExptMean = domExptMean, domExptSD = domExptSD, domSize = domSize)})
## Simple visualization of TPF and IPF
## plot(TPF$x, TPF$y, "l", lwd = 4)
## lines(IPF[[1]]$ipf$x, IPF[[1]]$ipf$y, lwd = 3, col = "blue")

# Initialize agents' confidence Perc (confPerc)
## The lapply function returns a list of length n for confPerc
## Each element of confPerc holds one agent's matrix of confidence perceptions (e.g., confPerc[1] = agent 1's confidence perceptions, confPerc[2] = agent 2's confidence perceptions)
## Note that the createConfPerc function maintains the same agent ordering in the confidence matrices for all agents. Thus agent 1's self-confidence rating is stored in  confPerc[[1]][1,], agent 2's self-confidence rating is stored in confPerc[[2]][2,], etc.
confPerc = lapply(1:n, function(x) {
  createConfPerc(n = n, agentNdx = x, H = H, domRng = IPF[[x]]$domRng, domSize = domSize)
})
## Simple visualization of confidence functions for one individual
## matplot(x = TPF$x, y = t(confPerc[[1]]), type = "l", col = 1:10, lty = 1)

# Initialize social network structure
teamNet = matrix(0, nrow = n, ncol = n)
## netType == 1 creates "LMX" structure based on Dionne et al.'s conceptualization
if (netType == 1) {
  inGrpNdx = c(ldrNdx, sample((1:n)[-ldrNdx], K-1)) # randomly samples leader in-group
  outGrpNdx = (1:n)[-inGrpNdx] # identifies out-group
  teamNet[inGrpNdx, inGrpNdx] <- 1 # place link in teamNet among all in-group members 
  teamNet[ldrNdx, outGrpNdx] <- 1 # place link in teamNet from leader to outgroup members
  teamNet[outGrpNdx, ldrNdx] <- 1 # place link in teamNet from outgroup members to leader
  diag(teamNet) <- 1 # place self-referent link to allow speaker to influence themselves
  # plot(graph_from_adjacency_matrix(teamNet, mode = "undirected", diag = F))
}
## netType == 2 creates a Watts-Strogatz small-world model
if (netType == 2) {
  ### In small world network, need to specify the nei and p parameters:
  ### nei = number of neighbors to connect to in small-world network
  ### p = rewiring probability in small-world network
  pplList <- sample_smallworld(dim = 1, size = n, nei = 2, p = .3)
  # plot(pplList, layout = layout_in_circle(pplList)) # Run this line to see what network looks like
  teamNet <- as_adjacency_matrix(pplList, sparse = F)
  diag(teamNet) <- 1 # place self-referent link to allow speaker to influence themselves
}
## netType == 3 creates a scale-free/prefential attachment network as described by Barabasi & Albert
if (netType == 3) {
  pplList <- sample_pa(n, m = 1, directed = F)
  # plot(pplList, layout = layout_with_fr(pplList)) # Run this line to see what network looks like
  teamNet <- as_adjacency_matrix(pplList, sparse = F)
  diag(teamNet) <- 1 # place self-referent link to allow speaker to influence themselves
}
## netType == 4 creates a random graph based on the expected density of the network
if (netType == 4) {
  pplList <- sample_gnp(n, p = .50)
  # plot(pplList, layout = layout_with_fr(pplList)) # Run this line to see what network looks like
  teamNet <- as_adjacency_matrix(n, sparse = F)
  diag(teamNet) <- 1 # place self-referent link to allow speaker to influence themselves
}

# Model output
## Group problem function (GPF; see Fig. 1 & p.1040 in Dionne et al)
### GPF is structured like TPF; column 1 = problem domain indices, columns 2:(nIter + 2) = team belief about problem domain at each time point
GPF = data.frame("x" = TPF$x,
                 matrix(nrow = length(TPF$x), ncol = nIter+1))
names(GPF) = c("x", paste("y", 0:(nIter), sep = ""))
GPF$y0 = computeGPF(IPF, confPerc) # Compute initial GPF at time = 0
### Simple visualization of TPF and GPF
### plot(TPF$x, TPF$y, "l", lwd = 4)
### lines(GPF$x, GPF$y0, lwd = 3, col = "red")

## Speaker -- who speaks at each time point
speaker = rep(NA, nIter)

## MM statistics
mmStats = data.frame("time" = 0:nIter,
                     "acc" = NA,
                     "confDist" = NA)
### Accuracy -- sum of squared differences between GPF and TPF
mmStats$acc[1] = -1*sum((TPF$y - GPF[,2])^2)
### Distance -- sum of Euclidean distances among all members' confience perceptions
mmStats$confDist[1] = confDist(confPerc = confPerc, n = n)

#########
# MODEL #
#########
for (i in 1:nIter) {
  # Step 1: Select speaker
  ## Compute probability of speaking by integrating self-confidence function of each agent
  ## Here we estimate the area under the curve using the trapezium method. This method essentially splits the curve into x number of trapezoids, computes their area, and then sums them together
  selfConf = sapply(1:n, function(p) {
    sum(diff(TPF$x) * (head(confPerc[[p]][p,], -1) + tail(confPerc[[p]][p,], -1))/2)
  })
  ## Transform the self-confidence integrals into probabilities
  pSpeak = selfConf/sum(selfConf)
  speaker[i] = sample(1:n, 1, prob = pSpeak)
  
  # Step 2: Orientation -- speaker selects topic to speak about
  ## Transform self-confidence values into probabilities
  pTopic = confPerc[[speaker[i]]][speaker[i],]/sum(confPerc[[speaker[i]]][speaker[i],])
  ## Samples problem domain based on pTopic and returns domain index/location
  topic = which(TPF$x == sample(TPF$x, 1, prob = pTopic))
  
  # Step 3: Differentiation -- expressed opinion is evaluated by members
  ## Create index identifying which members hear the expressed opinion (speaker hears its own opinion); T = heard, F = not heard
  rcvrNdx = teamNet[,speaker[i]] == 1
  ## Have ALL members evaluate opinion
  ## Note this is NOT what actually happens in process model; only those who hear the opinion evaluate it. We do it like this in this step for convenicence. It's easier to just "zero" out the rspEval for those who weren't supposed to hear later
  rspEval = sapply(1:n, function(x) {
    evalBelief(ipfSpkr = IPF[[speaker[i]]]$ipf$y[topic], ipfRcvr = IPF[[x]]$ipf$y[topic], confRcvr = confPerc[[x]][x,topic])
  })
  ## Remove rspEval for those who didn't actually hear the opinion
  rspEval[rcvrNdx == F] <- 0
  
  # Step 4: Integration -- update confidence perceptions and IPFs
  ## Update confidence perceptions
  ## Result of this stage is a new list (confPercUpdate) of length n that contains the updated confidence perceptions each agent holds about the speaker
  if (M == 0) { # M = 0 means agents decide to update based on their own rspEval
    confPerc = lapply(1:n, function(x) {
      confUpdate(confPerc = confPerc[[x]], rspEval = rspEval[x], speaker = speaker[i], topic = topic, confWidth = confWidth, confChg = confChg, domSize = domSize)
    })
    ## Update IPFs
    IPF = lapply(1:n, function(x) {
      ipfUpdate(ipfSpkr = IPF[[speaker[i]]], ipfRcvr = IPF[[x]], rspEval = rspEval[x], topic = topic, lambda = lambda)
    })
  } else { # M = 1 means agents decide to update based on the average rspEval for all members with whom the agent shares a connection
    ## Compute average response evaluation of the agents that each agent is connected to
    ## This computation does NOT include the agent's own evaluation
    aggRspEval = sapply(1:n, function(x) {
      mean(rspEval[-x][(teamNet[x,] == 1)[-x]])
    })
    ## This computation does include the agent's own evaluation
    # aggRspEval = sapply(1:n, function(x) {
    #   mean(rspEval[(teamNet[x,] == 1)])
    # })
    confPerc = lapply(1:n, function(x) {
      confUpdate(confPerc = confPerc[[x]], rspEval = aggRspEval[x], speaker = speaker[i], topic = topic, confWidth = confWidth, confChg = confChg, domSize = domSize)
    })
    ## Update IPFs
    IPF = lapply(1:n, function(x) {
      ipfUpdate(ipfSpkr = IPF[[speaker[i]]], ipfRcvr = IPF[[x]], rspEval = aggRspEval[x], topic = topic, lambda = lambda)
    })
  }
  
  # Record output of step
  GPF[,i+2] = computeGPF(IPF = IPF, confPerc = confPerc)
  mmStats$acc[i+1] = -1*sum((TPF$y - GPF[,i+2])^2)
  mmStats$confDist[i+1] = confDist(confPerc = confPerc, n = n)
}

###############
# DATA OUTPUT #
###############
return(list("mmStats" = mmStats,
            "GPF" = GPF,
            "IPF" = lapply(IPF, function(x) x$ipf),
            "TPF" = TPF,
            "speaker" = speaker,
            "teamNet" = teamNet))
}