###################
# MODEL FUNCTIONS #
###################
# Function for creating randomly generated true problem function (TPF)
## Returns data frame with columns giving x and y coordinates of TPF 
createTPF = function(rndmDom, domSize) {
  x = sort(c(1, sample(2:(domSize-1), rndmDom), domSize)) # randomly select rndmDom integers between 1 and 100; to ensure the final range of the TPF always spans 1 to 100, we force 1 and 100 to be included in this range of x values
  y = runif(rndmDom+2, 0, 1) # randomly sample rndmDom+2 values from uniform distribution bounded between 0 and 1; the "+2" values is so that we sample values for the hard-coded 1 and 100 x-values
  yInterp = spline(x, y, n = domSize*10) # interpolate 1000 values between the randomly sampled x and y values
  tpf = data.frame(x = yInterp$x, # create data frame containing x and y values
                   y = yInterp$y)
  # problem function is bounded between 0 and 1; transform any interpolated values below 0 into 0, and above 1 into 1
  tpf$y[tpf$y < 0] = 0
  tpf$y[tpf$y > 1] = 1
  return(list("tpf" = tpf,
              "x" = x,
              "y" = y))
}

# Function for creating randomly generated individual problem functions (IPF)
## Returns a list containing two named elements:
## (1) ipf = data frame with columns giving x and y coordinates of IPF and a column (dom) indicating whether x is within the agent's domain of expertise
## (2) domRng = vector giving range of expertise domain
createIPF = function(H, tpf, domExptMean, domExptSD, domSize, rndmDom) {
  ## Identify domain expertise range -- note we do this even if agent will not have a domain of expertise
  ### Dionne et al do not describe exactly how agent's domains of expertise are specified; they simply note that they are "formulated as a 
  ### randomly selected continuous finite range in the 0-100 problem domain, whose width does not exceed 50" (p. 1040). We add some control around this
  ### and allow agents to have differing sizes of expertise range by randomly sampling a value to determine the domain expertise range
  domExptSize = round(rnorm(1, mean = domExptMean, sd = domExptSD))
  ## Impose limits that an agent's range of expertise cannot be less than 10% or greater than 50% of domSize
  if (domExptSize > domSize*.5) {domExptSize = domSize*.5}
  if (domExptSize < domSize*.1) {domExptSize = domSize*.1}
  ## Randomly select a consecutive range of domains between 1:100 equal to length of domExptSize
  domExptStrt = seq(domSize - domExptSize + 1)
  domExptStrt = sample(domExptStrt, 1)
  domExptRng = domExptStrt:(domExptStrt + domExptSize - 1)
  # H = 1 means that individuals in teams have "domains of expertise"
  if (H == 1) {
    ## Create the agent's IPF by creating new belief values for domains outside the area of expertise while making areas inside the expertise range = tpf
    ipfObj = createTPF(rndmDom, domSize) # create brand new ipf for agents to reflect that an agent's IPF will be very different from TPF outside the expertise range
    ndx = findInterval(ipfObj$tpf$x, c(min(domExptRng), max(domExptRng))) == 1 # create an index of all the rows in tpf that fall inside the domain of expertise; just for convenience
    ipfObj$tpf$y[ndx] = tpf$tpf$y[ndx] # make ipf values inside of expertise domain equal to corresponding tpf values
    ipf = ipfObj$tpf
  } else { # H = 0 means that individuals do not have domains of expertise and everybody is about equally off from TPF
    y = rnorm(length(tpf$x), tpf$y, .25) ## create ipf for agents that are near the tpf
    yInterp = spline(tpf$x, y, n = domSize*10)
    ipf = data.frame(x = yInterp$x,
                     y = yInterp$y)
    ipf$y[ipf$y < 0] = 0
    ipf$y[ipf$y > 1] = 1
  }
  return(list("ipf" = ipf,
              "domRng" = domExptRng))
}

# Function for creating confidence distributions
## Returns a dataframe n x domSize*10 corresponding to that agents' confidence perceptions for itself and all other agents across each aspect of the problem domain
createConfPerc = function(n, agentNdx, H, domRng, domSize) {
  confPerc = matrix(NA, nrow = n, ncol = domSize*10) # Initialize matrix that will hold final confindence perception values
  ## H = 1 means that individuals in teams have "domains of expertise"; Dionne et al. state that individuals have higher confidence in their area of expertise (p. 1040)
  if (H == 1) {
    ## Create agent's self-confidence perceptions
    selfConf = runif(domSize, .05,.5) # Randomly sample self-confidence perceptions across all aspects of problem domain from a lower range
    selfConf[domRng] = runif(length(domRng), .7,.9) # Replace self-confidence perceptions in areas of expertise with randomly sampled from a higher range
    selfConf = spline(x = 1:domSize, y = selfConf, n = domSize*10) # Interpolate self-confidence perceptions across same range of problem domain values as in IPF/TPF
    ## Create agent's perceptions of other's confidence
    otherConf = t(replicate(n-1, expr = runif(domSize, .05, .5))) # Randomly sample (n-1) other-confidence perceptions across all aspects of problem domain from a lower range
    otherConf = t(sapply(1:(n-1), function(x) { # Interpolate (n-1) other-confidence perceptions across same range of problem domain values as in IPF/TPF
      out = spline(x = 1:domSize, y = otherConf[x,], n = domSize*10)
      return(out$y)
    }))
    ## Insert confidence perceptions in correct location of final matrix
    confPerc[agentNdx,] = selfConf$y # Note that this parameterization always places an agent's self-confidence ratings in row = agentNdx. This ensures that the rows in each agent's final confidence perception matrix always refer to the same agents
    confPerc[(1:n)[-agentNdx],] = otherConf
  } else { ## H = 0 means that individuals do not have domains of expertise
    confRaw = t(replicate(n, expr = runif(domSize, .05, .5))) # Confidence perceptions for all agents (including self) are randomly sampled from a lower range
    confPerc = t(sapply(1:n, function(x) { # Interpolate confidence perceptions across same range of problem domain values as in IPF/TPF
      out = spline(x = 1:domSize, y = confRaw[x,], n = domSize*10)
      return(out$y)
    }))
  }
  ## Transform any negative confidence values to 0 (negative values can occassionally be generated because of the spline procedure)
  confPerc[which(confPerc < 0)] <- 0
  ## Normalize all confidence values to sum to 1
  confPercNorm = apply(confPerc, 2, function(x) {x/sum(x)})
  return(confPercNorm)
}

# Function for computing group problem function (GPF)
## Dionne et al. define GPF as sum of each member's IPF weighted by average confidence in each member's knowledge (p. 1040)
computeGPF = function(IPF, confPerc) {
  sapply(1:length(IPF[[1]]$ipf$x), function(dmnNdx) { # Loop through each part of the problem domain
    ## Breakdown of this computation:
    ## sapply(IPF, function(w) w$ipf$y[dmnNdx]) -> extracts the value in each member's IPF at point dmnNdx in the problem domain. This will be a vector of n values
    ## sapply(confPerc, function(z) z[,dmnNdx]) -> extracts each member's confidence perceptions about all other members at point dmnNdx in the problem domain. This will be a n x n matrix, in which column 1 = agent 1's confidence perceptions, column 2 = agent 2's confidence perceptions, etc. Because of normalization, each column sums to 1
    ## rowMeans(sapply(confPerc, function(z) z[,dmnNdx])) -> computes average degree of confidence the team has about each agent at point dmnNdx in the problem domain. This will be a vector of n values, where higher values mean more confidence
    ## sapply(IPF, function(w) w$ipf$y[dmnNdx]) * rowMeans(sapply(confPerc, function(z) z[,dmnNdx])) -> computes agents' IPF at point dmnNdx in the problem domain weighted by the average degree of confidence the team has about an agent at point dmnNdx. This will be a vector of n values
    ## sum(sapply(IPF, function(w) w$ipf$y[dmnNdx]) * rowMeans(sapply(confPerc, function(z) z[,dmnNdx]))) -> computes aggregate (sum) of each member's IPF weighted by average confidence in each member's knowledge at point dmnNdx in the problem domain. This will be a vector of size equal to the problem domain
    sum(sapply(IPF, function(w) w$ipf$y[dmnNdx]) * rowMeans(sapply(confPerc, function(z) z[,dmnNdx])))
  })
}

# Function for evaluating expressed beliefs
## Equation 1 in Dionne et al. (p. 1041)
evalBelief = function(ipfSpkr, ipfRcvr, confRcvr) {
  diffEval = abs(ipfSpkr - ipfRcvr)
  rspEval = (1-2*diffEval)*confRcvr
  return(rspEval)
}

# Function for updating confidence beliefs
confUpdate = function(confPerc, rspEval, speaker, topic, confWidth, confChg, domSize) {
  confNew = confPerc[speaker,]
  lRange = (topic-(confWidth*10/2))
  uRange = (topic+(confWidth*10/2))
  ## Need to adjust upper and lower range if triangle function extends beyond limits of problem domain
  if (lRange < 1) {
    confChg = confChg[(-lRange+2):length(confChg)]
    lRange = 1
  }
  if (uRange > domSize*10) {
    confChg = confChg[1:(length(confChg)-(uRange-(domSize*10)))]
    uRange = domSize*10
  }
  ## If opinion is evaluated positively, we increase confidence for a range of topic domains centered on the expressed opinion by an amt determined by the triangle function
  if (rspEval > 0) {
    confNew[lRange:uRange] <- confNew[lRange:uRange] + confChg
  }
  ### If opinion is evaluated negatively, we decrease confidence for a range of topic domains centered on the expressed opinion by an amt determined by the triangle function
  if (rspEval < 0) {
    confNew[lRange:uRange] <- confNew[lRange:uRange] - confChg
  }
  ## If opinion is zero or not evaluted, we don't change confidence perceptions
  if (rspEval == 0) {
    confNew = confPerc[speaker, ]
  }
  ## Insert updated confidence perceptions for the speaker into the overall confidence perception matrix
  confPerc[speaker,] <- confNew
  ## If any confidence values become negative, set to 0
  confPerc[speaker,][which(confPerc[speaker,] < 0)] <- 0
  ## Normalize confidence perceptions for ALL topic domains so they sum to 1
  confPerc = apply(confPerc, 2, function(x) {x/sum(x)})
  return(confPerc)
}

# Function for updating IPFs
ipfUpdate = function(ipfSpkr, ipfRcvr, rspEval, topic, lambda) {
  ### If opinion is evaluated positively, we reduce difference between speaker and receiver IPF at topic by a factor of lambda
  if (rspEval > 0) {
    ipfRcvr$ipf$y[topic] = lambda*(ipfSpkr$ipf$y[topic] - ipfRcvr$ipf$y[topic]) + ipfRcvr$ipf$y[topic]
  }
  return(ipfRcvr)
}

# Function for computing sum of Euclidean distance across all confidence functions for all agents
confDist = function(confPerc, n) {
  agentDist = sapply(1:n, function(x) {
    # ndx = (1:n)[-x] # identifies vector of agent IDs excluding the target agent
    tmp = t(sapply(confPerc, function(y) {y[x,]})) # Extracts confidence perceptions about target agent from all other agents (including perceptions from target agent)
    out = matrix(mapply(function(a,b) {sqrt(sum(abs(tmp[a,] - tmp[b,])^2))}, a = rep(1:n, each = n), b = rep(1:n, n)), n, n) # Computes Euclidean distance between the difference in confidence functions for all agents
    out = sum(out[lower.tri(out)]) # remove self-to-self and duplicate comparisons and sum all differences together
    return(out)
  })
  aggDist = -1*sum(agentDist)
  return(aggDist)
}