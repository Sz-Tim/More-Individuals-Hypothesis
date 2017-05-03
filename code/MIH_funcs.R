# More Individuals Hypothesis
# Simulation functions

summarize.out <- function(N) {
  # N: array (dim=c(tMax, nSpp, nSim))
  
  require(plyr); require(data.table)
  
  N.df <- data.table(adply(N, 1:3))
  names(N.df) <- c("Year", "Species", "Simulation", "N")
  
  N.comm <- N.df[, .(S=sum(N>0), 
                     N.tot=sum(N), 
                     N.noDom=sum(N)-max(N),
                     N.Dom=max(N)),
                by=.(Year, Simulation)]
  N.comm$Year <- as.numeric(N.comm$Year)
  
  return(list(N.df=N.df, N.comm=N.comm))
}







fit.mods <- function(comm.df, tMax) {
  # comm.df: ldply(comm.ls)
    # Year, Simulation, S, N.tot
  
  require(dplyr); require(AICcmodavg)
  out.all <- data.table(Simulation=rep(1:nlevels(comm.df$Simulation), each=3), 
                    Model=c("Linear", "Log-Normal", "Quadratic"),
                    dAICc=NA)
  out.noDom <- out.all
  
  for(s in 1:nlevels(comm.df$Simulation)) {
    c.s <- droplevels(filter(comm.df, Simulation==s & Year==tMax))
    
    # all species
    lm.s <- lm(S ~ N.tot, data=c.s)
    ln.s <- lm(S ~ log(N.tot), data=c.s)
    qu.s <- lm(S ~ N.tot + I(N.tot^2), data=c.s)
    out.all$dAICc[out.all$Simulation==s] <- aictab(list("linear"=lm.s, 
                                                        "log-normal"=ln.s,
                                                        "quadratic"=qu.s), 
                                                   sort=FALSE)$Delta_AICc
    out.all$BestMod <- out.all$dAICc==0
    
    # excluding the dominant species
    lm.s <- lm(S ~ N.noDom, data=c.s)
    ln.s <- lm(S ~ log(N.noDom), data=c.s)
    qu.s <- lm(S ~ N.noDom + I(N.noDom^2), data=c.s)
    out.noDom$dAICc[out.noDom$Simulation==s] <- aictab(list("linear"=lm.s, 
                                                        "log-normal"=ln.s,
                                                        "quadratic"=qu.s), 
                                                   sort=FALSE)$Delta_AICc
    out.noDom$BestMod <- out.noDom$dAICc==0
  }
  return(list(out.all=out.all, out.noDom=out.noDom))
}








