# More Individuals Hypothesis
# Simulation functions

summarize.out <- function(N) {
  # N: array (dim=c(tMax, nSpp, nSim))
  if(is.array(N)) {
    N <- adply(N, 1:3) 
    names(N.df) <- c("Year", "Species", "Simulation", "N")
  }
  
  N.comm <- N %>% 
    group_by(Year, Simulation) %>% 
    summarise(., S=sum(N>0), N.tot=sum(N), N.Dom=max(N), N.noDom=N.tot-N.Dom)
  N.comm$Year <- as.numeric(N.comm$Year)
  
  return(list(N.df=N, N.comm=N.comm))
}






fit.mods <- function(comm.df, tMax) {
  # comm.df: ldply(comm.ls)
    # Year, Simulation, S, N.tot
  nSim <- nlevels(comm.df$Simulation)
  out.all <- tibble(Simulation=rep(1:nSim, each=3), 
                    Model=rep(c("Linear", "Log-Normal", "Quadratic"), nSim),
                    dAICc=NA)
  out.noDom <- out.all
  
  for(s in 1:nSim) {
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
    lm.s <- update(lm.s, . ~ N.noDom)
    ln.s <- update(ln.s, . ~ log(N.noDom))
    qu.s <- update(qu.s, . ~ I(N.noDom^2))
    out.noDom$dAICc[out.noDom$Simulation==s] <- aictab(list("linear"=lm.s, 
                                                        "log-normal"=ln.s,
                                                        "quadratic"=qu.s), 
                                                   sort=FALSE)$Delta_AICc
    out.noDom$BestMod <- out.noDom$dAICc==0
  }
  return(list(out.all=out.all, out.noDom=out.noDom))
}









