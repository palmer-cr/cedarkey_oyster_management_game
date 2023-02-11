updateMain<-function(cstar, effort, monthsOpen, size_lim, numBoats, habitat,
                     timesteps, rest_flag, shell_rest,
                     min_H, previousValues, spatialValues){
  
  roundCounter = previousValues$roundCounter
  
  ### time and age dynamics
  #time
  years = timesteps                     #just time steps--most of model calibrated for monthly, probably.
  sizeLimit = size_lim*21.7 #Converts inches to mm (Not anymore, but is calibrated to Eds model)
  if(rest_flag==FALSE){
    rest_flag=0
  }else{
    rest_flag=1
  }
  maxTake = effort
  effort = effort*0.2*numHeat
  
  stormChance = 0
  stormChance = sample(1:60, 1) #Random chance of a major or minor hurricane existing
  
  #Plot dependent variables
  obs_err = vector(length=numHeat);
  proc_err = vector(length=numHeat);
  obs_err = rnorm(numHeat,0,sdobs)
  proc_err = rnorm(numHeat, 0, sdpro)
  
  #Fishery Conditions dependent on Size Limits
  vlh = sizeLimit        #length at 50% vulnerability
  Vul = 1/(1+exp(-(TL-vlh)/vsd))                #Vulnerability
  Lfished = vector(length=amax); Lfished[1]=1;  for (i in 2:amax){Lfished[i]=Lfished[i-1]*Surv[i-1]*(1-U*Vul[i-1])}; #Lfished[Amax] = Lfished[(Amax-1)]*surv[(Amax-1)]*(1-U*vul[(Amax-1)])/(1-surv[(Amax-1)]*(1-U*vul[(Amax-1)]))       #Standard with special calculation for terminal fished survivorship
  eprf = sum(Lfished*Fec)               #eggs-per-recruit fished conditions
  spr = epro/eprf                       #spawning potential ratio
  vbpro = sum(Vul*Wt*Lo)                #vulnerable biomass per recruit unfished conditions
  vbprf = sum(Vul*Wt*Lfished)           #vulnerable biomiass per recruit fished conditions
  vbo = Ro*vbpro                        #vulnerable biomass at unfished conditions
  
  #Initializing Model
  eggs = matrix(nrow=years, ncol=numHeat); 
  N=matrix(nrow=years, ncol=numHeat); 
  B=matrix(nrow=years, ncol=numHeat); 
  VB=matrix(nrow=years, ncol=numHeat); 
  PB=matrix(nrow=years, ncol=numHeat);
  plotProb=NULL;
  plotBoats=matrix(nrow=years, ncol=numHeat);
  boatTotals=NULL;
  yield = vector(length=years);       #Varies across time
  yield[]=0
  shell=matrix(nrow=years, ncol=numHeat); 
  shell_add=matrix(nrow=years, ncol=numHeat); 
  eff=matrix(nrow=years, ncol=numHeat); 
  hr=matrix(nrow=years, ncol=numHeat);
  ahab=matrix(nrow=years, ncol=numHeat); 
  bhab=matrix(nrow=years, ncol=numHeat);
  H=matrix(nrow=years, ncol=numHeat);
  nage = array(0, c(years, amax, numHeat))
  Surv_mat = array(NA, c(years, amax, numHeat)) #set up array for age-structured abundance
  
  restArea<-spatialValues$shellRest
  sancZones<-spatialValues$noTake
  harvPlots<-spatialValues$harvestPlots
  
  if(roundCounter == 1){
    ### Initilization (year one) 
    #initializing first year of all year dependent values, note, these must go in a specific order
    nage[1,,]=Ro*Lo                            #numbers at age, year 1
    eggs[1,]= sum(Fec*nage[1,,1])                #first value of eggs
    N[1,] = sum(nage[1,,1])                      #first value of total numbers
    B[1,] = sum(nage[1,,1]*Wt)                   #first value of total biomass
    VB[1,] = sum(nage[1,,1]*Wt*Vul)              #first value of vulernable biomass
    PB[1,] = sum(nage[1,,1]*Wt*PO)
    plotBoats[1,]<-0
    eff[1,] = 0                               #effort at time t, here just a constant
    hr[1,] = 1-exp(-(q*eff[1,]))                #calculate harvest rate (U) and 1-exp(-F)
    yield[1] = sum(hr[1,]*VB [1,])                   #first value of yield as hr * vulnerable biomass
    
    for(p in 1:numHeat){
      Surv_mat[1,,p] = (1+vblinf/TL*(exp(vbk)-1))^(-Mt[p]/vbk)    #Survival in each time step, could have alterantively done: exp(-Madult*linf/base_len)
      shell_add[1,p] = sum(nage[1,,p]*(1-Surv_mat[1,,p])*(TL/100)^2)    #the shell added in each year is a function of the numbers that didn't survive (i.e. nage*(1-surv)) and the lengths.
    }
    
    #shell and habitat stuff
    shell[1,] = shell_init                     #defining shell as shell init. Note that Carl does have an equilibrium calc in his model, but not implemented here
    #    shell_add[1] = sum(nage[1,]*(1-Surv)*(TL/100)^2)    #the shell added in each year is a function of the numbers that didn't survive (i.e. nage*(1-surv)) and the lengths. 
    
    H[1,] = 1                         #null assumption that 100% of area is suitable for recruitment. Can always start with this
    
    ahab[1,] = bha*H[1,]^cstar         #habitat modified Beverton Holt a parm according to walters et al. 2007
    bhab[1,] = bhb*H[1,]^cstar/H[1,]    #hab modified bev holt b parm according to same. note that cstar is a switch determining whether hab affects a or b
  }
  else{
    ### Initilization from previous values
    nage[1,,]=previousValues$nage
    eggs[1,]= previousValues$eggs                #first value of eggs !!!! need to check to make sure this is correct
    N[1,] = previousValues$N                      #first value of total numbers
    B[1,] = previousValues$B                   #first value of total biomass
    VB[1,] = previousValues$VB              #first value of vulernable biomass
    PB[1,] = previousValues$PB              #first value of vulernable biomass
    plotBoats[1,]<-0
    eff[1,] = previousValues$eff                              #effort at time t, here just a constant
    hr[1,] = previousValues$hr                #calculate harvest rate (U) and 1-exp(-F)
    yield[1] = previousValues$yield                #first value of yield as hr * vulnerable biomass
    
    for(p in 1:numHeat){
      Surv_mat[1,,p] = (1+vblinf/TL*(exp(vbk)-1))^(-Mt[p]/vbk)    #Survival in each time step, could have alterantively done: exp(-Madult*linf/base_len)
    }
    
    #shell and habitat stuff
    shell[1,] = previousValues$shell                    #defining shell as shell init. Note that Carl does have an equilibrium calc in his model, but not implemented here
    #    shell_add[1] = sum(nage[1,]*(1-Surv)*(TL/100)^2)    #the shell added in each year is a function of the numbers that didn't survive (i.e. nage*(1-surv)) and the lengths. this 
    shell_add[1,] = sum(nage[1,,]*(1-Surv_mat[1,,])*(TL/100)^2)    #the shell added in each year is a function of the numbers that didn't survive (i.e. nage*(1-surv)) and the lengths. this 
    
    H[1,] = previousValues$H
    
    ahab[1,] = previousValues$ahab         #habitat modified Beverton Holt a parm according to walters et al. 2007
    bhab[1,] = previousValues$bhab    #hab modified bev holt b parm according to same. note that cstar is a switch determining whether hab affects a or b
  }
  
  #calculations for vectors--i.e. year but not age varying components of model
  for( i in 2:years) {
    #recruitment stuff
    shell[i,] = shell[i-1,]*shell_surv+shell_add[i-1,]
    
    for(p in 1:numHeat){
      if(rest_flag==1 && restArea[p]==1) shell[i,p] = shell[i,p] + shell_rest
      
      H[i,p] = pmax(min_H, 1/(1+exp(-(shell[i,p]-sht)/shsd)))   #This is a "safety net" function where we say look habitat can't be worse than 0.1 no matter what happens to shell. Basically it says oysters can recruit on the rock under the reef.
      #Operationalizing habitat (H) on recruitment parms
      ahab[i,p] = bha*H[i,p]^cstar         #habitat modified Beverton Holt a parm according to walters et al. 2007
      bhab[i,p] = bhb*H[i,p]^cstar/H[i,p]    #hab modified bev holt b parm according to same. note that cstar is a switch determining whether hab affects a or b
      
      # ahab <- bha*H^cstar                   #habitat specific a parm of bev holt
      # bhab <- bhb*H^cstar/H                 #habitat specific b parm of bev holt
    }
    
    
    #Recruitment, simple beverton holt
    for(p in 1:numHeat){
      nage[i,1,p] = ahab[i-1,p]*eggs[i-1,p]/(1+bhab[i-1,p]*eggs[i-1,p])*exp(proc_err[p])              #the recruitment each year, a product of beverton holt, eggs, and process error ****note this is going to be changed to be a function of a habitat dependent b parmameter, see DD habitat excel file
    }
    
    #fishery stuff
    #Used to determine harvest locations
    plotBoats[i,]<-0
    boatsAssigned<-0
    denom<-0
    plotProb[]=0
    
    if(i%%12<monthsOpen){ #Effort based on open season or summer closure
      for(p in 1:numHeat){
        if(sancZones[p]!=1){
          denom=denom+PB[i-1,p]
        }
      }
      
      for(p in 1:numHeat){
        if(sancZones[p]==1){
          plotProb[p]=0
        }else{
          plotProb[p]<-PB[i-1,p]/denom
        }
      }
      
      while(boatsAssigned<numBoats){
        randNum<-runif(1)
        for(p in 1:numHeat){
          if(randNum < plotProb[p]){
            plotBoats[i,p]=plotBoats[i,p]+1
            boatsAssigned<-boatsAssigned+1
            break()
          }else{
            randNum<-randNum-plotProb[p]
          }
        } 
      }
    }else{
      plotBoats[i,]<-0
    }
    
    for(p in 1:numHeat){
      eff[i,p] = effort*plotBoats[i,p]   
      hr[i,p] = 1-exp(-q*eff[i,p])
      if(hr[i,p] > maxTake){ #Actual harvest rate caps at bushel limit
        hr[i,p] = maxTake
      }
    }
    
    for(p in 1:numHeat){
      Surv_mat[i,,p] = (1+vblinf/TL*(exp(vbk)-1))^(-Mt[p]/vbk)    #Survival in each time step, could have alterantively done: exp(-Madult*linf/base_len)
      #Age loop 
      for(j in 2:amax)
      {
        #nage[i,j] = nage[i-1,j-1]*Surv[j-1]*(1-Vul[j-1]*hr[i])
        nage[i,j,p] = nage[i-1,j-1,p]*Surv_mat[i,j-1,p]*(1-Vul[j-1]*hr[i,p])
        
        if(stormChance <=4){ #Minor hurricane approximately every fifteen years
          nage[i,j,p] = nage[i,j,p] * (1 - runif(1, min = 0, max = 0.02))
        }else if (stormChance == 5){ #Major hurricane 1 in 60 chance
          nage[i,j,p] = nage[i,j,p] *(1 - runif(1, min = 0.024, max = 0.067)) #Mortality numbers for storms configured from Patrick et. al 2020
        }
      }
      #shell_add[i] = sum(nage[i,]*(1-Surv)*(TL/100)^2)
      shell_add[i,p] = sum(nage[i,,p]*(1-Surv_mat[i,,p])*(TL/100)^2) #Storms are not impacting shell availability currently, this choice was made because storms both increase mortality but also degrade and wash away shell material. A more realistic estimate can be made later.
      eggs[i,p] = sum(Fec*nage[i,,p])                                               #eggs in each year is simply the sum of numbers at age in that year * Fecundity
      N[i,p] = sum(nage[i,,p])                                                      #total numbers, sum of numbers at each age
      B[i,p] = sum(nage[i,,p]*Wt)                                                   #total biomass, sum of numbers at age * weight at age
      VB[i,p] = sum(nage[i,,p]*Wt*Vul)                                              #vulernable biomass, sum of numbers at age * weight at age * vul at age
      PB[i,p] = sum(nage[i,,p]*Wt*PO)
      yield[i] = yield[i]+(hr[i,p]*VB[i,p])                                                   #yield, dependent on hr and vulnerable biomass
    }
  }
  for(p in 1:numHeat){
    boatTotals[p]=sum(plotBoats[,p])
  }
  boatTotals<-0-boatTotals
  
  for(k in 1:8){ #8 NPC boats will be shown
    boatTotals[which.min(boatTotals)]=1
  }
  boatTotals<-pmax(boatTotals, 0)
  
  yield=mean(yield)
  
  roundCounter = roundCounter + 1
  list (years=years, eggs = eggs[timesteps,], N=N[timesteps,], B=B[timesteps,], VB=VB[timesteps,], PB=PB[timesteps,],  yield = yield,
        shell=shell[timesteps,], shell_add=shell_add[timesteps,], eff=eff[timesteps,], hr=hr[timesteps,], ahab=ahab[timesteps,], bhab=bhab[timesteps,],H=H[timesteps,],
        nage = nage[timesteps,,], boatTotals = boatTotals, roundCounter = roundCounter)
}