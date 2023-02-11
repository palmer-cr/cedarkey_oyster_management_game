totalTurns=51
permShell = TRUE #This was a quick add used to remove the shell restoration checkbox


#Define labels for UI
introLbl = "In this game, you are tasked with managing the Cedar Key oyster fishery. You will be scored based on a combination of how well you maintain reef health and how many oysters are commercially harvested under your management regime. <br/> <br/> To begin, set your management preferences in the sidebar on the left. You can also change the map to display overall reef health, the amount of shell available, or no-take zones. On the 'Oyster Sanctuary Map' you can click plots that you wish to make no-take zones. These zones will also appear on the other maps. On the 'Shell Restoration Map', you can click plots to enable shell restoration in these areas. Once you start playing, you will also see which plots are being most heavily harvested. These plots will have fishing boats shown on them. Once you have your settings the way you want them, click the 'Update' button. This will advance the simulation forward by 1 year. Between each year, you will have another opportunity to adjust your management settings. After 50 years, you will be given a final score.<br/> <br/>"
effortLbl = "<br/>How many months out of the year should be open?"
amountLbl = "What is the maximum number of sacks that should be legally harvestable in a single day?"
sizeLbl = "Select a minimum size for legal harvest (inches)"
boatsLbl = "What is the maximum number of commercial harvest licenses that should be available in the Cedar Key oyster fishery?"
shellLbl="Check this box to implement shell restoration"
mapLbl="Choose which map to display"
mapChoices=c("Population Map", "No-Take Map", "Shell Restoration Map")
breakLbl="<br/>"

introVid<-HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/e842qdxOvKs" frameborder="0" gesture="media" allow="autoplay; encrypted-media" allowfullscreen></iframe>')

#Constants needed for Ed's Model
### Constants, life history and fishery ###
timesteps = 12
amax = 36       #maximum age, (or years)
Ro = 1000       #recruitment at unfished conditions
CR = 8          #recruitment compensation ratio
vbk = 0.1      #von bertelanfy metabolic parameter
vblinf = 95    #von bertelanfy length infinity parameter
vbt0 = 0        #von bertelanfy t0 parameter
alw = 6e-8    # length weight alometry parameter, this came from Carl as (1/28)/(85)^3--so probably a metabolic derivation. 
alwb = 3        #length weight alometry b parm
wmat = alw*(0.5*vblinf)^alwb       #weight at maturity
M = 0.1        #natural mortality
U = 0.2        #baseline harvest rate

#fishery
vsd = 5         #standard deviation of length at 50% vulnerability
q = 0.005       #catchbility, only used if you're using effort (U= 1-exp(-F)), where F=q*effort. This q is just guessed, may need to be 1 or 2 orders of mag larger
sdobs = 0.6     #the standard deviation of the observation error (i.e. mean 0, sd = sdobs), can be considered the coefficient of variation in observation error
sdpro = 0.0001  #the standard devation of the process effor, same as above
#adding effort: U=.25, which is an F of about 0.28. if q=0.005, Effort necesary to cause an F of 0.28 is F/q, or about 56.
#Et = 125     # Run in function, not needed #56 matches a hr of 0.25, but explore how effort changes things--dropping effort to 35 makes a big difference

###Plot Specific Vectors###
Mt=vector(length=numHeat);

### Age-specifc vectors ###
Age = seq(1,amax)                             #Age
TL = vblinf*(1-exp(-vbk*(Age-vbt0)))          #Total length
Wt = (alw*(TL)^alwb)                          #Weight
Surv = (1+vblinf/TL*(exp(vbk)-1))^(-M/vbk)    #Survival in each time step, could have alterantively done: exp(-Madult*linf/base_len)
Lo = vector(length=amax); Lo[1]=1;  for (i in 2:amax){Lo[i]=Lo[i-1]*Surv[i-1]}; #Lo[Amax] = Lo[(Amax-1)]*surv[(Amax-1)]/(1-surv[(Amax-1)])       #survivorship in unfished condtions with special survivorship for terminal age
Fec = ifelse((Wt-wmat)<0,0,Wt-wmat)           #Fecundity

### Calculations from vectors ###
epro = sum(Lo*Fec)                    #eggs-per-recruit unfished conditions
bpro = sum(Wt,Lo)                     #biomass-per-recruit unfished conditions
npro = sum(Lo)                        #numbers-per-recruit unfished conditions
bo = Ro*bpro                          #biomass at unfished condtions
no = Ro*npro                          #numbers at unfished conditions

#recruitment parms and calcs#
bha = CR/epro                         #beverton holt a parm
bhb = (CR-1)/(Ro*epro)                #beverton holt b parm
#r.eq = (bha*eprf-1)/(bhb*eprf)       #equilibrium recruitment, should not be used here if assuming habitat-based recruitment (i.e. reparameterization of a & b based on H)
#yield.eq = U*vbprf*r.eq              #equilibrium yield--standard calc, but not applicable here unless using U and constant recruitment


#Calculations for Standard Output (Otherwise VB is linked to harvested adults only)
plh = 63.5        #length at 50% vulnerability
PO = 1/(1+exp(-(TL-plh)/vsd))                #Productive Length

#Shell stuff
shell_surv = 0.8    #survival rate of dead shell (i.e. some shell remains and is useful, other disintegrates)
#cstar=0             #switch for whether a or b parm (of beverton holt) affected by habitat suitable for recruitment (cstar=1 corresponds to b parm, thus affecting carrying capacity rather than max survival rate). Walters et al 2007 for reference
shell_init = 360.3       #initial shell, guessed by fiddling, but there's a relationship here that I'm not aware of. Can be done with Carl equilibrium approach from Ed's/Carl's age structured model
#to explain, you can turn off fishing by Et=0, then run model and set shell_init to the total shell. BUT see above, this isn't very biologically based right now,
#thus until shell is calculated in a more biologically designed fashion, shell functions as a scalar.
#logistic relationship between habitat for recruitment (H in model below) and shell
sht = 0.25*shell_init             #saying there's a threshold, below which there's not enough shell for good habitat. technically the inflection point
shsd = 5                          #standard deviation, this determines how sharp the sigmoidal-shaped relationship between shell and habitat is. 
#Scrap relationships between shell and available habitat. These will be implemented in model
sh = seq(0,shell_init, by=1)      #sequence of shell, analogous to "shell" (shell[i]) in model.
hab = 1/(1+exp(-(sh-sht)/shsd))   #simple habitat, allowing habitat to go to zero
hab1 = pmax(0.1, 1/(1+exp(-(sh-sht)/shsd)))   #This is a "safety net" function where we say look habitat can't be worse than 0.1 no matter what happens to shell.

restCost = 1725 #Cost to restore shell in a single plot, derived from Lone Cabbage Reef shell restoration cost

#Map Constants (Create a Sequence of Coordinates within the Map, these will not change, only value parameter needs to be changed)
nplts<-1600
moveRow<-sqrt(nplts) #length of one row

cedKey <- readOGR(dsn=path.expand("shapefile"), layer="LC_10_Area") #Imports Cedar Key shape file
ckCrd <- spTransform(cedKey, CRSobj = CRS("+init=epsg:4326")) #Converts shape file coordinate to longitude/latitude
matCrd=expand.grid(x=seq(from=-83.1164,to=-83.06251,length.out=moveRow), #Generates a series of coordinates within range
                   y=seq(from=29.2169,to=29.26528,length.out=moveRow))

cedKey <- readOGR(dsn=path.expand("shapefile"), layer="LC_10_Area") #Imports Cedar Key shape file
ckCrd <- spTransform(cedKey, CRSobj = CRS("+init=epsg:4326")) #Converts shape file coordinate to longitude/latitude
matCrd<-expand.grid(x=seq(from=-83.1164,to=-83.06251,length.out=moveRow), #Generates a series of coordinates within range
                   y=seq(from=29.2169,to=29.26528,length.out=moveRow))

plot(cedKey)

cordData = data.frame(value = seq(1:length(nplts)), x = matCrd$x, y = matCrd$y)
cordSpace = SpatialPixelsDataFrame(cordData[,c('x', 'y')], data = cordData, proj4string = crs(ckCrd))
cordClp <- over(cordSpace[,c("x", "y")], ckCrd)
cordCond <- !is.na(cordClp$Id)
cordSpaceNew <-cordSpace[cordCond,]
numHeat <- length(cordSpaceNew)
cordSdfNew = as.data.frame(cordSpaceNew)
clkCords = data.frame(x=(trunc(cordSdfNew$x*1000)/1000), y=(trunc(cordSdfNew$y*1000)/1000))

#Possible Colors and Titles for Map
harvestPal<-colorNumeric(c("#DD0000", "#FFFF00", "#00DD00", "#000000"), c(0,132), na.color = "transparent")
harvLegTitle<-"Percentage Reef Health"
harvLegCol<-c("#DD0000", "#E43300", "#EB6600", "#F19900", "#F8CC00", "#FFFF00", "#CCF800", "#99F100", "#66EB00", "#33E400", "#00DD00")
harvLegLbl<-c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")

sancPal<-colorNumeric(c("#FFFFFF", "#000000"), c(0,132), na.color = "transparent")
sancLegTitle<-"No-Take Zones"
sancLegCol<-c("#FFFFFF", "#000000")
sancLegLbl<-c("Open", "No-Take")

restPal<-colorNumeric(c("#0000AA", "#A040A0", "#FFBB00", "#FFFFFF"), c(0,132), na.color = "transparent")
restLegTitle<-"Available Shell Cover"
restLegCol<-c("#0000AA", "#200DA8", "#401AA6", "#6026A4", "#8033A2", "#A040A0", "#B35980", "#C67160", "#D98A40", "#ECA220", "#FFBB00")
restLegLbl<-c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")

#Icons used in map
npc <- makeIcon("boat2.png", iconWidth=20, iconHeight=20, iconAnchorX = 10, iconAnchorY = 10) #Add icons
closure <- makeIcon("closed.png", iconWidth=12, iconHeight=12, iconAnchorX = 6, iconAnchorY = 6)
closure2 <- makeIcon("closed2.png", iconWidth=12, iconHeight=12, iconAnchorX = 6, iconAnchorY = 6)
empty <- makeIcon("empty.png", 20, 20, iconAnchorX = 10, iconAnchorY = 10)

Mt<-vector(length=numHeat)
Mt[] = rnorm(numHeat, M, 0.003)