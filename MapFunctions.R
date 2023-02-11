updateMap<-function(popHeat, shellHeat, sancHeat, boatVec, mapType, mapVals, roundCounter, monthsCond, effortCond, bounds, zoomLevel){

  boatIcon<-npc #NPCs will be displayed on all maps
  closedIcon<-closure #Will be changed to empty if we are on the sanctuary map
  
  if(mapType=="Population Map"){
    heatVec<-popHeat
    lblVec<-popHeat
    usedVec<-mapVals$harvestPlots
    pal<-harvestPal
    legPal<-harvLegCol
    legTitle<-harvLegTitle
    legLbl<-harvLegLbl
    
  } else if(mapType=="No-Take Map"){
    heatVec<-sancHeat
    lblVec<-popHeat
    usedVec<-mapVals$noTake
    pal<-sancPal
    legPal<-sancLegCol
    legTitle<-sancLegTitle
    legLbl<-sancLegLbl
    closedIcon<-empty #Makes sanctuary icons invisible on sanctuary map
    
  } else if(mapType=="Shell Restoration Map"){
    heatVec<-shellHeat
    lblVec<-shellHeat
    usedVec<-mapVals$shellRest
    pal<-restPal
    legPal<-restLegCol
    legTitle<-restLegTitle
    legLbl<-restLegLbl
    closedIcon<-closure2
  }
  
  anyOpen=0
  spNew<-cordSpaceNew
  
  for(i in 1:length(heatVec)){
    if(is.na(heatVec[i])){
      heatVec[i]==0
    }
    spNew$value[i]=pmin(heatVec[i],100)
    spNew$lblValue[i]=pmin(lblVec[i],100)
    if(usedVec[i]==1){
      spNew$value[i]=132
    }
    
    if(mapVals$noTake[i]!=1){
      spNew$sanc[i]=0
    }else{
      spNew$sanc[i]=1
      anyOpen=1
    }
  }
  
  if(anyOpen==0){
    closedIcon<-empty
    spSanc<-spNew
  }else{
    spSanc<-spNew[spNew$sanc==1,]
  }
  
  spNPC<-spNew[boatVec==1,]

  if(roundCounter<=2){
    boatIcon=empty
  }
  
  if(monthsCond==0 || effortCond==0){
    boatIcon=empty
  }
  
  r = raster(spNew)
    
  showMap = leaflet() %>% addTiles()  %>%
    addRasterImage(r, colors = pal, opacity = 1) %>%
    addLegend(position="bottomleft", colors = legPal, title = legTitle, labels = legLbl) %>%
    
    fitBounds(lng1=bounds$east, lat1=bounds$north, lng2=bounds$west, lat2=bounds$south)%>%
    
    addMarkers(icon=boatIcon, lng= spNPC$x, lat = spNPC$y, 
               label = round(spNPC$lblValue, digits = 0), labelOptions = labelOptions(textsize = "20px")) %>%
    
    addMarkers(icon=closedIcon, lng=spSanc$x, lat=spSanc$y, 
               label = round(spSanc$lblValue, digits = 0), labelOptions = labelOptions(textsize = "20px")) %>%
    
    addCircleMarkers(lng= spNew$x, lat = spNew$y, label = round(spNew$lblValue, digits = 0),
                     radius = 9, color = pal, fillOpacity = 0, labelOptions = labelOptions(textsize = "20px"))
  
  return(showMap)
}