library(shiny)
library('leaflet')
library(raster)
library('sf')
library(rgdal)
source('PopulationFunctions.R')
source('MapFunctions.R')
source('global.R')

ui <- fluidPage(

    titlePanel("Cedar Key Oyster Management Game"),

    sidebarLayout(
        sidebarPanel(
            selectInput(inputId="mapType", label=mapLbl, choices=mapChoices, multiple=F),
            numericInput(inputId="months", label=HTML(effortLbl), value=9, min=0, max=12, step=1),
            numericInput(inputId="effort", label=amountLbl, value=20, min=0, max=100, step=1),
            numericInput(inputId="size", label=sizeLbl, value=3, min=1, max=6, step=0.5),
            numericInput(inputId="boats", label=boatsLbl, value=100, min=10, max=300, step=10),
            #checkboxInput(inputId="shell", label=shellLbl),
            actionButton(inputId="update", label="Update")
        ),

        mainPanel(textOutput("scoreExplained"),
                  textOutput("scoreTurn"),
                  textOutput("scoreFinal"),
                  textOutput("turns"),
                  leafletOutput("map"),
                  plotOutput("history"),
                  #textOutput("licenses"), #Removed because the number of maximum licenses is shown in the input
                  textOutput("cost"),
                  textOutput("sacks"),
                  #textOutput("size"), #Can be added back later, oystermen seem not to care about this
                  textOutput("avgSacks"),
                  #textOutput("avgSize"),
                  textOutput("health"),
                  textOutput("test") #To be removed later
        )
    )
)

server <- function(input, output) {
    showModal(modalDialog(
        title = "Welcome to the Cedar Key Oyster Management Game",
        introVid))
    
    mapVecs <- reactiveValues() #All vectors used for map blackouts will be stored here
    mapVecs$harvestPlots<-vector(length=numHeat)
    mapVecs$noTake<-vector(length=numHeat)
    mapVecs$shellRest<-vector(length=numHeat)
    mapVecs$closedVec<-vector(length=numHeat)
    mapVecs$boatTotals<-vector(length=numHeat)
    
    myReactives <- reactiveValues() #Initialize all variables to be carried through between rounds
    myReactives$roundCounter = 1
    myReactives$eggs = vector(length=numHeat)
    myReactives$N=vector(length=numHeat)
    myReactives$B=vector(length=numHeat) 
    myReactives$VB=vector(length=numHeat)
    myReactives$PB=vector(length=numHeat)
    myReactives$yield=NULL
    myReactives$shell=vector(length=numHeat)
    myReactives$shell_add=vector(length=numHeat)
    myReactives$eff=vector(length=numHeat)
    myReactives$hr=vector(length=numHeat)
    myReactives$ahab=vector(length=numHeat) 
    myReactives$bhab=vector(length=numHeat)
    myReactives$H=vector(length=numHeat)
    myReactives$nage = matrix(nrow=amax, ncol=numHeat)
    myReactives$shownWarning=FALSE
    myReactives$totalRestCost = 0
    myReactives$roundCounter = 1
    myReactives$turnScore=vector(length=totalTurns)
    myReactives$averageSacks=vector(length=totalTurns)
    myReactives$tookTurn=FALSE #used to indicate whether a turn has been taken more recently than the map has changed
    
    observeEvent(input$map_marker_click, {
        click<-input$map_marker_click
        if(is.null(click))
            return()
        
        xClk=trunc(click$lng*1000)/1000
        yClk=trunc(click$lat*1000)/1000
        
        xCor = which(clkCords$x == xClk)
        yCor = which(clkCords$y == yClk)
        myPlot = intersect(xCor, yCor)
        
        if(input$mapType=="Population Map"){
            return() #This functionality was taken away because the player no longer harvests independently, instead acting only as a manager.
            if(mapVecs$noTake[myPlot]==1){
                #showModal(modalDialog(
                #    title = "You Cannot Harvest a No-Take Zone",
                #    "The area you have chosen to harvest is marked as an oyster sanctuary. Please select another area, or unmark this spot as an oyster sanctuary on the oyster Sanctuary Map."))
                return()
            }
            if(mapVecs$harvestPlots[myPlot]==1){
                mapVecs$harvestPlots[myPlot]=0
            } else{
                mapVecs$harvestPlots[myPlot]=1
            }
        } else if(input$mapType=="No-Take Map"){
            if(mapVecs$harvestPlots[myPlot]==1){
                #showModal(modalDialog(
                #    title = "You Cannot Harvest a No-Take Zone",
                #    "The area you have chosen is marked as a preferred harvest zone. You cannot make a preferred harvest zone an oyster sanctuary. Please chose another area or unmark this plot on the Population Map."))
                return()
            }
            if(mapVecs$noTake[myPlot]==1){
                mapVecs$noTake[myPlot]=0
            } else{
                mapVecs$noTake[myPlot]=1
            }
        } else if(input$mapType=="Shell Restoration Map"){
            if(mapVecs$shellRest[myPlot]==1){
                mapVecs$shellRest[myPlot]=0
            } else{
                if(permShell==FALSE && myReactives$shownWarning==FALSE){
                    #showModal(modalDialog(
                    #    title = "Remember to turn on shell restoration using the side bar.",
                    #    "You have selected an area for shell restoration, but you have not enabled shell restoration on the side bar. If you wish to implement shell restoration in any plot, please check the box on the sidebar to enable shell restoration."))
                    myReactives$shownWarning=TRUE
                }
                mapVecs$shellRest[myPlot]=1
            }
        }
        
    })
    
    observeEvent(input$update, {
        if(myReactives$roundCounter>=52){
            showModal(modalDialog(
                title="You have reached the end of the game.",
                "Thank you for playing. You can check your final score on the main page."
            ))
            return()
        }
        prevVal<-myReactives
        spatialVec<-mapVecs
        newVals<-updateMain(cstar=0, effort=input$effort, monthsOpen=input$months, size_lim=input$size, numBoats=input$boats, habitat="threshSafe", timesteps=12,
                            rest_flag=permShell, shell_rest=0.05*shell_init, min_H=0.05, previousValues = prevVal, spatialValues=spatialVec)
        myReactives$nage <- newVals$nage
        myReactives$eggs = newVals$eggs
        myReactives$N=newVals$N
        myReactives$B= newVals$B
        myReactives$VB= newVals$VB
        myReactives$PB = newVals$PB
        myReactives$yield = newVals$yield
        myReactives$shell=newVals$shell
        myReactives$shell_add=newVals$shell_add
        myReactives$eff=newVals$eff
        myReactives$hr=newVals$hr
        myReactives$ahab= newVals$ahab
        myReactives$bhab=newVals$bhab
        myReactives$H=newVals$H
        myReactives$roundCounter = newVals$roundCounter
        mapVecs$boatTotals = newVals$boatTotals
        
        myReactives$totalRestCost = myReactives$totalRestCost + (sum(mapVecs$shellRest)*restCost)
        
        myReactives$shownWarning=FALSE
        
        if(input$months==0 || input$effort==0){
            myReactives$yield=0
        }
        
        if(myReactives$roundCounter>2){
            myReactives$turnScore[myReactives$roundCounter-2]=round((myReactives$yield*10)*mean((myReactives$PB/14)),0)
            myReactives$averageSacks[myReactives$roundCounter-2]=round(myReactives$yield,0)
        }
        
        myReactives$tookTurn=TRUE
    })
    
    observeEvent(input$shell, {
        if(input$shell==TRUE){
            anyClicked=0
            for(p in 1:numHeat){
                if(mapVecs$shellRest[p]==1){
                    anyClicked=anyClicked+1
                }
            }
            if(anyClicked==0){
                #showModal(modalDialog(
                #    title = "Remember to select plots for restoration using the Shell Restoration map",
                #    "You have enabled shell restoration, but you have not selected any areas for shell restoration on the Shell Restoration map. If you wish to implement shell restoration, please indicated which plots you would like to restore in the Shell Restoration map."))
            }
        }
    })
    
    output$map<-renderLeaflet({
        if(myReactives$roundCounter==1){
            prevVal<-myReactives
            spatialVec<-mapVecs
            newVals<-updateMain(cstar=0, effort=input$effort, monthsOpen=input$months, size_lim=input$size, numBoats=input$boats, habitat="threshSafe", timesteps=12,
                                rest_flag=permShell, shell_rest=0.05*shell_init, min_H=0.05, previousValues = prevVal, spatialValues=spatialVec)
            myReactives$nage <- newVals$nage
            myReactives$eggs = newVals$eggs
            myReactives$N=newVals$N
            myReactives$B= newVals$B
            myReactives$VB= newVals$VB
            myReactives$PB = newVals$PB
            myReactives$yield = newVals$yield
            myReactives$shell=newVals$shell
            myReactives$shell_add=newVals$shell_add
            myReactives$eff=newVals$eff
            myReactives$hr=newVals$hr
            myReactives$ahab= newVals$ahab
            myReactives$bhab=newVals$bhab
            myReactives$H=newVals$H
            myReactives$roundCounter = newVals$roundCounter
            mapVecs$boatTotals = newVals$boatTotals
            
            if(input$months==0 || input$effort==0){
                myReactives$yield=0
            }
            
            if(myReactives$roundCounter>2){
                myReactives$turnScore[roundCounter-2]=round((myReactives$yield*10)*mean((myReactives$PB/14)),0)
                myReactives$averageSacks[myReactives$roundCounter-2]=round(myReactives$yield,0)
            }
            
            zoomLevel=14
            northBound<- 29.260
            southBound<- 29.220
            eastBound<- (-83.085)
            westBound<- (-83.090)
            
            bounds<-list (north=northBound, south=southBound , east=eastBound , west=westBound)
            
        }else{
            bounds<-isolate(input$map_bounds)
            zoomLevel<-isolate(input$map_zoom)
        }
        
        if(is.null(zoomLevel)){
            zoomLevel=14
            bounds$north<- 29.260
            bounds$south<- 29.220
            bounds$east<- (-83.085)
            bounds$west<- (-83.090)
        }
        
        monthsCond<-isolate(input$months)
        effortCond<-isolate(input$effort)
        
        mapVals<-mapVecs
        popHeat<-round((myReactives$PB*100)/14, 0)
        shellHeat<-myReactives$shell
        boatVec<-mapVecs$boatTotals
        roundCounter<-myReactives$roundCounter
        for(h in 1:numHeat){
            if(shellHeat[h]<100){
                shellHeat[h]=100
            }
        }
        shellHeat<-round(((shellHeat-100)*100)/680, 0)
        sancHeat<-mapVecs$closedVec
        
        myMap<-updateMap(popHeat, shellHeat, sancHeat, boatVec, input$mapType, mapVals, roundCounter, monthsCond, effortCond, bounds, zoomLevel)
    })
    
    output$scoreExplained <- renderText({
      "Your score is determined by the average health of the reef and by the average number of bushels harvested by oystermen. Increasing the number of live oysters and increasing harvest will improve your score."  
    })
    output$scoreTurn <- renderText({
        if(myReactives$roundCounter<=2){
            c("Turn Score:",0)
        }else{
            c("Turn Score:",round((myReactives$yield*10)*mean((myReactives$PB/14)),0))
        }
    })
    output$scoreFinal <- renderText({
        if(myReactives$roundCounter<=2){
            c("Cumulative Score:",0)
        }else if(myReactives$roundCounter>=52){
            c("Final Score:",sum(myReactives$turnScore))
        }else{
            c("Cumulative Score:",sum(myReactives$turnScore))
        }
    })
    output$turns <- renderText({
        c("Turns Remaining:", totalTurns-(myReactives$roundCounter-1))
    })
    output$history <- renderPlot({
        if(myReactives$roundCounter>3){
            x <- c(1:(myReactives$roundCounter-2))
            y <- myReactives$turnScore[1:(myReactives$roundCounter-2)]
            upperlim<-pmax((1.5*max(y)),60)
            plot(x=x, y=y, type = "l", lwd=2, xlab = "Turns Since Game Start", ylab = "Score Added Each Turn",
                 main="Turn Score Over Time", xlim = c(0,50), ylim = c(0,upperlim))
        }
    })
    output$cost <- renderText({
        if(myReactives$roundCounter<=2){
            c("Total amount spent on restoration: $",0)
        }else{
            c("Total amount spent on restoration: $",myReactives$totalRestCost)
        }
    })
    output$sacks <- renderText({
        if(myReactives$roundCounter<=2){
            c("Average yield per oysterman this turn:",0,"bushels per day")
        }else{
            c("Average yield per oysterman this turn:",round(myReactives$yield,0),"bushels per day")
        }
    })
    output$avgSacks <- renderText({
        if(myReactives$roundCounter<=2){
            c("Average yield per oysterman since Game Start:",0, "bushels per day")
        }else{
            c("Average yield per oysterman since Game Start:",round(mean(myReactives$averageSacks[1:(myReactives$roundCounter-2)]),2), "bushels per day")
        }
    })
    output$health <- renderText({
        healthNum<-pmin(round((myReactives$PB*100)/14, 0),100)
        if(myReactives$roundCounter<=2){
            c("Average reef health:",0, "%")
        }else{
            c("Average reef health:",round(mean(healthNum),2), "%")
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

