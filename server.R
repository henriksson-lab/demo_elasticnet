library(plotly)
library(Cairo)
library(glmnet)
options(shiny.usecairo=T)


if(FALSE){
  install.packages("matlib")
}



if(FALSE){
  #To run this app
  library(shiny)
  runApp(".")
}


server <- function(input, output, session) {


  ##############################################################################
  ########### General functions ################################################
  ##############################################################################
  
  
  getDataTable <- reactive({    
    current_ds <- input$input_ds
    available_datasets[[current_ds]]
  })
  
  getTrainingIndex <- reactive({    
    thedat <- getDataTable()
    set.seed(input$random_seed)
    usepoint <- sample(1:nrow(thedat), input$num_training_point)
    usepoint
  })
  
  getTestIndex <- reactive({
    setdiff(1:nrow(getDataTable()), getTrainingIndex())
  })
  
  
  getTrainingPoints <- reactive({
    getDataTable()[getTrainingIndex(),,drop=FALSE]
  })
  
  getTestPoints <- reactive({
    getDataTable()[getTestIndex(),,drop=FALSE]
  })
  

  # Note that this returns for both train and test set; subset later when fitting  
  getBases <- reactive({
    thedat <- getDataTable()

    output <- matrix(NA, nrow=nrow(thedat), ncol=input$num_bases)
    
    if(input$basefunction=="Polynomial"){
      for(i in 1:input$num_bases){
        output[,i] <- thedat$x ** i
      }
    } else if(input$basefunction=="Sinusoidal"){
      
      
    } else if(input$basefunction=="Trapezoid"){  #rect too
      
      
      
      
    } else {
      stop(paste("Unhandled base function",input$basefunction))
    }
    colnames(output) <- paste0("b",1:ncol(output))
    
    as.data.frame(output)
  })
  
  
  
  ######################
  # Find 
  getSolutionCV <- reactive({
    ##### Load the data
    thedat <- getDataTable()
    bases <- getBases()
    
    cvfit <- cv.glmnet(
      as.matrix(bases), 
      thedat$y,
      intercept = FALSE
    )
    cvfit
  })
  
  
  getSolution <- reactive({

    ##### Load the data
    thedat <- getDataTable()

    ##### Solve; Predict for all points, including test set ---- basic LM
    bases <- getBases()
    if(!input$penalty_enable){
      
      ## Only works if not penalized. but handles ncol(x)=1 case
      tosolve <- bases[getTrainingIndex(),,drop=FALSE]
      tosolve$y <- thedat$y[getTrainingIndex()]
      themod <- lm(data=tosolve, formula="y~.-1")
      pred.y <- predict(themod,bases)
      coeff_val <- coef(themod)

    } else {
      
      ##### Solve; Predict for all points, including test set ---- elastic net LM
      fit.glm <- glmnet(
        as.matrix(bases[getTrainingIndex(),,drop=FALSE]), 
        thedat$y[getTrainingIndex()],
        intercept = FALSE, 
        alpha = input$penalty_alpha,  #from 0 to 1
        lambda = exp(input$penalty_loglambda)  #can be a full list  ##### manual suggests not giving lambda here, but ask for it later
      )
      
      pred.y <- predict(fit.glm,as.matrix(bases))
      coeff_val <- coef(fit.glm)[,1][-1] #first is intercept. do not include
    }
    

    #####
    # https://glmnet.stanford.edu/articles/glmnet.html
    
    ##### Return stuff
    thefit <- list(
      coeff=colnames(bases),#tosolve$coefficients,
      coeff_val=coeff_val,
      
      x=thedat$x,
      pred.y=pred.y,
      
      bases=bases,
      index.training=getTrainingIndex(),
      index.test=getTestIndex()
    )

    thefit
  })
  
# sol<- getSolution()
  
  
  ##############################################################################
  ########### Callbacks - dataset ##############################################
  ##############################################################################
  
  observeEvent(c(input$input_ds),{
    thedat <- getDataTable()

    ######### Side bar
    updateSliderInput(session, "num_training_point", min=0, max=nrow(thedat), value = nrow(thedat), step = 1)


  })
  
  
  observeEvent(c(input$input_ds, input$num_training_point, input$random_seed, input$basefunction, input$num_bases),{
    
    ### Update solution here, then depend on solution to update views
    
  })

  
  ##############################################################################
  ########### Data table tab ###################################################
  ##############################################################################
  
  output$plotDataTable <- renderTable(getTrainingPoints())
  
  
  ##############################################################################
  ########### Scatter plot tab #################################################
  ##############################################################################

  output$plotScatter <- renderPlot({

    thedat_train <- getTrainingPoints()
    thedat_test <- getTestPoints()
    
    
    
    sol <- getSolution()

    p1 <- ggplot(thedat_train,aes(x,y)) + 
      geom_point() +
      geom_line(data=data.frame(x=sol$x[sol$index.training],y=sol$pred.y[sol$index.training]),mapping=aes(x,y)) +
      ggtitle("Training points")
    
    p2 <- ggplot(thedat_test,aes(x,y)) + 
      geom_point() +
      geom_line(data=data.frame(x=sol$x[sol$index.test],y=sol$pred.y[sol$index.test]),mapping=aes(x,y)) +
      ggtitle("Test points")
    
    ### TODO show all bases, scaled
    ### TODO show all bases, unscaled
    
    
    egg::ggarrange(p1,p2)
  })

    
  
  
  ##############################################################################
  ########### CV fit plot tab ##################################################
  ##############################################################################
  
  output$plotCVfit <- renderPlot({
    
    
    cvfit <- getSolutionCV()
    
    
    #coef(fit.glm, s = 0.1)
    #plot(cvfit) #error vs lambda     
    #cvfit$lambda.min
    #coef(cvfit, s = "lambda.min")
    #predict(cvfit, newx = x[1:5,], s = "lambda.min")
    
    plot(cvfit)
    
#    p1 <- ggplot(thedat_train,aes(x,y)) + 
#      geom_point() +
#      geom_line(data=data.frame(x=sol$x[sol$index.training],y=sol$pred.y[sol$index.training]),mapping=aes(x,y)) +
#      ggtitle("Training points")
#    p1
    #egg::ggarrange(p1,p2)
  })
  
  

}



