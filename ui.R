library(plotly)
library(shiny)
library(ggplot2)

################################################################################
########### Samplemeta #########################################################
################################################################################


tab_about <- fluidPage(
  p("This demo was originally developed by ", a("Johan Henriksson", href="http://www.henlab.org")),
  p("Elastic net fit is done using ", a("glmnet", href="https://glmnet.stanford.edu/articles/glmnet.html")),
  p("Licensed under 2-clause BSD license, https://opensource.org/license/bsd-2-clause/")
)


tab_datatable <- fluidPage(
  p("Training points:"),
  tableOutput(outputId = "plotDataTable")
)

tab_scatter <- fluidPage(

  plotOutput(outputId = "plotScatter", height = "400px")
)


tab_cvfit <- fluidPage(
  
  plotOutput(outputId = "plotCVfit", height = "400px")
)

################################################################################
########### Total page #########################################################
################################################################################

#https://stackoverflow.com/questions/72040479/how-to-position-label-beside-slider-in-r-shiny

ui <- fluidPage(
  tags$style(HTML(
    "
    .label-left .form-group {
      display: flex;              /* Use flexbox for positioning children */
      flex-direction: row;        /* Place children on a row (default) */
      width: 100%;                /* Set width for container */
      max-width: 400px;
    }

    .label-left label {
      margin-right: 2rem;         /* Add spacing between label and slider */
      align-self: center;         /* Vertical align in center of row */
      text-align: right;
      flex-basis: 100px;          /* Target width for label */
    }

    .label-left .irs {
      flex-basis: 300px;          /* Target width for slider */
    }
    "
  )),
  
  titlePanel("Demo of linear model fitting, optional penalization"),

  sidebarLayout(
    sidebarPanel(
      
      selectInput(
        inputId = "input_ds",
        label = "Dataset:",
        selectize = FALSE,
        multiple = FALSE,
        choices = names(available_datasets),
        selected = "trivial.csv"
      ),


      div(class = "label-left",
          
          sliderInput(
            inputId = "num_training_point",
            label = "Use # training points:",
            min=0,
            max=1,
            value=1
          ),
          
          sliderInput(
            inputId = "random_seed",
            label = "Random seed:",
            min=0,
            max=10,
            step = 1,
            value=1
          ),
      ),
      
      selectInput(
        inputId = "basefunction",
        label = "Base functions:",
        choices = c("Polynomial","Sinusoidal","Trapezoid","ReLU"),
        selected = "Polynomial"
      ),
        
      div(class = "label-left",
          
          sliderInput(
            inputId = "num_bases",
            label = "Use # base functions:",
            min=1,
            max=10,
            step = 1,
            value=1
          ),
          
          checkboxInput("penalty_enable", "Penalize", FALSE),
          
          sliderInput(
            inputId = "penalty_loglambda",
            label = "Penalty log(lambda):",
            min=-15,
            max=0,
            step = 0.1,
            value=-15
          ),
          
          sliderInput(
            inputId = "penalty_alpha",
            label = "Penalty alpha:",
            min=0,
            max=1,
            step = 0.01,
            value=1
          ),
          
          img(src='glmnet_equation.png', width="100%", align = "right"),
          p(HTML('&nbsp;')),
      )

    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Scatter plot", tab_scatter),
                  tabPanel("CV fit", tab_cvfit),
                  tabPanel("Data table", tab_datatable),
                  tabPanel("About", tab_about)
      )
    )
  )
  
)



