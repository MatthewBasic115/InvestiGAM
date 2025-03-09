#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
# https://shiny.posit.co/r/components/inputs/select-multiple/

library(shiny)
library(mgcv)            # Fit and interrogate GAMs
library(tidyverse)       # Tidy and flexible data manipulation
library(marginaleffects) # Compute conditional and marginal effects
library(ggplot2)         # Flexible plotting
library(patchwork)       # Combining ggplot objects
library(gratia)
library(DT)              # render data table outputs in Shiny
library(glue)
library(rlang)
source("ui_functions.R")
source("wrapper_helpers.R")
source("math_helpers.R")

# preload the C02 dataset
# load C02 dataset
data(CO2, package = "datasets")

# define custom ggplot2 theme
theme_set(theme_classic(base_size = 12, base_family = 'serif') +
            theme(axis.line.x.bottom = element_line(colour = "black",
                                                    linewidth = 1),
                  axis.line.y.left = element_line(colour = "black",
                                                  linewidth = 1),
                  panel.spacing = unit(0, 'lines'),
                  legend.margin = margin(0, 0, 0, -15)))

# manipulations for modelling
plant <- CO2 |>
  as_tibble() |>
  rename(plant = Plant, type = Type, treatment = Treatment) |>
  mutate(plant = factor(plant, ordered = FALSE))

# fit model
model_1 <- gam(uptake ~ treatment * type + 
                 s(plant, bs = "re") +
                 s(conc, by = treatment, k = 7),
               data = plant, 
               method = "REML", 
               family = Gamma(link = "log"))

# ~~~~~~~~~~~~~~~~~~ SHINY SECTION ~~~~~~~~~~~~~~~~~~~~~~

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    selectInput("dataset", label = "Dataset", choices = ls("package:datasets"), selected="CO2"),
    responseScaleSelector("link_response"),
    
    titlePanel("Spline Splitter"),
    tabsetPanel(
      tabPanel("Build",
          # Build model
          titlePanel("Build GAM"),
          # Note, not currently functional, CO2 is always the base
          selectInput("dataset", label = "Dataset", choices = ls("package:datasets"), selected="CO2"),
          
          selectInput("gam_method", label="Method", choices=getGamMethods()),
          
          selectInput("gam_family", label="Family", choices=getGamFamilies()),
          
          # gam_link is updated once a family is selected
          selectInput("gam_link", label="Link", choices=c()),
          
          # Build Model - User can expressly enter their formula
          textInput("formula", "raw formula", "enter raw formula"),
          
          # button to build the model
          actionButton("build","create"),
          
          #dataTableOutput("userModelSummary"),
          
          titlePanel("Appraisal Plots")
          
          plotOutput("userModelAppraisal")
          
      ), # end tabPanel Build
      tabPanel("Interpret",
               
        titlePanel("Interpret"),
        
        navlistPanel(
          id="Compare",
          ########### PREDICTIONS PANEL ###########################
          tabPanel("Predictions", 
              verbatimTextOutput("Plot Predictions"),
              ##### PLOT PREDICTIONS
              # Select the response scale for the plot predictions
              selectizeInput("plot_pred_cond", "Select Features",
                             # ensure there is a default selection, make lower case for consistency
                             tolower(colnames(CO2)), multiple=TRUE, selected=tolower(colnames(CO2)[1])),
              plotOutput("plot_pred"),
          ), # end tabPanel Predictions
          ######## SLOPES PANEL ##########
          tabPanel("Slopes", 
              # Select variable for the slopes
              selectizeInput("plot_slope_var", "Select Variable",
                             # ensure there is a default selection, make lower case for consistency
                             tolower(colnames(CO2)), multiple=TRUE, selected=tolower(colnames(CO2)[1])),
              # Select condition for the slope plot
              selectizeInput("plot_slope_cond", "Select Features",
                             # ensure there is a default selection, make lower case for consistency
                             tolower(colnames(CO2)), multiple=TRUE, selected=tolower(colnames(CO2)[1])),
              plotOutput("plot_slope"),
          ),
          ########## SIMULATE PANEL ##########
          tabPanel("Simulate", 
              # SIMULATE
              # Select condition for the slope plot
              selectizeInput("simulated_feature_select", "Select Variable to Simulate",
                             # ensure there is a default selection, make lower case for consistency
                            tolower(colnames(CO2)), multiple=FALSE, selected=tolower(colnames(CO2)[4])),
              
             selectizeInput("simulated_smooth_select", "Select Smooth for Variable",
                             smooths(model_1), multiple=FALSE),
              
              plotOutput("basis_func"),
          ),
          ##### COMPARISONS PANEL ##########
          tabPanel("Comparisons",
              selectizeInput("comp_seq", "Select var for sequence",
                             tolower(colnames(CO2)),multiple=FALSE,selected=tolower(colnames(CO2)[4])),
              
              selectizeInput("comp_interest", "Select var to compare",
                             tolower(colnames(CO2)),multiple=FALSE,selected=tolower(colnames(CO2)[4])),
              
              selectizeInput("comp_by", "by",
                             tolower(colnames(CO2)),multiple=TRUE,selected=tolower(colnames(CO2)[4])),
              
              plotOutput("comparisons")
          )
        ), # end tabPanel Interpret
        
      ),
      tabPanel("Design")
    ),
    #selectInput("feature", label = "Feature", choices = smooths(model_1)),
    #plotOutput("plot"),
    #tableOutput("table")
)

# Render functions is designed to produce a particular type of output.
# Often paired with an Output function. e.g. renderPrint will call verbatimtextoutput
server <- function(input, output, session) {
  
  # reactive expressions act like functions however will only run the first time called
  # then the output will be cached and only updated if required
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })
  
  # Map the smooth name to it's ID
  feature_mappings <- reactive({
    mapped_idx = which_smooths(model_1, input$feature)
  })
  
  output$plot <- renderPlot({
    plot(model_1, select=feature_mappings(), shade=TRUE)
    abline(h=0, lty='dashed')
  })
  
  gam_formula <- reactive({
      typed_word <- input$formula 
  })
  
  ############### Build #################
  userModel <- reactive({
    # user model
    m <- gam2(formula=as.formula(input$formula),
                   data = plant, 
                   method = input$gam_method, 
                   #family = Gamma(link = "log"))
                   family = buildFamilyValue(input$gam_family,input$gam_link))
  }) |> bindEvent(input$build)
  
  output$userModelAppraisal <- renderPlot({
    appraise(userModel(), point_col = "steelblue", point_alpha = 0.4)
  }) |> bindEvent(userModel())
  
  # Update the options for link functions based on the distribution family selected.
  observeEvent(input$gam_family,{
    links <- getValidLinkFunction(input$gam_family)
    updateSelectInput(session,"gam_link",choices=links)
  })
  
  #output$userModelSummary <- renderTable({
  #  summary(userModel())
  #})
  
    ######### PART 1 - PLOT PREDICTIONS
  output$plot_pred <- renderPlot({
    # ensure that feature selections are lower case
    plot_predictions(model_1,condition=c({input$plot_pred_cond}),type=input$link_response)+
      labs(y=str_c("Linear predictor (",input$pred_link_response," scale)"), title=paste("Average smooth effect of ", {input$plot_pred_cond}), subtitle="aggregated across treatment and types")
  })
  
  ######### PART 2 - PLOT SLOPES
  
  #Input for conditions
  output$plot_slope <- renderPlot({
    plot_slopes(model_1, variables=c({input$plot_slope_var}), condition=c({input$plot_slope_cond}), type=input$link_response) +
      labs(y = "1st Derivative of linear predictor", title = "Conditioal slopes of the concentration effect", subtitle="Per treatment, per type")
  })
  
  
  ######## PART 3 - SIMULATING FROM fitted GAM MODELS
  
  # Map the smooth name to it's ID for simulation
  feature_mappings_simu <- reactive({
    mapped_idx = which_smooths(model_1, input$simulated_smooth_select)
  })
  
  # plot the basis functions
  output$basis_func <- renderPlot({
    selection <- select(plant, {input$simulated_feature_select})
    # check to see if numeric or factor which will impact sequence generation
    selection_dtype <- lapply(plant,class)[[input$simulated_feature_select]]
    
    beta <- coef(model_1)
    
    # Sequences for numerics and factors must be treated separetly
    if (selection_dtype=="factor"){
      seq <- unique(selection)
      seq <- lapply(seq, as.character)[[1]]
    } else { #numeric
      seq <- seq(from=min(selection),max(selection), length.out=500)
    }
    
    # Call datagrid wrapper for easier function calls
    dg <- datagrid2("{input$simulated_feature_select}" := seq,model=model_1)
    # get predictions for the datagrid
    newXp <- predict(model_1, type='lpmatrix',newdata=dg)
    
    # TODO: Find out smooth selection for a given var
    
    # Find which coefficients belong to the first smooth of conc
    # from the first para of the 2nd smooth to the last, get the coefficients
    conc_coefs <- model_1$smooth[[feature_mappings_simu()]]$first.para:model_1$smooth[[feature_mappings_simu()]]$last.para
    
    newXp_adj <- matrix(0, ncol = NCOL(newXp), nrow = NROW(newXp))
    newXp_adj[,conc_coefs] <- newXp[, conc_coefs]
    # do.call do.call(what, args, quote = FALSE, envir = parent.frame()) 
    # constructs and executes a function
    # do - rowbind with argument of a sequence generation for conc_coef
    plot_dat = do.call(rbind,lapply(seq_along(conc_coefs), function(basis){
      #paste0 concatenate vectors after converting to char
      df2(basis_func=paste('bs',basis),"{input$simulated_feature_select}":=seq
          , value=newXp_adj[, conc_coefs[basis]] * beta[conc_coefs[basis]])
    }))
    
    ftcol <- eval_tidy({input$simulated_feature_select})
    # Factor do a different 
    if(selection_dtype=="factor"){
      ggplot(plot_dat,aes(x=basis_func, y=value, fill=.data[[ftcol]])) +
        geom_bar(stat="identity", position=position_dodge())+
        labs(y='Basis function value')
    } else {
      #Eval the input and ppass it in using the .data pronoun
      # This is required as the user input is a char vec, not the data object itself
      ggplot(plot_dat,aes(x=.data[[ftcol]], y=value, col=basis_func)) +
        geom_line(linewidth=0.75)+theme(legend.position='none') +
        labs(y='Basis function value')
    }
  })
  
  # COMPARISONS
  
  output$comparisons <- renderPlot({
    selection <- select(plant, {input$comp_seq})
    seq <- seq(from=min(selection),max(selection), length.out=500)
    plot_comparisons(model_1, newdata=datagrid2("{input$comp_seq}":=seq,treatment=unique,type=unique),
     variables={input$comp_interest},
     by=c({input$comp_seq}, "type"),
     type={input$link_response}) +
      geom_hline(yintercept=0,linetype='dashed') +
      labs(y="Estimated difference",
           title="Difference between treatment levels",
           subtitle="Chilled - nonchilled, per type")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
