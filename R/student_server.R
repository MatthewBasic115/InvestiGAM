
#
#' Title
#'
#' @param id ID to be used for server namespace
#' @param page_len number of pages in the student journey wizard
#'
#' @returns module session holding objects and logic for teach section
#' @export
#'
#' @examples 
#' # Load a student server with ID with ID "student" and 5 pages for the wizard.
#' # Do not test as must be run in a Shiny session.
#' \donttest{
#' if (interactive()) {
#'   studentServer("student", 5)
#' }
#' }
studentServer <- function(id){
  # Get the engine dataset from Gamair package
  engine <- get("engine", envir = asNamespace("InvestiGAM"))
  
  portal_data <- get("portal_data", envir = asNamespace("InvestiGAM"))
  
  # Model for student journey provided by Clark (private correspondence)
  model_1 <- mgcv::gam(
    captures ~ 
      series + 
      s(time, by = series, k = 6) +
      s(ndvi_ma12, k = 4) +
      s(ndvi_ma12, series, k = 4, bs = 'sz') +
      s(mintemp, k = 4) +
      s(mintemp, series, k = 4, bs = 'sz'),
    family = poisson,
    data = portal_data
  )
  
  model_2 <- mgcv::gam(
    captures ~ 
      series + 
      s(time, by = series, k = 12) +
      s(ndvi_ma12, k = 4) +
      s(ndvi_ma12, series, k = 4, bs = 'sz') +
      s(mintemp, k = 4) +
      s(mintemp, series, k = 4, bs = 'sz'),
    family = poisson,
    data = portal_data
  )
  
  model_3 <- mgcv::gam(
    captures ~ 
      series + 
      s(time, by = series, k = 20) +
      s(ndvi_ma12, k = 4) +
      s(ndvi_ma12, series, k = 4, bs = 'sz') +
      s(mintemp, k = 4) +
      s(mintemp, series, k = 4, bs = 'sz'),
    family = poisson,
    data = portal_data
  )
  
  shiny::moduleServer(id, function(input,output,session){
    
    ###### Student Journey ######
    
    # Render Datatable
    output$example_data<- DT::renderDataTable({
      DT::datatable(portal_data)
    })
    
    ###### Clark (2025) Code #######
    
    # Introductory Plots for Model
    # plot the basis functions
    output$exampleCountHistogram <- renderPlot({
      ggplot(portal_data,
             aes(x = captures)) +
        geom_histogram(col = 'white') +
        theme_classic()
    })
    
    # Introductory Plots for Model
    # plot the basis functions
    
    output$exampleTimeSeries <- renderPlot({
      ggplot(portal_data,
             aes(x = time,
                 y = captures)) +
        geom_line() +
        geom_point() +
        facet_wrap(~series) +
        theme_bw()
    })
    
    
    output$exampleTempnvdi <- renderPlot({
      portal_data %>% 
        dplyr::filter(series == 'DM') %>%
        ggplot(aes(x = mintemp, y = log(captures))) +
        geom_point() +
        geom_smooth(method = "gam", formula = y ~ s(x, k = 10),
                    col = 'darkred', fill = "#A25050") +
        labs(title = 'DM',
             y = "log(captures)", 
             x = 'Minimum temperature') +
        theme_classic() +
        
        portal_data %>% 
        dplyr::filter(series == 'DM') %>%
        ggplot(aes(x = ndvi_ma12, y = log(captures))) +
        geom_point() +
        geom_smooth(method = "gam", formula = y ~ s(x, k = 10),
                    col = 'darkred', fill = "#A25050") +
        labs(y = NULL, 
             x = 'NDVI moving average') +
        theme_classic()
    })
    
    ######## END Clark (2025) Code ########
    
    ###### Gam.check() and summary section ######
      
    # Model 1 
    output$model1Summary <- shiny::renderPrint({
      summary(model_1)
    })
    
    output$model1GamCheck <- shiny::renderPrint({
      mgcv::gam.check(model_1)
    })
    
    output$model1Appraisal <- shiny::renderPlot({
      gratia::appraise(model_1, point_col = "steelblue", point_alpha = 0.4, method="simulate")
    })
    
    # Model 2
    output$model2Summary <- shiny::renderPrint({
      summary(model_2)
    })
    
    output$model2GamCheck <- shiny::renderPrint({
      mgcv::gam.check(model_2)
    })
    
    output$model2Appraisal <- shiny::renderPlot({
      gratia::appraise(model_2, point_col = "steelblue", point_alpha = 0.4, method="simulate")
    })
    
    # Model 3
    output$model3Summary <- shiny::renderPrint({
      summary(model_3)
    })
    
    output$model3GamCheck <- shiny::renderPrint({
      mgcv::gam.check(model_3)
    })
    
    output$model3Appraisal <- shiny::renderPlot({
      gratia::appraise(model_3, point_col = "steelblue", point_alpha = 0.4, method="simulate")
    })
    
    
    #### Interpret Section ####
    
    ######## PLOT BASIC SMOOTHS AND BASIS FUNCTIONS ############
    
    # plot the basis functions
    output$basis_func <- renderPlot({
      smidx <- which_smooths(model_2, input$simulated_smooth_select)
      gratia::draw(basis(model_2, select=smidx))
    })
    
    # output$plot_gam_condeff <- renderPlot({
    #   conditional_effects.gam(model_2, type=input$link_response)
    # })
    
    output$plot_gam_parteff <- renderPlot({
      smidx <- which_smooths(model_2, input$interpret_smooth_select)
      draw(model_2, select=smidx)
    })
    
    ###### Plot others #####
    
    # Predictions
    output$plot_pred <- renderPlot({
      # ensure that feature selections are lower case
      getPlotPredictions(model_2,c({input$plot_pred_cond}),
                         "response", FALSE, c({input$plot_pred_by}))
    })
    
    #Slope
    output$plot_slope <- renderPlot({
      # Need to refactor this.
      getPlotSlopes(model_2,c({input$plot_slope_cond_var}), c({input$plot_slope_cond_cond}), c({input$plot_slope_by_by}))
    })
    
    output$plot_comp <- renderPlot({
      getStudentPlotComparisons(model_2,c({input$comp_var}),c({input$comp_cond}),c({input$comp_by}), input$comp_opt)
    })
    # Turn page code
    
    # Functions sourced from Shiny Book to allow for page changes for the Wizard.
    changePage <- function(from, to) {
      shiny::observeEvent(input[[paste0("go_", from, "_", to)]], {
        shiny::updateTabsetPanel(session, "wizard", selected = paste0("page_", to))
      })  
    }
    ids <- seq_len(10)
    lapply(ids[-1], function(i) changePage(i, i - 1))
    lapply(ids[-10], function(i) changePage(i, i + 1))
    
    # End Shiny page change functions
    
    ###### LEARN SECTION #########
    
    # Code in this section is adapted from Wood (2017, p.164-165) to allow interaction with the number of knots.
    # Generate the graph from Wood (2017, p.164)
    output$plot_knot <- shiny::renderPlot({
      # size data
      ## generate 6 evenly spaced knots
      sj <- seq(min(engine$size),max(engine$size),length=input$teach_nknots)
      # get the model matrix
      X <- tf.X(engine$size,sj)
      # fit the model, no intercept
      b <- lm(engine$wear ~ X - 1)
      # prediction data
      s <- seq(min(engine$size), max(engine$size), length=200)
      # prediction matrix
      Xp <- tf.X(s,sj)
      # plot data
      plot(engine$size,engine$wear)
      # overlay estimated f
      lines(s, Xp %*% coef(b))
    })
  })
}

# Code in this section is directly sourced from Wood (2017, p.164-165).
# Please see Reference list for full citation.

# Write an R function defining b_j(x)
tf <- function(x,xj,j) {
  ## generate the jth tent function from set defined by knots x_j
  dj <- xj*0
  dj[j] <- 1
  # returns a list of points which linearly interpolate given data points
  approx(xj,dj,x)$y
}

tf.X <- function(x,xj) {
  ## tent function basis matrix given data x
  ## and knot sequence x_j
  # number of basis functions
  nk <- length(xj)
  # number of points
  n <- length(x)
  # design/model matrix
  X <- matrix(NA,n,nk)
  # for knot in knots
  for (j in 1:nk) {
    X[,j] <- tf(x,xj,j)
  }
  X
}
## END Wood (2017) Code