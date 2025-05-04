
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
studentServer <- function(id, page_len){
  # Get the engine dataset from Gamair package
  engine <- get("engine", envir = asNamespace("InvestiGAM"))
  
  # preload the C02 dataset
  # load C02 dataset
  CO2 <- datasets::CO2
  
  # manipulations for modelling
  plant <- CO2 |>
    as_tibble() |>
    rename(plant = Plant, type = Type, treatment = Treatment) |>
    mutate(plant = factor(plant, ordered = FALSE))
  
  # fit model - from GAMbler blog
  # Problems with mvgam
  model_1 <- mgcv::gam(uptake ~ treatment * type + 
                   s(plant, bs = "re") +
                   s(conc, by = treatment, k = 7),
                 data = plant, 
                 method = "REML", 
                 family = Gamma(link = "log"))
  
  shiny::moduleServer(id, function(input,output,session){
    
    ###### Student Journey ######
      
    # Appraise
    output$userModelSummary <- shiny::renderPrint({
      summary(model_1)
    })
    
    output$userModelGamCheck <- shiny::renderPrint({
      mgcv::gam.check(model_1)
    })
    
    output$userModelAppraisal <- shiny::renderPlot({
      gratia::appraise(model_1, point_col = "steelblue", point_alpha = 0.4, method="simulate")
    })
    
    # Render Datatable
    output$example_data<- DT::renderDataTable({
      DT::datatable(plant)
    })
    
    # Functions sourced from Shiny Book to allow for page changes for the Wizard.
    changePage <- function(from, to) {
      shiny::observeEvent(input[[paste0("go_", from, "_", to)]], {
        shiny::updateTabsetPanel(session, "wizard", selected = paste0("page_", to))
      })  
    }
    ids <- seq_len(page_len)
    lapply(ids[-1], function(i) changePage(i, i - 1))
    lapply(ids[-page_len], function(i) changePage(i, i + 1))
    
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