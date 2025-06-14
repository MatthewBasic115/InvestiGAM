#' teach_ui.R
#'
#' This file contains functions for generating the UI of the teach module.
#' This includes the theory section and the student journey.
#' There are also helper functions such as for the Wizard.
#'

#' generateTeachBuildPage
#' @importFrom bslib accordion accordion_panel
#' @returns Accordion which contains teach section markdown files
#' @export
generateTeachInterpretPage <- function(){
  accordion(
    accordion_panel(
      title = "Interpret Introduction",
      withMathJax(loadMarkdown("teach_interpret_intro.md"))
    ),
    accordion_panel(
      title = "Predictions",
      withMathJax(loadMarkdown("teach_interpret_pred.md"))
    ),
    accordion_panel(
      title = "Slopes",
      withMathJax(loadMarkdown("teach_interpret_slopes.md"))
    ),
    accordion_panel(
      title = "Comparisons",
      withMathJax(loadMarkdown("teach_interpret_comparisons.md"))
    ),
  )
}

#' generateTeachBuildPage
#' @importFrom bslib accordion accordion_panel
#' @returns Accordion which contains teach section markdown files
#' @export
generateTeachBuildPage <- function(){
  accordion(
    accordion_panel(
      title = "Build Introduction",
      loadMarkdown("teach_build_intro.md")
    ),
    accordion_panel(
      title = "Hyperparameters",
      loadMarkdown("teach_build_params.md")
    ),
    accordion_panel(
      title = "Formula Terms",
      loadMarkdown("teach_build_terms.md")
    ),
    accordion_panel(
      title = "GAM Appraisal",
      loadMarkdown("teach_gam_appraise.md")
    ),
  )
}

#' generateTeachGAMPage
#' 
#' Generates the UI for the GAM section of the Teach module.
#' 
#' @importFrom bslib accordion accordion_panel
#' @importFrom shiny numericInput NS plotOutput
#' @returns Accordion which contains teach section markdown files
#' @export
generateTeachGAMPage <- function(){
  student_id = "student"
  accordion(
    accordion_panel(
      title = "Generalised Additive Models",
      loadMarkdown("teach_gam_intro.md")
    ),
    accordion_panel(
      title = "Knots and Basis Functions",
      loadMarkdown("teach_gam_knots.md")
    ),
    accordion_panel(
      title = "Knot Example",
      loadMarkdown("teach_gam_knots_ex.md"),
      numericInput(NS(student_id,"teach_nknots"),"Knots (k)", value=6,min=2, max=10),
      plotOutput(NS(student_id,"plot_knot")),
    ),
    accordion_panel(
      title = "Regularisation of 'Wiggliness'",
      loadMarkdown("teach_gam_lambda.md")
    ),
    accordion_panel(
      title = "Smoothing Splines",
      loadMarkdown("teach_gam_smooths.md")
    ),
    accordion_panel(
      title = "Wrap Up",
      loadMarkdown("teach_gam_wrapup.md")
    ),
  )
}

#' generateTeachGLMPage
#' @importFrom bslib accordion accordion_panel
#' @returns Accordion which contains the GLM section of the Teach module
generateTeachGLMPage <- function(){
  accordion(
    accordion_panel(
      title = "Generalised Linear Models",
      loadMarkdown("teach_glm_intro.md")
    ),
  )
}

#' generateTeachTabPanel
#' 
#' Generates the Tab Panel for the Learn module.
#' 
#' @importFrom bslib accordion accordion_panel page_fluid navset_pill
#' @importFrom shiny numericInput NS plotOutput tabPanel fluidPage
#' @returns Accordion which contains teach section markdown files
#' @export
generateTeachTabPanel <- function(){
  student_id = "student"
  tabPanel("Learn",
    page_fluid(
      navset_pill(
        nav_panel("Introduction", 
          loadMarkdown("teach_intro_text.md")
        ),
        nav_panel("1. GLMs",
          generateTeachGLMPage()
        ),
        nav_panel("2. GAMs",
          generateTeachGAMPage()
        ),
        nav_panel("3. Build",
          generateTeachBuildPage()
        ),
        nav_panel("4. Interpret",
          generateTeachInterpretPage()
        ),
        nav_panel("5. Walkthrough",
          fluidPage(
            wizardUI(student_id, pages=list(
              generateExampleIntro(),generateExampleDataset(student_id),generateExampleDatasetPlots(student_id),
              generateExampleGam(),
              generateExampleAppraise(student_id), generateExampleAppraise2(student_id),
              generateExampleAppraise3(student_id), generateExampleWhatNow(student_id),
              generateExampleBasicInterpret(student_id), generateExampleConclusion()
            ))
          )
        )
      )
    )
  )
}

generateExampleConclusion <- function(){
    withMathJax(loadMarkdown("example_conclusion.md"))
}

generateExampleIntro <- function(){
  tagList(
    withMathJax(loadMarkdown("example_welcome.md"))
  )
}

generateExampleDataset <- function(id){
  tagList(
    withMathJax(loadMarkdown("example_dataset.md")),
    DT::dataTableOutput(NS(id, "example_data"))
  )
}

generateExampleDatasetPlots <- function(id){
  tagList(
    h2("Capture Count Histogram"),
    withMathJax(loadMarkdown("example_data_plots.md")),
    plotOutput(NS(id,"exampleCountHistogram")),
    h3("Species Captures Counts"),
    withMathJax(loadMarkdown("example_data_plots2.md")),
    plotOutput(NS(id,"exampleTimeSeries")),
    h3("DM Captures vs NDVI and mintemp"),
    withMathJax(loadMarkdown("example_data_plot3.md")),
    plotOutput(NS(id,"exampleTempnvdi"))
  )
}


generateExampleGam <- function(){
  withMathJax(loadMarkdown("example_gam.md"))
}
  
generateExampleAppraise <- function(id){
  tagList(
    withMathJax(loadMarkdown("example_appraise.md")),
    accordion(
      accordion_panel(
        title = "Summary",
        verbatimTextOutput(NS(id,"model1Summary"))
      ),
      accordion_panel(
        title = "gam.check()",
        withMathJax(loadMarkdown("example_appraise_check.md")),
        verbatimTextOutput(NS(id,"model1GamCheck")),
        
      ),
      accordion_panel(
        title = "Appraisal Plots",
        withMathJax(loadMarkdown("example_appraise_appraise.md")),
        plotOutput(NS(id,"model1Appraisal"))
      )
    )
  )
}

generateExampleAppraise2 <- function(id){
  tagList(
    withMathJax(loadMarkdown("example_appraise_2.md")),
    accordion(
      accordion_panel(
        title = "Summary",
        verbatimTextOutput(NS(id,"model2Summary"))
      ),
      accordion_panel(
        title = "gam.check()",
        withMathJax(loadMarkdown("gam_appraise_2_check.md")),
        verbatimTextOutput(NS(id,"model2GamCheck")),
      ),
      accordion_panel(
        title = "Appraisal Plots",
        withMathJax(loadMarkdown("gam_appraise_2_app.md")),
        plotOutput(NS(id,"model2Appraisal"))
      )
    )
  )
}

generateExampleAppraise3 <- function(id){
  tagList(
    withMathJax(loadMarkdown("gam_appraise_3.md")),
    accordion(
      accordion_panel(
        title = "Summary",
        verbatimTextOutput(NS(id,"model3Summary"))
      ),
      accordion_panel(
        title = "gam.check()",
        verbatimTextOutput(NS(id,"model3GamCheck")),
      ),
      accordion_panel(
        title = "Appraisal Plots",
        plotOutput(NS(id,"model3Appraisal"))
      )
    )
  )
}

generateExampleWhatNow <- function(id){
  tagList(
    withMathJax(loadMarkdown("example_what_now.md")),
    navset_card_tab(
      # Conditional Effects
      nav_panel("Partial Effects",
        card_header("Partial Effects Plot"),
        withMathJax(loadMarkdown("example_interpret_cond_eff.md")),
        #responseScaleSelector(NS(id,"link_response")),
        selectizeInput(NS(id,"interpret_smooth_select"), "Select Smooth to Plot", getPortalModelSmooths(), multiple=FALSE),
        plotOutput(NS(id,"plot_gam_parteff"))
      ),
      nav_panel("Basis Functions",
        card_header("Basis Function Plots"),
        withMathJax(loadMarkdown("example_interpret_basis_functions.md")),
        selectizeInput(NS(id,"simulated_smooth_select"), "Select Smooth to Plot Basis Functions", getPortalModelSmooths(), multiple=FALSE),
        plotOutput(NS(id,"basis_func"))
      ),
    )
  )
}

generateExampleBasicInterpret <- function(id){
  tagList(
    withMathJax(loadMarkdown("example_interpret_main.md")),
    navset_card_tab(
      # Conditional Effects
      nav_panel("Predictions",
        card_header("Plot Predictions"),
        withMathJax(loadMarkdown("example_interpret_pred.md")),
        selectizeInput(NS(id,"plot_pred_cond"), "Select Conditional Predictors", getPortalModelVars(), multiple=TRUE, selected=c("series", "time")),
        selectizeInput(NS(id,"plot_pred_by"), "Select Marginal Predictors", getPortalModelVars(), multiple=TRUE),
        plotOutput(NS(id,"plot_pred"))
      ),
      nav_panel("Slopes",
        card_header("Plot Slopes"),
        withMathJax(loadMarkdown("example_interpret_slopes.md")),
        layout_column_wrap(
          selectizeInput(NS(id,"plot_slope_cond_var"), "Select variable of interest",getPortalModelVars(), multiple=TRUE, selected="mintemp"),
          selectizeInput(NS(id,"plot_slope_cond_cond"), "Select Conditional Predictors",getPortalModelVars(), multiple=TRUE),
          selectizeInput(NS(id,"plot_slope_by_by"), "Select Marginal Predictors",getPortalModelVars(), multiple=TRUE, selected="series")
        ),
        plotOutput(NS(id,"plot_slope"))
      ),
      nav_panel("Comparisons",
        card_header("Plot Comparisons"),
        withMathJax(loadMarkdown("example_interpret_comps.md")),
        selectInput(NS(id,"comp_opt"), "Select 'Comparison' function",getValidCompareArguments(),selected="difference",multiple=FALSE),
        layout_column_wrap(
          selectizeInput(NS(id,"comp_var"), "Select Variable/s of Interest",getPortalModelVars(),multiple=TRUE, selected="series"),
          selectizeInput(NS(id,"comp_cond"), "Select Conditional Predictors", getPortalModelVars(),multiple=TRUE, selected="time"),
          selectizeInput(NS(id,"comp_by"), "Select Marginal Predictors", getPortalModelVars(),multiple=TRUE),
        ),
        plotOutput(NS(id,"plot_comp"))
      ),
    )
  )
}

# Smooths only
getPortalModelSmooths <- function(){
  return(c("s(time):seriesDM", "s(time):seriesDO", "s(time):seriesPB", "s(time):seriesPP", "s(ndvi_ma12)", "s(ndvi_ma12,series)","s(mintemp)","s(mintemp,series)"))
}

# Predictors Only
getPortalModelVars <- function(){
  return(c("series", "time", "ndvi_ma12", "mintemp"))
}

# Includes response
getPortalModelCols <- function(){
  return(c("series", "time", "ndvi_ma12", "mintemp","captures"))
}

#### Code below adapted from Mastering Shiny (Wickham, 2021)
#### Sourced from: https://mastering-shiny.org/scaling-modules.html#module-wizard
#### See references.md for Citation

nextPage <- function(id, i) {
  actionButton(NS(id, paste0("go_", i, "_", i + 1)), "next")
}
prevPage <- function(id, i) {
  actionButton(NS(id, paste0("go_", i, "_", i - 1)), "prev")
}

wrapPage <- function(title, page, button_prev, button_next) {
  shiny::tabPanel(
    title = title,
    fluidRow(
      layout_column_wrap(
        button_prev,
        button_next
      )
    ),
    fluidRow(
      page
    ),
  )
}


wizardUI <- function(id, pages) {
  n <- length(pages)
  
  wrapped <- vector("list", n)
  for (i in seq_along(pages)) {
    lhs <- if (i > 1) prevPage(id, i)
    rhs <- if (i < n) nextPage(id, i) else NULL
    wrapped[[i]] <- wrapPage(paste0("page_", i), pages[[i]], lhs, rhs)
  }
  # Cannot use !!! due to tagList() being used to generate pages.
  wrapped$id <- NS(id, "wizard")
  wrapped$type <- "hidden"
  do.call("tabsetPanel", wrapped)
}

#### End Mastering Shiny Code