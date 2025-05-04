# 
# This file contains helper functions for generating common UI elements.
#
library(shiny)

# Simple function to select whether the user wants the link or response scale for a given scenario
responseScaleSelector <- function(id) {
  selectInput(id, label="Display Results on Link or Response Scale?", choices=c('link','response'))
}

#' Load a Markdown File from the Package's inst/markdown Directory
#'
#' @param filename Name of the markdown file, e.g. "teach_intro_text.md"
#'
#' @return Shiny generation of Markdown file
#' @export
loadMarkdown <- function(filename) {
  withMathJax(includeMarkdown(system.file("markdown", filename, package = "InvestiGAM")))
}

#
addDefaultToInputChoices <- function(input){
  ret <- append(input, "Default")
  return(ret)
}

generateDesignTabPanel <- function(){
  tabPanel("Design",
           actionButton("quickload_brain","quickload_brain"),
           actionButton("quickload_brain_te","quickload_brain_te"),
           actionButton("quickload_brain_ti","quickload_brain_ti")
  ) # end tabPanel design
}

# Builds a Modal Dialog box for helpful tips
#' buildModalDialog
#'
#' @param mtitle Title of the pop-up
#' @param mdpath Name of markdown file located in markdown/ to display.
#'
#' @returns Modal pop-up for Shiny with markdown page as content
#' @export
#'
#' @examples observe({buildModalDialog("Load Data Help", "load_help.md")}) %>% bindEvent(input$load_help_button)
#' 
#' Above is example of a help button observer which shows the modal dialog on click.
buildModalDialog <- function(mtitle,mdpath){
  showModal(
    modalDialog(
      title=mtitle,
      easy_close=TRUE,
      size='l',
      loadMarkdown(mdpath)
    )
  )
}

generateBuildNavsetCardList <- function(){
  navset_card_tab(
    # Card which holds parametric terms
    nav_panel("Parametric",
      card_header("Parametric Terms"),
      layout_column_wrap(
        selectInput("build_parametric_term",label="Select Covariate", choices=c()),
        actionButton("build_add_parametric_button","Add Parametric Term", style = "margin-top: 25px;"),
      ),
    ), # end parametric nav panel
    # Card which holds ui elements for building smooth terms
    nav_panel("Smooths",
      card_header("Terms"),
      
      # Go for a item -> help pattern
      layout_column_wrap(
        selectInput("build_smooth_term",label="Select Smooth term", choices=c("s","te","ti","t2"), selected="s"),
        actionButton("build_smooth_term_help","Smooth Term Help", style = "margin-top: 25px;"),
      ),
      # options for smooth terms
      
      layout_column_wrap(
        selectInput("build_covariates_term",label="Select Covariates", choices=c(),multiple=TRUE),
        actionButton("build_smooth_covariate_help","Covariate Help", style = "margin-top: 25px;"),
      ),
      
      layout_column_wrap(
        selectInput("build_by_term",label="by", choices=c(),multiple=FALSE, selected="Default"),
        actionButton("build_smooth_by_help","By Term Help", style = "margin-top: 25px;"),
      ),
      
      layout_column_wrap(
        numericInput("build_nknots","Knots (k) - Set to -1 for Default", value=-1,min=-1),
        selectInput("build_smooth_classes (bs)", "Smooth Class", choices=addDefaultToInputChoices(getSmoothClasses()),selected="Default"),
      ),

      radioButtons("build_penalised", label="Fixed d.f. or penalized?", 
                   choices = list( 
                     "True" = TRUE, 
                     "False" = FALSE
                   ),
                   selected=FALSE
      ),
      actionButton("build_add_smooth","Add Smooth Term"),
    ), # end smoooths nav panel
    nav_panel("Raw Forumla",
      layout_column_wrap(
        textInput("formula", "Raw Text Formula"),
        actionButton("raw_form_help_button", "Raw Formula Help", style = "margin-top: 25px;"),
      ),
    )
  )
}

# This function generates the navigation UI for the Interpret tab
generateInterpretTabPanel <- function(){
  tabPanel("Interpret",
  titlePanel("Interpret"),
  ##### PUT CONFIG HERE #######
   accordion(
     accordion_panel(
       title="Plotting Options",
       checkboxGroupInput("int_plot_opt_checkbox", "Plotting Options",
                          c("Rug"="rug")
       ),
       responseScaleSelector("link_response")
     ) # End interpret accordion
   ),
   # Generates the navigation list for the Interpret panel and it's contents
   navlistPanel(
     id="Compare",
     widths=c(2,10),
     ########### INTRO PANEL #################
     tabPanel("Introduction",
       titlePanel("Interpret Introduction"),
       loadMarkdown("interpret_intro_text.md"),
     ),
      ########## BASIS PANEL ##########
     tabPanel("Partial Effect of Smooths", 
       # SIMULATE
       # Select condition for the slope plot
       # TODO: Talk to Nick and see if this should be deleted
       #selectizeInput("simulated_feature_select", "Select Variable to Simulate", c(), multiple=FALSE),
       
       selectizeInput("interpret_smooth_select", "Select Smooth to Plot", c(), multiple=FALSE),
       plotOutput("plot_gam"),
      ),
      tabPanel("Condtional Effect of Smooths",
        plotOutput("plot_gam_condeff")
      ),
      ##########  BASIS FUNCTIONS PANEL ##########
      tabPanel("Basis Plots", 
       # SIMULATE
       # Select condition for the slope plot
       # TODO: Talk to Nick and see if this should be deleted
       #selectizeInput("simulated_feature_select", "Select Variable to Simulate", c(), multiple=FALSE),
       
       selectizeInput("simulated_smooth_select", "Select Smooth to Plot Basis Functions", c(), multiple=FALSE),
       
       plotOutput("basis_func"),
      ),
      ########### QUICK NICK PLOTS ##############
      ########### PREDICTIONS PANEL #############
      tabPanel("Predictions", 
        titlePanel("Plot Predictions"),
        actionButton("plot_pred_help_button","Help"),
        ##### PLOT PREDICTIONS
        # Select the response scale for the plot predictions
        accordion(
          accordion_panel(
            title = "Conditional Predictions",
            selectizeInput("plot_pred_cond", "Select Conditional Predictors", c(), multiple=TRUE),
          ),
          accordion_panel(
            title="Conditional Predictions Advanced",
            layout_column_wrap(
              textInput("plot_pred_cond_topt", "Raw Text Input"),
              actionButton("plot_pred_cond_add","Generate Plot", style = "margin-top: 25px;"),
            ),
            plotOutput("plot_pred_cond_advanced")
          ),
          accordion_panel(
            title = "Marginal Predictions",
            selectizeInput("plot_pred_by", "Select Marginal Predictors", c(), multiple=TRUE),
          ),
        ),
        plotOutput("plot_pred"),
      ), # end tabPanel Predictions
     
      ######## SLOPES PANEL ##########
      tabPanel("Slopes",
       card(
         card_header("Conditional Slope"),
         layout_column_wrap(
           # Select variable for the slopes
           selectizeInput("plot_slope_cond_var", "Select variable of interest",c(), multiple=TRUE),
           # Select condition for the slope plot
           selectizeInput("plot_slope_cond_cond", "Select conditional variables",c(), multiple=TRUE),
           selectInput("plot_slope_cond_slope", "Select Slope",getValidSlopeFunctions(), selected="dydx"),
         ),
         
         plotOutput("plot_slope_cond"),
       ),
       card(
         card_header("Marginal Slope"),
         layout_column_wrap(
           # Select variable for the slopes
           selectizeInput("plot_slope_by_var", "Select variable of interest",c(), multiple=TRUE),
           # Select factor for the by argument
           selectizeInput("plot_slope_by_by", "Select Factor variables",c(), multiple=TRUE),
           selectInput("plot_slope_by_slope", "Select Slope",getValidSlopeFunctions(), selected="dydx"),
         ),
         plotOutput("plot_slope_by"),
       ),
               
      ), # End slopes tabPanel
      ##### COMPARISONS PANEL ##########
      generateComparisonsTabPanel(),
    ) # end navlistPanel Interpret
  )
}

# Generates the UI for the 'Comparisons' tab in the Interpret menu.
generateComparisonsTabPanel <- function(){
  tabPanel("Comparisons",
    page_fluid(
      h3("Plot Comparisons"), 
      accordion(
        accordion_panel(
          title = "Introduction & Help",
          loadMarkdown("teach_interpret_comparisons.md"),
        ),
        accordion_panel(
          title = "Datagrid Generation",
          # comp_seq old ID
          layout_column_wrap(
            selectInput("comp_opt_comp", "Select 'Comparison' function",getValidCompareArguments(),selected="difference",multiple=FALSE),
            actionButton("comp_opt_help_button", "Comparisons Help", style = "margin-top: 25px;")
          ),
          card(
            card_header("Generate Data"),
            p("Below allows you to build a datagrid to pass into comparisons functions"),
            layout_column_wrap(
              selectInput("grid_type", "Select Grid Type", getValidDataGridTypes(),multiple=FALSE), 
              textInput("comp_dg_text","Raw Text Input"),
              actionButton("comp_dg_button", "Add Grid", style = "margin-top: 25px;"),
            ),
            textOutput("comp_text"),
            actionButton("comp_dg_clear", "Clear Datagrid", style = "margin-top: 25px;")
          ),
        ),
        accordion_panel(
          title = "Conditional Comparisons",
          # comp_seq old ID
          layout_column_wrap(
            selectizeInput("comp_cond_var", "Select Variable/s of Interest",c(),multiple=TRUE),
            selectizeInput("comp_cond_conc", "Select Conditional Variables", c(),multiple=TRUE),
          ),
          plotOutput("comp_cond_plot")
        ),
        accordion_panel(
          title = "Marginal Comparisons",
          layout_column_wrap(
            selectizeInput("comp_by_interest", "Select Variable/s of Interest", c(),multiple=TRUE),
            selectizeInput("comp_by_by", "Select Factor Variables", c(),multiple=TRUE),
          ),
          plotOutput("comp_by_plot")
        ),
        accordion_panel(
          title = "Adv Conditional Comparisons",
          # comp_seq old ID
          layout_column_wrap(
            card(
              card_header("Specify Variable/s"),
              layout_column_wrap(
                selectizeInput("comp_cond_var_adv", "Select Variable/s of Interest",c(),multiple=TRUE),
                textInput("comp_cond_text_adv_var","Raw Variable Input Method"),
              ),
              layout_column_wrap(
                selectizeInput("comp_cond_cond_adv", "Select Conditional Variable",c(),multiple=TRUE),
                textInput("comp_cond_text_adv_cond","Raw Condition Input"),
              ),
              actionButton("comp_adv_cond_plot_button", "Generate Plot")
            ),
          ),
          plotOutput("comp_adv_cond_plot")
        ),
        accordion_panel(
          title = "Adv Marginal Comparisons",
          # comp_seq old ID
          layout_column_wrap(
            card(
              card_header("Specify Variable/s"),
              layout_column_wrap(
                selectizeInput("comp_by_var_adv", "Select Variable/s of Interest",c(),multiple=TRUE),
                textInput("comp_by_text_adv_var","Raw Variable Input Method"),
              ),
              layout_column_wrap(
                selectizeInput("comp_by_by_adv", "Select Marginal Variable",c(),multiple=TRUE),
              ),
              actionButton("comp_adv_by_plot_button", "Generate Plot", style = "margin-top: 25px;")
            ),
          ),
          plotOutput("comp_adv_by_plot")
        ),
      ),
    )
  ) # end tabPanel Comparisons
}

generateRefTabPanel <- function(){
  tabPanel("References",
    loadMarkdown("reference_list.md")
  ) # End TabPanel
}

generateAppraiseTabPanel <- function(){
  tabPanel("Appraise",
    accordion(
      accordion_panel(
        title = "Summary",
        verbatimTextOutput("userModelSummary")
      ),
      accordion_panel(
        title = "gam.check()",
        verbatimTextOutput("userModelGamCheck")
      ),
      accordion_panel(
        title = "Appraisal Plots",
        radioButtons("appraise_gratia_simulate", label="Simulate?", 
                     choices = list( 
                       "True" = TRUE, 
                       "False" = FALSE
                     ),
                     selected=TRUE
        ),
        plotOutput("userModelAppraisal")
      ),
    )
  ) # End TabPanel
}