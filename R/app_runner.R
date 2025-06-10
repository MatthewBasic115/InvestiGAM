#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
# https://shiny.posit.co/r/components/inputs/select-multiple/

#' run_app 
#'
#' Runs the Shiny app
#'
#' @import shiny
#' @importFrom bslib card card_header layout_column_wrap page_fluid page_fillable
#' @importFrom gratia appraise which_smooths smooths basis model_vars draw
#' @importFrom marginaleffects plot_predictions plot_comparisons plot_slopes
#' @importFrom ggplot2 theme_set theme_classic theme labs geom_hline
#' @importFrom graphics lines
#' @importFrom mgcv gam.check gam
#' @param ... ...
#'
#' @returns Shiny app to run
#' @export
run_app <- function(...){
  
  # define custom ggplot2 theme
  theme_set(theme_classic(base_size = 12, base_family = 'serif') +
              theme(axis.line.x.bottom = element_line(colour = "black",
                                                      linewidth = 1),
                    axis.line.y.left = element_line(colour = "black",
                                                    linewidth = 1),
                    panel.spacing = unit(0, 'lines'),
                    legend.margin = margin(0, 0, 0, -15)))
  ##### Data #####
  # Variable to hold ID for student journey module-ui pair.
  student_id = "student"
  
  # https://cran.r-project.org/web/packages/gamair/gamair.pdf pg 52
  # Simon Wood
  brain <- get("brain", envir = asNamespace("InvestiGAM"))
  brain <- brain[brain$medFPQ>5e-3,]
  
  # preload the C02 dataset
  # load C02 dataset
  CO2 <- datasets::CO2
  
  # manipulations for modelling
  plant <- CO2 |>
    dplyr::as_tibble() |>
    dplyr::rename(plant = Plant, type = Type, treatment = Treatment) |>
    dplyr::mutate(plant = factor(plant, ordered = FALSE))
  
  # fit model - from GAMbler blog
  model_1 <- mgcv::gam(uptake ~ treatment * type + 
                   s(plant, bs = "re") +
                   s(conc, by = treatment, k = 7),
                 data = plant, 
                 method = "REML", 
                 family = Gamma(link = "log"))
  # Define UI for application that draws a histogram
  ui <- fluidPage(
    
    titlePanel("InvestiGAM"),
    tabsetPanel(
      # The Welcome and load modules are small enough to not be moved to the ui_helpers file.
      tabPanel("Welcome",
               page_fillable(
                 loadMarkdown("welcome.md")
               )
      ), # End Welcoime TabPanel
      ######## LOAD ########
      tabPanel("Load",
               accordion(
                 accordion_panel(
                   title = "Quick Load",
                   generateDesignTabPanel()
                 ),
                 open=FALSE
               ),
               titlePanel("Load Dataset"),
               # Build model
               layout_column_wrap(
                 fileInput('load_upload', 'Choose file to upload'),
                 textInput("load_col_types", "Column Types"),
                 radioButtons("load_separator","Separator: ",choices = c(";",",",":"), selected=",",inline=TRUE),
               ),
               layout_column_wrap(
                 actionButton("load_go_button","Import"),
                 actionButton("load_help_button", "Help")
               ),
               DT::dataTableOutput("load_data"),
      ),
      # Build Tab Panel. Contains the UI for building a GAM.
      generateBuildTabPanel(),
      # Appraisal Tab Panel. Contains plots and tools for evaluating the model
      generateAppraiseTabPanel(),
      # Generates Interpret tabs. Contains predictions, comparisons etc.
      generateInterpretTabPanel(),
      # This function generates a new UI for the student journey.
      generateTeachTabPanel(),
      # References
      generateRefTabPanel(),
    ), # End Tabset panel
  )
  
  # Render functions is designed to produce a particular type of output.
  # Often paired with an Output function. e.g. renderPrint will call verbatimtextoutput
  server <- function(input, output, session) {
    
    # Student server, runs code for the teach section.
    studentServer(student_id)
    
    ##### QUICKLOAD #######
    
    # Models from Gamair documentation
    # Builders for 'quickloading' models for testing.
    observe({
      # Basic model with two smooths, simple alternative model to the CO2 model
      model_brain <- gam(medFPQ~s(Y,k=30)+s(X,k=30), data=brain, family=Gamma(link=log))
      # Set the user data to the brain data to update the UI
      userData(brain)
      # Set the active model to the quick built model
      userModel(model_brain)
    }) %>% bindEvent(input$quickload_brain)
    
    # Builders for 'quickloading' models for testing.
    observe({
      # Test tensor term
      model_brain_te <- gam(medFPQ~te(Y,X,k=10),data=brain,family=Gamma(link=log))
      # Set the user data to the brain data to update the UI
      userData(brain)
      # Set the active model to the quick built model
      userModel(model_brain_te)
    }) %>% bindEvent(input$quickload_brain_te)
    
    # Builders for 'quickloading' models for testing.
    observe({
      # Test ti term
      model_brain_ti <- gam(medFPQ ~ s(Y,k=10,bs="cr") + s(X,bs="cr",k=10) +
                              ti(X,Y,k=10), data=brain, family=Gamma(link=log))
      # Set the user data to the brain data to update the UI
      userData(brain)
      # Set the active model to the quick built model
      userModel(model_brain_ti)
    }) %>% bindEvent(input$quickload_brain_ti)
    
    observe({
      portal_data <- get("portal_data", envir = asNamespace("InvestiGAM"))
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
      userData(portal_data)
      userModel(model_2)
    }) %>% bindEvent(input$quickload_portal)
    
    gam_formula <- reactive({
      typed_word <- input$formula 
    })
    
    # Model present by default
    userModel <- reactiveVal(model_1)
    
    # Map the smooth name to it's ID
    feature_mappings <- reactive({
      mapped_idx = which_smooths(userModel(), input$feature)
    })
    
    ########################################
    ############### DATA ###################
    ########################################
    
    # Load user dataset
    load_csv <- reactive({
      req(input$load_upload)
      if(isTruthy(input$load_col_types)){
        sb <- paste0("list(",input$load_col_types,")")
        col_types <- eval(str2expression(sb))
        df <- vroom::vroom(input$load_upload$datapath, delim=input$load_separator, col_types=col_types) 
      } else {
        df <- vroom::vroom(input$load_upload$datapath, delim=input$load_separator)
      }
      return(df)
    }) %>% bindEvent(input$load_go_button)
    
    output$load_data<- DT::renderDataTable({
      df <- load_csv()
      userData(df)
      DT::datatable(df)
    })
    
    # Temporarily have userData set
    userData <- reactiveVal(plant)
    
    # Update UI with new dataset when a new dataset is loaded
    observe({
      nm = names(userData())
      
      # Response variable selector
      updateSelectInput(session, "build_response", choices=nm)
      
      # Building formula items
      updateSelectInput(session,"build_covariates_term",choices=nm)
      updateSelectInput(session,"build_by_term",choices=addDefaultToInputChoices(nm)) 
      updateSelectInput(session,"build_parametric_term",choices=nm)
    }) %>% bindEvent(userData())
    
    # When a new model is created, only offer vars used in the model in the interpret tab
    observe({
      mv <- model_vars(userModel())
      
      # Interpret tab 
      # Interpret predictions
      updateSelectInput(session,"plot_pred_cond",choices=mv, selected=mv[1])
      updateSelectInput(session,"plot_pred_cond_var",choices=mv,selected=mv[1])
      updateSelectInput(session,"plot_pred_by",choices=mv)
      
      # Slopes
      updateSelectInput(session,"plot_slope_cond_var",choices=mv, selected=mv[1])
      updateSelectInput(session,"plot_slope_by_var",choices=mv, selected=mv[1])
      updateSelectInput(session,"plot_slope_cond_cond",choices=mv, selected=mv[1])
      updateSelectInput(session,"plot_slope_by_by",choices=mv)
      
      # Basis function plotter
      updateSelectInput(session,"simulated_feature_select",choices=mv,selected=mv[1])
      
      # Comparisons tab
      updateSelectInput(session,"comp_seq",choices=mv,selected=mv[1])
      updateSelectInput(session,"comp_interest",choices=mv,selected=mv[1])
      
      # Conditional Comparisons
      updateSelectInput(session,"comp_cond_var",choices=mv,selected=mv[1])
      updateSelectInput(session,"comp_cond_conc",choices=mv,selected=mv[1])
      
      # Adv. Conditional Comparisons
      updateSelectInput(session,"comp_cond_var_adv",choices=mv)
      updateSelectInput(session,"comp_cond_cond_adv",choices=mv)
      
      # Marginal Comparisons Inputs
      updateSelectInput(session,"comp_by_by",choices=mv)
      updateSelectInput(session,"comp_by_interest",choices=mv,selected=mv[1])
      
      # Adv. Marginal Inputs
      updateSelectInput(session,"comp_by_var_adv",choices=mv)
      updateSelectInput(session,"comp_by_by_adv",choices=mv)
      
      # No default selection for 'by' in comparisons
      updateSelectInput(session,"comp_dg_var",choices=mv,selected=mv[1])
    }) %>% bindEvent(userModel())
    
    
    #######################################
    ############### Build #################
    #######################################
    
    # Contains all the smooth terms for the user defined model. e.g. s(x), s(x2)
    # TODO: Update to all terms
    build_terms <- reactiveVal(c())
    
    # When the model is updated, update UI for smooth selection
    observe({
      m <- userModel()
      updateSelectInput(session,"simulated_smooth_select",choices=smooths(m))
      updateSelectInput(session,"interpret_smooth_select",choices=smooths(m))
    }) %>% bindEvent(userModel())
    
    # Variable which holds the user defined model.
    # Bound to the input button, so will only be defined once a user has built their model.
    observe({
      
      # Build the formula beginning with mandatory values and followed by params
      # If the raw text is empty, use the build_terms() to build the fomrula.
      if(isTruthy(input$formula)){
        form_string <- paste0(input$build_response, "~", input$formula)
        fmla <- as.formula(form_string)
      } else {
        term_string <- paste(build_terms(), collapse='')
        form_string <- paste(input$build_response, "~", term_string, collapse='')
        (fmla <- as.formula(paste(input$build_response, " ~ ", paste(build_terms(), collapse=''))))
      }
      
      tryCatch({
        m <- gam2(formula=fmla,
                  data = userData(),
                  method = input$gam_method,
                  family = buildFamilyValue(input$gam_family,input$gam_link))
      }, error = function(e) {
        showModalErrorMessage(c(e[1]))
        shiny::reactiveStop(conditionMessage(e))
      })
      
      userModel(m)
    }) %>% bindEvent(input$build)
    
    # Update the options for link functions based on the distribution family selected.
    observe({
      links <- getValidLinkFunction(input$gam_family)
      methods <- getGamMethods(input$gam_family)
      # First item is the canonical link, select by default
      updateSelectInput(session,"gam_link",choices=links, selected=links[1])
      updateSelectInput(session,"gam_method",choices=methods, selected="REML")
    }) %>% bindEvent(input$gam_family)
    
    ########################################
    ###### Formula builder observers #######
    ########################################
    
    # Observe when the smooth button is pressed, add the smooth to the list.
    observe({
      # build the smooth
      smooth <- buildSmooth({input$build_smooth_term},{input$build_covariates_term},{input$build_nknots},{input$build_penalised},{input$build_smooth_classes},by=input$build_by_term)
      # Append the new smooth to the list of smooths for the model
      msmooths <- append(build_terms(), smooth)
      # set the value of the build_terms reactive var to the new list with the appended smooth
      build_terms(msmooths)
    }) %>% bindEvent(input$build_add_smooth)
    
    # Observe when the add operator button is added
    observe({
      operator <- paste0(" ", input$build_parametric_interaction, " ")
      moperator <- append(build_terms(),operator)
      build_terms(moperator)
    }) %>% bindEvent(input$build_operator_add_button)
    
    # When the add parametric term button is pressed, add it to the list of terms.
    observe({
      para <- input$build_parametric_term
      mpara <- append(build_terms(),para)
      build_terms(mpara)
    }) %>% bindEvent(input$build_add_parametric_button)
    
    # When the undo button is pressed, remove the last item from the formula list
    observe({
      undone <- build_terms()[-length(build_terms())]
      build_terms(undone)
    }) %>% bindEvent(input$build_undo_button)
    
    #########################################
    ##### Formula builder text outputs ######
    #########################################
    
    output$build_splines <- renderText({
      build_terms()
    })
    ################################
    ########## Appraisal ###########
    ################################
    
    # Render the output of the gratia's appraise function on the user model
    output$userModelAppraisal <- renderPlot({
      if(input$appraise_gratia_simulate){
        appraise(userModel(), point_col = "steelblue", point_alpha = 0.4, method="simulate")
      } else {
        appraise(userModel(), point_col = "steelblue", point_alpha = 0.4)
      }
    })
    
    output$userModelSummary <- renderPrint({
      summary(userModel())
    }) %>% bindEvent(userModel())
    
    output$userModelGamCheck <- renderPrint({
      gam.check(userModel())
    }) %>% bindEvent(userModel())
    
    ##############################
    ########## INTERPRET #########
    ##############################
    
    ########################################
    ####### Interpret UI Observers #########
    ########################################
    
    observe({
      buildModalDialog("Plot Predictions",getPlotPredHelpText())
    }) %>% bindEvent(input$plot_pred_help_button)
    
    
    ##############################
    ########### PLOTS ############
    ##############################
    
    ######## PLOT BASIC SMOOTHS AND BASIS FUNCTIONS ############
    output$plot_gam <- renderPlot({
      smidx <- which_smooths(userModel(), input$interpret_smooth_select)
      draw(userModel(), select=smidx)
    })
    
    # plot the basis functions
    output$basis_func <- renderPlot({
      smidx <- which_smooths(userModel(), input$simulated_smooth_select)
      draw(basis(userModel(), select=smidx))
    })
    
    output$plot_gam_condeff <- renderPlot({
      conditional_effects.gam(userModel(), type=input$pred_link_response, rug=checkPlotOption("rug",input$int_plot_opt_checkbox))
    })
    
    ########## PLOT PREDICTIONS ###############
    
    #### Generate UI elements based on options
    
    output$plot_pred <- renderPlot({
      # ensure that feature selections are lower case
      getPlotPredictions(userModel(),c({input$plot_pred_cond}),
                         input$link_response,rug=checkPlotOption("rug",input$int_plot_opt_checkbox),by=input$plot_pred_by) +
        labs(y=str_c("Linear predictor (",input$pred_link_response," scale)"), title=paste("Average smooth effect of ", {input$plot_pred_cond}), subtitle="aggregated across treatment and types")
    })
    
    output$plot_pred_cond_advanced <- renderPlot({
      sb <- paste0("list(",input$plot_pred_cond_topt,")")
      cond_list <- eval(str2expression(sb))
      getPlotPredictions(userModel(),cond_list,
                         input$link_response,rug=checkPlotOption("rug",input$int_plot_opt_checkbox),by=input$plot_pred_by) +
        labs(y=str_c("Linear predictor (",input$pred_link_response," scale)"), title=paste("Average smooth effect of ", {input$plot_pred_cond}), subtitle="aggregated across treatment and types")
    }) %>% bindEvent(input$plot_pred_cond_add)
    
    ######### PART 2 - PLOT SLOPES ###########
    
    #Input for conditions
    output$plot_slope_cond <- renderPlot({
      req(input$plot_slope_cond_var)
      req(input$plot_slope_cond_cond)
      plot_slopes(userModel(), variables=c({input$plot_slope_cond_var}), condition=c({input$plot_slope_cond_cond}), slope=input$plot_slope_cond_slope, type=input$link_response,
                  rug=checkPlotOption("rug",input$int_plot_opt_checkbox))
    })
    
    #Input for conditions
    output$plot_slope_by <- renderPlot({
      req(input$plot_slope_by_by)
      req(input$plot_slope_by_var)
      plot_slopes(userModel(), variables=c({input$plot_slope_by_var}), by=c({input$plot_slope_by_by}), slope=input$plot_slope_by_slope, type=input$link_response,
                  rug=checkPlotOption("rug",input$int_plot_opt_checkbox))
    })
    
    
    ###########################
    ###### COMPARISONS ########
    ###########################
    
    ##### Data #####
    
    # Holds the datagrid
    comp_dg <- reactiveVal()
    # Text used to generate the datagrid
    comp_dg_arg <- reactiveVal()
    
    ##### Observers #####
    
    # When a variable is selected to generate a sequence for, get the valid generation methods
    observe({
      selection_dtype <- lapply(userData(),class)[[input$comp_dg_var]]
      updateSelectInput(session,"comp_dg_method",choices=getValidDataGridMethods(selection_dtype))
    }) %>% bindEvent(input$comp_dg_var, ignoreInit=TRUE) # ignoreInit=TRUE is important here as input$comp_dg_var does not have an initial value (c())
    
    # Save the user specified datagrid when the add button is clicked
    observe({
      req(input$comp_dg_text)
      # If the raw text is empty, then use the provided method
      sb <- paste0("datagrid2(model=userModel(),",input$comp_dg_text,",grid_type=\"",input$grid_type,"\")")
      # Need to parse and then eval to get the datagrid object back
      dg <- eval(str2expression(sb))
      comp_dg_arg(input$comp_dg_text)
      # Must set the reactive value as so
      comp_dg(dg)
    }) %>% bindEvent(input$comp_dg_button)
    
    # Clear the datagrid variables
    observe({
      comp_dg_arg(NULL)
      comp_dg(NULL)
    }) %>% bindEvent(input$comp_dg_clear)
    
    # Text output of current plan
    output$comp_text <- renderPrint({
      comp_dg_arg()
    })
    
    ################################
    ###### COMPARISONS PLOTS #######
    ################################
    
    # Basic marginal comparisons plot.
    output$comp_by_plot <- renderPlot({
      req(input$comp_by_interest)
      req(input$comp_by_by)
      
      if(isTruthy(comp_dg())){
        plot_comparisons(userModel(),
                         variables = c({input$comp_by_interest}),
                         by=c({input$comp_by_by}),
                         comparison=input$comp_opt_comp,
                         type=input$link_response,
                         newdata=comp_dg()
        ) + geom_hline(yintercept = 0, linetype = 'dashed')
      } else {
        plot_comparisons(userModel(),
                         variables = c({input$comp_by_interest}),
                         by=c({input$comp_by_by}),
                         type=input$link_response,
                         comparison=input$comp_opt_comp
        ) + geom_hline(yintercept = 0, linetype = 'dashed')
      }
      
    })
    
    # Basic Conditional Comparisons plot
    output$comp_cond_plot <- renderPlot({
      req(input$comp_cond_var)
      req(input$comp_cond_conc)
      # If the user has provided a datagrid, use it.
      if(isTruthy(comp_dg())){
        plot_comparisons(userModel(),
                         variables=c({input$comp_cond_var}),
                         condition=c({input$comp_cond_conc}),
                         comparison=input$comp_opt_comp,
                         newdata=comp_dg()
        )
      } else {
        plot_comparisons(userModel(),
                         variables=c({input$comp_cond_var}),
                         condition=c({input$comp_cond_conc}),
                         comparison=input$comp_opt_comp
        )
      }
      
    })
    
    # Advanced Conditional Comparisons Plot
    output$comp_adv_cond_plot <- renderPlot({
      # Comparisons is tricky. For the basic case, we want a c()
      # But when specific values are defined (e.g. text input) we want a list 
      if(isTruthy(input$comp_cond_var_adv)){
        var_list <- {input$comp_cond_var_adv}
      } else {
        sbc <- paste0("list(",input$comp_cond_text_adv_var,")")
        var_list <- eval(str2expression(sbc))
      }
      
      if(isTruthy(input$comp_cond_cond_adv)){
        cond_list <- {input$comp_cond_cond_adv}
      } else {
        sbv <- paste0("list(",input$comp_cond_text_adv_cond,")")
        cond_list <- eval(str2expression(sbv))
      }
      
      # Use the user defined datagrid if provided
      if(isTruthy(comp_dg())){
        plot_comparisons(userModel(), variables=var_list,
                         condition=cond_list,
                         comparison=input$comp_opt_comp,
                         newdata=comp_dg(),
                         type={input$link_response}) +
          geom_hline(yintercept=0,linetype='dashed')
      } else {
        plot_comparisons(userModel(), variables=var_list,
                         condition=cond_list,
                         comparison=input$comp_opt_comp,
                         type={input$link_response}) +
          geom_hline(yintercept=0,linetype='dashed')
      }
      
      
    }) %>% bindEvent(input$comp_adv_cond_plot_button)
    
    # Advanced Marginal Comparisons Plot
    output$comp_adv_by_plot <- renderPlot({
      # Comparisons is tricky. For the basic case, we want a c()
      # But when specific values are defined (e.g. text input) we want a list 
      if(isTruthy(input$comp_by_var_adv)){
        var_list <- {input$comp_by_var_adv}
      } else {
        sbc <- paste0("list(",input$comp_by_text_adv_var,")")
        var_list <- eval(str2expression(sbc))
      }
      
      by_list <- {input$comp_by_by_adv}
      
      # Use the user defined datagrid if provided
      if(isTruthy(comp_dg())){
        plot_comparisons(userModel(), variables=var_list,
                         by=by_list,
                         comparison=input$comp_opt_comp,
                         newdata=comp_dg(),
                         type={input$link_response}) +
          geom_hline(yintercept=0,linetype='dashed')
      } else {
        plot_comparisons(userModel(), variables=var_list,
                         by=by_list,
                         comparison=input$comp_opt_comp,
                         type={input$link_response}) +
          geom_hline(yintercept=0,linetype='dashed')
      }
    }) %>% bindEvent(input$comp_adv_by_plot_button)
    
    ########### HELP MENU ITEMS #############
    
    observe({
      buildModalDialog("Smooth Term Help", "build_smooth_term_help.md")
    }) %>% bindEvent(input$build_smooth_term_help)
    
    observe({
      buildModalDialog("GAM Methods Help", "build_method_help.md")
    }) %>% bindEvent(input$build_methods_help_button)
    
    observe({
      buildModalDialog("GAM Families Help", "build_families_help_button.md")
    }) %>% bindEvent(input$build_families_help_button)
    
    observe({
      buildModalDialog("Link Function Help", "build_link_help.md")
    }) %>% bindEvent(input$build_links_help_button)
    
    observe({
      buildModalDialog("Load Data Help", "load_help.md")
    }) %>% bindEvent(input$load_help_button)
    
    observe({
      buildModalDialog("Raw Formula Help", "raw_formula_help.md")
    }) %>% bindEvent(input$raw_form_help_button)
    
    observe({
      buildModalDialog("Input Variable Help", "build_input_var_help.md")
    }) %>% bindEvent(input$build_smooth_covariate_help)
    
    observe({
      buildModalDialog("By Help", "build_input_by_help.md")
    }) %>% bindEvent(input$build_smooth_by_help)
    
    observe({
      buildModalDialog("Knots Help", "build_input_knots_help.md")
    }) %>% bindEvent(input$build_smooth_k_help)
    
    observe({
      buildModalDialog("Smooth Class Help", "build_smooth_class_help.md")
    }) %>% bindEvent(input$build_smooth_class_help)
    
    observe({
      buildModalDialog("Formula Terms Help", "build_terms_help_button.md")
    }) %>% bindEvent(input$build_terms_help_button)
  }
  # Run the application 
  
  # Optional runGadget
  #runGadget(ui, server, viewer = dialogViewer("InvestiGAM", width = 1200, height = 1800))
  #
  # shinyApp(ui,server,...)
  shinyApp(ui = ui, server = server)
}



