###
### This file contains functions which generate UI elements for the teach section
###

generateTeachInterpretPage <- function(){
  accordion(
    accordion_panel(
      title = "Interpret Introduction",
      withMathJax(includeMarkdown("markdown/teach_interpret_intro.md"))
    ),
    accordion_panel(
      title = "Predictions",
      withMathJax(includeMarkdown("markdown/teach_interpret_pred.md"))
    ),
    accordion_panel(
      title = "Slopes",
      withMathJax(includeMarkdown("markdown/teach_interpret_slopes.md"))
    ),
    accordion_panel(
      title = "Comparisons",
      withMathJax(includeMarkdown("markdown/teach_interpret_comparisons.md"))
    ),
  ) # end accordion
}

generateTeachBuildPage <- function(){
  accordion(
    accordion_panel(
      title = "Build Introduction",
      withMathJax(includeMarkdown("markdown/teach_build_intro.md"))
    ),
    accordion_panel(
      title = "Hyperparameters",
      withMathJax(includeMarkdown("markdown/teach_build_params.md"))
    ),
    accordion_panel(
      title = "Formula Terms",
      withMathJax(includeMarkdown("markdown/teach_build_terms.md"))
    ),
    accordion_panel(
      title = "GAM Appraisal",
      withMathJax(includeMarkdown("markdown/teach_gam_appraise.md"))
    ),
  ) # end accordion
}

generateTeachGAMPage <- function(){
  accordion(
    accordion_panel(
      title = "Generalised Additive Models",
      withMathJax(includeMarkdown("markdown/teach_gam_intro.md"))
    ),
    accordion_panel(
      # https://stackoverflow.com/questions/56157839/rendering-html-outputs-from-r-markdown-in-shiny-app 
      # if I was interested in using rmd
      title = "Knots and Basis Functions",
      withMathJax(includeMarkdown("markdown/teach_gam_knots.md")),
    ),
    accordion_panel(
      title = "Knot Example",
      withMathJax(includeMarkdown("markdown/teach_gam_knots_ex.md")),
      numericInput(NS("student","teach_nknots"),"Knots (k)", value=6,min=2, max=10),
      plotOutput(NS("student","plot_knot")),
    ),
    accordion_panel(
      title = "Regularisation of 'Wiggliness'",
      withMathJax(includeMarkdown("markdown/teach_gam_lambda.md"))
    ),
    accordion_panel(
      title = "Smoothing Splines",
      withMathJax(includeMarkdown("markdown/teach_gam_smooths.md"))
    ),
    accordion_panel(
      title = "Wrap Up",
      withMathJax(includeMarkdown("markdown/teach_gam_wrapup.md"))
    ),
  ) # end accordion
}

# Navigation tabset for the teach UI
generateTeachTabPanel <- function(){
  tabPanel("Learn",
           page_fluid(
             navset_pill(
               nav_panel("Introduction",
                 includeMarkdown("markdown/teach_intro_text.md"), 
               ),
               nav_panel("1. GLMs",
                 generateTeachGLMPage(),
               ), # End GLMs
               nav_panel("2. GAMs",
                 generateTeachGAMPage(),
               ), # End GAMs
               nav_panel("3. Build",
                 generateTeachBuildPage(),
               ), # End build
               nav_panel("4. Interpret",
                 generateTeachInterpretPage(),
               ), # End Interpret
               nav_panel("5. General Help"
                 
               ), # end General Help
             ), # end navset_pill
           ) # end page fluid
  ) # end teach tabpanel
}

# Generates the page for the GLM section of the student tutorial
generateTeachGLMPage <- function(){
  accordion(
    accordion_panel(
      title = "Generalised Linear Models",
      withMathJax(includeMarkdown("markdown/teach_glm_intro.md"))
    ),
    accordion_panel(
      title = "Canonical Link Functions",
      "TODO"
    )
  ) # end accordion
}