student_id = "student"

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

generateTeachGAMPage <- function(){
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

generateTeachGLMPage <- function(){
  accordion(
    accordion_panel(
      title = "Generalised Linear Models",
      loadMarkdown("teach_glm_intro.md")
    ),
  )
}

generateTeachTabPanel <- function(){
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
        nav_panel("5. Example",
          fluidPage(
            wizardUI(student_id, pages=list(
              generateExampleIntro(),generateExampleDataset(),generateExampleGam(),
              generateExampleAppraise()
            ))
          )
        )
      )
    )
  )
}

generateExampleIntro <- function(){
  tagList(
    withMathJax(loadMarkdown("example_welcome.md"))
  )
}

generateExampleDataset <- function(){
  tagList(
    withMathJax(loadMarkdown("example_dataset.md")),
    DT::dataTableOutput(NS(student_id, "example_data"))
  )
}

generateExampleGam <- function(){
  withMathJax(loadMarkdown("example_gam.md"))
}
  
generateExampleAppraise <- function(){
  tagList(
    withMathJax(loadMarkdown("example_appraise.md")),
    verbatimTextOutput(NS(student_id,"userModelSummary")),
    verbatimTextOutput(NS(student_id,"userModelGamCheck")),
    plotOutput(NS(student_id,"userModelAppraisal"))
  )
}

# Functions sourced from Mastering Shiny book https://mastering-shiny.org/scaling-modules.html#module-wizard
# 19.4.2 Wizard
nextPage <- function(id, i) {
  actionButton(NS(id, paste0("go_", i, "_", i + 1)), "next")
}
prevPage <- function(id, i) {
  actionButton(NS(id, paste0("go_", i, "_", i - 1)), "prev")
}

wrapPage <- function(title, page, button_left = NULL, button_right = NULL) {
  tabPanel(
    title = title, 
    fluidRow(
      column(12, page)
    ), 
    fluidRow(
      column(6, button_left),
      column(6, button_right)
    )
  )
}

wizardUI <- function(id, pages, doneButton = NULL) {
  stopifnot(is.list(pages))
  n <- length(pages)
  
  wrapped <- vector("list", n)
  for (i in seq_along(pages)) {
    # First page only has next; last page only prev + done
    lhs <- if (i > 1) prevPage(id, i)
    rhs <- if (i < n) nextPage(id, i) else doneButton
    wrapped[[i]] <- wrapPage(paste0("page_", i), pages[[i]], lhs, rhs)
  }
  
  # Create tabsetPanel
  # https://github.com/rstudio/shiny/issues/2927
  wrapped$id <- NS(id, "wizard")
  wrapped$type <- "hidden"
  do.call("tabsetPanel", wrapped)
}

### END Mastering Shiny Code
