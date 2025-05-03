###
###
###


generateAppraisalServerInfo <- function(){
  # Render the output of the gratia's appraise function on the user model
  output$userModelAppraisal <- renderPlot({
    appraise(userModel(), point_col = "steelblue", point_alpha = 0.4)
  }) |> bindEvent(buildUserModel())
}