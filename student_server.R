#
# Code in this section is adapted from Wood (2017, p.164-165) to allow interaction with the number of knots.
# Please see Reference list for full citation.
#
studentServer <- function(id){
  data(engine);
  moduleServer(id, function(input,output,session){
    # Generate the graph.
    output$plot_knot <- renderPlot({
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
      # %*% 
      lines(s, Xp %*% coef(b))
    })
  })
}

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