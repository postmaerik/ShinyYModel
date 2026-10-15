#
# This is the server logic of a Shiny web application that 
# illustrates the Van Noordwijk and De Jong 1986 model
#

library(shiny)
library(MASS)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  
  # Simulate individual data points
  
  ind.data  <- reactive({
    mean.A  <- input$mean.A
    var.A   <- input$var.A
    mean.X  <- input$mean.X
    var.X   <- input$var.X
    n       <- input$n
    r.A.X   <- input$r.A.X
    
    sd.A <- var.A*mean.A
    sd.X <- var.X*mean.X
    
    cov.matrix <- matrix(c(
      sd.A^2, r.A.X * sd.A * sd.X,
      r.A.X * sd.A * sd.X, sd.X^2), nrow = 2)
    
    samples <- mvrnorm(n, mu = c(mean.A, mean.X), Sigma = cov.matrix)
    total_energy <- pmax(samples[, 1], 0)            # A
    proportion_survival <- pmin(pmax(samples[, 2], 0), 1) # X
    
    proportion_reproduction <- 1 - proportion_survival
    S <- total_energy * proportion_survival
    R <- total_energy * proportion_reproduction
    
    
    # Calculate mean R and S
    mean.R <- (1-mean.X) * mean.A
    mean.S <- mean.A - mean.R
    
    data.frame(R.vector=R, S.vector=S)
  })
  
  # Create plot
  output$Y.model.plot <- renderPlot({
    
    mean.A <- input$mean.A; var.A <- input$var.A
    mean.X  <- input$mean.X; var.X <- input$var.X
    sd.A <- var.A*mean.A
    sd.X <- var.X*mean.X
    
    min.A <- mean.A - 2 * sd.A
    max.A <- mean.A + 2 * sd.A
    
    min.X <- pmax(mean.X - 2 * sd.X, 0.001)
    max.X <- pmin(mean.X + 2 * sd.X, 0.999)
    
    par(pty='s', lwd=2, cex=1.5, mar=c(5,5,2, 2))
    plot(NA, xaxs='i', xlim=c(0, 10000), yaxs='i', ylim=c(0,10000),
         xlab= "Reproduction (Y)", ylab= "Survival (X)", cex.lab=1.3)
    axis(1, lwd=2); axis(2, lwd=2)
    
    # Generate lines
    ## Acquisition
    if ("A.lines" %in% input$show) {
      lines(x=c(0, mean.A), y=c(mean.A, 0), lty=2)
      if (var.A>0) {
        lines(x=c(0, min.A), y=c(min.A, 0), lty=0)
        lines(x=c(0, max.A), y=c(max.A, 0), lty=0)
        polygon(x = c(0, min.A, max.A, 0), y = c(min.A, 0, 0, max.A),
                col = rgb(1, 1, 0, 0.2), border = NA)
      }
    }
    
    ## Allocation
    if ("X.lines" %in% input$show){
      lines(x=c(0,10000), y=c(0, 10000 * mean.X/(1-mean.X)), lty=2)
      if (var.X>0){
        polygon(x = c(0, 10000, 10000, 0), y = c(0, 10000 * min.X/(1-min.X), 10000 * max.X/(1-max.X), 0),
                col = rgb(0,0,1,0.2), border = NA)
      }
    }
    
    # Add individual data points
    R.vector <- ind.data()$R.vector
    S.vector <- ind.data()$S.vector
    
    if("points" %in% input$show) {
      points(R.vector, S.vector, 
             pch=19, col=rgb(0,0,0,0.3), cex=2)
    }
    
    # Add regression line
    reg <- lm(S.vector ~ R.vector, ind.data())
    if ("reg" %in% input$show) lines(sort(R.vector), predict(reg)[order(R.vector)], lwd=3)
    
    # Add ellipse
    if ("ellipse" %in% input$show){
      
      A <- cov(cbind(R.vector, S.vector))
      
      ctr <- c(mean(R.vector), mean(S.vector))  
      angles <- seq(0, 2*pi, length.out=200)
      
      eigVal  <- eigen(A)$values
      eigVec  <- eigen(A)$vectors
      
      eigScl  <- 2*eigVec %*% diag(sqrt(eigVal))  # scale eigenvectors to length = square-root
      
      ellBase <- cbind(2*sqrt(eigVal[1])*cos(angles), 2*sqrt(eigVal[2])*sin(angles)) # normal ellipse
      ellRot  <- eigVec %*% t(ellBase)                                 # rotated ellipse
      
      xMat    <- rbind(ctr[1] + eigScl[1, ], ctr[1] - eigScl[1, ])
      yMat    <- rbind(ctr[2] + eigScl[2, ], ctr[2] - eigScl[2, ])  
      
      # Add to plot  
      points((ellRot+ctr)[1, ], (ellRot+ctr)[2, ], type="l", lwd=3)
      matlines(xMat, yMat, lty=1, col = "black", lwd=3) # These lines look wrong if axes are not same length (use pty='s)
      #     points(ctr[1], ctr[2], pch=4, lwd=3)
    }
    
  }, height = reactive(input$plot.size))
  
  # Correlation coefficient
  
  corr.input <- reactive({
    if(input$var.A>0 | input$var.X >0) {
      round(cor(ind.data()$R.vector, ind.data()$S.vector), 2)
    } else {
      "not estimable"
    }
  })
  
  # Regression coefficient
  reg.input <- reactive({
    if(input$var.A>0 | input$var.X >0) {
      round(lm(ind.data()$S.vector ~ ind.data()$R.vector)$coefficients[2], 2)
    } else {
      "not estimable"
    }
  })
  
  
  
  output$corr.result <- renderUI({
    withMathJax(HTML(
      
      paste("The <i>correlation</i> between reproduction and survival is <b>", corr.input(), "</b>.<br/>",
            "The <i>slope</i> of a regression of survival against reproduction is <b>", reg.input(), "</b>.<br/><br/>", sep="") 
    )
    )
  })
  
}
