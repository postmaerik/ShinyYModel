#
# This is the user-interface definition of a Shiny web application that 
# illustrates the Van Noordwijk and De Jong 1986 model
#

library(shiny)
library(bslib)
fluidPage(
  theme = bs_theme(version = 5, bootswatch = "default"),
  
  withMathJax(),
  # section below allows in-line LaTeX via $ in mathjax
  tags$div(HTML("<script type='text/x-mathjax-config'>
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$']]}
                });
                </script>
                ")),
  HTML("<h3>A big house <i>and</i> a big car?</h3>"),
  HTML("<p><b>Trade-off's are fundamental to life-history theory, and to much of behavioural 
               and evolutionary ecology. They can explain why, for example,  
               selection does not always favour the production of more offspring, 
               and why there are no <i>Darwinian demons</i>. Although
               we would therefore expect negative correlations among life-history
               traits to be the rule, it turns out that they rather are the exception. Why do we so
               often find positive correlations where negative correlations are
               expected? Here you can explore why this is.</b></p>"),
  checkboxInput("more", "Check the box to read more", value = FALSE),
  conditionalPanel(condition = 'input["more"]==true',
                   HTML("<p>It was Van Noordwijk and De Jong's Y-model, published
               in <i>The American Naturalist</i> in 1986, that provided an explanation
               for this finding (<a href='https://www.jstor.org/stable/2461293' target='_blank'>link to pdf on JSTOR</a>).
               This interactive Shiny app provides a visual 
               illustration of their model, and why we tend to see big cars parked in
           front of big houses, even though you can only spend your money once, either 
           on the house or on the car.</p>"),
                   h4("The model"),
                   HTML("<p>An individual has a certain amount of resources at its disposal, which we
               call $A$. This $A$ stands for <i>acquisition</i>, which measures an individual's 
               ability to acquire resources. We don't make any assumptions about 
               what determines an individual's resource acquisition, or what drives
               variation in $A$ among individuals (denoted by $\\sigma^2(A)$).</p>"),
                   HTML("<p>An individual can invest the resources at its disposal in either
               <i>reproduction</i> ($R$) or <i>survival</i> ($S$). Hence, there is a
               <i>trade-off</i> between both life-history traits:</p>"),
                   HTML("$$\\ A = S + R $$"),
                   HTML("How much of $A$ an individual <i>allocates</i> to $S$ and $R$ is given by $b$, which
               is the proportion of $A$ it allocates to $S$. Hence, the proportion 
               of $A$ allocated to $R$ is equal to $1-b$:<br>"),
                   HTML("$$\\ A = b S + (1-b) R$$"),
                   HTML("<p>Again, we make no assumptions about what determines an individual's 
               allocation $b$, or variation in $b$ among individuals ($\\sigma^2(b)$).</p>"),
                   HTML("<p>Although $A$ is typically unknown, we can measure $S$ and $R$, and most importantly,
          quantify the correlation between the two. Because resources can either be allocated to
          $R$ <i>or</i> $S$, <i>i.e.</i> there is a trade-off, we expect them to be negatively correlated. 
              But are they? How is 
               is this correlation affected by variation in acquisition ($\\sigma^2(A)$) and 
               alloction ($\\sigma^2(b)$)? Use the sliders on the left to explore this.</p>")
  ),
  
  
  # Sidebar with slider inputs
  sidebarLayout(
    sidebarPanel(
      width=4,
                 h3("Parameters"),
                 HTML("<i>Mean</i> resource aquisition $\\bar{A}$:"),
                 sliderInput("mean.A",
                             label=NULL,
                             min = 0,
                             max = 200,
                             value = 100),
                 HTML("<i>Variance</i> in resource acquisition $\\sigma^2(A)$:"),
                 sliderInput("var.A",
                             label=NULL,
                             min = 0,
                             max = 1500,
                             value = 0),
                 HTML("<hr>"),
                 HTML("<i>Mean</i> resource allocation ($\\bar{b}$):"),
                 sliderInput("mean.b",
                             label=NULL,
                             min = 0,
                             max = 1,
                             value = 0.5),
                 HTML("<i>Variance</i> in resource allocation  ($\\sigma^2(b)$):"),
                 sliderInput("var.logit.b",
                             label = NULL,
                             min = 0,
                             max = 1,
                             value = 0,
                             step = 0.01),
                 HTML("<hr>"),
                 checkboxGroupInput("show", "Plot:", 
                                    c("Acquisition" = "A.lines",
                                      "Allocation" = "b.lines",
                                      "Individual data points" = "points",
                                      "Regression line" = "reg",
                                      "Ellipse" = "ellipse"
                                    )
                 ),
      HTML("<hr>"),
      sliderInput(inputId = "plot.size",
                  label = "Size:",
                  min = 100,
                  max = 1000,
                  value = 500,
                  step = 50, post = " px"),
      HTML(
        "<p style=\"font-size:10px; text-align:left\">Written by Erik Postma | e.postma@exeter.ac.uk</p>"
      )
      
      
    ),
    
    # Create plot
    mainPanel(
      htmlOutput("corr.result"),
      plotOutput("Y.model.plot")
      
    ) # close mainPanel
  )
)
