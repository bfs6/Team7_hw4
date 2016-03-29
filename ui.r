library(shiny)

shinyUI(
  fluidPage(
    titlePanel( "socks"),
    sidebarPanel(
      h4('how many simulations'),
      numericInput("sims", "simulations", value =1e4, min =0, max=1e7),
      hr(),
      h4("observed sock data"),
      sliderInput("n_pairs", "Actual pairs observed", value=0, min=0, max=50),
      
      sliderInput("n_odds", "Actual odds observed", value=11, min=0, max=50),
      hr(),
      
      
      selectInput("priorsock", "Prior for socks:", c("Negative Binomial"="neg", "Poisson"="pois", "Binomial"="bin")),
      conditionalPanel(
        condition = "input.priorsock == 'neg'",
        sliderInput("p_mu", "mu", value=30, min=0, max=99),
        sliderInput("p_sd", "sd", value=15, min=0, max=99)
      ),
      conditionalPanel(
        condition = "input.priorsock == 'pois'",
        sliderInput("lambda", "lambda", value=30, min=0, max=50)
        
      ),
      conditionalPanel(
        condition = "input.priorsock == 'bin'",
        sliderInput("nmu", "mu", value=50, min=0, max=100),
        sliderInput("np", "p", value=0.6, min=0, max=1, step=0.05)
      ),
      
      hr(),
      selectInput("priorpair", "Priors for proportion of pairs:", c("Beta"="beta", "Uniform"="uni", "Truncated-normal"="tnorm")),
      conditionalPanel(
        condition = "input.priorpair == 'beta'",
        sliderInput("prop_a", "alpha", value=15, min=0, max=50),
        sliderInput("prop_b", "beta", value=2, min=0, max=50)
      ),
      conditionalPanel(
        condition = "input.priorpair == 'uni'",
        sliderInput("unif_range","range", value=c(0.5,1), min=0, max=1)
        
      ),
      conditionalPanel(
        condition = "input.priorpair == 'tnorm'",
        sliderInput("t_mu", "mu", value=0.75, min=0, max=1),
        sliderInput("t_sd", "sd", value=0.2, min=0, max=3, step=0.05)
      ),
      

      checkboxGroupInput("sumprior", label = h3("summary on prior"), 
                         choices = list("show mean" = 1, "show median" = 2, "show confidence intervals"=3)
      ),
      checkboxGroupInput("sumpost", label = h3("summary on posterior"), 
                         choices = list("show mean" = 1, "show median" = 2, "show confidence intervals"=3)
      ),
      selectInput("core", label = h3("cores"), 
                  choices = list(" 1" = 1, " 4" = 4, "16" = 16, "24"=24), 
                  selected = "16"),
      selectInput("displayans", "See Karls Answer:", c( "No"="no", "Yes"="yes"), selected="no")
      
    ),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Prior Plot", h2("Prior Plots"), plotOutput("prior"), plotOutput("prop"), 
                           plotOutput('pairs'), plotOutput('odds')
                  ), 
                  tabPanel("Prior Summary",h4("summary of prior socks"), hr(),  verbatimTextOutput('priorsocksum'),
                           h4("summary of prior pair proportions"), hr(),  verbatimTextOutput('priorpropsum'),
                           h4("summary of prior pairs"), hr(),  verbatimTextOutput('priorpriorpairsum'),
                           h4("summary of prior odds"), hr(),  verbatimTextOutput('priorprioroddsum')),
                  tabPanel("Posterior Plots" , h2("Posterior Plots"), plotOutput("post"), plotOutput("postprop"), 
                           plotOutput('postpairs'), plotOutput('postodds')),
                  tabPanel("Posterior Summary",h4("summary of posterior socks"), hr(),  verbatimTextOutput('postsocksum'),
                           h4("summary of posterior pair proportions"), hr(),  verbatimTextOutput('postpropsum'),
                           h4("summary of posterior pairs"), hr(),  verbatimTextOutput('postpairsum'),
                           h4("summary of posterior odds"), hr(),  verbatimTextOutput('postoddsum')),
                  tabPanel("Karl Bromans Answer", imageOutput("answer"))
                  
      )
      
    )
  )
  
  
)

