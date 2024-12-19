#' distribDiffModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_distribDiffModule_ui <- function(id){
  ns <- NS(id)

  shinydashboard::tabItem(tabName = 'distribdiff',
                          shiny::fluidRow(
                            shinyjs::useShinyjs(),
                            shiny::column(
                              width = 3,
                              shinydashboard::box(
                                width = NULL,
                                title = 'Parameters',

                                shinyWidgets::pickerInput(
                                  inputId = ns('dist'),
                                  label = 'Select Distribution:',
                                  choices = c(
                                    'Beta' = "beta",
                                    'Gaussian' = "norm",
                                    'Weibull' = "Weib",
                                    "Log Normal" = "lnorm"
                                  ),
                                  selected = 'norm'
                                ) %>%
                                  bsplus::shinyInput_label_embed(
                                    bsplus::shiny_iconlink() %>%
                                      bsplus::bs_embed_popover(
                                        title = "More info", content = "Parent distribution from which samples are drawn. Gaussian (AKA normal), Weibull, Beta, or log-normal", placement ="right"
                                )),

                                shiny::numericInput(
                                  inputId = ns('alpha'),
                                  label = 'Significance level',
                                  min = 0.00000000000000001,
                                  max = 0.5,
                                  value = 0.05
                                ) %>%
                                  bsplus::shinyInput_label_embed(
                                    bsplus::shiny_iconlink() %>%
                                      bsplus::bs_embed_popover(
                                        title = "More info", content = "Significance level or p-value cut-off level. Please note this app does not account for any multiple testing; corrections should be made outside of the app (e.g. manually apply a Bonferroni correction)", placement ="right"
                                  )),

                                shiny::sliderInput(
                                  ns("n"),
                                  label = "Total number of samples in group A and group B",
                                  min = 5,
                                  max = 500,
                                  value = c(20, 40),
                                  step = 5
                                ) %>%
                                  bsplus::shinyInput_label_embed(
                                    bsplus::shiny_iconlink() %>%
                                      bsplus::bs_embed_popover(
                                        title = "More info", content = "The total number of samples in group 'A' (left slider) and group 'B' (right slider). Sample sizes for the modes within each group are calcualted as floor(n*p) in mode 1 and floor(n*[1-p]) in mode 2", placement ="right"
                                )),

                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "norm"', ns("dist")),
                                  shiny::sliderInput(
                                    ns("p1"),
                                    label = "Proportion in mode 1 group A",
                                    min = 0.01,
                                    max = 0.99,
                                    value = .5,
                                    step = .01
                                  )  %>% bsplus::shinyInput_label_embed(
                                    bsplus::shiny_iconlink() %>%
                                      bsplus::bs_embed_popover(
                                        title = "More info", content = "The proportion in mode 2 is 1 minus the user supplied proportion", placement ="right"
                                      )
                                  ),
                                  shiny::sliderInput(
                                    ns("p2"),
                                    label = "Proportion in mode 1 group B",
                                    min = 0.01,
                                    max = 0.99,
                                    value = .5,
                                    step = .01
                                  )   %>% bsplus::shinyInput_label_embed(
                                    bsplus::shiny_iconlink() %>%
                                      bsplus::bs_embed_popover(
                                        title = "More info", content = "The proportion in mode 2 is 1 minus the user supplied proportion", placement ="right"
                                      )
                                  ),

                                  shiny::sliderInput(
                                    ns("mus1"),
                                    label = "Mean of mode 1 and mode 2 group A",
                                    value = c(0, 3),
                                    min = 0,
                                    max = 100
                                  ),
                                  shiny::sliderInput(
                                    ns("sds1"),
                                    label = "SD of mode 1 and mode 2 group A",
                                    value = c(.5, .5),
                                    min = .5,
                                    max = 10
                                  ),
                                  shiny::sliderInput(
                                    ns("mus2"),
                                    label = "Mean of mode 1 and mode 2 group B",
                                    value = c(2, 10),
                                    min = 0,
                                    max = 100
                                  ),
                                  shiny::sliderInput(
                                    ns("sds2"),
                                    label = "SD of mode 1 and mode 2 group B",
                                    value = c(.5, 2),
                                    min = .5,
                                    max = 10
                                )),

                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "lnorm"', ns("dist")),
                                  shiny::sliderInput(
                                    ns("p1"),
                                    label = "Proportion in mode 1 group A",
                                    min = 0.01,
                                    max = 0.99,
                                    value = .5,
                                    step = .01
                                  )   %>% bsplus::shinyInput_label_embed(
                                    bsplus::shiny_iconlink() %>%
                                      bsplus::bs_embed_popover(
                                        title = "More info", content = "The proportion in mode 2 is 1 minus the user supplied proportion", placement ="right"
                                      )
                                  ),
                                  shiny::sliderInput(
                                    ns("p2"),
                                    label = "Proportion in mode 1 group B",
                                    min = 0.01,
                                    max = 0.99,
                                    value = .5,
                                    step = .01
                                  )   %>% bsplus::shinyInput_label_embed(
                                    bsplus::shiny_iconlink() %>%
                                      bsplus::bs_embed_popover(
                                        title = "More info", content = "The proportion in mode 2 is 1 minus the user supplied proportion", placement ="right"
                                      )
                                  ),
                                  shiny::sliderInput(
                                    ns("mus1"),
                                    label = "Mean of mode 1 and mode 2 group A",
                                    value = c(0, 3),
                                    min = 0,
                                    max = 100
                                  ),
                                  shiny::sliderInput(
                                    ns("sds1"),
                                    label = "SD of mode 1 and mode 2 group A",
                                    value = c(.5, .5),
                                    min = .5,
                                    max = 10
                                  ),
                                  shiny::sliderInput(
                                    ns("mus2"),
                                    label = "Mean of mode 1 and mode 2 group B",
                                    value = c(2, 10),
                                    min = 0,
                                    max = 100
                                  ),
                                  shiny::sliderInput(
                                    ns("sds2"),
                                    label = "SD of mode 1 and mode 2 group B",
                                    value = c(.5, 2),
                                    min = .5,
                                    max = 10
                                  )
                                ),


                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "beta"', ns("dist")),
                                  shiny::sliderInput(
                                    ns("sp1"),
                                    label = "Shape parameters group A",
                                    value = c(.5, .75),
                                    min = .05,
                                    max = 10,
                                    step = 0.05
                                  ),
                                  shiny::sliderInput(
                                    ns("sp2"),
                                    label = "Shape parameters group B",
                                    value = c(.25, 1),
                                    min = .05,
                                    max = 10,
                                    step = 0.05
                                  )
                                ),

                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "Weib"', ns("dist")),
                                  shiny::sliderInput(
                                    ns("p1"),
                                    label = "Proportion in mode 1 group A",
                                    min = 0.01,
                                    max = 0.99,
                                    value = 0.5,
                                    step = 0.01
                                  )   %>% bsplus::shinyInput_label_embed(
                                    bsplus::shiny_iconlink() %>%
                                      bsplus::bs_embed_popover(
                                        title = "More info", content = "The proportion in mode 2 is 1 minus the user supplied proportion", placement ="right"
                                      )
                                  ),
                                  shiny::sliderInput(
                                    ns("p2"),
                                    label = "Proportion in mode 1 group B",
                                    min = 0.01,
                                    max = 0.99,
                                    value = 0.5,
                                    step = 0.01
                                  )   %>% bsplus::shinyInput_label_embed(
                                    bsplus::shiny_iconlink() %>%
                                      bsplus::bs_embed_popover(
                                        title = "More info", content = "The proportion in mode 2 is 1 minus the user supplied proportion", placement ="right"
                                      )
                                  ),
                                  shiny::sliderInput(
                                    ns("sps1"),
                                    label = "Shape parameter of mode 1 and mode 2 group A",
                                    value = c(1, 1.5),
                                    min = 0.01,
                                    max = 10
                                  ),
                                  shiny::sliderInput(
                                    ns("scs1"),
                                    label = "Scale parameter of mode 1 and mode 2 group A",
                                    value = c(1, 5),
                                    min = 0.01,
                                    max = 10
                                  ),
                                  shiny::sliderInput(
                                    ns("sps2"),
                                    label = "Shape parameter of mode 1 and mode 2 group B",
                                    value = c(2, 3),
                                    min = 0.01,
                                    max = 10
                                  ),
                                  shiny::sliderInput(
                                    ns("scs2"),
                                    label = "Scale parameter of mode 1 and mode 2 group B",
                                    value = c(2, 10),
                                    min = 0.01,
                                    max = 10
                                  )
                                ),

                                shiny::numericInput(
                                  inputId = ns('nsim'),
                                  label = 'Number of Simulations',
                                  min = 5,
                                  max = 5000,
                                  value = 10
                                )  %>%
                                  bsplus::shinyInput_label_embed(
                                    bsplus::shiny_iconlink() %>%
                                      bsplus::bs_embed_popover(
                                        title = "More info", content = "Number of simulations to run for estimation of power and false positive rate. The more simulations the more accurate and precise the estimates will be, but run time will increase", placement ="right"
                                      )
                                  ),

                                shiny::numericInput(
                                  inputId = ns('nperm'),
                                  label = 'Number of bootstraps and/or permutations',
                                  min = 5,
                                  max = 5000,
                                  value = 10
                                ) %>%
                                  bsplus::shinyInput_label_embed(
                                    bsplus::shiny_iconlink() %>%
                                      bsplus::bs_embed_popover(
                                        title = "More info", content = "Number of bootstrap datatsets to create for tests that utilize resampling. The higher this is the more accurate the results but the longer the run time", placement ="right"
                                      )
                                  ),

                                shinyWidgets::pickerInput(
                                  inputId = ns('effect'),
                                  label = 'Select Effect:',
                                  choices = c('Mean', 'Variance', 'Distribution'),
                                  selected = 'Mean'
                                ) %>%
                                  bsplus::shinyInput_label_embed(
                                    bsplus::shiny_iconlink() %>%
                                      bsplus::bs_embed_popover(
                                        title = "More info", content = "Drop down menu that will bring up options to test for mean differences, variance, and/or distribution differneces", placement ="right"
                                      )
                                  ),

                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "Distribution"', ns("effect")),
                                  shiny::checkboxGroupInput(
                                    inputId = ns('meanvar'),
                                    label = 'Select Test(s):',
                                    choices = c('Anderson-Darling' = "ad",
                                               'Kolmogorov–Smirnov' = "ks",
                                               'Cramer-Von Mises' = "cvm",
                                               'DTS' = "dts")
                                  )
                                ),

                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "Mean"', ns("effect")),
                                  shiny::checkboxGroupInput(
                                    inputId = ns('meaneff'),
                                    label = 'Mean Methods',
                                    choices = c('ANOVA',
                                                'Non-parametric ANOVA',
                                                'Permutations (Raw)'),
                                    selected = 'ANOVA'
                                  )
                                ),

                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "Variance"', ns("effect")),
                                  shiny::checkboxGroupInput(
                                    inputId = ns('vareff'),
                                    label = 'Variance Methods',
                                    choices = c('Levene',
                                                'Permutations (MAD)'),
                                    # 'Permutation (Gini Index)'),
                                    selected = 'Levene'
                                  )
                                ),

                                shiny::h5("To run simulations, be sure all of your parameters are set as desired and hit the 'Run Simulation' button. Please be patient as simualtions can take awhile to run."),

                                shinyWidgets::actionBttn(inputId = ns("runsim3"),
                                                         label = "Run Simulation",
                                                         style = "gradient",
                                                         color = "primary",
                                                         size = "sm"),


                                shiny::h5("To save parameters, enter file name and click the Download button:"),

                                shiny::textInput(
                                  inputId = ns("filename"),
                                  label = "File Name",
                                  value = "params3"
                                ),

                                shinyWidgets::downloadBttn(
                                  outputId = ns("downloadParams"),
                                  label = "Download",
                                  style = "gradient",
                                  color = "primary",
                                  size = "sm"
                                )
                              ) # END BOX
                            ), # END FIRST COLUMN

                            shiny::column(
                              width = 9,
                              shinydashboard::box(width = NULL,
                                                  title = 'Comparing two Bimodal Groups',
                                                  # verbatimTextOutput(ns('description')),
                                                  plotly::plotlyOutput(ns("pplt")),
                                                  DT::dataTableOutput(ns("paramsTable"))
                              ) # END box
                            ) # END column
                          ) # END fluidRow
  ) # END tabItem
}

#' distribDiffModule Server Functions
#'
#' @noRd
mod_distribDiffModule_server <- function(id){
  moduleServer( id, function(input, output, session){
    vals <- reactiveValues(ss = data.frame(Test=rep("Example of selected tests",2), Sample.sizes = "[n1, n2]",value=c(1,0),variable=c("Power","FP")),
                           tbl = data.frame(Distribution = "TBD",Parameters_Group_A = c(""),Parameters_Group_B = c(""), Test="Example of selected tests", Power=1,FP = 0, "Sample.sizes" = "[n1, n2]", Proportions = "[p1, p2]"))

    #If any params are reset, the output plot should be reset accordingly
    observeEvent(input$dist ,{vals$ss = data.frame(Test=rep("Example of selected tests",2),  "Sample.sizes" = "[n1, n2]", Proportions = "[p1, p2]",value=c(1,0),variable=c("Power","FP"))})
    observeEvent(input$n ,{vals$ss = data.frame(Test=rep("Example of selected tests",2),  "Sample.sizes" = "[n1, n2]", Proportions = "[p1, p2]",value=c(1,0),variable=c("Power","FP"))})
    observeEvent(input$mus1 ,{vals$ss = data.frame(Test=rep("Example of selected tests",2),  "Sample.sizes" = "[n1, n2]", Proportions = "[p1, p2]",value=c(1,0),variable=c("Power","FP"))})
    observeEvent(input$sds1 ,{vals$ss = data.frame(Test=rep("Example of selected tests",2),  "Sample.sizes" = "[n1, n2]", Proportions = "[p1, p2]",value=c(1,0),variable=c("Power","FP"))})
    observeEvent(input$sps1 ,{vals$ss = data.frame(Test=rep("Example of selected tests",2),  "Sample.sizes" = "[n1, n2]", Proportions = "[p1, p2]",value=c(1,0),variable=c("Power","FP"))})
    observeEvent(input$scs1 ,{vals$ss = data.frame(Test=rep("Example of selected tests",2),  "Sample.sizes" = "[n1, n2]", Proportions = "[p1, p2]",value=c(1,0),variable=c("Power","FP"))})
    observeEvent(input$sp1 ,{vals$ss = data.frame(Test=rep("Example of selected tests",2),  "Sample.sizes" = "[n1, n2]", Proportions = "[p1, p2]",value=c(1,0),variable=c("Power","FP"))})
    observeEvent(input$p1 ,{vals$ss = data.frame(Test=rep("Example of selected tests",2),  "Sample.sizes" = "[n1, n2]", Proportions = "[p1, p2]",value=c(1,0),variable=c("Power","FP"))})
    observeEvent(input$mus2 ,{vals$ss = data.frame(Test=rep("Example of selected tests",2),  "Sample.sizes" = "[n1, n2]", Proportions = "[p1, p2]",value=c(1,0),variable=c("Power","FP"))})
    observeEvent(input$sds2 ,{vals$ss = data.frame(Test=rep("Example of selected tests",2),  "Sample.sizes" = "[n1, n2]", Proportions = "[p1, p2]",value=c(1,0),variable=c("Power","FP"))})
    observeEvent(input$sps2 ,{vals$ss = data.frame(Test=rep("Example of selected tests",2),  "Sample.sizes" = "[n1, n2]", Proportions = "[p1, p2]",value=c(1,0),variable=c("Power","FP"))})
    observeEvent(input$scs2 ,{vals$ss = data.frame(Test=rep("Example of selected tests",2),  "Sample.sizes" = "[n1, n2]", Proportions = "[p1, p2]",value=c(1,0),variable=c("Power","FP"))})
    observeEvent(input$sp2 ,{vals$ss = data.frame(Test=rep("Example of selected tests",2),  "Sample.sizes" = "[n1, n2]", Proportions = "[p1, p2]",value=c(1,0),variable=c("Power","FP"))})
    observeEvent(input$p2 ,{vals$ss = data.frame(Test=rep("Example of selected tests",2),  "Sample.sizes" = "[n1, n2]", Proportions = "[p1, p2]",value=c(1,0),variable=c("Power","FP"))})
    observeEvent(input$nperm ,{vals$ss = data.frame(Test=rep("Example of selected tests",2),  "Sample.sizes" = "[n1, n2]", Proportions = "[p1, p2]",value=c(1,0),variable=c("Power","FP"))})
    observeEvent(input$nsim ,{vals$ss = data.frame(Test=rep("Example of selected tests",2),  "Sample.sizes" = "[n1, n2]", Proportions = "[p1, p2]",value=c(1,0),variable=c("Power","FP"))})
    observeEvent(input$alpha ,{vals$ss = data.frame(Test=rep("Example of selected tests",2),  "Sample.sizes" = "[n1, n2]", Proportions = "[p1, p2]",value=c(1,0),variable=c("Power","FP"))})

    observeEvent(input$runsim3,{
      if(input$dist %in% c("norm","lnorm")){
        param = list(p_1 = input$p1,p_2=input$p2,
                     mu1_1 = input$mus1[1],
                     mu2_1 = input$mus1[2],
                     mu1_2 = input$mus2[1],
                     mu2_2 = input$mus2[2],
                     sd1_1 = input$sds1[1],
                     sd2_1 = input$sds1[2],
                     sd1_2 = input$sds2[1],
                     sd2_2 = input$sds2[2])

        ## est_pow_2samp.R was written to accpt LNorm, mkaing the switch here instead of changing all functions.
        ## long-term maintenance need to harmonize these
        dist.pass = ifelse(input$dist == "lnorm","LNnorm",input$dist)
        calcs = data.frame(as.data.frame(est_pow_2samp(input$n[1],
                                                           input$n[2],
                                                           input$alpha,
                                                           input$nsim,
                                                           modes = 2,
                                                           dist = dist.pass,
                                                           params = param,
                                                           c(input$meaneff,
                                                             input$vareff,
                                                             input$meanvar),
                                                             input$nperm)),
                              Sample.sizes = paste0("[",input$n[1],", ", input$n[2],"]"),
                              Proportions = paste0("[",input$p1,", ", input$p1,"]"),
                              Parameters_Group_A = paste(paste0("mus: ",paste(input$mus1,collapse=", ")),paste0("sds: ",paste(input$sds1,collapse=", ")),collapse="; "),
                              Parameters_Group_B = paste(paste0("mus: ",paste(input$mus2,collapse=", ")),paste0("sds: ",paste(input$sds2,collapse=", ")),collapse="; "))

      } else {
        if(input$dist == "beta"){

          param = list(s1_1 = input$sp1[1],s2_1 = input$sp1[2],
                       s1_2 = input$sp2[1],s2_2 = input$sp2[2])

          calcs =  data.frame(as.data.frame(est_pow_2samp(input$n[1],
                                                              input$n[2],
                                                              input$alpha,
                                                              input$nsim,
                                                              modes = 2,
                                                              dist= 'beta',
                                                              params=param,
                                                              c(input$meaneff,input$vareff,input$meanvar),
                                                              input$nperm)),
                                 Sample.sizes = paste0("[",input$n[1],", ", input$n[2],"]"),
                                 Proportions = paste0("[",input$p1,", ", input$p1,"]"),
                                 Parameters_Group_A = paste0("mus: ",paste(input$sp1,collapse=", ")),
                                 Parameters_Group_B = paste0("mus: ",paste(input$sp2,collapse=", ")))
        } else {
          if(input$dist == "Weib"){
            param = list(p_1 = input$p1,p_2=input$p2,
                         sp1_1 = input$sps1[1],
                         sp2_1 = input$sps1[2],
                         sc1_1 = input$scs1[1],
                         sc2_1 = input$scs1[2],
                         sp1_2 = input$sps2[1],
                         sp2_2 = input$sps2[2],
                         sc1_2 = input$scs2[1],
                         sc2_2 = input$scs2[2])

            calcs = data.frame(as.data.frame(est_pow_2samp(input$n[1],
                                                               input$n[2],
                                                               input$alpha,
                                                               input$nsim,
                                                               modes = 2,
                                                               dist = 'weib',
                                                               params = param,
                                                               c(input$meaneff,
                                                                 input$vareff,
                                                                 input$meanvar),
                                                                 input$nperm)),
                                  Sample.sizes = paste0("[",input$n[1],", ", input$n[2],"]"),
                                  Proportions = paste0("[",input$p1,", ", input$p1,"]"),
                                  Parameters_Group_A = paste(paste0("Shapes: ",paste(input$sps1,collapse=", ")),paste0("Scales: ",paste(input$scs1,collapse=", ")),collapse="; "),
                                  Parameters_Group_B = paste(paste0("Shapes: ",paste(input$sps2,collapse=", ")),paste0("Scales: ",paste(input$scs2,collapse=", ")),collapse="; "))

          }
        }
      }
      vals$ss = vals$ss[vals$ss$Test != "Example of selected tests",]
      vals$tbl = vals$tbl[vals$tbl$Test != "Example of selected tests",]

      vals$ss <- reshape2::melt(calcs,id.vars=c("Test","Sample.sizes","Parameters_Group_A","Parameters_Group_B","Proportions"))
      calcs$Distribution = input$dist

      vals$tbl <-  rbind(vals$tbl,calcs)
    })


    output$pplt <- plotly::renderPlotly({
      p1 = ggplot2::ggplot() +
        ggplot2::theme_classic(14) +
        ggplot2::ylab("Population density") +
        ggplot2::xlab("Modes") +
        ggplot2::theme(legend.text = ggplot2::element_text(10))

      if(input$dist == "norm"){
        p1 = p1 + ggplot2::stat_function(fun = function(x) {(dnorm(x, mean = input$mus1[1], sd = input$sds1[1]) * input$p1)  +
            (dnorm(x, mean = input$mus1[2], sd = input$sds1[2]) * (1-input$p1)) },
            alpha=0.5,linewidth=0.7,color = "black") +

          ggplot2::stat_function(fun = function(x) {(dnorm(x, mean = input$mus2[1], sd = input$sds2[1]) * input$p2)  +
              (dnorm(x, mean = input$mus2[2], sd = input$sds2[2]) * (1-input$p2)) },
              alpha=0.5,linewidth=0.7,color = "blue")     +

        ggplot2::scale_x_continuous(limits=c(min(input$mus1[1],input$mus2[1]) - 3*max(input$sds1,input$sds2)  , max(input$mus1[2],input$mus2[2]) + 3*max(input$sds1,input$sds2)))



      }

      if(input$dist == "beta"){
        p1 = p1 + ggplot2::stat_function(fun = function(x) {(dbeta(x, shape1 = input$sp1[1], shape2 = input$sp1[2]))  },
                                         alpha=0.5,linewidth=0.7,color = "black") +
                  ggplot2::stat_function(fun = function(x) {(dbeta(x, shape1 = input$sp2[1], shape2 = input$sp2[2]))  },
                                         alpha=0.5,linewidth=0.7,color = "blue")+
                  ggplot2::scale_x_continuous(limits=c(0,1))

      }

      if(input$dist == "Weib"){

        p1 = p1 + ggplot2::stat_function(fun = function(x) {(dweibull(x, shape = input$sps1[1], scale = input$scs1[1]) * input$p1)  +
            (dweibull(x, shape = input$sps1[2], scale = input$scs1[2]) * (1-input$p1)) },alpha=0.5,linewidth=0.7,color = "black") +
          ggplot2::stat_function(fun = function(x) {(dweibull(x, shape = input$sps2[1], scale = input$scs2[1]) * input$p2)  +
                (dweibull(x, shape = input$sps2[2], scale = input$scs2[2]) * (1-input$p2)) },alpha=0.5,linewidth=0.7,color = "blue") +
          ggplot2::scale_x_continuous(limits=c(0  , 5*max(input$scs1,input$scs2)*log(2)^(1/min(input$sps1,input$sps2))))

      }

      if(input$dist == "lnorm"){
        p1 = p1 + ggplot2::stat_function(fun = function(x) {(dlnorm(x, mean = input$mus1[1], sd = input$sds1[1]) * input$p1)  +
            (dlnorm(x, mean = input$mus1[2], sd = input$sds1[2]) * (1-input$p1)) },
            alpha=0.5,linewidth=0.7,color = "black") +
                  ggplot2::stat_function(fun = function(x) {(dlnorm(x, mean = input$mus2[1], sd = input$sds2[1]) * input$p2)  +
                    (dlnorm(x, mean = input$mus2[2], sd = input$sds2[2]) * (1-input$p2)) },
                     alpha=0.5,linewidth=0.7,color = "blue")  +
          ggplot2::scale_x_continuous(limits=c(0  , max(input$mus1[2],input$mus2[2]) + 6*max(input$sds1,input$sds2)))
      }


      fig1 = plotly::ggplotly(p1)

      # p2 = plotly::ggplotly(ggplot2::ggplot(data=ss()[["calcs"]],
      #                                       ggplot2::aes(x=Test,y=value,color=variable)) +
      #                         ggplot2::geom_point(position = ggplot2::position_dodge(width = .25)) +
      #                         ggplot2::theme_classic(14) +
      #                         ggplot2::ylab("Probability") +
      #                         ggplot2::xlab("Test") +
      #                         ggplot2::theme(legend.title=ggplot2::element_blank(),
      #                                        legend.text = ggplot2::element_text(10),
      #                                        axis.text.x = ggplot2::element_text(angle=45,hjust=1)) +
      #                         ggplot2::scale_color_manual(values=c("black","red")) +
      #                         ggplot2::coord_cartesian(ylim=c(-.02,1.01)))

      if(vals$ss$Test[1] != "Example of selected tests" ){
        p2 = ggplot2::ggplot(data =vals$ss, ggplot2::aes(
          x = Test, y = value, color = variable
        )) + ggplot2::geom_point(position = ggplot2::position_dodge(width = .25)) +
          ggplot2::theme_classic(14) +
          ggplot2::ylab("Probability") +
          ggplot2::xlab("Test") +
          ggplot2::labs(color=NULL)+
          ggplot2::theme(
            legend.title = ggplot2::element_blank(),
            legend.text = ggplot2::element_text(10),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
          ) +
          ggplot2::scale_color_manual(values = c("black", "red")) +
          ggplot2::coord_cartesian(ylim = c(-.01, 1.01))
      }  else {
        p2 = ggplot2::ggplot()+
          ggplot2::geom_text(ggplot2::aes(1,0.5,label="Please run simulation to plot results",color="darkred"))+
          ggplot2::theme_classic()+ggplot2::theme(legend.key = ggplot2::element_rect(fill = "white"), legend.text = ggplot2::element_text(color = "white"), legend.title = ggplot2::element_text(color = "white")) +
          ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(color = NA)))+
          ggplot2::scale_y_continuous(limits=c(0,1))
      }

      fig2 = plotly::ggplotly(p2)

      plotly::subplot(fig1, fig2, nrows=1,margin=c(0.02,0.02,.21,.21))

    })



    output$paramsTable <- DT::renderDataTable( vals$tbl )

    output$downloadParams <- shiny::downloadHandler(

      filename = function() {
        paste0(input$filename, ".csv")
      },

      content = function(file) {
        write.csv(paramsTable(), file, row.names = FALSE)
      }
    )
  })
}

## To be copied in the UI
# mod_distribDiffModule_ui("distribDiffModule_1")

## To be copied in the server
# mod_distribDiffModule_server("distribDiffModule_1")
