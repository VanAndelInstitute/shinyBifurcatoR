#' analysisModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import decisionSupportExtra patchwork ggbeeswarm
mod_analysisModule_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(tabName = 'analysis',
                          shiny::fluidRow(
                            column(
                              width = 3,
                              height=15,
                              shinydashboard::box(
                                width = NULL,
                                title = 'Settings',

                                shiny::fileInput(ns("file_name"),"Please input a two column csv file (One column 'group' names, second column 'value'",accept=".csv"),

                                shiny::numericInput(ns("alpha"),
                                             label = "Significance Level (adjusted for multiple testing)",min = 0.0000000001, max = 0.999, value =0.05),


                                shiny::numericInput(
                                  inputId = ns('nboot'),
                                  label = 'Number of Permutation or Bootstrap Resamples',
                                  min = 10,
                                  max = 5000,
                                  value = 100
                                ),

                                shinyWidgets::pickerInput(
                                  inputId = ns('effect'),
                                  label = 'Select Effect:',
                                  choices = c('Mean', 'Variance','Bimodality', 'Mean-Variance'),
                                  selected = 'Mean'
                                ),

                                div(id = "add"),


                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "Mean-Variance"', ns("effect")),
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
                                                'Permutations (Raw)')
                                  )
                                ),

                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "Variance"', ns("effect")),
                                  shiny::checkboxGroupInput(
                                    inputId = ns('vareff'),
                                    label = 'Variance Methods',
                                    choices = c('Levene',
                                                'Permutations (MAD)',
                                                'Permutations (Gini Index)',
                                                'Permutations (SD)')
                                  )
                                ),

                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "Bimodality"', ns("effect")),
                                  shiny::checkboxGroupInput(
                                    inputId = ns('bimode'),
                                    label = ("Bimodality Tests"),
                                    choices = list(
                                      "Hartigans dip test" = "dip",
                                      "Mclust" = "mclust",
                                      "Gaussian mixR" = "GmixR",
                                      "Weibull mixR" = "WmixR",
                                      "Bimodality Coefficient" = "mt",
                                      "Silverman Bandwidth test" = "SI",
                                      "Hall and York Bandwidth test" = "HY",
                                      "Cheng and Hall Excess Mass" = "CH",
                                      "Ameijeiras-Alonso et al. Excess Mass" = "ACR",
                                      "Fisher and Marron Carmer-von Mises" = "FM"))
                                ),
                                actionButton(ns("analysisButton"), "Analyze data"),
                                uiOutput(outputId = ns("choice"))
                                # actionButton(ns("plotButton"), "Plot data")
                              )
                            ),

                            shiny::column(
                              width = 9,
                              shinydashboard::box(width = NULL,
                                                  height=600,
                                                  title = 'Descriptive Plots of Your Data',
                                                  shiny::plotOutput(ns("pplt"))
                              ) # END box
                            ), # END column

                            shiny::column(
                              width = 9,
                              shinydashboard::box(width = NULL,
                                                  height=200,
                                                  title = 'Results',
                                                  DT::dataTableOutput(ns("resTable"))
                              ) # END box
                            ) # END column
                          ) # END fluidRow
  ) # END tabItem
}


mod_analysisModule_server <- function(id) {
  ns <- shiny::NS(id)
  moduleServer(id,
               function(input, output, session) {

              vals <- reactiveValues(tbl=NULL,upld.file=NULL)
              observeEvent(input$effect,{
                if(input$effect == "Bimodality"){
                  if(is.null(input$choice)){
                    insertUI(
                      selector = "#add",
                      where = "afterBegin",
                      session = session,
                      immediate = T,
                      ui = shinyWidgets::pickerInput(inputId = ns("choice"),
                                                     label = "Choose Group",
                                                     selected = vals$upld.file$group[1],
                                                     choices = unique(vals$upld.file$group))
                    )
                  } else {
                    shinyWidgets::updatePickerInput(
                      session = session,
                      inputId = 'choice',
                      label = 'Choose Group',
                      selected = vals$upld.file$group[1],
                      choices = unique(vals$upld.file$group)
                    )
                  }

                }
              })

              observeEvent(input$file_name,{
                   vals$tbl = NULL
                   output$resTable <- DT::renderDataTable({
                     vals$tbl
                   })
                   file = input$file_name
                   upld.file = read.csv(file$datapath,header=T)
                   upld.file = na.omit(upld.file)
                   upld.file$value = as.numeric(as.character(upld.file$value))
                   vals$upld.file <- upld.file
                   data = upld.file
                   if(length(unique(data$group)) <2){
                   shinyWidgets::updatePickerInput(
                       session = session,
                       inputId = 'effect',
                       label = 'Select Effect:',
                       choices = c('Bimodality'),
                       selected = 'Bimodality'
                     )
                   } else {
                     shinyWidgets::updatePickerInput(
                       session = session,
                       inputId = 'effect',
                       label = 'Select Effect:',
                       choices = c('Mean', 'Variance', 'Bimodality','Mean-Variance'),
                       selected = 'Mean'
                     )
                   }

                   if(!is.null(input$choice)){
                     shinyWidgets::updatePickerInput(
                       session = session,
                       inputId = 'choice',
                       label = 'Choose Group',
                       selected = vals$upld.file$group[1],
                       choices = unique(vals$upld.file$group)
                     )
                   }

                   print(length(unique(data$group)))
                   print(input$effect)
                output$pplt <- shiny::renderPlot({
                  if(length(unique(data$group))>1 & input$effect != "Bimodality"){

                    p1 = ggplot2::ggplot(data, ggplot2::aes(x = group, y = value, color = group, fill = group)) +
                      ggplot2::scale_color_manual(values = c("darkorchid4","springgreen3","darkblue","orange")) +
                      ggplot2::scale_fill_manual(values = c("darkorchid4","springgreen3","darkblue","orange")) +
                      ggbeeswarm::geom_quasirandom() +
                      geom_boxplot(fill=NA, outliers = F, notch = F) +
                      stat_summary(fun=mean, geom="point", size=4,shape=5) +
                      ggplot2::theme_classic(16) +
                      theme(legend.position = "none")


                    p2 = ggplot_descdist.cust(data)

                    p3 = ggplot2::ggplot(data, ggplot2::aes(x = value, color = group)) +
                      ggplot2::scale_color_manual(values = c("darkorchid4","springgreen3","darkblue","orange")) +
                      ggplot2::scale_fill_manual(values = c("darkorchid4","springgreen3","darkblue","orange")) +
                      ggplot2::geom_density(fill=NA,linewidth=1.5) +
                      ggplot2::theme_classic(16) +
                      theme(legend.position = "none")

                    p4 = plot_spacer() + ggplot_table(data) +plot_layout(ncol=1)

                    p1 + p2 + p3 + p4 +plot_layout(ncol=2)

                  } else {
## Set the color based on number of groups.
                    subdata = data[data$group == input$choice,]
                    if(length(unique(data$group))>1){
                      ix = which(unique(data$group) == input$choice)
                      color = c("darkorchid4","springgreen3","darkblue","orange")[ix]
                    } else { color = "darkorchid4"}
                    p1 = ggplot2::ggplot(data=subdata, ggplot2::aes(x = group, y = value, color = group, fill = group)) +
                      ggplot2::scale_color_manual(values =color) +
                      ggplot2::scale_fill_manual(values =color)

                    p1 = p1 +  ggplot2::geom_boxplot(
                        width = .15, fill = "white", outlier.shape = 17, notch = F
                      ) +
                      ggdist::stat_halfeye(
                        color = NA, ## remove slab interval
                        position = ggplot2::position_nudge(x = .15),
                        trim=FALSE,
                        alpha=0.75,
                        width = .67,
                      ) +
                      gghalves::geom_half_point(
                        side = "l", range_scale = .25,
                        alpha = .5, size = 2,
                        position = ggplot2::position_nudge(x = -.15)
                      ) +
                      ggplot2::theme_classic(16) +
                      ggplot2::theme(legend.text = ggplot2::element_text(10),
                                     strip.background = ggplot2::element_blank(),
                                     axis.text.x = ggplot2::element_blank(),
                                     axis.ticks.x = ggplot2::element_blank(),
                                     legend.title = ggplot2::element_blank(),
                                     axis.line.x = ggplot2::element_blank(),
                                     legend.position = "none")+
                      ggplot2::xlab("") + ggplot2::ylab("")+ggtitle(paste0("Raincloud plot for ", input$choice))

                    p1 = p1 +
                      ggplot2::theme(legend.text = ggplot2::element_text(10),
                                     strip.background = ggplot2::element_blank(),
                                     axis.text.y = ggplot2::element_blank(),
                                     axis.ticks.y = ggplot2::element_blank(),
                                     legend.title = ggplot2::element_blank(),
                                     axis.line.y = ggplot2::element_blank(),
                                     legend.position = "none")+
                      ggplot2::ylab("") + ggplot2::xlab("") + ggplot2::coord_flip(clip="off")+
                      annotate(geom="text",x = c(1.65),y = max(subdata$value,na.rm=T)*1.1,2,label=paste0("Mean = ", round(mean(data$value,na.rm=T),2)),color=color)+
                      annotate(geom="text",x = c(1.5),y = max(subdata$value,na.rm=T)*1.1,label=paste0("SD = ", round(sd(data$value,na.rm=T),2)),color=color) +
                      annotate(geom="text",x = c(1.15),y = max(subdata$value,na.rm=T)*1.1,label=paste0("Median = ", round(median(data$value,na.rm=T),2)),color=color) +
                      annotate(geom="text",x = c(1),y = max(subdata$value,na.rm=T)*1.1,label=paste0("MAD = ", round(sd(data$value,na.rm=T),2)),color=color) +
                      annotate(geom="text",x = c(0.75),y = max(subdata$value,na.rm=T)*1.1,label=paste0("n = ", length(data$value)),color=color) +
                      annotate(geom="text",x = c(0.6),y = max(subdata$value,na.rm=T)*1.1,label=paste0("GiniMD = ", round(Hmisc::GiniMd(data$value,na.rm=T),2)),color=color)

                    p2 = ggplot_mixfit_desc(subdata$value,"normal",color) +
                      ggplot_mixfit_desc(subdata$value,"lnorm",color) +
                      ggplot_mixfit_desc(subdata$value,"weibull",color) +
                      ggplot_mixfit_desc(subdata$value,"gamma",color)

                    p2 = p2  +  plot_layout(ncol=2) +plot_annotation(
                      title = 'Best fits under different distirbutional assumptions',
                      caption = "Note that none of these plots give you an overall best fit. You must determine this yourself. \n
                      Mean (SD)"
                    )
                    cowplot::plot_grid(p1+ ggplot2::theme(plot.margin = unit(c(1,2,0,1), "cm")),p2+ ggplot2::theme(plot.margin = unit(c(0,0,0,0), "cm")),ncol=1,scale=c(0.9,0.8),rel_heights=c(.6,1))
                  }
                 }, height = 550) #close renderPlot

            }) #close the inputfile observeEvent

             observeEvent(input$analysisButton,{
                   print(head(vals$upld.file))
                   if(length(unique(vals$upld.file$group))>1 & input$effect != "Bimodality"){
                    comp = paste0(unique(vals$upld.file$group)[1]," - ",unique(vals$upld.file$group)[2])
                    vals$tbl = rbind(vals$tbl,data.frame(Group = comp,bifurcatoR_Analysis(data = vals$upld.file, c(input$vareff,input$meanvar,input$meaneff),input$nboot,input$alpha)))
                   } else {
                    print(input$choice)
                    vals$tbl = rbind(vals$tbl,data.frame(Group = input$choice,bifurcatoR_Analysis(data = vals$upld.file[vals$upld.file$group == input$choice,], c(input$bimode),input$nboot,input$alpha)))
                   }
                   if(length(vals$tbl)>0){
                     print(vals$tbl)
                     vals$tbl$p.value = ifelse(vals$tbl$p.value < 0.0001, 0.0001,round(vals$tbl$p.value,4))
                     vals$tbl$Stat = round(vals$tbl$Stat,3)

                     #let's remove duplicate tests too. Easier to do this than forcibly uncheck input options
                     vals$tbl = vals$tbl[! duplicated(paste0(vals$tbl$Group,vals$tbl$Test,vals$tbl$nboot)),]
                   }
                   rownames(vals$tbl) = NULL
                   output$resTable <- DT::renderDataTable({
                     vals$tbl
                   },height=200)
              })
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
# mod_analysisModule_ui("bimodalityModule_1")

## To be copied in the server
# mod_analysisModule_server("bimodalityModule_1")
