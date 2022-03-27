
pan1 <- tabItem(tabName = "pan1",
                bs4Card(maximizable = F,width = 12,id = "pa1card",collapsible = F,label =   uiOutput("topright"),
                        title = uiOutput("topleft"),style=" width: 100%; height: 100%; overflow-y: hidden;position:relative;",
                
                div(
                plotlyOutput("pan1plot",height = plotHeight,width = "100%",reportTheme = T),
                style="width:100%;"
                ),
                fluidRow(
                  column(12,"Working range"),
                  style="text-align:center; font-weight:bold"),
                fluidRow(
                  column(4,uiOutput("botleft")),
                  column(4,uiOutput("botcenter"),style="text-align:center;"),
                  column(4,uiOutput("botright"))
                ),
                fluidRow(column(10,
                                offset = 1,
                                br(),
                                sliderInput(
                                  inputId = "plotrange",
                                  label = NULL,
                                  min = 0,
                                  max = 100,
                                  width = "100%",
                                  dragRange = T,
                                  value = c(1, 100)
                                )
                                
                )
                )
                )
                
)


pan2<-tabItem(tabName = "pan2",
              "For Future developments"
)