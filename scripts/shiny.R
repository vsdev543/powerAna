hotkeys<-c(
  "q"
)


ui <- dashboardPage(
  preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
  dark = TRUE,
  help = FALSE,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  header = dashboardHeader(
    title = dashboardBrand(
      title = tags$b(title),
      color = titcolor,
      image = "https://www.lifewire.com/thmb/M_mfW0gmnjiEaVjRmj8HESi1wEw=/1280x1280/smart/filters:no_upscale()/power-button-bb823922c3e94579ab285aa33c7b4d20-f363a15db38c49b39f221922a047202b.png",
      opacity = 0.8,href = titlink
    ),
    leftUi = uiOutput("navleft"),
    rightUi = uiOutput("navright"),
    uiOutput("navCenter1"),
    uiOutput("navCenter2"),
    uiOutput("navCenter3"),
    fixed = TRUE    
  ),
  
  
  sidebar = dashboardSidebar(
    fixed = T,
    skin = "light",
    status = "primary",
    id = "sidebar",collapsed = T,
    
    # sidebarUserPanel(image = "https://image.flaticon.com/icons/svg/1149/1149168.svg",
    #                  name = "Power Monitor Dashboard"),
    hr(),
    sidebarMenu(
      id = "current_tab",
      flat = FALSE,
      compact = FALSE,
      childIndent = TRUE,
      
      sidebarHeader("Dashboard Panels"),
      menuItem("Panel View 1", tabName = "pan1", icon = icon("sliders")),
      menuItem("Panel View 2", tabName = "pan2", icon = icon("sliders"))
    ),
    customArea =  a(href = "www.fiverr.com/ozi543",
                    target = "_blank", "by @ozi543 for yomiwole")
    
  ),
  selectInput(
    inputId = "age",
    label = "Age",
    choices = c("A")
  ),
  
  body = dashboardBody(use_waiter(),
                       tags$head(
                         tags$style(
                           # body {overflow-y: hidden;
                           # }
                           ".form-group, form-control, .selectize-control{
                                 margin:0 !important;
                                 }
                            
                                 "
                         )
                       ),
                       useKeys(),
                       keysInput("keys", hotkeys),
                       tabItems(pan1,
                                pan2)),
  controlbar = dashboardControlbar(
    id = "controlbar",
    skin = "dark",
    pinned = T,
    overlay = FALSE,collapsed = F,
    
    br(),
    fluidRow(column(
      10,
      offset = 1,
      fileInput(inputId = "filePath",label = "Select the data file",accept = c(".csv",".xlsx")),
      hr(),
      switchInput(
        inputId = "utilityS",
        label = "Generator",
        labelWidth = 100,
        width = "100%",value = F
        
      ),
      br(),
      prettyCheckbox(
        inputId = "showMin",
        label = "Show Minimum", 
        value = FALSE,
        icon = icon("check"),
        status = "success",
        animation = "rotate"
      ),
      prettyCheckbox(
        inputId = "showAv",
        label = "Show Avarage", 
        value = FALSE,
        icon = icon("check"),
        status = "success",
        animation = "rotate"
      ),
      prettyCheckbox(
        inputId = "showMax",
        label = "Show Maximum", 
        value = FALSE,
        icon = icon("check"),
        status = "success",
        animation = "rotate"
      ),
      prettyCheckbox(
        inputId = "showLF",
        label = "Show Load Factor", 
        value = FALSE,
        icon = icon("check"),
        status = "success",
        animation = "rotate"
      ),
      hr(),
      prettyCheckbox(
        inputId = "cumS",
        label = "Cumiliative", 
        value = FALSE,
        icon = icon("check"),
        status = "success",
        animation = "rotate"
      ),
      prettyCheckbox(
        inputId = "addAv",
        label = "Add avarage Line", 
        value = FALSE,
        icon = icon("check"),
        status = "success",
        animation = "rotate"
      ),
      prettyCheckbox(
        inputId = "fillp",
        label = "Fill Chart Area", 
        value = FALSE,
        icon = icon("check"),
        status = "success",
        animation = "rotate"
      ),
      br(),
      
      tags$b("Page Navigation",style="color:white;"),
      fluidRow(
        style="margin-top:10px;margin-left:2px;",
        div(style="display:inline-block",actionBttn(
          inputId = "toend",
          style = "fill", 
          icon = icon("angle-double-left"),
          color = "primary",
        ), style="float:right"),
        div(style="display:inline-block",actionBttn(
          inputId = "toleft",
          label = NULL,
          style = "fill", 
          color = "primary",
          icon = icon("chevron-left")
        ), style="float:right"),
        div(style="display:inline-block",actionBttn(
          inputId = "toright",
          label = NULL,
          style = "fill", 
          color = "primary",
          icon = icon("chevron-right")
        ), style="float:right"),
        div(style="display:inline-block",actionBttn(
          inputId = "tostart",
          icon = icon("angle-double-right"),
          style = "fill", 
          color = "primary",
        ), style="float:right")
        
      )
    )),
    
    
    
  ),
  footer = NULL,
  title = apptit
  
  
)




server <- function(input, output, session) {
  useAutoColor()
  
  
  base <-
    read.csv("./data/base.csv")
  
  r <- reactiveValues(
    param = unique(base$param),
    para = base$param[1],
    subparam = NULL,
    subpara = NULL,
    viewGap=14400,
    summary=NULL,
    prange=NULL,
    prangeH=FALSE,
    prangey=NULL,
    y2tit=NULL,
    left=NULL,
    right=NULL,
    start=NULL,
    end=NULL
  )
  
  df <- reactive({
    if(!is.null(input$filePath)){
      k<-tail(unlist(strsplit(input$filePath$name, "\\.")),1)
      path<-input$filePath$datapath
      if(k=="xlsx"){
        x<-as.data.frame(read_excel(path,sheet = 1,progress = T))
      }
      if(k=="csv"){
        x<-as.data.frame(read.csv(path))
      }
      saveRDS(x,"data/lastData.Rds")
    }else{
      x<-readRDS("data/lastData.Rds")
    }
    
    updateSliderInput(session = session,inputId = "plotrange",min = min(x$event),max = max(x$event),value = c((max(x$event)-2.628e+6),max(x$event)))
    
    x
  })
  
  
  observe({
    if (!is.null(input$param)) {
      r$para <- input$param
      r$subparam <- base[base$param %in% input$param, ]
    }
    
    if (!is.null(input$subpara)) {
      r$subpara <- input$subpara
    }
  })
  
  output$navleft <- renderUI({
    selectizeInput(
      inputId = "param",
      label = NULL,
      choices = r$param,
      selected = r$para,
      multiple = T,
      options = list(maxItems = 2)
    )
  })
  
  output$navCenter1 <- renderUI({
    selectizeInput(
      inputId = "subpara",
      label = NULL,
      choices = r$subparam$subparam,
      # selected = r$subpara,
      selected = r$subparam$subparam[1],
      multiple = TRUE
    )
  })
  
  output$navright<-renderUI({
    fluidRow(
      column(6,
    actionBttn(inputId = "printPlot",label = "Download",color = "success",style = 'material-flat',size = 'sm')
      ),
    column(5,offset = 1,
           dropdownButton(circle = F,size = 'sm',label = "colors",tooltip = "Change Series Colors",right = F,up = F,
                          
                          boxDropdownItem(
                            selectizeInput(
                              inputId = 'avgcol',
                              label = "Avarage Color",
                              choices = c("red", "blue", "green", "yellow", "auto"),
                              selected = "auto",
                              options = list(create = TRUE)
                            )
                          ),
                          boxDropdownItem(
                            selectizeInput(
                              inputId = 's1col',
                              label = "Series 1 Color",
                              choices = c("red", "blue", "green", "yellow", "auto"),
                              selected = "auto",
                              options = list(create = TRUE)
                            )
                          ),
                          boxDropdownItem(
                            selectizeInput(
                              inputId = 's2col',
                              label = "Series 2 Color",
                              choices = c("red", "blue", "green", "yellow", "auto"),
                              selected = "auto",
                              options = list(create = TRUE)
                            )
                          ),
                          boxDropdownItem(
                            selectizeInput(
                              inputId = 's3col',
                              label = "Series 3 Color",
                              choices = c("red", "blue", "green", "yellow", "auto"),
                              selected = "auto",
                              options = list(create = TRUE)
                            )
                          ),
                          boxDropdownItem(
                            selectizeInput(
                              inputId = 's4col',
                              label = "Series 4 Color",
                              choices = c("red", "blue", "green", "yellow", "auto"),
                              selected = "auto",
                              options = list(create = TRUE)
                            )
                          )
           )
           
           )
    )
  })
  
  observeEvent(input$printPlot,{
    screenshot(selector = "#pa1card",filename = "plot",scale = 2)
  })
  
  observeEvent(input$keys, {
    if(input$keys=="q"){
      screenshot(selector = "#pa1card",filename = "plot",scale = 2)
    }
  })
  
  dt<-reactive({
    dt <- df()[, c("event", r$subpara, "GeneratorSup")]
    dt<-dt%>%
      dplyr::filter(event<input$plotrange[2] & event>input$plotrange[1])
    
    dt$GeneratorSup[dt$GeneratorSup<1]<-NA
    dt$GeneratorSup[dt$GeneratorSup>=0.999]<-1.000000000000000000000
    
    dt$Utility<-dt$GeneratorSup*dt[,r$subpara[1]]
    
    
    if(input$cumS){
      for(sub in r$subpara){
        dt[,sub] = cumsum(dt[,sub])
      }
    }
    
    r$summary<-summary(dt[,r$subpara[1]])
    r$prange<-c(tail(dt$event,1)-r$viewGap*60,tail(dt$event,1))
    r$prangey<-c(min(dt[,r$subpara[1]]),max(dt[,r$subpara[1]]))
    dt
  })
  
  
  fig<-reactive({
    dt<-dt()
    
    prange<-r$prange
    
    fig <-
      plot_ly(
        data = dt,
        type = 'scatter',
        mode = "lines",
        fill=if(input$fillp){'tozeroy'},
        x = dt[, "event"],
        y = dt[, r$subpara[1]],
        name = r$subpara[1],
        source = "source",
        yaxis = 'y1',
        color = if(input$s1col!="auto"){I(input$s1col)}else{NULL}
      )
    
    if(input$addAv){
      fig <- fig %>%
        add_trace(
          type = "scatter",
          y = mean(dt[,r$subpara[1]]),
          mode = "lines",
          name = paste("Avarage",r$subpara[1]),
          yaxis = 'y1',
          fill=NULL,
          color = if(input$avgcol!="auto"){I(input$avgcol)}else{NULL}
        )
    }
    
    if (input$utilityS) {
      fig <- fig %>%
        add_trace(
          type = "scatter",
          y = dt$Utility,
          mode = "lines",
          fill = 'tozeroy',
          name = "Generator"
        )
    }
    
    if (length(r$subpara) > 1) {
      for (i in 2:length(r$subpara)) {
        so<-base[base$subparam==r$subpara[i],"yaxis"]==base[base$subparam==r$subpara[1],"yaxis"]
        
        fig <- fig %>%
          add_trace(y = dt[, r$subpara[i]],
                    name = r$subpara[i],
                    mode = "lines",
                    fill=NULL,
                    color = if(input[[paste0("s",i,"col")]]!="auto"){I(input[[paste0("s",i,"col")]])}else{NULL},
                    yaxis = if(so){"y1"}else{
                      "y2"
                    }
          )
        
        if(!so){
          r$y2tit<-r$subpara[i]
        }
        
        
        fig
      }
    }
    
    
    
    
    ytit<-base[base$subparam == r$subpara[1],"yaxis"]
    y2tit<-base[base$subparam == r$y2tit,"yaxis"]
    
    fig <- fig %>%
      layout(hovermode = 'compare',
             
             xaxis=list(range=r$prange,
                        rangeselector=list(
                          buttons = list(
                            list(
                              count = 1,
                              label = "1 hr",
                              step = "hour",
                              stepmode = "backward"),
                            list(
                              count = 5,
                              label = "5 hr",
                              step = "hour",
                              stepmode = "backward"),
                            list(
                              count = 15,
                              label = "15 hr",
                              step = "hour",
                              stepmode = "backward"),
                            list(
                              count = 1,
                              label = "1 day",
                              step = "day",
                              stepmode = "backward"),
                            list(
                              count = 5,
                              label = "5 day",
                              step = "day",
                              stepmode = "backward"),
                            list(
                              count = 10,
                              label = "10 day",
                              step = "day",
                              stepmode = "backward"),
                            list(
                              count = 1,
                              label = "1 mnth",
                              step = "month",
                              stepmode = "backward"),
                            list(
                              count = 3,
                              label = "3 mnth",
                              step = "month",
                              stepmode = "backward"),
                            list(
                              count = 6,
                              label = "6 mnth",
                              step = "month",
                              stepmode = "backward"),
                            list(
                              count = 1,
                              label = "1 year",
                              step = "year",
                              stepmode = "backward"),
                            list(
                              count = 1,
                              label = "YTD",
                              step = "year",
                              stepmode = "todate"),
                            list(step = "all"))),
                        
                        rangeslider = list(type = "date")
             ),
             yaxis = list(range=r$prangey,
                          autorange = F,
                          fixedrange = F,
                          title=ytit
             ),
             yaxis2 = list(
               tickfont = list(color = "red"),
               overlaying = "y",
               side = "right",
               title = y2tit,
               showgrid = FALSE, zeroline = FALSE
             ),
             title=paste(paste(c(r$subpara),collapse = "/ "),"Time Series"),
             margin=list(l=50,r=50,t=100,b=30,p=4)
      )%>%
      config(displaylogo = FALSE)
    
    
    
    
    
    
    fig
  })
  
  
  observeEvent(input$toleft,{
    r$left<-T
  })
  observeEvent(input$toright,{
    r$right<-T
  })
  observeEvent(input$toend,{
    r$end<-T
  })
  observeEvent(input$tostart,{
    r$start<-T
  })
  
  
  zoom<-reactive({
    event_data("plotly_relayout", "source")
  })
  
  observe({
    zoom <- zoom()
    
    if (!is.null(zoom$`xaxis.range[0]`)) {
      l <-
        na.omit(c(
          ymd_hms(zoom$`xaxis.range[0]`),
          ymd_hm(zoom$`xaxis.range[0]`)
        ))
      r$prange[1] <- l[1]
    }
    if (!is.null(zoom$`xaxis.range[1]`)) {
      m <- na.omit(c(
        ymd_hms(zoom$`xaxis.range[1]`),
        ymd_hm(zoom$`xaxis.range[1]`)
      ))
      r$prange[2] <- m[1]
    }
    
    # if(!is.null(zoom$xaxis.range)){
    #   r$prange<-zoom$xaxis.range
    # }
    
    if(isTRUE(r$left)){
      k<-(r$prange[2]-r$prange[1])
      
      r$prange<-r$prange-k
      
      r$left<-NULL
    }
    
    if(isTRUE(r$right)){
      k<-(r$prange[2]-r$prange[1])
      
      r$prange<-r$prange+k
      
      r$right<-NULL
    }
    
    if(isTRUE(r$start)){
      k<-(r$prange[2]-r$prange[1])
      l<-max(dt()$event)
      
      r$prange<-c(l-k,l)
      
      r$start<-NULL
    }
    
    if(isTRUE(r$end)){
      k<-(r$prange[2]-r$prange[1])
      l<-min(dt()$event)
      
      r$prange<-c(l,l+k)
      
      r$end<-NULL
    }
    
    if(!is.null(zoom$xaxis.autorange)){
      if(zoom$xaxis.autorange){
        r$prange<-c(min(dt()$event),max(dt()$event))
      }
    }
    
  })
  
  
  observe({
    
    zoom <- event_data("plotly_relayout", "source")
    
    if(!is.null(zoom$`yaxis.range[0]`)){
      r$prangey[1]<-zoom$`yaxis.range[0]`
    }
    if(!is.null(zoom$`yaxis.range[1]`)){
      r$prangey[2]<-zoom$`yaxis.range[1]`
    }
  })
  
  
  
  output$topleft<-renderUI({
    hover<-event_data(event = "plotly_hover",source = "source")
    if (!is.null(hover)) {
      tagList(
        apply(hover, 1, function(row) {
          l<-r$subpara[as.numeric(row["curveNumber"])+1]
          
          bs4Badge(color ="info",rounded = F,position = "left",
                   paste(l,
                         ":",
                         row["y"],
                         "|",
                         row["x"])
          )
        })
      )
    }else{
      "-"
    }
    
    
  })
  
  
  output$topright<-renderUI({
    dtvp<-dt()%>%
      filter(event<r$prange[2] & event>r$prange[1])
    
    sum<-summary(dtvp[,r$subpara[1]])
    
    tagList(
      
      if (input$showMin) {
        bs4Badge(
          color = "primary",
          position = 'left',
          paste("Min:", round(sum["Min."], 3)),
          style = "font-size:1em; color:white;"
        )
      }
      ,
      if (input$showAv) {
        bs4Badge(
          color = "success",
          position = 'left',
          paste("Avarage:", round(sum["Mean"], 3)),
          style = "font-size:1em; color:white;"
        )
        
      },
      if (input$showMax) {
        bs4Badge(
          color = "danger",
          position = 'left',
          paste("Max:", round(sum["Max."], 3)),
          style = "font-size:1em; color:white;"
        )
      },
      if (input$showLF) {
        bs4Badge(
          color = "warning",
          position = 'left',
          paste("LF:", round((sum["Mean"] / sum["Max."]), 3)),
          style = "font-size:1em; color:white;"
        )
      } else{
        ""
      }
    )
    
    
  })
  
  output$botcenter <- renderUI({
    tagList(
      if (input$showMin) {
        bs4Badge(
          color = "primary",
          position = 'left',
          paste("Min:", round(r$summary["Min."], 3))
          ,
          style = "color:white;"
        )
      },
      if (input$showAv) {
        
        bs4Badge(
          color = "success",
          position = 'left',
          paste("Avarage:", round(r$summary["Mean"], 3))
          ,
          style = "color:white;"
        )
      },
      if (input$showMax) {
        
        bs4Badge(
          color = "danger",
          position = 'left',
          paste("Max:", round(r$summary["Max."], 3))
          ,
          style = "color:white;"
        )
      },
      if (input$showLF) {
        
        bs4Badge(
          color = "warning",
          position = 'left',
          paste("LF:", round((
            r$summary["Mean"] / r$summary["Max."]
          ), 3))
          ,
          style = "color:white;"
        )
      } else{
        
        ""
      })
    
  })
  
  
  
  
  fig1<-reactive({
    dt<-dt()
    fig<-ggplot(dt,aes(x = event))+
      geom_line(aes(y=dt[,r$subpara[1]]))+
      geom_col(aes(y=Utility,color=Utility))
    
    
    
    
    fig<-ggplotly(fig,dynamicTicks = T)
    
    fig%>%
      layout(hovermode = 'compare',
             xaxis=list(rangeselector=list(
               buttons = list(
                 list(
                   count = 1,
                   label = "10 hr",
                   step = "hour",
                   stepmode = "backward"),
                 list(
                   count = 2,
                   label = "2 hr",
                   step = "hour",
                   stepmode = "backward"),
                 list(
                   count = 15,
                   label = "15 hr",
                   step = "hour",
                   stepmode = "backward"),
                 list(
                   count = 1,
                   label = "1 day",
                   step = "day",
                   stepmode = "backward"),
                 list(
                   count = 5,
                   label = "5 day",
                   step = "day",
                   stepmode = "backward"),
                 list(
                   count = 10,
                   label = "10 day",
                   step = "day",
                   stepmode = "backward"),
                 list(
                   count = 1,
                   label = "1 mnth",
                   step = "month",
                   stepmode = "backward"),
                 list(
                   count = 3,
                   label = "1 mnth",
                   step = "month",
                   stepmode = "backward"),
                 list(
                   count = 6,
                   label = "6 mnth",
                   step = "month",
                   stepmode = "backward"),
                 list(
                   count = 1,
                   label = "1 year",
                   step = "year",
                   stepmode = "backward"),
                 list(
                   count = 1,
                   label = "YTD",
                   step = "year",
                   stepmode = "todate"),
                 list(step = "all"))),
               
               rangeslider = list(type = "date")
             ),
             
             title=paste(paste(c(r$subpara),collapse = "/ "),"Time Series"),
             margin=list(l=50,r=50,t=100,b=100,p=4)
      )%>%
      toWebGL()
    
  })
  
  output$pan1plot <- renderPlotly({
    fig()
  })
  
  
}
