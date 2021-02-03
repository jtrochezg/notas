# the 'mpg' dataset.
library(ggplot2)


library(magrittr)
function(input, output,session) {
  
  
  observe({
    # Direccion del archivo    
    inFile<-input$file
    
    if(is.null(inFile)) 
      return(NULL)
    
    # Lectura del archivo con direccion inFile$datapath y almacenamiento en data1
    bcl=read.table(inFile$datapath, header = TRUE, sep = ";", quote="\"", dec=",",
                      fill = TRUE)
    updateSelectInput(session, "semes", choices = names(bcl))
    updateSelectInput(session, "facu", choices = names(bcl))  
    updateSelectInput(session, "campus", choices = names(bcl))
    updateSelectInput(session, "sga", choices = names(bcl))
    updateSelectInput(session, "prom", choices = names(bcl))
      })
  
  
 

  
  	
  # Filter data based on selections
  output$table <- renderDataTable(({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    bcl=read.table(inFile$datapath, header = TRUE, sep = ";", quote="\"", dec=",",
                     fill = TRUE)
    bcl
  }))
  
  
  # Filter data based on selections
  output$table2 <- renderDataTable(({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    bcl=read.table(inFile$datapath, header = TRUE, sep = ";", quote="\"", dec=",",
                    fill = TRUE)
    bcl
  }))
  
  
  # Filter data based on selections
  output$table1 <- renderDataTable({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    bcl=read.table(inFile$datapath, header = TRUE, sep = ";", quote="\"", dec=",",
                    fill = TRUE)
    
    
    g <- function(x)c(N=length(x),MEAN=round(mean(x,na.rm=TRUE),2),
                      SD=round(sd(x,na.rm=TRUE),2),
                      cv=round(sd(x)/mean(x)*100,2),MIN=min(x,na.rm=TRUE),
                      MAX=max(x,na.rm=TRUE))
    
    nots=as.numeric(bcl[, input$prom])
    
    tabla=summarize(nots,by=llist(bcl[, input$semes],bcl[, input$campus]),g)
    
    
    colnames(tabla) <- c("factor1","factor2","conteo","media","sd","cv","min","max")
    
    tots=summarize(nots,by=llist(bcl[, input$semes]),length)
    
    
    xx=tabla$conteo
    xx1=table(tabla$factor1)
    xx1=data.frame(xx1)
    
    valor=as.vector(tots$nots)
    vvv=as.vector(xx1$Freq)
    
    
    
    vec=rep(valor,(vvv))
    prop=round((xx/vec)*100,1)
    
    tablas=cbind(tabla,prop)
    kable(tablas)
    
    tablas
    
  })
  
  
  output$countryOutput <- renderUI({
    inFile<-input$file
    
    if(is.null(inFile)) 
      return(NULL)
    
    # Lectura del archivo con direccion inFile$datapath y almacenamiento en data1
    bcl =read.table(inFile$datapath, header = TRUE, sep = ";", quote="\"", dec=",",
                    fill = TRUE)
    selectizeInput("countryInput", "Factor",
                   levels(bcl[, input$beca]),
                   selected = NULL, multiple = TRUE,
                   options = list(placeholder = "Select"))
  })  
  
  
  
  
 
  
  
  
  
  
  
  # ============= TAB TO SHOW DATA IN TABLE ===========
 
  
  
  output$countryOutput <- renderUI({
    inFile<-input$file
    
    if(is.null(inFile)) 
      return(NULL)
    
    # Lectura del archivo con direccion inFile$datapath y almacenamiento en data1
    bcl =read.table(inFile$datapath, header = TRUE, sep = ";", quote="\"", dec=",",
                    fill = TRUE)
    
    selectizeInput("countryInput", "Estado a graficar",
                   levels(bcl[, input$campus]),
                   selected = NULL, multiple = TRUE,
                   options = list(placeholder = "Select"))
  })  
  

  
  
  #fltrado por semestre y estado
  
  filtered <- reactive({
    inFile<-input$file
    if(is.null(inFile)) 
      return(NULL)
    
    # Lectura del archivo con direccion inFile$datapath y almacenamiento en data1
    bcl =read.table(inFile$datapath, header = TRUE, sep = ";", quote="\"", dec=",",
                    fill = TRUE)
    if (is.null(input$typeInput)) {
      return(NULL)
    }    
    
    bcl1=bcl %>%
      filter(
        Type == input$typeInput
      )
    
    if (is.null(input$countryInput)) {
      return(NULL)
    }   
    bcl2=bcl1 %>%
      filter(
        Country %in% input$countryInput
      )
    
        bcl2
  })
  
  
  ##filtrado por semestre
  filtered1 <- reactive({
    inFile<-input$file
    if(is.null(inFile)) 
      return(NULL)
    
    # Lectura del archivo con direccion inFile$datapath y almacenamiento en data1
    bcl =read.table(inFile$datapath, header = TRUE, sep = ";", quote="\"", dec=",",
                    fill = TRUE)
    
    if (is.null(input$countryInput)) {
      return(NULL)
    }   
    bcl2=bcl %>%
      filter(
        Type %in% input$typeInput
      )
    
    bcl2
  })
  
  ##filtrado por estado
  
  filtered2 <- reactive({
    inFile<-input$file
    if(is.null(inFile)) 
      return(NULL)
    
    # Lectura del archivo con direccion inFile$datapath y almacenamiento en data1
    bcl =read.table(inFile$datapath, header = TRUE, sep = ";", quote="\"", dec=",",
                    fill = TRUE)
   
    if (is.null(input$countryInput)) {
      return(NULL)
    }   
    bcl2=bcl %>%
      filter(
        Country %in% input$countryInput
      )
    
    bcl2
  })
  
  output$pred_plot61<- renderPlot({
    
    
    if (is.null(filtered2())) {
      return()
    }
    
    datos=filtered2()
    
    
    fdata=as.data.frame(datos)
    
    g <- ggplot(fdata,aes(as.factor(datos[, input$semes]),
                          as.numeric(datos[, input$prom])))
    g + geom_boxplot(fill = "white",colour="#3366FF")+
      theme(axis.text.x = element_text(angle=60,hjust = 1,size=rel(1)), 
            axis.text.y = element_text(hjust = 1,size=rel(1)),
            axis.title.x = element_text(size = rel(1)),
            axis.title.y = element_text(size = rel(1)),
            legend.text = element_text(size = rel(1)),
            axis.text = element_text(size = 16),legend.position = "left",
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      facet_grid(~(datos[, input$campus]), scales="free", space="free")+
      labs(y= "Nota Promedio",x= "Período")
  })
  
  
  
  output$graf22<- renderPlot({
    
    
    
    if (is.null(filtered2())) {
      return()
    }
    
    datos=filtered2()
    
    ggplot(data=datos,aes((datos[, input$prom]),group=(datos[, input$semes]))) +
      geom_bar(stat = "count",fill=("#4168c3"),aes(y=..prop..),bins=10)+
      scale_y_continuous(labels = scales::percent)+  
      facet_grid(~datos[, input$semes],  space="free")+
      theme(axis.text.x = element_text(angle = 60, hjust = 1,size=rel(1.5)),
            axis.text.y = element_text(hjust = 1,size=rel(1.5)),
            axis.text = element_text(size = 12),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      labs(x = "Estado de la asignatura",y = "Frecuencia relativa")
    
    
    
  })
  
  
  
  output$preee <- renderPlotly({
    
    if (is.null(filtered1())) {
      return()
    }
    
    bass=filtered1()
    t2=ftable(bass$Country,row.vars = 1)
    t2=data.frame(t2)
    prop=round(t2[2]/sum(t2[2])*100,1)
    tt11=cbind(t2,prop)
    tt22=data.frame(tt11[ordered(tt11$Freq),])
    colnames(tt22) <- c("beca","Conteo","Proporcion")
    
    
    colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
    
    
    p <- plot_ly(tt22, labels = ~ tt22$beca, values = ~ tt22$Proporcion,
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 insidetextfont = list(color = '#FFFFFF'),
                 hoverinfo = 'text',marker = list(colors = colors,
                                                  line = list(color = '#FFFFFF', width = 1)),
                 #The 'pull' attribute can also be used to create space between the sectors
                 showlegend = FALSE) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>%
      add_pie(hole = 0.6) 
    p
  })
  
  output$preee1 <- renderPlotly({
    
    if (is.null(filtered1())) {
      return()
    }
    
    bass=filtered1()
    t2=ftable(bass$sga,row.vars = 1)
    t2=data.frame(t2)
    prop=round(t2[2]/sum(t2[2])*100,1)
    tt11=cbind(t2,prop)
    tt22=data.frame(tt11[ordered(tt11$Freq),])
    colnames(tt22) <- c("beca","Conteo","Proporcion")
    
    
    colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
    
    
    p <- plot_ly(tt22, labels = ~ tt22$beca, values = ~ tt22$Proporcion,
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 insidetextfont = list(color = '#FFFFFF'),
                 hoverinfo = 'text',marker = list(colors = colors,
                                                  line = list(color = '#FFFFFF', width = 1)),
                 #The 'pull' attribute can also be used to create space between the sectors
                 showlegend = FALSE) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>%
      add_pie(hole = 0.6) 
    p
  })
  output$coolplot <- renderPlot({
  
      if (is.null(filtered())) {
      return()
    }
    
    bass=filtered()
    d=bass$prom
    
    d.cut <- cut(d, c(seq(min(d),max(d),0.48), Inf),right = FALSE)
    
        barplot(table(d.cut)/length(d.cut)*100,col="#4168c3",space=0,border=0, main='', 
            cex.axis=1.5, cex.names=1.2,xlab="Intervalos de Frecuencia",cex.lab=2,
            ylab="Frecuencia relativa")
    
    
    box()
  })
  
  output$results <- renderDataTable({
    
    if (is.null(filtered1())) {
      return()
    }
    
    bass=filtered1()
    
    t2=ftable(bass$Country,row.vars = 1)
    t2=data.frame(t2)
    prop=round(t2[2]/sum(t2[2])*100,1)
    tt11=cbind(t2,prop)
    tt22=data.frame(tt11[ordered(tt11$Freq),])
    colnames(tt22) <- c("beca","Conteo","Proporcion")
    tt22
  })
 
  output$results2<- renderDataTable({
    if (is.null(filtered())) {
      return()
    }
    
    bass=filtered()
    t2=ftable(country,row.vars = 1)
    t2=data.frame(t2)
    prop=round(t2[2]/sum(t2[2])*100,1)
    tt11=cbind(t2,prop)
    tt22=data.frame(tt11[ordered(tt11$Freq),])
    colnames(tt22) <- c("beca","Conteo","Proporcion")
    tt22
  })
  
 
  
  
  
  
  output$results1 <- renderDataTable({
    if (is.null(filtered1())) {
      return()
    }
    
    bass=filtered1()
    t2=ftable(bass$sga,row.vars = 1)
    t2=data.frame(t2)
    prop=round(t2[2]/sum(t2[2])*100,1)
    tt11=cbind(t2,prop)
    tt22=data.frame(tt11[ordered(tt11$Freq),])
    colnames(tt22) <- c("beca","Conteo","Proporcion")
    tt22
  })
  
  
  
  output$total_plot <- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T, sep = ";", quote="\"", dec=".",
                     fill = TRUE)
    
    Causas = as.factor(data1[, input$semes])
    Razns = as.factor(data1[, input$campus])
    
    
    haz.cero.na=function(x){
      ifelse(x==0,NA,x)}
    
    
    datas=data.frame(Causas, Razns)
    
    ttt=na.omit(datas)
    
    
    
    ggplot(data=ttt,aes((Razns),group=Causas)) +
      geom_bar(stat = "count",fill=("#4168c3"),aes(y=..prop..))+
      scale_y_continuous(labels = scales::percent)+  
      facet_grid(~Causas,  space="free")+
      theme(axis.text.x = element_text(angle = 60, hjust = 1,size=rel(1.5)),
            axis.text.y = element_text(hjust = 1,size=rel(1.5)),
            axis.text = element_text(size = 12),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      labs(x = "Estado de la asignatura",y = "Frecuencia relativa")
    
    
      
    
  })
  
  
  
  output$total_plot1 <- renderPlot({
    
    
    if (is.null(filtered1())) {
      return()
    }
    
    bass=filtered1()
    
    Causas = as.factor(bass$sga)
    Razns = as.factor(bass$Country)
    
    
    haz.cero.na=function(x){
      ifelse(x==0,NA,x)}
    
    
    datas=data.frame(Causas, Razns)
    
    ttt=na.omit(datas)
    
    ggplot(data=ttt,aes((Razns),group=Causas)) +
      geom_bar(stat = "count",fill=("#4168c3"),aes(y=..prop..))+
      scale_y_continuous(labels = scales::percent)+  
      facet_grid(~Causas,  space="free")+
      theme(axis.text.x = element_text(angle = 60, hjust = 1,size=rel(1.5)),
            axis.text.y = element_text(hjust = 1,size=rel(1.5)),
            axis.text = element_text(size = 12),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      labs(x = "Estado de la asignatura",y = "Frecuencia relativa")
    
    
    
    
    
  })
  
  
  output$tabless7 <- renderDataTable({
    
    if (is.null(filtered())) {
      return()
    }
    
    bass=filtered()
    d=bass$prom
    
    d.cut <- cut(d, c(seq(min(d),max(d),0.48), Inf),right = FALSE)
    mitab=data.frame(table(d.cut))
    propvec=round(mitab$Freq/(sum(mitab$Freq))*100,1)
    new=cbind(mitab,propvec)
    colnames(new)=c("Intervalos","Frecuencia absoluta","Proporción")
    new
  })
  
  
  output$summary3 <- renderPrint({
    
    if (is.null(filtered1())) {
      return()
    }
    
    bass=filtered1()
    
    Causas = as.factor(bass$sga)
    facultad=as.factor(bass$Country)
    
    
    yy=addmargins(table(facultad,Causas),2)
    u=prop.table(yy,2)
    
    
    kable(round(u,3)*100)
  })
  
  
  
  
  output$summary4<- renderPrint({
    
    if (is.null(filtered1())) {
      return()
    }
    
    bass=filtered1()
    
    Causas = as.factor(bass$sga)
    facultad=as.factor(bass$Country)
    
    
    yy=addmargins(table(facultad,Causas))
    
    kable(yy)
  })
}
