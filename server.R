library(shiny)
library(ggplot2)
library(gganimate)
library(gapminder)
library(ggthemes)
library(ggcorrplot)
library(d3heatmap)
library(rworldmap)
library(readxl)

#Carga de datos para el gráfico principal
HDI2015Data <- get(load(url("https://github.com/Arturof71/ArturoFernandezEFMasterBIgData/blob/master/HDI2015Data.Rda?raw=true")))

#Carga de datos para el gráfico evolución
HDIEvolution <- get(load(url("https://github.com/Arturof71/ArturoFernandezEFMasterBIgData/blob/master/HDIEvolution.Rda?raw=true")))

#Transformamos Columnas a Filas
HDIEvolution['Year'] = NA
HDIEvolution['HDI'] = NA
HDIEvolution['GNI'] = NA
HDIEvolution = HDIEvolution[rep(seq_len(nrow(HDIEvolution)),each=8),]
n=1
while (n < 1317) {
  HDIEvolution$Year[n] = "1990"
  HDIEvolution$Year[n+1] = "2000"
  HDIEvolution$Year[n+2] = "2010"
  HDIEvolution$Year[n+3] = "2011"
  HDIEvolution$Year[n+4] = "2012"
  HDIEvolution$Year[n+5] = "2013"
  HDIEvolution$Year[n+6] = "2014"
  HDIEvolution$Year[n+7] = "2015"
  HDIEvolution$HDI[n] = HDIEvolution$HDI1990[n]
  HDIEvolution$HDI[n+1] = HDIEvolution$HDI2000[n]
  HDIEvolution$HDI[n+2] = HDIEvolution$HDI2010[n]
  HDIEvolution$HDI[n+3] = HDIEvolution$HDI2011[n]
  HDIEvolution$HDI[n+4] = HDIEvolution$HDI2012[n]
  HDIEvolution$HDI[n+5] = HDIEvolution$HDI2013[n]
  HDIEvolution$HDI[n+6] = HDIEvolution$HDI2014[n]
  HDIEvolution$HDI[n+7] = HDIEvolution$HDI2015[n]
  HDIEvolution$GNI[n] = HDIEvolution$GNI1990[n]
  HDIEvolution$GNI[n+1] = HDIEvolution$GNI2000[n]
  HDIEvolution$GNI[n+2] = HDIEvolution$GNI2010[n]
  HDIEvolution$GNI[n+3] = HDIEvolution$GNI2011[n]
  HDIEvolution$GNI[n+4] = HDIEvolution$GNI2012[n]
  HDIEvolution$GNI[n+5] = HDIEvolution$GNI2013[n]
  HDIEvolution$GNI[n+6] = HDIEvolution$GNI2014[n]
  HDIEvolution$GNI[n+7] = HDIEvolution$GNI2015[n]
  n=n+8
}
HDIEvolution <- HDIEvolution[,-(4:19),drop=FALSE]
HDIEvolution = HDIEvolution[complete.cases(HDIEvolution),]

shinyServer(function(input, output) {
  
  #Gráfico de Analisis de Variables
  output$plot <- renderPlot({
    theme_set(theme_bw())
    g1 <- ggplot(HDI2015Data, 
                aes_string(x=input$x, y=input$y))
    g1 <- g1 + geom_point()
    
    if (input$color != 'None')
      g1 <- g1 + aes_string(color=input$color)
    
    facets <- paste(input$facet_row, "~ .")
    if (facets != '. ~ .')
      g1 <- g1 + facet_grid(facets)
    
    if (input$lm)
      g1 <- g1 + geom_smooth(method='lm',formula=y~x, na.rm = T)
    if (input$smooth)
      g1 <- g1 + geom_smooth(method='loess',formula=y~x, na.rm = T)
    
    print(g1)

  })
  
  #Gráfico de Analisis por Pais (HDI)
  output$plot21 <- renderPlot({
    theme_set(theme_bw())
    g2 <- ggplot(HDI2015Data, aes("Country", HDI))
    #Filtramos la información en el caso de anális regional
    if (input$a == 'r' & input$c != '.') {
      region = HDI2015Data$Region[HDI2015Data$Country == input$c]
      HDI2015DataRegional = subset(HDI2015Data, Region == region)
      g2 <- ggplot(HDI2015DataRegional, aes("Country", HDI))
    }
    #Imprimimos el grafico boxplot de la variable
    g2 <- g2 + geom_boxplot()
    #Analizamos la situación del páis respecto a la variable
    level = HDI2015Data$HDI[HDI2015Data$Country == input$c]
    if (input$c != '.') {
      if (input$a == 'r') {
        Percentil = quantile(HDI2015DataRegional$HDI, c(.25, .50, .75))}
      else {
        Percentil = quantile(HDI2015Data$HDI, c(.25, .50, .75))}
      #Determinaos el color del punto que representa el pais
      if (level <= Percentil[1])
        color="red"
        else if (level <= Percentil[2])
               color="orange"
               else if (level <= Percentil[3])
                 color="blue"
                 else color="green"
      #Imprimimos el punto del país  
      g2 <- g2 + geom_point(aes("Country", level),size=3,col=color)
    }
    print(g2)
  })

  #Gráfico de Analisis por Pais (InequalityAdjustedHDI)
  output$plot22 <- renderPlot({
    theme_set(theme_bw())
    g2 <- ggplot(HDI2015Data, aes("Country", InequalityAdjustedHDI))
    #Filtramos la información en el caso de anális regional
    if (input$a == 'r' & input$c != '.') {
      region = HDI2015Data$Region[HDI2015Data$Country == input$c]
      HDI2015DataRegional = subset(HDI2015Data, Region == region)
      g2 <- ggplot(HDI2015DataRegional, aes("Country", InequalityAdjustedHDI))
    }
    #Imprimimos el grafico boxplot de la variable
    g2 <- g2 + geom_boxplot()
    #Analizamos la situación del páis respecto a la variable
    level = HDI2015Data$HDI[HDI2015Data$Country == input$c]
    if (input$c != '.') {
      if (input$a == 'r') {
        Percentil = quantile(HDI2015DataRegional$InequalityAdjustedHDI, c(.25, .50, .75),na.rm=TRUE)}
      else {
        Percentil = quantile(HDI2015Data$InequalityAdjustedHDI, c(.25, .50, .75),na.rm=TRUE)}
      #Determinaos el color del punto que representa el pais
      if (level <= Percentil[1])
        color="red"
      else if (level <= Percentil[2])
        color="orange"
      else if (level <= Percentil[3])
        color="blue"
      else color="green"
      #Imprimimos el punto del país  
      g2 <- g2 + geom_point(aes("Country", level),size=3,col=color)
    }
    print(g2)
  })
  
  #Gráfico de Analisis por Pais (LifeExpectancyatBIrth)
  output$plot23 <- renderPlot({
    theme_set(theme_bw())
    g2 <- ggplot(HDI2015Data, aes("Country", LifeExpectancyatBirth))
    #Filtramos la información en el caso de anális regional
    if (input$a == 'r' & input$c != '.') {
      region = HDI2015Data$Region[HDI2015Data$Country == input$c]
      HDI2015DataRegional = subset(HDI2015Data, Region == region)
      g2 <- ggplot(HDI2015DataRegional, aes("Country", LifeExpectancyatBirth))
    }
    #Imprimimos el grafico boxplot de la variable
    g2 <- g2 + geom_boxplot()
    #Analizamos la situación del páis respecto a la variable
    level = HDI2015Data$LifeExpectancyatBirth[HDI2015Data$Country == input$c]
    if (input$c != '.') {
      if (input$a == 'r') {
        Percentil = quantile(HDI2015DataRegional$LifeExpectancyatBirth, c(.25, .50, .75))}
      else {
        Percentil = quantile(HDI2015Data$LifeExpectancyatBirth, c(.25, .50, .75))}
      #Determinaos el color del punto que representa el pais
      if (level <= Percentil[1])
        color="red"
      else if (level <= Percentil[2])
        color="orange"
      else if (level <= Percentil[3])
        color="blue"
      else color="green"
      #Imprimimos el punto del país  
      g2 <- g2 + geom_point(aes("Country", level),size=3,col=color)
    }
    print(g2)
  })  
  
  #Gráfico de Analisis por Pais (ExpectedYearsSchooling)
  output$plot24 <- renderPlot({
    theme_set(theme_bw())
    g2 <- ggplot(HDI2015Data, aes("Country", ExpectedYearsSchooling))
    #Filtramos la información en el caso de anális regional
    if (input$a == 'r' & input$c != '.') {
      region = HDI2015Data$Region[HDI2015Data$Country == input$c]
      HDI2015DataRegional = subset(HDI2015Data, Region == region)
      g2 <- ggplot(HDI2015DataRegional, aes("Country", ExpectedYearsSchooling))
    }
    #Imprimimos el grafico boxplot de la variable
    g2 <- g2 + geom_boxplot()
    #Analizamos la situación del páis respecto a la variable
    level = HDI2015Data$ExpectedYearsSchooling[HDI2015Data$Country == input$c]
    if (input$c != '.') {
      if (input$a == 'r') {
        Percentil = quantile(HDI2015DataRegional$ExpectedYearsSchooling, c(.25, .50, .75))}
      else {
        Percentil = quantile(HDI2015Data$ExpectedYearsSchooling, c(.25, .50, .75))}
      #Determinaos el color del punto que representa el pais
      if (level <= Percentil[1])
        color="red"
      else if (level <= Percentil[2])
        color="orange"
      else if (level <= Percentil[3])
        color="blue"
      else color="green"
      #Imprimimos el punto del país  
      g2 <- g2 + geom_point(aes("Country", level),size=3,col=color)
    }
    print(g2)
  })  

  #Gráfico de Analisis por Pais (MeanYearsSchooling)
  output$plot25 <- renderPlot({
    theme_set(theme_bw())
    g2 <- ggplot(HDI2015Data, aes("Country", MeanYearsSchooling))
    #Filtramos la información en el caso de anális regional
    if (input$a == 'r' & input$c != '.') {
      region = HDI2015Data$Region[HDI2015Data$Country == input$c]
      HDI2015DataRegional = subset(HDI2015Data, Region == region)
      g2 <- ggplot(HDI2015DataRegional, aes("Country", MeanYearsSchooling))
    }
    #Imprimimos el grafico boxplot de la variable
    g2 <- g2 + geom_boxplot()
    #Analizamos la situación del páis respecto a la variable
    level = HDI2015Data$MeanYearsSchooling[HDI2015Data$Country == input$c]
    if (input$c != '.') {
      if (input$a == 'r') {
        Percentil = quantile(HDI2015DataRegional$MeanYearsSchooling, c(.25, .50, .75))}
      else {
        Percentil = quantile(HDI2015Data$MeanYearsSchooling, c(.25, .50, .75))}
      #Determinaos el color del punto que representa el pais
      if (level <= Percentil[1])
        color="red"
      else if (level <= Percentil[2])
        color="orange"
      else if (level <= Percentil[3])
        color="blue"
      else color="green"
      #Imprimimos el punto del país  
      g2 <- g2 + geom_point(aes("Country", level),size=3,col=color)
    }
    print(g2)
  })   

  #Gráfico de Analisis por Pais (GrossNationalIncomeperCapita)
  output$plot26 <- renderPlot({
    theme_set(theme_bw())
    g2 <- ggplot(HDI2015Data, aes("Country", GrossNationalIncomeperCapita))
    #Filtramos la información en el caso de anális regional
    if (input$a == 'r' & input$c != '.') {
      region = HDI2015Data$Region[HDI2015Data$Country == input$c]
      HDI2015DataRegional = subset(HDI2015Data, Region == region)
      g2 <- ggplot(HDI2015DataRegional, aes("Country", GrossNationalIncomeperCapita))
    }
    #Imprimimos el grafico boxplot de la variable
    g2 <- g2 + geom_boxplot()
    #Analizamos la situación del páis respecto a la variable
    level = HDI2015Data$GrossNationalIncomeperCapita[HDI2015Data$Country == input$c]
    if (input$c != '.') {
      if (input$a == 'r') {
        Percentil = quantile(HDI2015DataRegional$GrossNationalIncomeperCapita, c(.25, .50, .75))}
      else {
        Percentil = quantile(HDI2015Data$GrossNationalIncomeperCapita, c(.25, .50, .75))}
      #Determinaos el color del punto que representa el pais
      if (level <= Percentil[1])
        color="red"
      else if (level <= Percentil[2])
        color="orange"
      else if (level <= Percentil[3])
        color="blue"
      else color="green"
      #Imprimimos el punto del país  
      g2 <- g2 + geom_point(aes("Country", level),size=3,col=color)
    }
    print(g2)
  })   
  
  output$plot3 <- renderPlot({
    theme_set(theme_bw())
    g <- ggplot(HDIEvolution, aes(GNI, HDI, size = Population, frame = Year, color=Region))
    g <- g + geom_point()
    g <- g +  geom_smooth(aes(group = Year), 
                          method = "lm", 
                          show.legend = FALSE)
    g <- g + scale_x_log10()
    gganimate(g,"HDIevolution.gif",interval=1.0,ani.width=1200, ani.height=400)
    g <- g + facet_wrap(~Region, scales = "free")
    gganimate(g,"HDIevolutionPerRegion.gif",interval=1.0,ani.width=1200, ani.height=400)
  })
  
  output$plot4 <- renderPlot({
    theme_set(theme_bw())
    corr <- round(cor(HDI2015Data[complete.cases(HDI2015Data),][,-(1:4),drop=FALSE]), 1)
    # Plot Correlogram
    ggcorrplot(corr, hc.order = TRUE, 
               type = "lower", 
               lab = TRUE, 
               lab_size = 3, 
               method="circle", 
               colors = c("tomato2", "white", "springgreen3"), 
               ggtheme=theme_bw)
  })
  output$plot5 <- renderD3heatmap({
    HDI2015Data2=as.data.frame(HDI2015Data)
    rownames(HDI2015Data2) = HDI2015Data$Country
    HDI2015Data2 <- HDI2015Data2[HDI2015Data2$Region == "Europe and Central Asia",]
    HDI2015Data2 = HDI2015Data2[,-(1:4)]
    d3heatmap(HDI2015Data2,Colv=NA,Rowv=NA,scale = "column", colors = "Reds", xaxis_font_size = "8px")
  })
  output$plot6 <- renderPlot({
    sPDF <- joinCountryData2Map(HDI2015Data, joinCode = "NAME", nameJoinColumn = "Country")
    par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
    mapCountryData( sPDF, nameColumnToPlot="HDI",mapTitle="")
  })
})
