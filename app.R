#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(epitools)
library(MASS)

ui <- tagList(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "./css/vampiros.css?v2")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=PT+Sans+Narrow|Pirata+One")),
  navbarPage("",
                 
                 tabPanel("Intro",{
                   fluidRow(
                     column(12,
                       
                       h1(style="font-size: 86px; font-family: 'Pirata One', 'Book Antiqua', Palatino, serif; text-align: center","Vampiros en Transversalia"),
                       h2(style="font-family: 'Book Antiqua', Palatino, serif; text-align: center; width: 60%; margin: auto;", "'Fueron humanos, pero ahora están en un estado intermedio entre la vida y la muerte, flacos, pálidos, y con largos y puntiagudos caninos'"
),
                       h3(style="font-family: 'Book Antiqua', Palatino, serif; text-align: center", "Julio Bonis y Verónica Bryant.")

                     )
                   )
                 }),
                 tabPanel("Presentación",{
                    fluidRow(
                      column(12,
                             verbatimTextOutput("tmp1"),
                             HTML("<iframe style='display: block; border: none; height: 80vh; width: 95vw; text-align: center' src='vampiros.html'></iframe>")
                             
                      )
                    )
                 }),
                 tabPanel("Dataset",{
                   h1("Dataset")
                   fluidRow(
                     column(12,
                       verbatimTextOutput("dataset")
                     )
                   )
                 }),
                 tabPanel("Univariable",{
                   h1("Univariable")
                   fluidRow(
                     column(4,
                       wellPanel(
                         uiOutput("univariable_list")
                         
                       ),
                       uiOutput("univariable_table")
                     ),
                     column(8,
                        uiOutput("univariable_report")
                     )
                   )
                   }),
                 tabPanel("Bivariable",{
                   h1("Bivariable")
                   fluidRow(
                     column(4,
                       wellPanel(
                         uiOutput("selector_bivariable")
                       )
                     ),
                     column(8,
                        uiOutput("bivariable_tabla")
                     )
                   )
                   }),
                 tabPanel("Multivariante crudo",{
                   h1("Multivariante")
                   fluidRow(
                     column(4,
                            wellPanel(
                              uiOutput("outcome_crude"),
                              uiOutput("exposures_crude")
                            )
                     ),
                     column(8,
                            uiOutput("multi_crude_output")
                            
                            
                     )
                   )
                 }),
                 tabPanel("Multivariante ajustado",{
                   h1("Multivariante")
                   fluidRow(
                     column(4,
                      wellPanel(
                         uiOutput("outcome"),
                         uiOutput("exposures"),
                         uiOutput("exposure_interacted"),
                         uiOutput("interaction"),
                         actionButton("run","Calcular")
                      )

                     ),
                     column(8,
                      uiOutput("regression_output")

                     )
                   )
                   })
                 
                )
)





# Define server logic required to draw a histogram
server <- function(input, output) {
  age_cutoff<-7000
  
  
  readMyData<-function(){
    data<-read.table("data/vampiros_dataset.txt", header=TRUE, sep="\t")
    data$anciano<-as.integer(data$edad>age_cutoff)
    return(data)
  }
  
  data <- data.frame()
  data<-readMyData() 
  valid_variables<-as.list(names(data)[names(data)!="id" & names(data)!="edad"])
  
  #Pantalla Dataset
  output$dataset<-renderPrint({data})
  
  
  #Pantalla Univariable
  
  output$univariable_list <- renderUI({
    checkboxGroupInput("univariable_list","Variables", choices=valid_variables, selected=valid_variables[1])
  })
  
  output$univariable_table <- renderUI({
    htmlTableUnivariable<-paste0("
      <table class='vampTable'>
        <thead><tr><th>n=",length(data[,1]),"</th><th class='tdempty'></th><th class='tdempty'></th><th class='tdempty'></th></tr></thead>
        <thead><tr><th>Variable</th><th>n</th><th>Proporcion</th><th>IC95%</th></tr></thead>")
    
    tableRowsUnivariable<-sapply(input$univariable_list,function(myvar){
      prop<-prop.test(table(data[,myvar])[2],length(data[,myvar]))
      return(paste("<tr><td>",if(myvar=='sexo'){"hombres"}else{myvar},":</td><td>",table(data[,myvar])[2],"</td><td>",format(round(prop$estimate,3),nsmall=3),"</td><td>[",format(round(prop$conf.int[1],2),nsmall=2),"-",format(round(prop$conf.int[2],2),nsmall=2),"]</td></tr>"))
    })
    
    for(row in tableRowsUnivariable) {
      htmlTableUnivariable<-paste(htmlTableUnivariable,row)
    }
    htmlTableUnivariable<-paste(htmlTableUnivariable,"</table>")
    mytaglist<-c(renderUI(HTML(htmlTableUnivariable)))
    
    tagList(mytaglist)
  })
  
  output$univariable_report <- renderUI({

    tmpuni<-sapply(input$univariable_list,function(myvar){
      prop<-prop.test(table(data[,myvar])[2],length(data[,myvar]))
      return(c(as.numeric(prop$estimate)))
      })
    mytaglist<-c(
      renderPlot(barplot(tmpuni,
                         ylim=c(0,1),
                         horiz=FALSE,
                         col=c("#FFEBCF")
                        )
                  )
    )
    
    if('anciano' %in% input$univariable_list) {
      #mostrar grafica de edad con punto de corte
      mytaglist<-c(mytaglist,renderPlot({
        hist(data$edad,main="Convertir variable contínua Edad en dicotómica Anciano",breaks=25)
        abline(v=age_cutoff,col="red")
      }))
    }
    
    tagList(mytaglist)
  })
  
  
  #Pantalla Bivariable
  
  output$selector_bivariable <- renderUI({
    tmp<-as.list(c(".",valid_variables))
    tagList(
      selectInput("bivar_a","Variable A", choices=valid_variables, selected=valid_variables[1]),
      selectInput("bivar_b","Variable B", choices=valid_variables, selected=valid_variables[2]),
      selectInput("bivar_c","Variable de ajuste", choices=tmp, selected=".")
    )
  })
  
  output$bivariable_tabla <- renderUI({
    cont_table<-table(data[,input$bivar_a],data[,input$bivar_b])
    dimnames(cont_table)[[1]]<-c(paste(input$bivar_a," NO"),paste(input$bivar_a," SI"))
    dimnames(cont_table)[[2]]<-c(paste(input$bivar_b," NO"),paste(input$bivar_b," SI"))
    or<-oddsratio(cont_table)
    if(input$bivar_c!=".") {
      #si se ha seleccionado una variable de ajuste
      #calculo tablas de contingencia por separado
      #y Maentel-Haenztsel
      cont_table_neg<-table(data[,input$bivar_a][data[,input$bivar_c]==0],data[,input$bivar_b][data[,input$bivar_c]==0])
      cont_table_pos<-table(data[,input$bivar_a][data[,input$bivar_c]==1],data[,input$bivar_b][data[,input$bivar_c]==1])
      or_neg<-oddsratio(cont_table_neg)
      or_pos<-oddsratio(cont_table_pos)
      mh_table<-array(
        c(cont_table_neg,cont_table_pos),
        dim=c(2,2,2)
      )
      mh<-mantelhaen.test(mh_table)
    }
    
    var_a_cat_0 <- if(input$bivar_a=="sexo"){"Mujer"}else{"No"}
    var_a_cat_1 <- if(input$bivar_a=="sexo"){"Hombre"}else{"Si"}
    var_b_cat_0 <- if(input$bivar_b=="sexo"){"Mujer"}else{"No"}
    var_b_cat_1 <- if(input$bivar_b=="sexo"){"Hombre"}else{"Si"}
    var_c_cat_0 <- if(input$bivar_c=="sexo"){"Mujer"}else{"No"}
    var_c_cat_1 <- if(input$bivar_c=="sexo"){"Hombre"}else{"Si"}
    
    tagList(
      fluidRow(
        column(width=3,""),
        column(width=6,
               HTML("<table class='vampTable'>
               <tr><td class='tdempty'>","","</td><td class='tdbold'>",input$bivar_b," ",var_b_cat_1,"</td><td class='tdbold'>",input$bivar_b," ",var_b_cat_0,"</td><td class='tdbold'>Total</td></tr>
                    <tr><td class='tdbold'>",input$bivar_a," ",var_a_cat_1,"</td><td>",cont_table[4],"</td><td>",cont_table[2],"</td><td>",cont_table[4]+cont_table[2],"</td></tr>
                    <tr><td class='tdbold'>",input$bivar_a," ",var_a_cat_0,"</td><td>",cont_table[3],"</td><td>",cont_table[1],"</td><td>",cont_table[3]+cont_table[1],"</td></tr>
                    <tr><td class='tdbold'>","Total","</td><td>",cont_table[4]+cont_table[3],"</td><td>",cont_table[2]+cont_table[1],"</td><td>",cont_table[4]+cont_table[2]+cont_table[3]+cont_table[1],"</td></tr>
                    <tr class='tdempty'><td colspan=4></td><tr>
                    <tr><td style='text-align: center;'>OR<br>[IC95%]</td><td>",format(round(or$measure[2,]['estimate'],2),nsmall=2),"<br>[",format(round(or$measure[2,]['lower'],2),nsmall=2),"-",format(round(or$measure[2,]['upper'],2),nsmall=2),"]</td><td style='text-align: right'>p=</td><td>",if(or$p.value[2,]['chi.square']<0.001){"<0.001"} else {format(round(or$p.value[2,]['chi.square'],3),nsmall=3)},"</td></tr>
                    "
               ),
               if(input$bivar_c!="."){
                 HTML("
                      <tr><td class='tdempty' colspan=4></td><tr>
                      <tr><td style='font-size: 14px'>ajustado<br>por",input$bivar_c,": </td><td>",format(round(mh$estimate,2),nsmall=2),"<br>[",format(round(mh$conf.int[1],2),nsmall=2),"-",format(round(or$measure[2,]['upper'],2),nsmall=2),"]</td><td style='text-align: right'>p=</td><td>",if(mh$p.value<0.001){"<0.001"} else {format(round(mh$p.value,3),nsmall=3)},"</td></tr>
                      ")
               },
               HTML("</table>")
               ),
        column(width=3,"")
        
      ),
      
      if(input$bivar_c!=".") {
        fluidRow(
          column(width=6,
                 HTML("<h3 style='text-align: center'>",input$bivar_c," = ",if(input$bivar_c=="sexo"){"Hombre"}else{"Si"},"</h3>",
                      "<table class='vampTable'>
                      <tr><td class='tdempty'></td><td class='tdbold'>",input$bivar_b," ",var_b_cat_1,"</td><td class='tdbold'>",input$bivar_b," ",var_b_cat_0,"</td><td class='tdbold'>Total</td></tr>
                      <tr><td class='tdbold'>",input$bivar_a," ",var_a_cat_1,"</td><td>",cont_table_pos[4],"</td><td>",cont_table_pos[2],"</td><td>",cont_table_pos[4]+cont_table_pos[2],"</td></tr>
                      <tr><td class='tdbold'>",input$bivar_a," ",var_a_cat_0,"</td><td>",cont_table_pos[3],"</td><td>",cont_table_pos[1],"</td><td>",cont_table_pos[3]+cont_table_pos[1],"</td></tr>
                      <tr><td class='tdbold'>Total</td><td>",cont_table_pos[4]+cont_table_pos[3],"</td><td>",cont_table_pos[2]+cont_table_pos[1],"</td><td>",cont_table_pos[4]+cont_table_pos[2]+cont_table_pos[3]+cont_table_pos[1],"</td></tr>
                      <tr class='tdempty'><td colspan=4></td><tr>
                      <tr><td style='text-align: center;'>OR<br>[IC95%]</td><td>",format(round(or_pos$measure[2,]['estimate'],2),nsmall=2),"<br>[",format(round(or_pos$measure[2,]['lower'],2),nsmall=2),"-",format(round(or_pos$measure[2,]['upper'],2),nsmall=2),"]</td><td style='text-align: right'>p=</td><td>",if(or_pos$p.value[2,]['chi.square']<0.001){"<0.001"} else {format(round(or_pos$p.value[2,]['chi.square'],3),nsmall=3)},"</td></tr>
                      </table>")
                 ),
          column(width=6,
                        HTML("<h3 style='text-align: center'>",input$bivar_c," = ",if(input$bivar_c=="sexo"){"Mujer"}else{"No"},"</h3>",
             "<table class='vampTable'>
                   <tr><td class='tdempty'></td><td class='tdbold'>",input$bivar_b," ",var_b_cat_1,"</td><td class='tdbold'>",input$bivar_b," ",var_b_cat_0,"</td><td class='tdbold'>Total</td></tr>
                   <tr><td class='tdbold'>",input$bivar_a," ",var_a_cat_1,"</td><td>",cont_table_neg[4],"</td><td>",cont_table_neg[2],"</td><td>",cont_table_neg[4]+cont_table_neg[2],"</td></tr>
                   <tr><td class='tdbold'>",input$bivar_a," ",var_a_cat_0,"</td><td>",cont_table_neg[3],"</td><td>",cont_table_neg[1],"</td><td>",cont_table_neg[3]+cont_table_neg[1],"</td></tr>
                   <tr><td class='tdbold'>Total</td><td>",cont_table_neg[4]+cont_table_neg[3],"</td><td>",cont_table_neg[2]+cont_table_neg[1],"</td><td>",cont_table_neg[4]+cont_table_neg[2]+cont_table_neg[3]+cont_table_neg[1],"</td></tr>
                   <tr class='tdempty'><td colspan=4></td><tr>
                   <tr><td style='text-align: center;'>OR<br>[IC95%]</td><td>",format(round(or_neg$measure[2,]['estimate'],2),nsmall=2),"<br>[",format(round(or_neg$measure[2,]['lower'],2),nsmall=2),"-",format(round(or_neg$measure[2,]['upper'],2),nsmall=2),"]</td><td style='text-align: right'>p=</td><td>",if(or_neg$p.value[2,]['chi.square']<0.001){"<0.001"} else {format(round(or_neg$p.value[2,]['chi.square'],3),nsmall=3)},"</td></tr>
                   </table>")
                 )
          
        )

      }
      

    )
  })
  
  #Pantalla Multivariable bruto
  
  output$outcome_crude <- renderUI({
    selectInput("outcome_crude","Variable principal", choices=valid_variables, selected=valid_variables[5])
  })
  
  output$exposures_crude <- renderUI({
    checkboxGroupInput("exposures_crude","Variables secundarias", choices=valid_variables, selected=valid_variables[2])
  })
  
  
  
  output$multi_crude_output <- renderUI({
    tmp3<-lapply(input$exposures_crude,function(myexposure){
      
      exp1Out0<-length(data[,myexposure][data[input$outcome_crude]==0&data[myexposure]==1])
      expAllOut0<-length(data[,myexposure][data[input$outcome_crude]==0])
      exp1Out1<-length(data[,myexposure][data[input$outcome_crude]==1&data[myexposure]==1])
      expAllOut1<-length(data[,myexposure][data[input$outcome_crude]==1])
      
      propout0<-prop.test(exp1Out0,expAllOut0)
      propout1<-prop.test(exp1Out1,expAllOut1)
      or.test<-oddsratio(table(data[,myexposure],data[,input$outcome_crude]))

      return(
          data.frame(
            name=myexposure,
            
            or=or.test$measure[2,]['estimate'],
            orlower=or.test$measure[2,]['lower'],
            orupper=or.test$measure[2,]['upper'],
            orchi2=or.test$p.value[2,]['chi.square'],
            out0exp1num=exp1Out0,
            out0exp0num=eval(expAllOut0-exp1Out0),
            out0prop=unname(propout0$estimate),
            out0proplower=propout0$conf.int[1],
            out0propupper=propout0$conf.int[2],
              
            out1exp1=exp1Out1,
            out1exp0=eval(expAllOut1-exp1Out1),
            out1prop=unname(propout1$estimate),
            out1proplower=propout1$conf.int[1],
            out1propupper=propout1$conf.int[2],
            expAllOut0=expAllOut0,
            expAllOut1=expAllOut1
              
          )
        )

      })
    
    ntrue <- length(data[,input$outcome_crude][data[input$outcome_crude]==1])
    nfalse <- length(data[,input$outcome_crude][data[input$outcome_crude]==0])

    myhtml<-"<table class='vampTable'>"
    
    myhtml<-paste(myhtml,"<thead><th class='tdempty'></th><th>",input$outcome_crude," ",if(input$outcome_crude=="sexo"){"Hombre"}else{"Si"},"<br>n=",ntrue,"</th><th>",input$outcome_crude," ",if(input$outcome_crude=="sexo"){"Mujer"}else{"No"},"<br>n=",nfalse,"</th><th class='tdbold'></th><th class='tdbold'></th></thead>")
    
    myhtml<-paste(myhtml,"<tr><td class='tdempty'></td><td>%<br>[IC95%]</td><td>%<br>[IC95%]</td><td>OR<br>[IC95%]</td><td>Chi2 p</td></tr>")

    for(row in tmp3) {
      myhtml<-paste(myhtml,"<tr><td class='tdbold'>",if(row$name=='sexo'){"hombres"}else{row$name},"</td><td>",format(round(row$out1prop*100,1),nsmall=1),"%<br>[",format(round(row$out1proplower*100,1),nsmall=1),"%-",format(round(row$out1propupper*100,1),nsmall=1),"%]</td><td>",format(round(row$out0prop*100,1),nsmall=1),"%<br>[",format(round(row$out0proplower*100,1),nsmall=1),"%-",format(round(row$out0propupper*100,1),nsmall=1),"%]</td><td>",format(round(row$or,2),nsmall=2),"<br>[",format(round(row$orlower,2),nsmall=2),"-",format(round(row$orupper,2),nsmall=2),"]</td><td>",if(row$orchi2<0.001){"<0.001"} else {format(round(row$orchi2,3),nsmall=3)},"</td></tr>", collapse="-")
    }
    
    myhtml<-paste(myhtml,"</table>")
    htmlTable<-renderUI(HTML(myhtml))
    
    tmp<-c(
      htmlTable
      )
    
    
    
    tagList(as.list(tmp))
  })
  
  #Pantalla Multivariable ajustado
  
 
  
  output$outcome <- renderUI({
    selectInput("outcome","Variable dependiente", choices=valid_variables, selected=valid_variables[1])
  })
  
  output$exposures <- renderUI({
    checkboxGroupInput("exposures","Variables independientes", choices=valid_variables, selected=valid_variables[1])
  })
  
  tmp<-as.list(c(".",valid_variables))
  
  output$exposure_interacted <- renderUI({
    selectInput("exposure_interacted","Variable de interacción", choices=tmp, selected=".")
  })
  
  output$interaction <- renderUI({
    selectInput("interaction","Variable de interacción", choices=tmp, selected=".")
  })
  
  

  htmlOutput<-reactiveValues(html="")
  
  observeEvent(input$run, {
    my_exposures <- paste0(input$exposures,collapse="+")
    if(input$exposure_interacted!="."&input$interaction!="."){
      my_interaction_term<-paste(c(input$exposure_interacted,"*",input$interaction),collapse="")
      my_exposures <- paste0(c(my_exposures,my_interaction_term),collapse="+")
    }
    my_formula <- sprintf("%s~%s",input$outcome,my_exposures)
    modelo.logit <- glm(as.formula(my_formula), data=data, family="binomial")
    my_results_names <- names(modelo.logit$coef)
    if(length(my_results_names)>1){
      my_results_ors <- format(round(exp(coef(modelo.logit)),2),nsmall=2)
      my_results_lowers <- format(round(exp(confint(modelo.logit)[,1]),2),nsmall=2)
      my_results_uppers <- format(round(exp(confint(modelo.logit)[,2]),2),nsmall=2)
      my_results_pvalues <- coef(summary(modelo.logit))[,4]
      htmlOutput$html<-""
      htmlOutput$html<-paste0(htmlOutput$html,"<table class='vampTable'>")
      htmlOutput$html<-paste0(htmlOutput$html,"<thead><th>Variable</th><th>OR</th><th colspan=2>IC95%</th><th>p-value</th></thead>")
      for(i in c(2:length(my_results_names))) { #2: to skip Intercept term
        
        htmlOutput$html<-paste0(htmlOutput$html,
                           "<tr>",
                           "<td class='tdbold'>",if(my_results_names[i]=='sexo'){"hombre"}else{my_results_names[i]},"</td>",
                           "<td>",my_results_ors[i],"</td>",
                           "<td>",my_results_lowers[i],"</td>",
                           "<td>",my_results_uppers[i],"</td>",
                           "<td>",if(my_results_pvalues[i]<0.001){"<0.001"}else{format(round(my_results_pvalues[i],4),nsmall=4)},"</td>",
                           "</tr>")
      }
      
      htmlOutput$html<-paste0(htmlOutput$html,"</table>")
      htmlOutput$html<-paste0(htmlOutput$html,"<div class='logit_formula'><span style='width: 100%; word-wrap:break-word; display:inline-block;' class='tdempty'>Fórmula de la regresión logística: ",my_formula,"</span></div>")
    }
  })  

  
  output$regression_output <- renderUI({
    tagList(as.list(c(renderUI(HTML(htmlOutput$html)))))
  }) 
  
  #Pantalla Presentación
  
  


  

  

  

  

  




}

# Run the application 
shinyApp(ui = ui, server = server)

