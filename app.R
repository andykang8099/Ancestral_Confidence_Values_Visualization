library("ggplot2")
library("shiny")
library('rsconnect')
library('shinydashboard')
library('dplyr')
library('tidyr')

# plotting function to be used in renderPlot()
PlotConfidenceValue=function(data,subtitle,range) {
  ggplot(data, aes(x=Position, y=value, fill=Nucleotide)) + 
    geom_area(alpha=0.6, size=0.5, colour="black") +
    ylab('Confidence Values') + 
    ggtitle('Confidence Values of Ancestral Sequences using Picornviridae',subtitle=subtitle) +
    scale_x_continuous(n.breaks = range)
}
PlotConfidenceValue2=function(data,subtitle,range) {
  ggplot(data, aes(x=Position, y=value, fill=Nucleotide)) + 
    geom_area(alpha=0.6, size=0.5, colour="black") +
    ylab('Confidence Values') + 
    ggtitle('Confidence Values of Ancestral Sequences using new approach, smaller datasets',subtitle=subtitle) +
    scale_x_continuous(n.breaks = range)
}
PlotConfidenceValue3=function(data,subtitle,range) {
  ggplot(data, aes(x=Position, y=value, fill=Nucleotide)) + 
    geom_area(alpha=0.6, size=0.5, colour="black") +
    ylab('Confidence Values') + 
    ggtitle('Confidence Values of Ancestral Sequences using Dicistrovirus',subtitle=subtitle) +
    scale_x_continuous(n.breaks = range)
}


# read the data

data1=read.table("./data/picornaviridae_triticum_5UTR_mitovirus_nucleotide_outgrouprooting_With_Triticum_nooutgroup_Ancestral_Confidence",header=TRUE)
data2=read.table("./data/picornaviridae_triticum_5UTR_mitovirus_nucleotide_outgrouprooting_noTriticum_nooutgroup_Ancestral_Confidence",header=TRUE)
data3=read.table("./data/flaviviridae_wo_outgroup_wo_flavivirus_N0_ConfValue.txt",header=TRUE)
data4=read.table("./data/picornaviridae_wo_outgroup_aligned_N0_ConfValue.txt",header=TRUE)
data5=read.table("./data/potyviridae_poacevirus_wo_outgroup_joint_ancestors_ConfValue.txt",header=TRUE)
data6=read.table("./data/dicistrovirus_I_wo_outgroup_aligned_N0_ConfValue.txt",header=TRUE)
data7=read.table("./data/dicistrovirus_II_wo_outgroup_aligned_N0_ConfValue.txt",header=TRUE)

# Set up the choices

choice1=c("Yes","No")
choice2=c("flaviviridae","picornaviridae","potyviridae")
choice3=c("IRES I","IRES II")


# User Interface ----
ui <- dashboardPage(
  
  dashboardHeader(title="Confidence Value Visualization", titleWidth=300),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Plot(Triticum mosaic virus)", tabName="Triticum_chart",icon = icon("line-chart")),
      menuItem("Table(Triticum mosaic virus)", tabName="Triticum_table",icon = icon("line-chart")),
      menuItem("Plot(New & smaller Dataset)", tabName="New_chart",icon = icon("line-chart")),
      menuItem("Table(New & smaller Dataset)", tabName="New_table",icon = icon("line-chart")),
      menuItem("Plot(Dicistrovirus)", tabName="Dic_chart",icon = icon("line-chart")),
      menuItem("Table(Dicistrovirus)", tabName="Dic_table",icon = icon("line-chart"))
    ),
    
    width=300, 
    
    radioButtons("metric",
                 label = "In the files with Triticum mosaic virus, Choose Keep Triticum or not:",
                 choices = choice1),
    
    radioButtons("metric2",
                 label = "In the Files 2021 (new approach, smaller datasets), Choose the type of virus:",
                 choices = choice2),
    
    radioButtons("metric3",
                 label = "In the Dicistrovirus files, Choose the type of IRES:",
                 choices = choice3),
    
    
    numericInput("fp", label="Select the first position", min = 1, max=nrow(data1)-1,step=1,value=1),
    numericInput("sp", label="Select the second position", min = 2, max=nrow(data1),step=1,value=10)
    #uiOutput("sliders")
  ),
  
  dashboardBody(
    tabItems(
      tabItem("Triticum_chart",
              plotOutput("f1", height="1000px")
              
      ),
      tabItem("Triticum_table",
              tableOutput("table1"),textOutput("text")
      ),
      tabItem("New_chart",
              plotOutput("f2", height="1000px")
      ),
      tabItem("New_table",
              tableOutput("table2"),textOutput("text2")
      ),
      tabItem("Dic_chart",
              plotOutput("f3", height="1000px")
      ),
      tabItem("Dic_table",
              tableOutput("table3"),textOutput("text3")
      )
      
      
    )
  )
)


seqlength=""

# Server logic ----
server <- function(input, output) {
  
  output$f1 <- renderPlot({
    if (input$sp <= input$fp) {
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, "Please Select a New First Position", 
           cex = 6, col = "black")
    } 
    
    else { if (input$metric =="Yes") {
      subtitle="With Triticum and Without Outgroup"
      data=data1[input$fp:input$sp,];data[,1]=1:nrow(data);data=pivot_longer(data,cols=2:5);colnames(data)[1]="Position";colnames(data)[2]="Nucleotide"
      
      PlotConfidenceValue(data,subtitle,range=(input$sp-input$fp+1))
    }
      else {
        subtitle="Without Triticum and Without Outgroup"
        data=data2[input$fp:input$sp,];data[,1]=1:nrow(data);data=pivot_longer(data,cols=2:5);colnames(data)[1]="Position";colnames(data)[2]="Nucleotide"
        PlotConfidenceValue(data,subtitle,range=(input$sp-input$fp+1))
      } 
    }
  })
  
  output$table1 <- renderTable({
    colnames(data1)[1]="Actual Position"
    if (input$metric =="Yes") {
      #("Confidence Value Table for With Triticum and Without Outgroup")
      data1[input$fp:input$sp,]
      #seqlength = paste0("The total length of the sequence is ", nrow(data1))
      #(paste0("The total length of the sequence is ", nrow(data1)))
    }
    else {
      #("Confidence Value Table for Without Triticum and Without Outgroup")
      data2[input$fp:input$sp,]
      #seqlength = paste0("The total length of the sequence is ", nrow(data2))
      #(paste0("The total length of the sequence is ", nrow(data1)))
    }
  })
  
  output$text <- renderPrint({
    if (input$metric =="Yes") {
      paste0("The total length of the sequence is ", nrow(data1))
      
    }
    else {
      paste0("The total length of the sequence is ", nrow(data2))
    }
  })
  
  output$f2 <- renderPlot({
    if (input$sp <= input$fp) {
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, "Please Select a New First Position", 
           cex = 6, col = "black")
    } 
    
    else { if (input$metric2 =="flaviviridae") {
      subtitle="flaviviridae"
      data=data3[input$fp:input$sp,];data[,1]=1:nrow(data);data=pivot_longer(data,cols=2:5);colnames(data)[1]="Position";colnames(data)[2]="Nucleotide"
      
      PlotConfidenceValue2(data,subtitle,range=(input$sp-input$fp+1))
    }
      else if (input$metric2 =="picornaviridae") {
        subtitle="picornaviridae"
        data=data4[input$fp:input$sp,];data[,1]=1:nrow(data);data=pivot_longer(data,cols=2:5);colnames(data)[1]="Position";colnames(data)[2]="Nucleotide"
        
        PlotConfidenceValue2(data,subtitle,range=(input$sp-input$fp+1))
      }
      else {
        subtitle="potyviridae"
        data=data5[input$fp:input$sp,];data[,1]=1:nrow(data);data=pivot_longer(data,cols=2:5);colnames(data)[1]="Position";colnames(data)[2]="Nucleotide"
        
        PlotConfidenceValue2(data,subtitle,range=(input$sp-input$fp+1))
      }
    }
  })
  
  output$table2 <- renderTable({
    if (input$metric2 =="flaviviridae") {
      
      data3[input$fp:input$sp,]
      
    }
    else if (input$metric2 =="picornaviridae") {
      
      data4[input$fp:input$sp,]
      
    }
    else {
      data5[input$fp:input$sp,]
    }
  })
  
  output$text2 <- renderPrint({
    if (input$metric2 =="flaviviridae") {
      paste0("The total length of the sequence is ", nrow(data3))
      
    }
    else if (input$metric2 =="picornaviridae") {
      paste0("The total length of the sequence is ", nrow(data4))
    }
    else {
      paste0("The total length of the sequence is ", nrow(data5))
    }
  })
  
  output$f3 <- renderPlot({
    if (input$sp <= input$fp) {
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, "Please Select a New First Position", 
           cex = 6, col = "black")
    } 
    
    else { if (input$metric3 == "IRES I") {
      subtitle="IRES I"
      data=data6[input$fp:input$sp,];data[,1]=1:nrow(data);data=pivot_longer(data,cols=2:5);colnames(data)[1]="Position";colnames(data)[2]="Nucleotide"
      
      PlotConfidenceValue3(data,subtitle,range=(input$sp-input$fp+1))
    }
      else {
        subtitle="IRES I"
        data=data7[input$fp:input$sp,];data[,1]=1:nrow(data);data=pivot_longer(data,cols=2:5);colnames(data)[1]="Position";colnames(data)[2]="Nucleotide"
        PlotConfidenceValue3(data,subtitle,range=(input$sp-input$fp+1))
      } 
    }
  })
  
  output$table3 <- renderTable({
    
    colnames(data6)[1]="Actual Position";colnames(data7)[1]="Actual Position"
    if (input$metric3 == "IRES I") {
      data6[input$fp:input$sp,]
    }
    else {
      data7[input$fp:input$sp,]
    }
  })
  
  output$text3 <- renderPrint({
    if (input$metric3 == "IRES I") {
      paste0("The total length of the sequence is ", nrow(data6))
      
    }
    else {
      paste0("The total length of the sequence is ", nrow(data7))
    }
  })
  
}

shinyApp(ui, server)



