library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinythemes)
#library(rgoslin)
library(janitor)
library(ggplot2)
library(png)
library(grid)
library(DT)
library(plotrix)
library(dplyr)
#library(RODBC)

#Connect to database

db = readRDS("db/PreanDatab.RDS")
fc = db$fc
an = db$an
con = db$con
ref = db$ref

#Filter respresented substance classes for each matrix (needs to be done only once)
plasma.mtrx = an$an_class[an$an_id%in%(fc$an_id[fc$prean_id %in% con$prean_id[con$matrix == "Plasma"]])] %>% unique() %>% sort() %>% as.list()
serum.mtrx = an$an_class[an$an_id%in%(fc$an_id[fc$prean_id %in% con$prean_id[con$matrix == "Serum"]])] %>% unique() %>% sort() %>% as.list()

plasma.an = an$an_name[an$an_id%in%(fc$an_id[fc$prean_id %in% con$prean_id[con$matrix == "Plasma"]])] %>% unique() %>% sort() %>% as.list()
serum.an = an$an_name[an$an_id%in%(fc$an_id[fc$prean_id %in% con$prean_id[con$matrix == "Serum"]])] %>% unique() %>% sort() %>% as.list()

cc.fc = fc$an_id[which(fc$prean_id %in% con$prean_id[grep("1|2",con$exp_id)])] %>% unique()
cc.mtrx = an[which(an$an_id %in% cc.fc),]
cc.fc = fc[which(fc$prean_id %in% con$prean_id[grep("1|2",con$exp_id)]),]
cc.class = cc.mtrx$an_class %>% unique() %>% as.list()

#Images for matrix selection menu
drops = data.frame(val = c("Plasma","Serum"))
drops$img <- c(sprintf('<img src="drops/plasma.png" width = 14px><div class="jhr">%s</div></img>',drops$val[1]),
               sprintf('<img src="drops/serum.png" width = 14px><div class="jhr">%s</div></img>',drops$val[2]))

#Warning flags for "Protocol search","Sample search" & "Data filtering mode"
flags <- c('<img src="flags/red.png" height="24"></img>',
           '<img src="flags/yel.png" height="24"></img>',
           '<img src="flags/gre.png" height="24"></img>',
           '<img src="flags/tra.png" height="24"></img>',
           '<img src="flags/x.png" height="24"></img>',
           '<img src="flags/gra.png" height="24"></img>')

#Warning flags for "Analyte search"
an.flag <- c("an.flags/moon.png",
             "an.flags/golf.png",
             "an.flags/looking.png",
             "an.flags/sea.png")

#Protocol names
prots <- c("A1","B1","A2","B2","C1","C2")

#QR code for contacting
#qr <- c("contact/qr.png")

#Protocol depictions
pr.png <- c("protos/a1a2.png",
            "protos/b1b2.png",
            "protos/c1c2.png")

trend = c('<img src="trend/trend_up.png" height="24"></img>',
          '<img src="trend/trend_down.png" height="24"></img>',
          '<img src="trend/trend_stable.png" height="24"></img>')

#Colors for pie plot
co <- c(red = "#cd6960",
        yel = "#f4d13b",
        gre = "#afe68b",
        tra = "#d2d2d2",
        x = "#4c4c4c",
        gra = "#a0a0a0")

#Function for generating pie plot
MyPie <- function(x){
    piecol = co[flags %in% x]
    ordered.flags = flags[match(x,flags)]
    piex = c(table(ordered.flags))
    piex = piex[match(flags,names(piex))] %>% na.omit()
    #Piex in wrong order
    pie3D(piex,
          theta = pi/3,
          col = piecol,
          border = "white",
          pty = "m",
          #explode = .1,
          mar = c(5,1,1,1))
}

#Download information
download.guide.cc = c("<ul><li>date = The date of your query</li>
                    <li>mode = Your chosen mode of protocol recommendation</li>
                    <li>query = Substance classes from your query</li>
                    <li>stability thresholds [%] = Chosen (or default) stability thresholds</li>
                    <li>recommendation = Overall sampling protocol recommendation (please also see provided PDFs)</li>
                    <li>analyte = A list of analytes with their respective protocols</li>
                    <li>ref = References for the shown estimation</li></ul>")

download.guide.samp = c("<ul><li>date = The date of your query</li>
                        <li>query = Substance classes from your query</li>
                        <li>stability thresholds [%] = Chosen (or default) stability thresholds</li>
                        <li>time to centrifugation [min] = Sample processing delay between collection and centrifugation. Chosen value and it's analyte individual approximation by experimental data</li>
                        <li>time to freeze [min] = Sample processing delay between centrifugation and freezing. Chosen value and it's analyte individual approximation by experimental data</li>
                        <li>temp. (during time to centr.)[°C] = Sample processing temperature during time to centrifugation</li>
                        <li>temp. (during time to freeze)[°C] = Sample processing temperature during time to freeze</li>
                        <li>status = stability estimation corresponding to each analyte depending on the chosen conditions</li>
                        <li>ref = References for the shown estimation</li></ul>")

download.guide.samp.serum = c("<ul><li>date = The date of your query</li>
                        <li>query = Substance classes from your query</li>
                        <li>stability thresholds [%] = Chosen (or default) stability thresholds</li>
                        <li>time to centrifugation [min] = Sample processing delay between collection and centrifugation. Chosen value and it's analyte individual approximation by experimental data</li>
                        <li>time to freeze [min] = Sample processing delay between centrifugation and freezing. Chosen value and it's analyte individual approximation by experimental data</li>
                        <li>temp. (during time to freeze)[°C] = Sample processing temperature during time to freeze</li>
                        <li>status = stability estimation corresponding to each analyte depending on the chosen conditions</li>
                        <li>ref = References for the shown estimation</li></ul>")

download.guide.an = c("<ul><li>date = The date of your query</li>
                    <li>query = Analyte from your query<</li>
                    <li>centr_dur = Duration of centrifugation</li>
                    <li>stability thresholds [%] = Chosen (or default) stability thresholds</li>
                    <li>recommendation = Overall sampling protocol recommendation (please also see provided PDFs)</li>
                    <li>time to centrifugation [min] = Sample processing delay between collection and centrifugation. Chosen value and it's analyte individual approximation by experimental data</li>
                    <li>time to freeze [min] = Sample processing delay between centrifugation and freezing. Chosen value and it's analyte individual approximation by experimental data</li>
                    <li>temp. (during time to centr.)[°C] = Sample processing temperature during time to centrifugation</li>
                    <li>temp. (during time to freeze)[°C] = Sample processing temperature during time to freeze</li>
                    <li>status = stability estimation corresponding to each analyte depending on the chosen conditions</li>
                    <li>trend = Indicator of exceedance of stability thresholds under given condition (including direction)</li>
                      <li>ref = References for the shown estimation</li></ul>")

download.guide.an.serum = c("<ul><li>date = The date of your query</li>
                    <li>query = Analyte from your query<</li>
                    <li>centr_dur = Duration of centrifugation</li>
                    <li>stability thresholds [%] = Chosen (or default) stability thresholds</li>
                    <li>recommendation = Overall sampling protocol recommendation (please also see provided PDFs)</li>
                    <li>time to centrifugation [min] = Sample processing delay between collection and centrifugation. Chosen value and it's analyte individual approximation by experimental data</li>
                    <li>time to freeze [min] = Sample processing delay between centrifugation and freezing. Chosen value and it's analyte individual approximation by experimental data</li>
                    <li>temp. (during time to freeze)[°C] = Sample processing temperature during time to freeze</li>
                    <li>status = stability estimation corresponding to each analyte depending on the chosen conditions</li>
                    <li>trend = Indicator of exceedance of stability thresholds under given condition (including direction)</li>
                      <li>ref = References for the shown estimation</li></ul>")

impressum = c("<p>Fraunhofer-Institut für Translationale Medizin und Pharmakologie ITMP<br>
            Theodor-Stern-Kai 7<br>
            60596 Frankfurt a. M.<br>
            Telefon: +49 69 6301-80231<br>
            Fax: +49 69 6301-7617<br>
            Email: info@itmp.fraunhofer.de<br>
            www.itmp.fraunhofer.de<br>
            ist eine rechtlich nicht selbständige Einrichtung der<br>
            Fraunhofer-Gesellschaft zur Förderung der angewandten
Forschung e.V.<br>
            Hansastraße 27 c<br>
            80686 München<br>
            Telefon: +49 89 1205-0<br>
            Fax: +49 89 1205-7531<br>
            www.fraunhofer.de</p>")

serum.guide = c("Pre-analytical data on serum stability is pretty sparse within our database right now. This is why we cant offer the 
                input for pre-analytical variation to be as flexible as in plasma search modes. We hope to include more data on serum
                stability in the future.")

samp.pro = data.frame(centr = c(60,240,60,240,1440,1440),
                      freeze = c(60,240,60,240,120,120),
                      temp = c(0,0,21,21,0,21))

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = shinytheme("paper"),
    sidebarLayout(
        sidebarPanel(
            HTML('<center><img src="alister_akronym.png" height = "320"></center>'),
            pickerInput("mtrx",h4(strong("Biomatrix")), choices = drops$val, choicesOpt = list(content = drops$img)),
            uiOutput("mode"),
            helpText("You can query the database for individual analytes (more detailed) or whole lipid & metabolite classes (for a broader overview)."),
            tags$head(tags$style("
                       .jhr{
                       display: inline;
                       vertical-align: middle;
                       padding-left: 8px;
                       }")),
            #Conditional Panel: Protocol search - Plasma====
            conditionalPanel(condition = "input.mode == `Protocol search` & input.mtrx == `Plasma`",
                             multiInput("look_cc", 
                                        label = "Search compound classes",
                                        choices = cc.class,
                                        selected = "Oxylipins"),
                             actionButton("ab.clear.cc","Clear all"),
                             helpText("Looking for a whole class of analytes"),
                             radioButtons("radio.cc.mode", 
                                          "", 
                                          choices = list("Maximize stable analytes" = 1,
                                                         "Majority vote" = 2),
                                          selected = 2),
                             bsCollapse(bsCollapsePanel("Info",helpText("Depending on whether you want to maximize the number of analytes, 
                                                           that are likely stable or just want to ensure, that most of the 
                                                           measured analytes are stable different protocols might be suitable.
                                                           `Choose `Maximize stable analytes` in order to choose the strictes 
                                                           of the protocols identified for your chosen compound class or choose 
                                                           `Majority Vote` in order to choose the protocol, that is identified 
                                                           most often."))),
                             checkboxInput("own.thresh.cc", "Define stability thresholds", value = FALSE),
                             uiOutput("own.t.cc"),
                             uiOutput("own.t.h.cc"),
                             HTML('<center><img src="sym/pdf.png" height = "90" width="90"></center>'),
                             column(12,downloadButton("download_protos_cl","Download"),align = "center"),
                             br(),
                             br(),
                             helpText("Download all of our sample handling protocols visualized as flow charts."),
                             HTML('<center><img src="sym/csv.png" height = "90" width="90"></center>'),
                             column(12,downloadButton("download_cl","Download"),align = "center"),
                             br(),
                             br(),
                             helpText("All information from your query can be downloaded in as a .csv-Table. Click below for a detailed description of the output variables."),
                             bsCollapse(bsCollapsePanel("Variable explanation",uiOutput("FeatList1", style = "info"))),
                             bsCollapse(bsCollapsePanel("Publishing Notes",uiOutput("imp.cc", style = "info")))
            ),
            #Conditional Panel: Sample Search - Plasma====
            conditionalPanel(condition = "input.mode == `Sample search` & input.mtrx == `Plasma`",
                             helpText("In this mode you can define the pre-analytical conditions, that are true for your samples. In additiona, when selecting a class, you will receive a recommendation based on the present presented data, as to whether compounds from the lipid class should still be considered for measurement."),
                             multiInput("look_samp", 
                                        label = "Search compound classes",
                                        choices = plasma.mtrx,#as.list(sort(unique(an$an_class))),
                                        selected = "Endocannabinoids & EC-like"),
                             fluidRow(
                                 column(3,actionButton("ab.all","Select all")),
                                 column(6,actionButton("ab.clear","Clear all"))
                             ),
                             fluidRow(
                                 column(9,sliderInput("slide.temp", h5(strong("Temperature [°C]")), min = 0, max = 24, value = 0)),
                                 column(3,numericInput("num.temp", "", min = 0, max = 24, value = 0))
                             ),
                             uiOutput("help.sec.samp.1"),
                             checkboxInput("sec.temp.samp","Other temperature after centrifugation", value = F),
                             fluidRow(
                                 column(9,uiOutput("post.temp.samp.slide")),
                                 column(3,uiOutput("post.temp.samp.num"))
                             ),
                             uiOutput("help.sec.samp.2"),
                             helpText("Intermediate storing temperature during sample processing."),
                             fluidRow(
                                 column(9, sliderInput("slide.time1", h5(strong("Delay until centrifugation [min]")), min = 1, max = 1440, value = 60)),
                                 column(3, numericInput("num.time1", "", min = 1, max = 1440, value = 60))
                             ),
                             helpText("Average duration in sample processing between blood drawing and centrifugation."),
                             fluidRow(
                                 column(9,sliderInput("slide.time2", h5(strong("Delay until final storage [min]")), min = 1, max = 240, value = 60)),
                                 column(3,numericInput("num.time2", "", min = 1, max = 240, value = 60))
                             ),
                             helpText("Average duration in sample processing between centrifugation and transfer to deep freezer."),
                             checkboxInput("own.thresh.samp", "Define stability thresholds", value = FALSE),
                             uiOutput("own.t.samp"),
                             uiOutput("own.t.h.samp"),
                             HTML('<center><img src="sym/csv.png" height = "90" width="90"></center>'),
                             column(12,downloadButton("download_s","Download"),align = "center"),
                             br(),
                             br(),
                             helpText("All information from your query can be downloaded in as a .csv-Table. Click below for a detailed description of the output variables."),
                             bsCollapse(bsCollapsePanel("Variable explanation",uiOutput("FeatList2", style = "info"))),
                             bsCollapse(bsCollapsePanel("Publishing Notes",uiOutput("imp.samp.p", style = "info")))
            ),
            #Conditional Panel: Analyte search - Plasma====
            conditionalPanel(condition = "input.mode == `Analyte search` & input.mtrx == `Plasma`",
                             selectInput("look_an",  h4(strong("Analyte")), choices = plasma.an, selected = 1),
                             helpText("Look up a specific analytes"),
                             #radioButtons("an.radio.tube", h5(strong("Blood sampling tube")), choices = list(c("K3EDTA"),c("GlucoExact"))),
                             helpText("Get stability information on your selected analyte by entering your sampling protocol below."),
                             fluidRow(
                                 column(9,sliderInput("an.slide.temp", h5(strong("Temperature [°C]")), min = 0, max = 24, value = 0)),
                                 column(3,numericInput("an.num.temp", "", min = 0, max = 24, value = 0))
                             ),
                             uiOutput("help.sec.an.1"),
                             checkboxInput("sec.temp.an","Other temperature after centrifugation", value = F),
                             fluidRow(
                                 column(9,uiOutput("post.temp.an.slide")),
                                 column(3,uiOutput("post.temp.an.num"))
                             ),
                             uiOutput("help.sec.an.2"),
                             helpText("Intermediate storing temperature during sample processing."),
                             fluidRow(
                                 column(9, sliderInput("an.slide.time1", h5(strong("Delay until centrifugation [min]")), min = 1, max = 1440, value = 60)),
                                 column(3, numericInput("an.num.time1", "", min = 1, max = 1440, value = 60))
                             ),
                             helpText("Average duration in sample processing between blood drawing and centrifugation."),
                             fluidRow(
                                 column(9,sliderInput("an.slide.time2", h5(strong("Delay until final storage [min]")), min = 1, max = 240, value = 60)),
                                 column(3,numericInput("an.num.time2", "", min = 1, max = 240, value = 60))
                             ),
                             helpText("Average duration in sample processing between centrifugation and transfer to deep freezer."),
                             checkboxInput("own.thresh.an", "Define stability thresholds", value = FALSE),
                             uiOutput("own.t.an"),
                             uiOutput("own.t.h.an"),
                             HTML('<center><img src="sym/pdf.png" height = "90" width="90"></center>'),
                             column(12,downloadButton("download_protos_an","Download"),align = "center"),
                             br(),
                             br(),
                             helpText("Download our sample handling protocols visualized as flow charts."),
                             HTML('<center><img src="sym/csv.png" height = "90" width="90"></center>'),
                             column(12,downloadButton("download_an","Download"),align = "center"),
                             br(),
                             br(),
                             helpText("All information that the results are based on can be downloaded in as a .csv-Table. Click below for a detailed description of the output variables."),
                             bsCollapse(bsCollapsePanel("Variable explanation",uiOutput("FeatList3", style = "info"))),
                             bsCollapse(bsCollapsePanel("Publishing Notes",uiOutput("imp.an.p", style = "info")))
            ),
            #Conditional Panel: Data filtering mode - Plasma====
            conditionalPanel(condition = "input.mode == `Data filtering mode` & input.mtrx == `Plasma`",
                             h4(strong("Data filtering")),
                             helpText("In data filtering mode you are able to upload your own analysis results, enter your pre-analytical conditions and filter out potentially unstable analytes. 
                                      Your data is expected to have samples in rows and analytes in columns. 
                                      We will not store or analyze your measurements. When in doubt, you can even upload an empty table, 
                                      with just column names."),
                             fileInput("csvtable","Choose CSV-file for filtering",
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")),
                             radioButtons("sep","Separator",
                                          choices = c(Comma = ",",
                                                      Semicolon = ";",
                                                      Tab = "\t"),
                                          selected = ","),
                             fluidRow(
                                 column(9,sliderInput("filt.slide.temp", h5(strong("Temperature [°C]")), min = 0, max = 24, value = 0)),
                                 column(3,numericInput("filt.num.temp", "", min = 0, max = 24, value = 0))
                             ),
                             uiOutput("help.sec.filt.1"),
                             checkboxInput("sec.temp.filt","Other temperature after centrifugation", value = F),
                             fluidRow(
                                 column(9,uiOutput("post.temp.filt.slide")),
                                 column(3,uiOutput("post.temp.filt.num"))
                             ),
                             uiOutput("help.sec.filt.2"),
                             helpText("Intermediate storing temperature during sample processing."),
                             fluidRow(
                                 column(9, sliderInput("filt.slide.time1", h5(strong("Delay until centrifugation [min]")), min = 1, max = 1440, value = 60)),
                                 column(3, numericInput("filt.num.time1", "", min = 1, max = 1440, value = 60))
                             ),
                             helpText("Average duration in sample processing between blood drawing and centrifugation."),
                             fluidRow(
                                 column(9,sliderInput("filt.slide.time2", h5(strong("Delay until final storage [min]")), min = 1, max = 240, value = 60)),
                                 column(3,numericInput("filt.num.time2", "", min = 1, max = 240, value = 60))
                             ),
                             helpText("Average duration in sample processing between centrifugation and transfer to deep freezer."),
                             checkboxInput("own.thresh.filt", "Define stability thresholds", value = FALSE),
                             uiOutput("own.t.filt"),
                             uiOutput("own.t.h.filt"),
                             helpText("Download your filtered data here"),
                             HTML('<center><img src="sym/csv.png" height = "90" width="90"></center>'),
                             column(12,downloadButton("download_filt","Download"),align = "center"),
                             br(),
                             br(),
                             bsCollapse(bsCollapsePanel("Publishing Notes",uiOutput("imp.filt.p", style = "info")))
            ),
            #Conditional Panel: Sample search - Serum====
            conditionalPanel(condition = "input.mode == `Sample search` & input.mtrx == `Serum`",
                             helpText("In this mode you can define the preanalytical conditions, that are true for your samples. When selecting a class in addition, you will receive a recommendation based on the present data, on whether compounds from the lipid class should still be considered for measurement."),
                             helpText(serum.guide),
                             multiInput("look_samp_serum", 
                                        label = "Search compound classes",
                                        choices = serum.mtrx,#as.list(sort(unique(an$an_class))),
                                        selected = "Ceramides"),
                             fluidRow(
                                 column(3,actionButton("ab.all_serum","Select all")),
                                 column(6,actionButton("ab.clear_serum","Clear all"))
                             ),
                             fluidRow(
                                 column(9,sliderInput("slide.temp_serum", h5(strong("Temperature [°C]")), min = 0, max = 24, value = 21)),
                                 column(3,numericInput("num.temp_serum", "", min = 0, max = 24, value = 21))
                             ),
                             helpText("Intermediate storing temperature during clotting."),
                             fluidRow(
                                 column(9,sliderTextInput("slide.time2_serum", h5(strong("Delay until final storage [min]")), choices = c(180,480,1440), selected = 1440, grid = T))
                             ),
                             helpText("Average duration in sample processing between centrifugation and transfer to deep freezer."),
                             checkboxInput("own.thresh.samp_serum", "Define stability thresholds", value = FALSE),
                             uiOutput("own.t.samp_serum"),
                             uiOutput("own.t.h.samp_serum"),
                             HTML('<center><img src="sym/csv.png" height = "90" width="90"></center>'),
                             column(12,downloadButton("download_s_serum","Download"),align = "center"),
                             br(),
                             br(),
                             helpText("All information from your query can be downloaded in as a .csv-Table. Click below for a detailed description of the output variables."),
                             bsCollapse(bsCollapsePanel("Variable explanation",uiOutput("FeatList2_serum", style = "info"))),
                             bsCollapse(bsCollapsePanel("Publishing Notes",uiOutput("imp.samp.s", style = "info")))
            ),
            #Conditional Panel: Analyte search - Serum====
            conditionalPanel(condition = "input.mode == `Analyte search` & input.mtrx == `Serum`",
                             selectInput("look_an_serum",  h4(strong("Analyte")), choices = serum.an, selected = 1),
                             helpText("Looking up a specific analytes"),
                             #radioButtons("an.radio.tube", h5(strong("Blood sampling tube")), choices = list(c("K3EDTA"),c("GlucoExact"))),
                             helpText(serum.guide),
                             helpText("Get stability information on your selected analyte by entering your sampling protocol below."),
                             fluidRow(
                                 column(9,sliderInput("an.slide.temp_serum", h5(strong("Temperature [°C]")), min = 0, max = 24, value = 21)),
                                 column(3,numericInput("an.num.temp_serum", "", min = 0, max = 24, value = 21))
                             ),
                             helpText("Intermediate storing temperature during clotting."),
                             fluidRow(
                                 column(9,sliderTextInput("an.slide.time2_serum", h5(strong("Delay until final storage [min]")), choices = list(180,480,1440), selected = 1440, grid = T))
                             ),
                             helpText("Average duration in sample processing between centrifugation and transfer to deep freezer."),
                             checkboxInput("own.thresh.an_serum", "Define stability thresholds", value = FALSE),
                             uiOutput("own.t.an_serum"),
                             uiOutput("own.t.h.an_serum"),
                             helpText("Download our sample handling protocols visualized as flow charts."),
                             HTML('<center><img src="sym/csv.png" height = "90" width="90"></center>'),
                             column(12,downloadButton("download_an_serum","Download"),align = "center"),
                             br(),
                             br(),
                             helpText("All information that the results are based on can be downloaded in as a .csv-Table. Click below for a detailed description of the output variables."),
                             bsCollapse(bsCollapsePanel("Variable explanation",uiOutput("FeatList3_serum", style = "info"))),
                             bsCollapse(bsCollapsePanel("Publishing Notes",uiOutput("imp.an.s", style = "info")))
            ),
            #Conditional Panel: Data filtering mode - Serum====
            conditionalPanel(condition = "input.mode == `Data filtering mode` & input.mtrx == `Serum`",
                             h4(strong("Data filtering")),
                             helpText("In data filtering mode you are able to upload your own analysis results, enter your pre-analytical conditions and filter out potentially unstable analytes. 
                                      Your data is expected to have samples in rows and analytes in columns. 
                                      We will not store or analyze your measurements. When in doubt, you can even upload an empty table, 
                                      with just column names."),
                             helpText(serum.guide),
                             fileInput("csvtable_serum","Choose CSV-file for filtering",
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")),
                             radioButtons("sep_serum","Separator",
                                          choices = c(Comma = ",",
                                                      Semicolon = ";",
                                                      Tab = "\t"),
                                          selected = ","),
                             fluidRow(
                                 column(9,sliderInput("filt.slide.temp_serum", h5(strong("Temperature [°C]")), min = 0, max = 24, value = 21)),
                                 column(3,numericInput("filt.num.temp_serum","",min = 0, max = 24, value = 21))
                             ),
                             helpText("Intermediate storing temperature during clotting."),
                             fluidRow(
                                 column(9,sliderTextInput("filt.slide.time2_serum", h5(strong("Delay until final storage [min]")), choices = list(180,480,1440), selected = 1440, grid = T))
                             ),
                             helpText("Average duration in sample processing between centrifugation and transfer to deep freezer."),
                             checkboxInput("own.thresh.filt_serum", "Define stability thresholds", value = FALSE),
                             uiOutput("own.t.filt_serum"),
                             uiOutput("own.t.h.filt_serum"),
                             helpText("Download filtered data here"),
                             HTML('<center><img src="sym/csv.png" height = "90" width="90"></center>'),
                             column(12,downloadButton("download_filt_serum","Download"),align = "center"),
                             br(),
                             br(),
                             bsCollapse(bsCollapsePanel("Publishing Notes",uiOutput("imp.filt.s", style = "info")))
            )
        ),
        mainPanel(h1(strong("Results"), align = "left"),
                  modalDialog(
                      HTML('<center><img src="limits/stamp.png" height = "160" width="160"></center>'),
                      tags$div(#tags$br(),
                          h4("Disclaimer",align = "center"),
                          tags$br(),
                          "We cannot assume any liability for the content of external pages. The operators of those linked pages are solely responsible for their content.", 
                          tags$br(), 
                          tags$br(),
                          "We make every reasonable effort to ensure that the content of this Web site is kept up to date, and that it is accurate and complete. Nevertheless, the possibility of errors cannot be entirely ruled out. We do not give any warranty in respect of the timeliness, 
                               accuracy or completeness of material published on this Web site, and disclaim all liability for (material or non-material) loss or damage incurred by third parties arising from the use of content obtained from the Web site.", 
                          tags$br(),
                          tags$br(),
                          "Registered trademarks and proprietary names, and copyrighted text and images, are not generally indicated as such on our Web pages. But the absence of such indications in no way implies that these names, 
                               images or text belong to the public domain in the context of trademark or copyright law."),
                      size = "l",
                      easyClose = FALSE,
                      footer = modalButton("Agree")
                  ),
                  #Main Panel: Protocol search - Plasma====
                  conditionalPanel(
                      condition = "input.mode == `Protocol search` & input.mtrx == `Plasma`",
                      h4(textOutput("text.cl")),
                      textOutput("pr"),
                      br(),
                      uiOutput("downpr_cc"),
                      bsModal("popup_cc","Recommended protocol","downpr_cc", uiOutput("show.prot.cc")),
                      br(),
                      dataTableOutput("clt"),
                      br(),
                      bsCollapse(bsCollapsePanel("Citation",dataTableOutput("cl.ref"))),
                      fluidRow(
                          column(4,img(src='leg/legcc.PNG', height = "200", align = "left")),
                          column(4,plotOutput("cc.pie", height = "300",width = "450"))
                          #column(4,HTML('<center><img src="contact/contact.png" height = "220"></center>'))
                      )
                      
                  ),
                  #Main Panel: Sample search - Plasma====
                  conditionalPanel(
                      condition = "input.mode == `Sample search` & input.mtrx == `Plasma`", 
                      h4(textOutput("pr.s")),
                      br(),
                      helpText("If experimental data from our data base do not exactly match the conditions you entered,\nthe conditions most close to your entry are considered.\nYou can hover your mouse over each row in order to see this approximation."),
                      textOutput("inter.time1"),
                      br(),
                      dataTableOutput("con_s"),
                      br(),
                      bsCollapse(bsCollapsePanel("Citation",dataTableOutput("samp.ref"))),
                      fluidRow(
                          column(4,HTML('<center><img src="leg/legsamps.PNG" height = "220"></center>')),
                          column(4,plotOutput("samp.pie", height = "300",width = "450"))
                          #column(4,HTML('<center><img src="contact/contact.png" height = "220"></center>'))
                      )
                  ),
                  #Main Panel: Analyte search - Plasma====
                  conditionalPanel(
                      condition = "input.mode == `Analyte search` & input.mtrx == `Plasma`",
                      h3(textOutput("text.an")),
                      fluidRow(
                          column(12,uiOutput("own.prot.an.sym"),
                                 column(9,
                                        strong(h4("General protocol recommendation")),
                                        textOutput("an.rec"),
                                        br(),
                                        uiOutput("downpr_an"),
                                        bsModal("popup_an","Recommended protocol","downpr_an", uiOutput("show.prot.an")),
                                        strong(h4("Assessment of your protocol")),
                                        textOutput("prot.an.rec"),
                                        helpText("If experimental data from our data base do not exactly match the conditions you entered,\nthe conditions most close to your entry are considered.\nYou can click on `Details` below in order to see the data used in this approximation."),
                                 )
                          )
                      ),
                      br(),
                      bsCollapse(bsCollapsePanel("Details", dataTableOutput("an.detail")),
                                 bsCollapsePanel("Citation", dataTableOutput("an.ref")))
                  ),
                  #Main Panel: Data filtering mode - Plasma====
                  conditionalPanel(
                      condition = "input.mode == `Data filtering mode` & input.mtrx == `Plasma`",
                      helpText("In this panel your data will be analyzed in real time and metabolite stability will be assessed depending on the pre-analytical conditions you entered. If experimental data from our data base do not exactly match the conditions you entered,\nthe conditions most close to your entry are considered.\nYou can hover your mouse over each row in order to see this approximation."),
                      strong(textOutput("upload.status")),
                      dataTableOutput("csvfilt"),
                      bsCollapse(bsCollapsePanel("Citation",dataTableOutput("filt.ref"))),
                      fluidRow(
                          column(4,img(src='leg/legfilt.PNG', height = "200", align = "left")),
                          column(4,plotOutput("filt.pie", height = "300",width = "450"))
                          #column(4,HTML('<center><img src="contact/contact.png" height = "220"></center>'))
                      )
                  ),
                  #Main Panel: Sample search - Serum====
                  conditionalPanel(
                      condition = "input.mode == `Sample search` & input.mtrx == `Serum`", 
                      h4(textOutput("pr.s_serum")),
                      br(),
                      helpText("If experimental data from our data base do not exactly match the conditions you entered,\nthe conditions most close to your entry are considered.\nYou can hover your mouse over each row in order to see this approximation."),
                      br(),
                      dataTableOutput("con_s_serum"),
                      br(),
                      bsCollapse(bsCollapsePanel("Citation",dataTableOutput("samp.ref_serum"))),
                      fluidRow(
                          column(4,img(src='leg/legsamps2.PNG', height = "200", align = "left")),
                          column(4,plotOutput("samp.pie_serum", height = "300",width = "450")),
                          #column(4,HTML('<center><img src="contact/contact.png" height = "220"></center>'))
                      )
                  ),
                  #Main Panel: Analyte search - Serum====
                  conditionalPanel(
                      condition = "input.mode == `Analyte search` & input.mtrx == `Serum`",
                      h3(textOutput("text.an_serum")),
                      fluidRow(
                          column(12,uiOutput("own.prot.an.sym_serum"),
                                 column(9,
                                        strong(h4("Assessment of your protocol")),
                                        textOutput("prot.an.rec_serum"),
                                        helpText("If experimental data from our data base do not exactly match the conditions you entered,\nthe conditions most close to your entry are considered.\nYou can click on `Details` below in order to see the data used in this approximation.")))
                      ),
                      br(),
                      bsCollapse(bsCollapsePanel("Details", dataTableOutput("an.detail_serum")),
                                 bsCollapsePanel("Citation", dataTableOutput("an.ref_serum")))
                  ),
                  #Main Panel: Data filtering mode - Serum====
                  conditionalPanel(
                      condition = "input.mode == `Data filtering mode` & input.mtrx == `Serum`",
                      helpText("In this panel your data will be analyzed in real time and metabolite stability will be assessed depending on the pre-analytical conditions you entered. 
                                A version of your dataset, where unstable analytes were filtered out can be accessed by clicking on `Download` to the left. If experimental data from our data base do not exactly match the conditions you entered,\nthe conditions most close to your entry are considered.\nYou can hover your mouse over each row in order to see this approximation."),
                      strong(textOutput("upload.status_serum")),
                      dataTableOutput("csvfilt_serum"),
                      bsCollapse(bsCollapsePanel("Citation",dataTableOutput("filt.ref_serum"))),
                      fluidRow(
                          column(4,img(src='leg/legfilt2.PNG', height = "200", align = "left")),
                          column(4,plotOutput("filt.pie_serum", height = "300",width = "450"))
                          #column(4,HTML('<center><img src="contact/contact.png" height = "220"></center>'))
                      )
                  ),
        )
    )
)

server <- function(input, output, session) {
    
    #Sidebar Info====
    output$FeatList1 <- renderUI(HTML(download.guide.cc))
    output$FeatList2 <- renderUI(HTML(download.guide.samp))
    output$FeatList3 <- renderUI(HTML(download.guide.an))
    
    output$FeatList2_serum <- renderUI(HTML(download.guide.samp.serum))
    output$FeatList3_serum <- renderUI(HTML(download.guide.an.serum))
    
    output$imp.cc <- renderUI(HTML(impressum))
    output$imp.samp.p <- renderUI(HTML(impressum))
    output$imp.an.p <- renderUI(HTML(impressum))
    output$imp.filt.p <- renderUI(HTML(impressum))
    
    output$imp.samp.s <- renderUI(HTML(impressum))
    output$imp.an.s <- renderUI(HTML(impressum))
    output$imp.filt.s <- renderUI(HTML(impressum))
    
    output$mode <- renderUI({
        if (input$mtrx == "Plasma"){
            selectInput("mode",
                        h5(strong("Search mode")),
                        choices = list(c("Sample search"),
                                       c("Analyte search"),
                                       c("Data filtering mode"),
                                       c("Protocol search")), 
                        selected = 1)
        } else if (input$mtrx == "Serum"){
            selectInput("mode",
                        h3(strong("Search mode")),
                        choices = list(c("Sample search"),
                                       c("Analyte search"),
                                       c("Data filtering mode")), 
                        selected = 1)
        }
    })
    
    # Protocol search - Plasma####
    output$own.t.cc <- renderUI({
        req(input$own.thresh.cc == TRUE)
        sliderInput("own.t.cc",
                    h5(strong("Stability Threshold [%]")),
                    min = 0,
                    max = 100,
                    step = 5,
                    value = c(20,30))
    })
    
    output$own.t.h.cc <- renderUI({
        req(input$own.thresh.cc == TRUE)
        helpText("ALISTER defines stability by setting cut-offs for fold changes specific
                 to analytes subjected to certain pre-analytical conditions. The lower
                 threshold poses as the critical cut-off for singular foldchanges (default is 20%). The upper
                 fold change is the critical cut-off for summed up fold changes (default is 30%).")
    })
    
    mult_names_cc <- reactive({
        names <- input$look_cc
        if (length(names) > 1){
            names <- paste("Currently looking at", paste(names, collapse = ", "))
        } else if (length(names) == 1){
            names <- paste("Currently looking at", names)
        } else {
            names <- c()
        }
        return(names)
    })    
    
    output$text.cl <- renderText({mult_names_cc()})
    
    rv.multi.cc = reactiveValues(data = NULL)
    
    observeEvent(input$ab.clear.cc,{
        rv.multi.cc$data = character(0)
    })
    
    observeEvent(input$ab.clear.cc,{
        updateMultiInput(session = session,
                         inputId = "look_cc",
                         selected = rv.multi.cc$data)
    })
    
    mult_class <- reactive({
        if(is.null(input$look_cc)){
            cl <- NA
        } else {
            lookup <- input$look_cc
            cl_list = lapply(lookup,function(x){return(an[which(an[,3] %in% x[1]),1])})
            cl <- unlist(cl_list)  
        }
        return(cl)
    })
    
    #anincl is a list of analytes from the queried class (FC/Prean)
    #exp_wise calculated the foldchanges
    #p.tests look up the fitting protocol and stores the results in p.rec (transformed to output table)
    #not.p finds analytes, that do not conform with chosen protocol and gives them a yellow flag
    #main.p gives a statement of which protocol is recommended
    tab_class <- reactive({
        if(is.null(input$look_cc)){
            p.rec <- data.frame(protocol = NA, status = NA)
        } else {
            #Load list with analyte ids
            cl <- mult_class()
            
            all_cl <- cc.fc[which(!is.na(match(cc.fc$an_id,cl))),]
            all_con <- con[unique(match(all_cl$prean_id,con$prean_id)),]
            all_con <- all_con[all_con$matrix == "Plasma",]
            all_con$prean_temp = cut(all_con$prean_temp, breaks = c(-Inf,8,16,Inf), labels = c(0,NA,21))
            all_con = subset(all_con, `blood_tube` == "K3EDTA")
            all_cl <- all_cl[all_cl$prean_id %in% all_con$prean_id,]
            
            ## Define stability thresholds----
            if(input$own.thresh.cc == TRUE){
                thresh1 = input$own.t.cc[1]/100
                thresh2.1 = 1-input$own.t.cc[2]/100
                thresh2.2 = 1+input$own.t.cc[2]/100
            } else {
                thresh1 = 0.2
                thresh2.1 = 0.7
                thresh2.2 = 1.3
            }
            
            uni.an = as.list(unique(all_cl$an_name))
            uni.ind = lapply(uni.an,function(x){all_cl[which(all_cl$an_name %in% x),]})
            uni.ind = lapply(uni.ind,function(x){return(x[unique(match(unique(x$prean_id),x$prean_id)),])})
            uni.con = lapply(uni.ind,function(x){all_con[which(all_con$prean_id %in% x$prean_id),]})
            names(uni.ind) = unlist(uni.an)
            names(uni.con) = unlist(uni.an)
            
            uni.exp = lapply(uni.con,function(x){as.list(unique(x$exp_id))})
            
            uni.prot = lapply(seq_along(uni.con),function(i){
                lapply(seq_along(uni.exp[[i]]),function(j){
                    time.to.centr = uni.con[[i]][which((uni.con[[i]]$exp_id == uni.exp[[i]][[j]])&(uni.con[[i]]$centr_time>0)),]
                    time.to.freeze = uni.con[[i]][which((uni.con[[i]]$exp_id == uni.exp[[i]][[j]])&(uni.con[[i]]$freeze_time>0)),]
                    return(list(time.to.centr = time.to.centr,
                                time.to.freeze = time.to.freeze))
                })
            })
            
            names(uni.prot) = unlist(uni.an)
            
            #Any missing conditions?
            any.na = lapply(uni.prot,function(x){lapply(x,function(y){lapply(y,function(z){return(nrow(z)>0)})})})
            any.na = lapply(any.na,function(x){lapply(x,function(y){any(!unlist(y))})})
            
            #all.na = lapply(any.na,function(x){all(!unlist(x))}) %>% unlist() %>% which()
            
            #Clear experiments,that dont contribute to assessment
            if(any(unlist(any.na))){
                uni.prot = lapply(seq_along(uni.prot),function(i){return(uni.prot[[i]][-which(unlist(any.na[[i]]))])})
            }
            
            all.na = sapply(uni.prot,function(x){length(x) == 0}) %>% which()
            #Clear analytes, with insufficient data (no data at all)
            
            if(length(all.na) > 0){
                na.ind = uni.ind[all.na]
                
                rem.ind = uni.ind[-all.na]
                rem.prot = uni.prot[-all.na]
            } else {
                na.ind = NULL
                
                rem.ind = uni.ind
                rem.prot = uni.prot
            }
            
            names(rem.prot) = names(rem.ind)
            
            exact.prot = lapply(rem.prot,function(x){
                lapply(x,function(y){
                    ttc = merge(samp.pro,y[[1]],by.y = c("centr_time","prean_temp"),by.x = c("centr","temp"),all.x = T, sort = T);
                    ttc = ttc[order(ttc$centr),];
                    ttf = merge(samp.pro,y[[2]],by.y = c("freeze_time","prean_temp"),by.x = c("freeze","temp"),all.x = T);
                    ttf = ttf[order(ttf$centr),];
                    return(list(centr.fc = ttc,freeze.fc = ttf))
                })
            })
            
            #Obtain pre-analytical conditions for protocol construction
            fc.prot = lapply(seq_along(exact.prot),function(i){
                lapply(seq_along(exact.prot[[i]]),function(j){
                    ttc.fc = merge(exact.prot[[i]][[j]]$centr.fc,rem.ind[[i]],by = "prean_id", all.x = T);
                    ttc.fc = ttc.fc[order(ttc.fc$centr),];
                    ttf.fc = merge(exact.prot[[i]][[j]]$freeze.fc,rem.ind[[i]],by = "prean_id", all.x = T);
                    ttf.fc = ttf.fc[order(ttf.fc$centr),];
                    return(list(ttc.fc = ttc.fc, ttf.fc = ttf.fc))
                })
            })
            
            #Construct data.frame with fold changes needed for protocol assessment
            df.prot = lapply(fc.prot,function(x){
                lapply(x,function(y){
                    prot.df = data.frame(centr = y[[1]]$fc, freeze = y[[2]]$fc)
                    return(prot.df)
                })
            })
            
            #Test for thresholds
            df.test = lapply(df.prot,function(x){
                lapply(x,function(y){
                    z = abs(y-1);
                    sum.z = abs(rowSums(y)-1);
                    #In the following tests surpassing the thresholds results in TRUE
                    test.z = rowSums(z>0.2) %>% as.logical();
                    test.sum.z = sum.z<thresh2.1 | sum.z>thresh2.2;
                    z = test.z|test.sum.z;
                })
            })
            
            vec.test = lapply(df.test,function(x){
                z = lapply(x,function(y){
                    a = which(y)
                    b = which(!y)
                    if(length(a)<1 & length(b)<1){
                        c = 0
                    } else if(length(a)<1 & length(b)>=1){
                        c = max(b,na.rm = T)
                    } else if(length(a)>=1){
                        c = min(a,na.rm = T)-1
                    }
                    return(c)
                })
                return(z)
            })
            
            df.test = lapply(seq_along(vec.test),function(i){
                min.red = Reduce(min,vec.test[[i]])
                ident.red = Reduce(identical,vec.test[[i]])
                
                df = data.frame(analyte = names(rem.prot)[i],
                                protocol = if(min.red == 0){
                                    "A1"
                                }else{
                                    prots[min.red]
                                },
                                status = if(min.red == 0){
                                    "<img src=\"flags/x.png\" height=\"24\"></img>"
                                }else if(ident.red){
                                    "<img src=\"flags/gre.png\" height=\"24\"></img>"
                                }else{
                                    "<img src=\"flags/yel.png\" height=\"24\"></img>"
                                })
            })
            
            p.rec = do.call(rbind,df.test) %>% as.data.frame()
            p.rec = p.rec[order(p.rec$analyte),]
            
            #Add red flags in case of majority vote
            if(input$radio.cc.mode == 2){
                most.p = names(table(p.rec$protocol))[which.max(table(p.rec$protocol))]
                not.p = prots[-c(grep(most.p,prots):6)]
                never.p = grep("x",p.rec$status)
                p.rec$status[which(!is.na(match(p.rec$protocol,not.p)))] <- flags[1]
                #Add black flags back in
                p.rec$status[never.p] = flags[5]
            }
            
            return(p.rec)
        }
    })
    
    #Table rendering
    output$clt <- renderDataTable({datatable(tab_class(), escape = F, rownames = F)})
    
    #Text output for cc search (plasma)
    class_prot <- reactive({
        if(is.null(input$look_cc)){
            main.p <- c("Please select at least one analyte group")
        } else {
            p.rec <- tab_class()
            if(input$radio.cc.mode == 2){
                sort.p <- names(table(p.rec$protocol))[which.max(table(p.rec$protocol))]
                if(length(sort.p) > 0){
                    main.p <- paste("Your query is run in `Majority Vote`-Mode. When analyzing all samples, this results in protocol ", sort.p,".", sep = "")    
                } else {
                    main.p <- c("Based on the present data we cant give you specific sample processing recommendations. We suggest to inspect detailed stability data on your analytes of interest using the `Analyte Search`.")
                }
            } else if (input$radio.cc.mode == 1){
                sort.p <- names(table(p.rec$protocol))
                if(length(sort.p) > 0){
                    sort.p <- prots[min(match(sort.p,prots))]
                    main.p <- paste("Your query is run in `Maximize stable analytes`-mode. When analyzing all samples, this results in protocol ", sort.p,".", sep = "") 
                } else {
                    main.p <- c("Based on the present data we cant give you specific sample processing recommendations. We suggest to inspect detailed stability data on your analytes of interest using the `Analyte Search`.")
                }
            }
        }
        return(main.p) 
    })
    
    #Text rendering
    output$pr <- renderText({class_prot()})
    
    #Plot generation
    cc.plot <- reactive({
        p.rec <- tab_class()
        if(!is.null(input$look_cc)){
            MyPie(p.rec$status)
        } else {
            MyPie(flags[4])
        }
        
    })
    
    #Plot rendering
    output$cc.pie <- renderPlot({cc.plot()})
    
    #PDF popup generation
    which.prot.cc <- reactive({
        if(!is.null(input$look_cc)){
            p.rec <- tab_class()
            if(input$radio.cc.mode == 2){
                sort.p <- names(table(p.rec$protocol))[which.max(table(p.rec$protocol))]
                if(length(sort.p) > 0){
                    if(sort.p == "A1"|sort.p == "A2"){
                        wpcc <- pr.png[1]
                    } else if (sort.p == "B1"|sort.p == "B2"){
                        wpcc <- pr.png[2]
                    } else {
                        wpcc <- pr.png[3]
                    }
                }
            } else if (input$radio.cc.mode == 1){
                sort.p <- names(table(p.rec$protocol))
                if(length(sort.p) > 0){
                    if(sort.p == "A1"|sort.p == "A2"){
                        wpcc <- pr.png[1]
                    } else if (sort.p == "B1"|sort.p == "B2"){
                        wpcc <- pr.png[2]
                    } else {
                        wpcc <- pr.png[3]
                    }
                }
            }
            return(wpcc)
        }
    })
    
    #PDF popup rendering
    output$show.prot.cc <- renderUI({img(src = which.prot.cc(), width = "600")})
    
    #PDF download selection
    output$downpr_cc <- renderUI({
        if(!is.null(input$look_cc)){
            p.rec <- tab_class()
            if(input$radio.cc.mode == 2){
                sort.p <- names(table(p.rec$protocol))[which.max(table(p.rec$protocol))]
                if(length(sort.p) > 0){
                    actionButton("prbut_cc","View protocol")   
                }
            } else if (input$radio.cc.mode == 1){
                sort.p <- names(table(p.rec$protocol))
                if(length(sort.p) > 0){
                    actionButton("prbut_cc","View protocol")   
                }
            }
        }
    })
    
    #Generate referencing
    ccref <- reactive({
        cl <- mult_class()
        all_fc_cl <- cc.fc[which(!is.na(match(cc.fc$an_id,cl))),]
        all_an_cl <- an[which(!is.na(match(all_fc_cl$an_id,an$an_id))),]
        all_con_cl <- con[match(all_fc_cl$prean_id,con$prean_id),]
        all_ref_cl <- ref[match(all_con_cl$exp_id,ref$exp_id),]
        ref <- data.frame(Ref = unique(all_ref_cl$ref),
                          Citation = unique(all_ref_cl$ref_long))
        return(ref)
    })
    
    #Rendering Reference
    output$cl.ref <- renderDataTable(ccref())
    
    detail_cl_download <- reactive({
        p.rec <- tab_class()       
        cl = paste(input$look_cc, collapse = ", ")
        own.t = if(is.null(input$own.t.cc)){c("20, 30")}else{paste(input$own.t.cc, collapse = ", ")}
        refo = paste(ref$ref[c(1,2)],collapse = "; ")
        
        mode.cc = if(input$radio.cc.mode == 1){
            c("Maximize stable analytes")
        }else{
            c("Majority vote")
        }
        
        if(input$radio.cc.mode == 1){
            reco = prots[min(which(prots %in% p.rec$protocol))]
        }else{
            reco = names(which.max(table(p.rec$protocol)))
        }
        
        if(nrow(p.rec) == 0){
            all_cl = data.frame(c("date","mode","query","recommendation","stability thresholds [%]","ref"),
                                c(date(),mode.cc,NA,NA,own.t,NA))
        }else{
            all_cl = data.frame(c("date","mode","query","recommendation","stability thresholds [%]","","analyte",p.rec$analyte,"ref"),
                                c(date(),mode.cc,cl,reco,own.t,"","protocol",p.rec$protocol,refo))
        }
        colnames(all_cl) = NULL
        return(all_cl)
    })
    
    #Render download
    output$download_cl <- downloadHandler(
        filename = function(){
            paste("ALISTER_Export_ProtocolSearch_",make.names(date()),".csv",sep="")
        },
        content = function(file){
            write.csv(detail_cl_download(),file,row.names = F)
        }
    )
    
    output$download_protos_cl <- downloadHandler(
        filename = "preanalytical_protocols.pdf",
        content = function(file){
            file.copy("www/protos.pdf",file)
        }
    )
    
    #Sample search - Plasma####
    observe({
        updateSliderInput(
            session = session,
            inputId = "slide.temp",
            value = input$num.temp
        )
    })
    
    observe({
        updateNumericInput(
            session = session,
            inputId = "num.temp",
            value = input$slide.temp
        )
    })
    
    output$help.sec.samp.1 <- renderUI({
        req(input$sec.temp.samp == TRUE)
        helpText("Temperature before centrifugation")
    })
    
    output$post.temp.samp.slide <- renderUI({
        req(input$sec.temp.samp == TRUE)
        sliderInput("post.temp.samp.slide","", min = 0, max = 24, value = 0)
    })
    
    output$post.temp.samp.num <- renderUI({
        req(input$sec.temp.samp == TRUE)
        numericInput("post.temp.samp.num","", min = 0, max = 24, value = 0)
    })
    
    output$help.sec.samp.2 <- renderUI({
        req(input$sec.temp.samp == TRUE)
        helpText("Temperature after centrifugation")
    })
    
    observe({
        updateSliderInput(
            session = session,
            inputId = "post.temp.samp.slide",
            value = input$post.temp.samp.num
        )
    })
    
    observe({
        updateNumericInput(
            session = session,
            inputId = "post.temp.samp.num",
            value = input$post.temp.samp.slide
        )
    })
    
    observe({
        updateSliderInput(
            session = session,
            inputId = "slide.time1",
            value = input$num.time1
        )
    })
    
    observe({
        updateNumericInput(
            session = session,
            inputId = "num.time1",
            value = input$slide.time1
        )
    })
    
    observe({
        updateSliderInput(
            session = session,
            inputId = "slide.time2",
            value = input$num.time2
        )
    })
    
    observe({
        updateNumericInput(
            session = session,
            inputId = "num.time2",
            value = input$slide.time2
        )
    })
    
    output$own.t.samp <- renderUI({
        req(input$own.thresh.samp == TRUE)
        sliderInput("own.t.samp",
                    h5(strong("Stability Threshold [%]")),
                    min = 0,
                    max = 100,
                    step = 5,
                    value = c(20,30))
    })
    
    output$own.t.h.samp <- renderUI({
        req(input$own.thresh.samp == TRUE)
        helpText("ALISTER defines stability by setting cut-offs for fold changes specific
                 to analytes subjected to certain pre-analytical conditions. The lower
                 threshold poses as the critical cut-off for singular foldchanges (default is 20%). The upper
                 fold change is the critical cut-off for summed up fold changes (default is 30%).")
    })
    
    # React to action buttons----
    rv.multi <- reactiveValues(data = "Sphingolipids")
    
    observeEvent(input$ab.all,{
        rv.multi$data = plasma.mtrx
    })
    
    observeEvent(input$ab.clear,{
        rv.multi$data = character(0)
    })
    
    observeEvent(input$ab.all,{
        updateMultiInput(session = session,
                         inputId = "look_samp",
                         selected = rv.multi$data)
    })
    
    observeEvent(input$ab.clear,{
        updateMultiInput(session = session,
                         inputId = "look_samp",
                         selected = rv.multi$data)
    })
    
    mult_samp <- reactive({
        lookup <- input$look_samp
        samp = lapply(lookup,function(x){an[which(an[,3] %in% x),]})
        cls <- do.call(rbind,samp)
        return(cls)
    })
    
    own.prot <- reactive({
        
        ## Define stability thresholds----
        if(input$own.thresh.samp == TRUE){
            thresh1 = input$own.t.samp[1]/100
            thresh2.1 = 1-input$own.t.samp[2]/100
            thresh2.2 = 1+input$own.t.samp[2]/100
        } else {
            thresh1 = 0.2
            thresh2.1 = 0.7
            thresh2.2 = 1.3
        }
        
        ######
        if(is.null(input$look_samp)){
            cc <- data.frame(analyte = NA, status = NA)
            exp_id = NULL
        } else {
            
            cls <- mult_samp()
            all_fc_cls <- fc[which(!is.na(match(fc$an_id,cls$an_id))),]
            all_con_cls <- con[match(unique(all_fc_cls$prean_id),con$prean_id),]
            all_con_cls <- all_con_cls[all_con_cls$matrix == "Plasma",]
            all_con_cls$prean_temp = cut(all_con_cls$prean_temp, breaks = c(-Inf,8,Inf), labels = c("cooled","roomtemp"))
            all_con_cls = all_con_cls[grep("EDTA",all_con_cls$blood_tube),]
            
            if(!is.null(input$look_samp) & input$sec.temp.samp == FALSE){
                mycond <- list(c("K3EDTA"),
                               input$slide.temp,
                               input$slide.time1,
                               input$slide.time2,
                               input$slide.temp)
                mycond[[2]] <- cut(mycond[[2]], breaks = c(-Inf,8,Inf), labels = c("cooled","roomtemp"))
                mycond[[5]] <- cut(mycond[[5]], breaks = c(-Inf,8,Inf), labels = c("cooled","roomtemp"))
                
            } else if(!is.null(input$look_samp) & input$sec.temp.samp == TRUE){
                mycond <- list(c("K3EDTA"),
                               input$slide.temp,
                               input$slide.time1,
                               input$slide.time2,
                               input$post.temp.samp.slide)
                mycond[[2]] <- cut(mycond[[2]], breaks = c(-Inf,8,Inf), labels = c("cooled","roomtemp"))
                mycond[[5]] <- cut(mycond[[5]], breaks = c(-Inf,8,Inf), labels = c("cooled","roomtemp"))
                
            } 
            
            cc.ind = as.list(unique(cls$an_id))
            
            cc.fc.samp = lapply(cc.ind,function(x){return(all_fc_cls[which(all_fc_cls$an_id %in% x),])})
            
            cc.con = lapply(cc.fc.samp,function(x){
                y = all_con_cls[which(all_con_cls$prean_id %in% x$prean_id),]
                
                y = y[c(which(mycond[[2]] == y$prean_temp & y$centr_time!=0),
                        which(mycond[[5]] == y$prean_temp & y$freeze_time!=0)),]
                
                #This decides which experiments are used
                #If conditions are matched exactly by one or more experiments, only those data will be used for decision making
                #If that is not the case: If conditions are approximated by one or more experiments, only those data will be used for decision making
                #If no data is available, no decision can be reached
                y = y[c(which(y$centr_time == min(y$centr_time[which(y$centr_time >= mycond[[3]])],na.rm = T)),
                        which(y$freeze_time == min(y$freeze_time[which(y$freeze_time >= mycond[[4]])],na.rm = T))
                ),] %>% suppressWarnings()
                
                contain.exp = unique(y$exp_id) %>% as.list()
                y.ls = lapply(contain.exp,function(z){
                    return(y[which(y$exp_id == z),])
                })
                
                if(any(sapply(y.ls,function(x){any(sum(x$centr_time) == 0,sum(x$freeze_time) == 0)}))){
                    y.ls = y.ls[-which(sapply(y.ls,function(x){any(sum(x$centr_time) == 0,sum(x$freeze_time) == 0)}))]
                }
                
                if(any(sapply(y.ls,nrow)!=2)){
                    y.ls = y.ls[-which(sapply(y.ls,nrow)!=2)]
                }
                return(y.ls)
            })
            
            exp_id = do.call(rbind,lapply(cc.con,function(x){do.call(rbind,x)}))$exp_id %>% unique()
            
            cc.test = lapply(seq_along(cc.con),function(i){
                if(length(cc.con[[i]]) == 0){
                    return(data.frame(analyte = cls$an_name[i],
                                      status = flags[4],
                                      temp_1 = NA,
                                      temp_2 = NA,
                                      ttc = NA,
                                      ttf = NA))
                }else{
                    
                    #Match fold changes
                    exp.test = lapply(cc.con[[i]],function(x){
                        return(merge(x,cc.fc.samp[[i]],by.x = "prean_id", by.y = "prean_id", all.x = T))
                    })
                    
                    #Test conditions
                    fc.test = lapply(exp.test,function(x){
                        #Should return TRUE, if threshold is exceeded
                        thresh1.test = any(abs(x$fc-1)>thresh1)
                        thresh2.test = abs(sum(x$fc)-1)<thresh2.1|abs(sum(x$fc)-1)>thresh2.2
                        thresh.test = any(thresh1.test,thresh2.test)
                        return(thresh.test)
                    })
                    
                    if(length(fc.test) > 1){
                        #Multiple studies found, result in different conclusion
                        if(!Reduce(identical,fc.test)){
                            return(data.frame(analyte = cls$an_name[i],
                                              status = flags[2],
                                              temp_1 = mycond[[2]],
                                              temp_2 = mycond[[5]],
                                              ttc = sum(cc.con[[i]][[1]]$centr_time),
                                              ttf = sum(cc.con[[i]][[1]]$freeze_time)))
                            #Multiple studies found, result in sam conclusion (non-identical)   
                        } else {
                            #All studies vote for instability
                            if(all(unlist(fc.test))){
                                return(data.frame(analyte = cls$an_name[i],
                                                  status = flags[1],
                                                  temp_1 = mycond[[2]],
                                                  temp_2 = mycond[[5]],
                                                  ttc = sum(cc.con[[i]][[1]]$centr_time),
                                                  ttf = sum(cc.con[[i]][[1]]$freeze_time)))
                                #All studies vote for stability
                            }  else {
                                return(data.frame(analyte = cls$an_name[i],
                                                  status = flags[3],
                                                  temp_1 = mycond[[2]],
                                                  temp_2 = mycond[[5]],
                                                  ttc = sum(cc.con[[i]][[1]]$centr_time),
                                                  ttf = sum(cc.con[[i]][[1]]$freeze_time)))
                                
                            }
                        }
                        #Only one study found
                    } else if(length(fc.test) == 1){
                        #Study votes for instability
                        if(unlist(fc.test)){
                            return(data.frame(analyte = cls$an_name[i],
                                              status = flags[1],
                                              temp_1 = mycond[[2]],
                                              temp_2 = mycond[[5]],
                                              ttc = sum(cc.con[[i]][[1]]$centr_time),
                                              ttf = sum(cc.con[[i]][[1]]$freeze_time)))
                            #Study votes for stability
                        } else {
                            return(data.frame(analyte = cls$an_name[i],
                                              status = flags[3],
                                              temp_1 = mycond[[2]],
                                              temp_2 = mycond[[5]],
                                              ttc = sum(cc.con[[i]][[1]]$centr_time),
                                              ttf = sum(cc.con[[i]][[1]]$freeze_time)))
                        }
                    }
                }
            })
            
            cc = do.call(rbind,cc.test)
            cc = cc[order(cc$analyte),]
            rownames(cc) = NULL
            
        }
        
        return(list(cc,exp_id))
    })
    
    output$con_s <- renderDataTable({datatable(own.prot()[[1]], escape = F,
                                               options = list(columnDefs = list(list(visible=FALSE, targets = 3:6)), rowCallback = JS(
                                                   "function(row, data) {",
                                                   "var full_text = 'Time to centr.: ' + data[5] + ' min (' + data[3] + '), ' + 'Time to freeze: ' + data[6] + ' min (' + data[4] + ')'",
                                                   "$('td', row).attr('title', full_text);",
                                                   "}"))
    )})
    
    mult_names_samp <- reactive({
        names <- input$look_samp
        if (length(names > 1)){
            names <- paste("Currently looking at",paste(names, collapse = ", "))
        } else if (length(names == 1)){
            names <- paste("Currently looking at", names)
        } else {
            names <- c("Please select at least one compound class")
        }
        return(names)
    })
    
    output$pr.s <- renderText({mult_names_samp()})
    
    sampref <- reactive({
        cl <- own.prot()[[2]]
        if(length(cl) == 0){
            s.ref = data.frame(Ref = NA,
                               Citation = NA)
        } else {
            match.ref = which(ref$exp_id %in% cl)
            s.ref = ref[match.ref,]
            s.ref = data.frame(Ref = s.ref$ref,
                               Citation = s.ref$ref_long)
        }
        return(s.ref)
    })
    
    output$samp.ref <- renderDataTable(sampref())
    
    samp.plot <- reactive({
        cc <- own.prot()[[1]]
        if(!is.null(input$look_samp)){
            MyPie(cc$status)
        } else {
            MyPie(flags[4])
        }
        
    })
    
    output$samp.pie <- renderPlot({samp.plot()})
    
    detail_s_download <- reactive({
        
        ttc = input$slide.time1
        ttf = input$slide.time2
        temp = input$slide.temp
        if(!is.null(input$slide.temp2)){
            temp2 = input$slide.temp2
        } else {
            temp2 = input$slide.temp
        }
        
        cl <- paste(input$look_samp,collapse = ",")
        cc = own.prot()[[1]]
        
        colnames.cc = c("analyte","status","temperature (before centr.)[°C]","temperature (before freezing)[°C]","time to centr. [min]","time to freeze [min]") %>% t() %>% as.data.frame()
        
        cc$status = recode(cc$status,
                           "<img src=\"flags/red.png\" height=\"24\"></img>" = "WARNING",
                           "<img src=\"flags/yel.png\" height=\"24\"></img>" = "INCONCLUSIVE",
                           "<img src=\"flags/gre.png\" height=\"24\"></img>" = "OK",
                           "<img src=\"flags/tra.png\" height=\"24\"></img>" = "")
        
        cc = unname(cc)
        
        refo = paste(sampref()[,1], collapse = "; ")
        refo = c("ref",refo,rep("",4)) %>% t() %>% as.data.frame()
        
        own.t = if(is.null(input$own.t.samp)){c("20, 30")}else{paste(input$own.t.samp, collapse = ", ")}
        
        query.df = data.frame(c("date","query","stability thresholds [%]","time to centrifugation [min]","time to freezing [min]","temp. (during time to centr.)[°C]","temp. (during time to freeze)[°C]"),
                              c(date(),cl,own.t,ttc,ttf,temp,temp2))
        
        query.df = cbind(query.df,matrix("",ncol = 4, nrow = 7)) %>% as.data.frame()
        
        empty.row = rep("",6) %>% t() %>% as.data.frame()
        
        col.row = c("analyte","status","temp. before centr. [°C]","temp. after centr. [°C]","time to centr. [°C]","time to freeze [°C]") %>% t() %>% as.data.frame()
        
        samp.ls = list(query.df,empty.row,col.row,cc,empty.row,refo)
        
        samp.ls = lapply(seq_along(samp.ls),function(i){colnames(samp.ls[[i]]) = make.names(c(1:6));return(samp.ls[[i]])})
        
        samp.down = do.call(rbind,samp.ls)
        
        return(samp.down)
    })
    
    output$download_s <- downloadHandler(
        filename = function(){
            paste("ALISTER_Export_SampleSearch_",make.names(date()),".csv",sep="")
        },
        content = function(file){
            write.csv(detail_s_download(),file,row.names = F)
        }
    )
    
    # Analyte Search - Plasma####
    
    output$text.an <- renderText({input$look_an})
    
    observe({
        updateSliderInput(
            session = session,
            inputId = "an.slide.temp",
            value = input$an.num.temp
        )
    })
    
    observe({
        updateNumericInput(
            session = session,
            inputId = "an.num.temp",
            value = input$an.slide.temp
        )
    })
    
    output$help.sec.an.1 <- renderUI({
        req(input$sec.temp.an == TRUE)
        helpText("Temperature before centrifugation")
    })
    
    output$post.temp.an.slide <- renderUI({
        req(input$sec.temp.an == TRUE)
        sliderInput("post.temp.an.slide","", min = 0, max = 24, value = 0)
    })
    
    output$post.temp.an.num <- renderUI({
        req(input$sec.temp.an == TRUE)
        numericInput("post.temp.an.num","", min = 0, max = 24, value = 0)
    })
    
    output$help.sec.an.2 <- renderUI({
        req(input$sec.temp.an == TRUE)
        helpText("Temperature after centrifugation")
    })
    
    observe({
        updateSliderInput(
            session = session,
            inputId = "post.temp.an.slide",
            value = input$post.temp.an.num
        )
    })
    
    observe({
        updateNumericInput(
            session = session,
            inputId = "post.temp.an.num",
            value = input$post.temp.an.slide
        )
    })
    
    observe({
        updateSliderInput(
            session = session,
            inputId = "an.slide.time1",
            value = input$an.num.time1
        )
    })
    
    observe({
        updateNumericInput(
            session = session,
            inputId = "an.num.time1",
            value = input$an.slide.time1
        )
    })
    
    observe({
        updateSliderInput(
            session = session,
            inputId = "an.slide.time2",
            value = input$an.num.time2
        )
    })
    
    observe({
        updateNumericInput(
            session = session,
            inputId = "an.num.time2",
            value = input$an.slide.time2
        )
    })
    
    output$own.t.an <- renderUI({
        req(input$own.thresh.an == TRUE)
        sliderInput("own.t.an",
                    h5(strong("Stability Threshold [%]")),
                    min = 0,
                    max = 100,
                    step = 5,
                    value = c(20,30))
    })
    
    output$own.t.h.an <- renderUI({
        req(input$own.thresh.an == TRUE)
        helpText("ALISTER defines stability by setting cut-offs for fold changes specific
                 to analytes subjected to certain pre-analytical conditions. The lower
                 threshold poses as the critical cut-off for singular foldchanges (default is 20%). The upper
                 fold change is the critical cut-off for summed up fold changes (default is 30%).")
    })
    
    ## Protocol recommendation ----
    tab_an <- reactive({ 
        ### Define stability thresholds ====
        if(input$own.thresh.an == TRUE){
            thresh1 = input$own.t.an[1]/100
            thresh2.1 = 1-input$own.t.an[2]/100
            thresh2.2 = 1+input$own.t.an[2]/100
        } else {
            thresh1 = 0.2
            thresh2.1 = 0.7
            thresh2.2 = 1.3
        }
        
        ana <- an[input$look_an == an[,2],1]
        all_an <- fc[which(!is.na(match(fc$an_id,ana))),]
        if(ana %in% cc.mtrx$an_id){
            all_an <- cc.fc[which(!is.na(match(cc.fc$an_id,ana))),]
            match_prean <- con[unique(match(all_an[,"prean_id"],con[,"prean_id"])),]
            match_prean = match_prean[grep("EDTA",match_prean$blood_tube),]
            match_prean$prean_temp = cut(match_prean$prean_temp, breaks = c(-Inf,8,16,Inf), labels = c(0,NA,21))
            
            uni.exp = as.list(unique(match_prean$exp_id))
            
            uni.exp = lapply(seq_along(uni.exp),function(j){
                time.to.centr = match_prean[which((match_prean$exp_id == uni.exp[[j]])&(match_prean$centr_time>0)),]
                time.to.freeze = match_prean[which((match_prean$exp_id == uni.exp[[j]])&(match_prean$freeze_time>0)),]
                return(list(time.to.centr = time.to.centr,
                            time.to.freeze = time.to.freeze))
            })
            
            any.na = lapply(uni.exp,function(x){lapply(x,function(y){return(nrow(y)>0)})})
            any.na = lapply(any.na,function(x){any(!unlist(x))})
            
            if(any(unlist(any.na))){
                uni.exp = uni.exp[-which(unlist(any.na))]
            }
            
            all.na = sapply(uni.exp,function(x){length(x) == 0}) %>% which()
            
            if(length(all.na) > 0){
                uni.exp = uni.exp[-which(all.na)]
            }
            
            if(length(uni.exp) == 0){
                p.rec <- c("Based on the present data we cant give you general sample processing recommendations for the selected analyte. We suggest to inspect the detailed stability information below.")
                return(p.rec)
            } else {
                uni.exp = lapply(uni.exp,function(x){
                    ttc = merge(samp.pro,x[[1]],by.y = c("centr_time","prean_temp"),by.x = c("centr","temp"),all.x = T, sort = T);
                    ttc = ttc[order(ttc$centr),];
                    ttf = merge(samp.pro,x[[2]],by.y = c("freeze_time","prean_temp"),by.x = c("freeze","temp"),all.x = T);
                    ttf = ttf[order(ttf$centr),];
                    return(list(centr.fc = ttc,freeze.fc = ttf))
                })
                
                uni.exp = lapply(seq_along(uni.exp),function(i){
                    ttc.fc = merge(uni.exp[[i]]$centr.fc,all_an,by = "prean_id", all.x = T);
                    ttc.fc = ttc.fc[order(ttc.fc$centr),];
                    ttf.fc = merge(uni.exp[[i]]$freeze.fc,all_an,by = "prean_id", all.x = T);
                    ttf.fc = ttf.fc[order(ttf.fc$centr),];
                    return(list(ttc.fc = ttc.fc, ttf.fc = ttf.fc))
                })
                
                df.prot = lapply(uni.exp,function(x){
                    prot.df = data.frame(centr = x[[1]]$fc, freeze = x[[2]]$fc)
                    return(prot.df)
                })
                
                df.test = lapply(df.prot,function(x){
                    y = abs(x-1);
                    sum.y = abs(rowSums(x)-1);
                    #In the following tests surpassing the thresholds results in TRUE
                    test.y = rowSums(y>0.2) %>% as.logical();
                    test.sum.y = sum.y<thresh2.1 | sum.y>thresh2.2;
                    y = test.y|test.sum.y;
                })
                
                vec.test = lapply(df.test,function(x){
                    a = which(x)
                    b = which(!x)
                    if(length(a)<1 & length(b)<1){
                        c = 0
                    } else if(length(a)<1 & length(b)>=1){
                        c = max(b,na.rm = T)
                    } else if(length(a)>=1){
                        c = min(a,na.rm = T)-1
                    }
                    return(c)
                })
                
                min.red = Reduce(min,vec.test)
                ident.red = Reduce(identical,vec.test)
                
                p.rec = if(min.red == 0){"A1"}else{prots[min.red]}
                
                p.rec <- list(paste("Based on the present stability data we recommend to use protocol",p.rec),p.rec)
                return(p.rec)
            }
        }else{
            p.rec <- list(c("Based on the present data we cant give you general sample processing recommendations for the selected analyte. We suggest to inspect the detailed stability information below."),NA)
            return(p.rec)
        }
    })
    
    
    output$downpr_an <- renderUI({
        p.rec <- tab_an()
        if(!is.na(p.rec[[2]])){
            actionButton("prbut_an","View protocol")
        }
    })
    
    which.prot.an <- reactive({
        p.rec <- tab_an()
        if(!is.na(p.rec[[2]])){
            p.rec <- p.rec[[2]]
            if(p.rec == "A1"|p.rec == "A2"){
                p.rec <- pr.png[1]
            } else if (p.rec == "B1"|p.rec == "B2"){
                p.rec <- pr.png[2]
            } else {
                p.rec <- pr.png[3]
            }
            return(p.rec)
        }
    })
    
    output$show.prot.an <- renderUI({img(src = which.prot.an(), width = 600, align = "center")})
    
    output$an.rec <- renderText({tab_an()[[1]]})
    
    ## Protocol assessment----
    own.prot.an <- reactive({
        
        if(input$own.thresh.an == TRUE){
            thresh1 = input$own.t.an[1]/100
            thresh2.1 = 1-input$own.t.an[2]/100
            thresh2.2 = 1+input$own.t.an[2]/100
        } else {
            thresh1 = 0.2
            thresh2.1 = 0.7
            thresh2.2 = 1.3
        }
        
        myan <- input$look_an
        my <- an[myan == an[,2],]
        all_fc_my <- fc[which(!is.na(match(fc$an_id,my$an_id))),]
        all_fc_my <- distinct(all_fc_my,prean_id,.keep_all = T)
        all_con_my <- con[match(unique(all_fc_my$prean_id),con$prean_id),]
        all_con_my$prean_temp = cut(all_con_my$prean_temp, breaks = c(-Inf,8,Inf), labels = c("cooled","roomtemp"))
        all_con_my = all_con_my[grep("EDTA",all_con_my$blood_tube),]
        
        if(!is.null(input$look_an) & input$sec.temp.an == FALSE){
            mycond <- list(c("K3EDTA"),
                           input$an.slide.temp,
                           input$an.slide.time1,
                           input$an.slide.time2,
                           input$an.slide.temp)
            mycond[[2]] <- cut(mycond[[2]], breaks = c(-Inf,8,Inf), labels = c("cooled","roomtemp"))
            mycond[[5]] <- cut(mycond[[5]], breaks = c(-Inf,8,Inf), labels = c("cooled","roomtemp"))
            
        } else if(!is.null(input$look_an) & input$sec.temp.an == TRUE){
            mycond <- list(c("K3EDTA"),
                           input$an.slide.temp,
                           input$an.slide.time1,
                           input$an.slide.time2,
                           input$post.temp.an.slide)
            mycond[[2]] <- cut(mycond[[2]], breaks = c(-Inf,8,Inf), labels = c("cooled","roomtemp"))
            mycond[[5]] <- cut(mycond[[5]], breaks = c(-Inf,8,Inf), labels = c("cooled","roomtemp"))
        } 
        
        an.con = all_con_my[which(all_con_my$prean_id %in% all_fc_my$prean_id),]
        
        an.con = an.con[c(which(mycond[[2]] == an.con$prean_temp & an.con$centr_time!=0),
                          which(mycond[[5]] == an.con$prean_temp & an.con$freeze_time!=0)),]
        
        an.con = an.con[c(which(an.con$centr_time == min(an.con$centr_time[which(an.con$centr_time >= mycond[[3]])],na.rm = T)),
                          which(an.con$freeze_time == min(an.con$freeze_time[which(an.con$freeze_time >= mycond[[4]])],na.rm = T))
        ),] %>% suppressWarnings()
        
        contain.exp = unique(an.con$exp_id) %>% as.list()
        
        cc.con = lapply(contain.exp,function(z){
            return(an.con[which(an.con$exp_id == z),])
        })
        
        if(any(sapply(cc.con,function(x){any(sum(x$centr_time) == 0,sum(x$freeze_time) == 0)}))){
            cc.con = cc.con[-which(sapply(cc.con,function(x){any(sum(x$centr_time) == 0,sum(x$freeze_time) == 0)}))]
        }
        
        if(any(sapply(cc.con,nrow)!=2)){
            cc.con = cc.con[-which(sapply(cc.con,nrow)!=2)]
        }
        
        #Merging with fold changes and reaching descision
        cc.test = if(length(cc.con) == 0){
            #No data found
            list(1,
                 data.frame("time_to_centrifugation" = NA,
                            'time_to_freeze' = NA,
                            'temp.' = NA,
                            'trend' = NA,
                            'ref' = NA)) %>% return()
        } else {
            #Match fold changes
            exp.test.an = lapply(cc.con,function(x){
                return(merge(x,all_fc_my,by.x = "prean_id", by.y = "prean_id", all.x = T))
            })
            
            #Test conditions
            fc.test = lapply(exp.test.an,function(x){
                #Should return TRUE, if threshold is exceeded
                thresh1.test = any(abs(x$fc-1)>thresh1)
                thresh2.test = abs(sum(x$fc)-1)<thresh2.1|abs(sum(x$fc)-1)>thresh2.2
                thresh.test = any(thresh1.test,thresh2.test)
                return(thresh.test)
            })
            
            updown.test = lapply(exp.test.an,function(x){
                thresh.updown = sapply(seq_along(x$fc),function(y){if(x$fc[y]>(1+thresh1)){return(trend[1])}else if(x$fc[y]<(1-thresh1)){return(trend[2])}else{return(trend[3])}})
                return(thresh.updown)
            })
            
            exp.out = do.call(rbind,exp.test.an)
            
            if(length(fc.test) > 1){
                #No identical outcomes between studies
                if(!Reduce(identical,fc.test)){
                    list(3,
                         data.frame("time_to_centrifugation" = exp.out$centr_time,
                                    'time_to_freeze' = exp.out$freeze_time,
                                    'temp.' = exp.out$prean_temp,
                                    'trend' = unlist(updown.test),
                                    'ref' = exp.out$ref))
                } else {
                    #All studies vote for instability
                    if(all(unlist(fc.test))){
                        list(4,
                             data.frame("time_to_centrifugation" = exp.out$centr_time,
                                        'time_to_freeze' = exp.out$freeze_time,
                                        'temp.' = exp.out$prean_temp,
                                        'trend' = unlist(updown.test),
                                        'ref' = exp.out$ref))
                        #All studies vote for stability
                    } else {
                        list(2,
                             data.frame("time_to_centrifugation" = exp.out$centr_time,
                                        'time_to_freeze' = exp.out$freeze_time,
                                        'temp.' = exp.out$prean_temp,
                                        'trend' = unlist(updown.test),
                                        'ref' = exp.out$ref))
                    }
                }
            } else if(length(fc.test) == 1){
                #Study votes for instability
                if(unlist(fc.test)){
                    list(4,
                         data.frame("time_to_centrifugation" = exp.out$centr_time,
                                    'time_to_freeze' = exp.out$freeze_time,
                                    'temp.' = exp.out$prean_temp,
                                    'trend' = unlist(updown.test),
                                    'ref' = exp.out$ref))
                    #Study votes for stability    
                } else {
                    list(2,
                         data.frame("time_to_centrifugation" = exp.out$centr_time,
                                    'time_to_freeze' = exp.out$freeze_time,
                                    'temp.' = exp.out$prean_temp,
                                    'trend' = unlist(updown.test),
                                    'ref' = exp.out$ref))
                }
            }
        }
        return(cc.test)
    })
    
    prot.an <- reactive({
        cc <- own.prot.an()
        text <- c("Our data does not seem to be able to approximate the protocol you entered.",
                  "The protocol seems to ensure analyte stability.",
                  "Our data comes to different conclusions regarding analyte stability for your protocol.",
                  "The analyte seems to be unstable under the suggested protocol.")
        cc <- text[cc[[1]]]
        return(cc)
    })
    
    output$prot.an.rec <- renderText({prot.an()})
    
    prot.an.sym <- reactive({
        cc <- own.prot.an()
        cc <- an.flag[cc[[1]]]
        return(cc)
    })
    
    output$own.prot.an.sym <- renderUI({img(src = prot.an.sym(), height = "200px", align = "right")})
    
    detail_view <- reactive({
        detail_view <- own.prot.an()
        detail_view = detail_view[[2]] %>% as.data.frame()
        colnames(detail_view) = c("time to centrifugation","time to freeze","temperature","trend","reference")
        return(detail_view)
    })
    
    output$an.detail <- renderDataTable({datatable(detail_view(), escape = F)})
    
    anref <- reactive({
        cl = own.prot.an()[[2]]
        if(all(is.na(cl$ref))){
            a.ref = data.frame(Ref = NA,
                               Citation = NA)
        } else {
            match.ref = which(ref$ref %in% cl$ref)
            a.ref = ref[match.ref,]
            a.ref = data.frame(Ref = a.ref$ref,
                               Citation = a.ref$ref_long)
        }
        return(a.ref)
    })
    
    output$an.ref <- renderDataTable(anref())
    
    detail_an_download<- reactive({
        
        query = unlist(input$look_an)
        own.t = if(is.null(input$own.t.an)){c("20, 30")}else{paste(input$own.t.an, collapse = ", ")}
        reco = tab_an()[[2]]
        
        ttc = input$an.slide.time1
        ttf = input$an.slide.time2
        temp = input$an.slide.temp
        if(!is.null(input$an.slide.temp2)){
            temp2 = input$an.slide.temp2
        } else {
            temp2 = input$an.slide.temp
        }
        status = recode(own.prot.an()[[1]],
                        "1" = "WARNING",
                        "2" = "INCONCLUSIVE",
                        "3" = "OK", 
                        "4" = "NA")
        
        empty.row = rep("",5) %>% t() %>% as.data.frame()
        
        col.row = c("time to centr.[min]","time to freeze [min]","temp[°C]","trend","ref") %>% t() %>% as.data.frame()
        
        det = own.prot.an()[[2]] %>% as.data.frame
        
        det$trend = recode(det$trend,
                           "<img src=\"trend/trend_up.PNG\" height=\"24\"></img>" = "UP",
                           "<img src=\"trend/trend_down.PNG\" height=\"24\"></img>" = "DOWN",
                           "<img src=\"trend/trend_stable.PNG\" height=\"24\"></img>" = "STABLE")
        
        query.df = cbind(data.frame(c("date","query","stability thresholds [%]","recommendation","","time to centrifugation [min]","time to freeze [min]","temp. (during time to centr.)[°C]","temp. (during time to centr.)[min]"),
                                    c(date(),query,own.t,reco,"",ttc,ttf,temp,temp2)),
                         matrix("",ncol = 3, nrow = 9)) %>% as.data.frame()
        
        df.ls = list(query.df,empty.row,col.row,det)
        
        df.ls = lapply(seq_along(df.ls),function(i){colnames(df.ls[[i]]) = make.names(c(1:5));return(df.ls[[i]])})
        
        df.ls = do.call(rbind,df.ls) %>% as.data.frame()
        
        return(df.ls)
    })
    
    output$download_an <- downloadHandler(
        filename = function(){
            paste("ALISTER_Export_AnalyteSearch_",make.names(date()),".csv",sep="")
        },
        content = function(file){
            write.csv(detail_an_download(),file,row.names = F)
        }
    )
    
    output$download_protos_an <- downloadHandler(
        filename = "preanalytical_protocols.pdf",
        content = function(file){
            file.copy("www/protos.pdf",file)
        }
    )
    
    # Data filtering mode - Plasma####
    
    observe({
        updateSliderInput(
            session = session,
            inputId = "filt.slide.temp",
            value = input$filt.num.temp
        )
    })
    
    observe({
        updateNumericInput(
            session = session,
            inputId = "filt.num.temp",
            value = input$filt.slide.temp
        )
    })
    
    output$help.sec.filt.1 <- renderUI({
        req(input$sec.temp.filt == TRUE)
        helpText("Temperature before centrifugation [°C]")
    })
    
    output$post.temp.filt.slide <- renderUI({
        req(input$sec.temp.filt == TRUE)
        sliderInput("post.temp.filt.slide","", min = 0, max = 24, value = 0)
    })
    
    output$post.temp.filt.num <- renderUI({
        req(input$sec.temp.filt == TRUE)
        numericInput("post.temp.filt.num","", min = 0, max = 24, value = 0)
    })
    
    output$help.sec.filt.2 <- renderUI({
        req(input$sec.temp.filt == TRUE)
        helpText("Temperature after centrifugation")
    })
    
    observe({
        updateSliderInput(
            session = session,
            inputId = "post.temp.filt.slide",
            value = input$post.temp.filt.num
        )
    })
    
    observe({
        updateNumericInput(
            session = session,
            inputId = "post.temp.filt.num",
            value = input$post.temp.filt.slide
        )
    })
    
    observe({
        updateSliderInput(
            session = session,
            inputId = "filt.slide.time1",
            value = input$filt.num.time1
        )
    })
    
    observe({
        updateNumericInput(
            session = session,
            inputId = "filt.num.time1",
            value = input$filt.slide.time1
        )
    })
    
    observe({
        updateSliderInput(
            session = session,
            inputId = "filt.slide.time2",
            value = input$filt.num.time2
        )
    })
    
    observe({
        updateNumericInput(
            session = session,
            inputId = "filt.num.time2",
            value = input$filt.slide.time2
        )
    })
    
    output$own.t.filt <- renderUI({
        req(input$own.thresh.filt == TRUE)
        sliderInput("own.t.filt",
                    h5(strong("Stability Threshold [%]")),
                    min = 0,
                    max = 100,
                    step = 5,
                    value = c(20,30))
    })
    
    output$own.t.h.filt <- renderUI({
        req(input$own.thresh.filt == TRUE)
        helpText("ALISTER defines stability by setting cut-offs for fold changes specific
                 to analytes subjected to certain pre-analytical conditions. The lower
                 threshold poses as the critical cut-off for singular foldchanges (default is 20%). The upper
                 fold change is the critical cut-off for summed up fold changes (default is 30%).")
    })
    
    filt.status <- reactive({
        if(is.null(input$csvtable)){
            text <- c("Please upload a dataset for filtering")
            return(text)
        } else {
            text <- c("Your filtered data")
            return(text)
        }
    })
    
    output$upload.status <- renderText({filt.status()})
    
    uploadcsv <- reactive({
        if(is.null(input$csvtable)){
            return(NULL)
        } else {
            df <- read.table(input$csvtable$datapath,
                             header = T,
                             sep = input$sep,
                             check.names = F)
            #Omit columns without names
            if(any(sapply(colnames(df),nchar) == 0)){
                df <- df[,-which(sapply(colnames(df),nchar) == 0)]
            }
            return(df)
        }
    })
    
    matchfilt <- reactive({
        if(is.null(input$csvtable)){
            return(NULL)
        } else {
            df <- uploadcsv()
            
            col.filt <- colnames(df)
            #col.parse <- parseLipidNames(col.filt)
            #col.parse <- col.parse$Normalized.Name[match(col.filt,col.parse$Original.Name)]
            col.parse = make_clean_names(col.filt)
            col.filt <- cbind(col.filt,col.parse)
            
            all.filt <- unlist(plasma.an)
            #Reactivate when rgoslin runs without problems again...
            # all.parse <- parseLipidNames(make_clean_names(all.filt))
            # all.parse <- all.parse$Normalized.Name[match(all.filt,all.parse$Original.Name)]
            all.parse = make_clean_names(all.filt)
            all.filt <- cbind(all.filt,all.parse)
            
            mn <- match(col.filt[,2],all.filt[,2], incomparables = col.filt[is.na(col.filt[,2]),2])
            mo <- match(col.filt[,1],all.filt[,1], incomparables = col.filt[is.na(col.filt[,1]),1])
            
            all.mat <- mo
            if(any(is.na(all.mat))){
                all.mat[is.na(all.mat)] <- mn[is.na(all.mat)]
            }
            names(all.mat) <- plasma.an[all.mat]
            
            col.filt <- colnames(df)
            # col.parse <- parseLipidNames(col.filt)
            # col.parse <- col.parse$Normalized.Name[match(col.filt,col.parse$Original.Name)]
            col.parse = make_clean_names(col.filt)
            col.filt <- cbind(col.filt,col.parse)
            
            all.filt <- an$an_name
            #Reactivate when rgoslin runs without problems again...
            # all.parse <- parseLipidNames(make_clean_names(all.filt))
            # all.parse <- all.parse$Normalized.Name[match(all.filt,all.parse$Original.Name)]
            all.parse = make_clean_names(all.filt)
            all.filt <- cbind(all.filt,all.parse)
            
            mn <- match(col.filt[,2],all.filt[,2], incomparables = col.filt[is.na(col.filt[,2]),2])
            mo <- match(col.filt[,1],all.filt[,1], incomparables = col.filt[is.na(col.filt[,1]),1])
            
            all.mat2 <- mo
            if(any(is.na(all.mat2))){
                all.mat2[is.na(all.mat2)] <- mn[is.na(all.mat2)]
            }
            names(all.mat2) <- an$an_name[all.mat2]
            
            all.mat[which(!is.na(all.mat))] = all.mat2[which(!is.na(all.mat))]
            
            filt.ls <- list(matched = na.omit(all.mat),
                            found = which(!is.na(all.mat)),
                            notfound = colnames(df)[which(is.na(all.mat))],
                            all.mat) 
            return(filt.ls)
        }
    })
    
    filt.table <- reactive({
        
        if(is.null(input$csvtable)){
            return(NULL)
            ## Data filtering mode evaluation---- 
        } else {
            filt.ls <- matchfilt()
            
            if(input$own.thresh.filt == TRUE){
                thresh1 = input$own.t.filt[1]/100
                thresh2.1 = 1-input$own.t.filt[2]/100
                thresh2.2 = 1+input$own.t.filt[2]/100
            } else {
                thresh1 = 0.2
                thresh2.1 = 0.7
                thresh2.2 = 1.3
            }
            
            if(input$sec.temp.filt == FALSE){
                mycond <- list(c("K3EDTA"),#input$radio.tube,
                               input$filt.slide.temp,
                               input$filt.slide.time1,
                               input$filt.slide.time2,
                               input$filt.slide.temp)
                mycond[[2]] <- cut(mycond[[2]], breaks = c(-Inf,8,Inf), labels = c("cooled","roomtemp"))
                mycond[[5]] <- cut(mycond[[5]], breaks = c(-Inf,8,Inf), labels = c("cooled","roomtemp"))
            } else {
                mycond <- list(c("K3EDTA"),#input$radio.tube,
                               input$filt.slide.temp,
                               input$filt.slide.time1,
                               input$filt.slide.time2,
                               input$post.temp.filt.slide)
                mycond[[2]] <- cut(mycond[[2]], breaks = c(-Inf,8,Inf), labels = c("cooled","roomtemp"))
                mycond[[5]] <- cut(mycond[[5]], breaks = c(-Inf,8,Inf), labels = c("cooled","roomtemp"))
            }
            
            find.filt <- filt.ls[[1]]
            
            if(length(find.filt) > 0){
                
                anid.filt <- an$an_id[find.filt]
                fc.filt <- fc[which(!is.na(match(fc$an_id,anid.filt))),]
                all_con_filt <- con[match(unique(fc.filt$prean_id),con$prean_id),]
                all_con_filt <- all_con_filt[all_con_filt$matrix == "Plasma",]
                all_con_filt$prean_temp = cut(all_con_filt$prean_temp, breaks = c(-Inf,8,Inf), labels = c("cooled","roomtemp"))
                all_con_filt = all_con_filt[grep("EDTA",all_con_filt$blood_tube),]
                
                filt.ind = as.list(unique(fc.filt$an_id))
                
                filt.fc.samp = lapply(filt.ind,function(x){return(fc.filt[which(fc.filt$an_id %in% x),])})
                
                filt.con = lapply(filt.fc.samp,function(x){
                    y = all_con_filt[which(all_con_filt$prean_id %in% x$prean_id),]
                    
                    y = y[c(which(mycond[[2]] == y$prean_temp & y$centr_time!=0),
                            which(mycond[[5]] == y$prean_temp & y$freeze_time!=0)),]
                    
                    #This decides which experiments are used
                    #If conditions are matched exactly by one or more experiments, only those data will be used for decision making
                    #If that is not the case: If conditions are approximated by one or more experiments, only those data will be used for decision making
                    #If no data is available, no decision can be reached
                    y = y[c(which(y$centr_time == min(y$centr_time[which(y$centr_time >= mycond[[3]])],na.rm = T)),
                            which(y$freeze_time == min(y$freeze_time[which(y$freeze_time >= mycond[[4]])],na.rm = T))
                    ),] %>% suppressWarnings()
                    
                    contain.exp = unique(y$exp_id) %>% as.list()
                    y.ls = lapply(contain.exp,function(z){
                        return(y[which(y$exp_id == z),])
                    })
                    
                    if(any(sapply(y.ls,function(x){any(sum(x$centr_time) == 0,sum(x$freeze_time) == 0)}))){
                        y.ls = y.ls[-which(sapply(y.ls,function(x){any(sum(x$centr_time) == 0,sum(x$freeze_time) == 0)}))]
                    }
                    
                    if(any(sapply(y.ls,nrow)!=2)){
                        y.ls = y.ls[-which(sapply(y.ls,nrow)!=2)]
                    }
                    return(y.ls)
                })
                
                exp_id = do.call(rbind,lapply(filt.con,function(x){do.call(rbind,x)}))$exp_id %>% unique()
                
                filt.test = lapply(seq_along(filt.con),function(i){
                    if(length(filt.con[[i]]) == 0){
                        return(c(TRUE,flags[4], temp_1 = NA, temp_2 = NA, ttc = NA, ttf = NA))
                    }else{
                        
                        #Match fold changes
                        exp.test = lapply(filt.con[[i]],function(x){
                            return(merge(x,filt.fc.samp[[i]],by.x = "prean_id", by.y = "prean_id", all.x = T))
                        })
                        
                        #Test conditions
                        fc.test = lapply(exp.test,function(x){
                            #Should return TRUE, if threshold is exceeded
                            thresh1.test = any(abs(x$fc-1)>thresh1)
                            thresh2.test = abs(sum(x$fc)-1)<thresh2.1|abs(sum(x$fc)-1)>thresh2.2
                            thresh.test = any(thresh1.test,thresh2.test)
                            return(thresh.test)
                        })
                        
                        if(length(fc.test) > 1){
                            #Multiple studies found, result in different conclusion
                            if(!Reduce(identical,fc.test)){
                                return(c(TRUE,flags[2], temp_1 = as.character(mycond[[2]]), temp_2 = as.character(mycond[[5]]), ttc = sum(exp.test[[1]]$centr_time), ttf = sum(exp.test[[1]]$freeze_time)))
                                #Multiple studies found, result in sam conclusion (non-identical)   
                            } else {
                                #All studies vote for instability
                                if(all(unlist(fc.test))){
                                    return(c(F,flags[1], temp_1 = as.character(mycond[[2]]), temp_2 = as.character(mycond[[5]]), ttc = sum(exp.test[[1]]$centr_time), ttf = sum(exp.test[[1]]$freeze_time)))
                                    #All studies vote for stability
                                }  else {
                                    return(c(T,flags[3], temp_1 = as.character(mycond[[2]]), temp_2 = as.character(mycond[[5]]), ttc = sum(exp.test[[1]]$centr_time), ttf = sum(exp.test[[1]]$freeze_time)))
                                    
                                }
                            }
                            #Only one study found
                        } else if(length(fc.test) == 1){
                            #Study votes for instability
                            if(unlist(fc.test)){
                                return(c(F,flags[1], temp_1 = as.character(mycond[[2]]), temp_2 = as.character(mycond[[5]]), ttc = sum(exp.test[[1]]$centr_time), ttf = sum(exp.test[[1]]$freeze_time)))
                                #Study votes for stability
                            } else {
                                return(c(T,flags[3], temp_1 = as.character(mycond[[2]]), temp_2 = as.character(mycond[[5]]), ttc = sum(exp.test[[1]]$centr_time), ttf = sum(exp.test[[1]]$freeze_time)))
                            }
                        }
                    }
                })
                
                fls <- do.call(cbind,filt.test)
                colnames(fls) = names(find.filt)
                
                nfls = matrix(rep(c(TRUE,flags[6],temp_1 = NA, temp_2 = NA, ttc = NA, ttf = NA),length(filt.ls[[3]])),nrow = 6, ncol = length(filt.ls[[3]]), byrow = F) %>% as.data.frame()
                colnames(nfls) = filt.ls[[3]]
                
                filt <- cbind(nfls,fls)
                return(list(filt,exp_id))
                
            } else {
                nfls <- filt.ls[[3]]
                allfls <- matrix(rep(c(TRUE,flags[6],temp_1 = NA, temp_2 = NA, ttc = NA, ttf = NA),length(nfls)),nrow = 6, ncol = length(nfls), byrow = F) %>% as.data.frame()
                names(allfls) <- nfls
                return(list(allfls,NA))
            }
        }
    })
    
    filt.dt <- reactive({
        if(is.null(input$csvtable)){
            return(NULL)
        } else {
            df <- uploadcsv()
            filt <- filt.table()[[1]]
            
            filt <- filt[2:6,colnames(df)]
            if(any(duplicated(colnames(df)))){
                colnames(filt)[which(duplicated(colnames(df)))] <- colnames(df)[which(duplicated(colnames(df)))]
            }
            dt <- rbind(filt,df)%>% t() %>% as.data.frame()
            colnames(dt)[1] = c("status")
            return(dt)
        } 
    })
    
    
    
    output$csvfilt <- renderDataTable({datatable(filt.dt(), escape = F, rownames = T, 
                                                 options = list(columnDefs = list(list(visible=FALSE, targets = 2:5)), rowCallback = JS(
                                                     "function(row, data) {",
                                                     "var full_text = 'Time to centr.: ' + data[4] + ' min (' + data[2] + '), ' + 'Time to freeze: ' + data[5] + ' min (' + data[3] + ')'",
                                                     "$('td', row).attr('title', full_text);",
                                                     "}")))})
    
    filtref <- reactive({
        cl <- filt.table()[[2]]
        if(length(cl)<1){
            f.ref = data.frame(Ref = NA,
                               Citation = NA)
        } else {
            match.ref = which(ref$exp_id %in% cl)
            m.ref = ref[match.ref,]
            f.ref = data.frame(Ref = m.ref$ref,
                               Citation = m.ref$ref_long)
        }
        return(f.ref)
    })
    
    output$filt.ref <- renderDataTable(filtref())
    
    filt.plot <- reactive({
        if(!is.null(input$csvtable)){
            filt <- filt.table()[[1]]
            filt.x <- unlist(filt[2,])
            MyPie(filt.x)
        } else {
            MyPie(flags[4])
        }
        
    })
    
    output$filt.pie <- renderPlot({filt.plot()})
    
    #Prepare filtered table for download
    filt.down <- reactive({
        if(is.null(input$csvtable)){
            return(NULL)
        } else {
            df <- uploadcsv()
            filt <- filt.table()[[1]]
            
            filt <- filt[,colnames(df)]
            
            if(any(duplicated(colnames(df)))){
                colnames(filt)[which(duplicated(colnames(df)))] <- colnames(df)[which(duplicated(colnames(df)))]
            }
            filt <- which(as.logical(filt))
            df <- df[,filt]
            return(df)
        }
    })
    
    output$download_filt <- downloadHandler(
        filename = function(){
            paste("ALISTER_Export_filtered_",make.names(date()),".csv",sep="")
        },
        content = function(file){
            write.csv(filt.down(),file,row.names = F)
        }
    )
    
    # Sample search - Serum####
    
    observe({
        updateSliderInput(
            session = session,
            inputId = "slide.temp_serum",
            value = input$num.temp_serum
        )
    })
    
    observe({
        updateNumericInput(
            session = session,
            inputId = "num.temp_serum",
            value = input$slide.temp_serum
        )
    })
    
    output$own.t.samp_serum <- renderUI({
        req(input$own.thresh.samp_serum == TRUE)
        sliderInput("own.t.samp_serum",
                    h5(strong("Stability Threshold [%]")),
                    min = 0,
                    max = 100,
                    step = 5,
                    value = c(20))
    })
    
    output$own.t.h.samp_serum <- renderUI({
        req(input$own.thresh.samp_serum == TRUE)
        helpText("ALISTER defines stability by setting cut-offs for fold changes specific
                 to analytes subjected to certain pre-analytical conditions. The selected 
                 threshold is the maximum allowed fold change occuring in serum during post-centrifugation
                 processing delay (default is 20%).")
    })
    
    # React to action buttons----
    rv.multi_serum <- reactiveValues(data = "Sphingolipids")
    
    observeEvent(input$ab.all_serum,{
        rv.multi_serum$data = serum.mtrx
    })
    
    observeEvent(input$ab.clear_serum,{
        rv.multi_serum$data = character(0)
    })
    
    observeEvent(input$ab.all_serum,{
        updateMultiInput(session = session,
                         inputId = "look_samp_serum",
                         selected = rv.multi_serum$data)
    })
    
    observeEvent(input$ab.clear_serum,{
        updateMultiInput(session = session,
                         inputId = "look_samp_serum",
                         selected = rv.multi_serum$data)
    })
    
    mult_samp_serum <- reactive({
        lookup <- input$look_samp_serum
        samp = lapply(lookup,function(x){return(an[x == an[,3],])})
        cls <- do.call(rbind,samp)
        mfc <- fc[fc$an_id %in% cls$an_id,]
        mcon <- con[con$prean_id %in% mfc$prean_id,]
        mcon <- mcon[mcon$matrix == "Serum",]
        mfc <- mfc[mfc$prean_id %in% mcon$prean_id,]
        cls <- cls[cls$an_id %in% mfc$an_id,]
        return(cls)
    })
    
    own.prot_serum <- reactive({
        
        ## Define stability thresholds----
        if(input$own.thresh.samp_serum == TRUE){
            thresh1 = input$own.t.samp_serum[1]/100
        } else {
            thresh1 = 0.2
        }
        
        if(is.null(input$look_samp_serum)){
            cc <- data.frame(analyte = NA, status = NA)
            exp_id.ser = NULL
            ### Sample search: One temp input====
        } else if(!is.null(input$look_samp_serum)){
            cls <- mult_samp_serum()
            all_fc_cls.ser <- fc[which(!is.na(match(fc$an_id,cls$an_id))),]
            all_con_cls.ser <- con[match(unique(all_fc_cls.ser$prean_id),con$prean_id),]
            all_con_cls.ser <- all_con_cls.ser[all_con_cls.ser$matrix == "Serum",]
            all_con_cls.ser$prean_temp <- cut(all_con_cls.ser$prean_temp, breaks = c(-Inf,8,Inf), labels = c("cooled","roomtemp"))
            
            mycond <- list(input$slide.temp_serum,
                           input$slide.time2_serum)
            
            mycond[[1]] <- cut(mycond[[1]], breaks = c(-Inf,8,Inf), labels = c("cooled","roomtemp"))
            
            cc.ind.ser = as.list(unique(cls$an_id))
            
            cc.fc.samp.ser = lapply(cc.ind.ser,function(x){return(all_fc_cls.ser[which(all_fc_cls.ser$an_id %in% x),])})
            
            cc.con.ser = lapply(cc.fc.samp.ser,function(x){
                y = all_con_cls.ser[which(all_con_cls.ser$prean_id %in% x$prean_id),]
                y = y[-which(y$freeze_time == 0),]
                
                y = y[c(which(mycond[[1]] == y$prean_temp)),]
                
                #This decides which experiments are used
                #If conditions are matched exactly by one or more experiments, only those data will be used for decision making
                #If that is not the case: If conditions are approximated by one or more experiments, only those data will be used for decision making
                #If no data is available, no decision can be reached
                y = y[c(which(y$freeze_time == min(y$freeze_time[which(y$freeze_time >= mycond[[2]])],na.rm = T))
                ),] %>% suppressWarnings()
                
                contain.exp = unique(y$exp_id) %>% as.list()
                
                y.ls = lapply(contain.exp,function(z){
                    return(y[which(y$exp_id == z),])
                })
                
                if(any(sapply(y.ls,function(x){sum(x$freeze_time) == 0}))){
                    y.ls = y.ls[-which(sapply(y.ls,function(x){sum(x$freeze_time) == 0}))]
                }
                
                if(any(sapply(y.ls,nrow)!=1)){
                    y.ls = y.ls[-which(sapply(y.ls,nrow)!=1)]
                }
                
                return(y.ls)
            })
            
            exp_id.ser = do.call(rbind,lapply(cc.con.ser,function(x){do.call(rbind,x)}))$exp_id %>% unique()
            
            cc.test.ser = lapply(seq_along(cc.con.ser),function(i){
                if(length(cc.con.ser[[i]]) == 0){
                    return(data.frame(analyte = cls$an_name[i],
                                      status = flags[4],
                                      temp_1 = NA,
                                      ttc = NA,
                                      ttf = NA))
                }else{
                    
                    #Match fold changes
                    exp.test = lapply(cc.con.ser[[i]],function(x){
                        return(merge(x,cc.fc.samp.ser[[i]],by.x = "prean_id", by.y = "prean_id", all.x = T))
                    })
                    
                    #Test conditions
                    fc.test = lapply(exp.test,function(x){
                        #Should return TRUE, if threshold is exceeded
                        thresh1.test = abs(x$fc-1)>thresh1
                        return(thresh1.test)
                    })
                    
                    if(length(fc.test) > 1){
                        #Multiple studies found, result in different conclusion
                        if(!Reduce(identical,fc.test)){
                            return(data.frame(analyte = cls$an_name[i],
                                              status = flags[2],
                                              temp_1 = mycond[[1]],
                                              ttc = "< 60",
                                              ttf = sum(cc.con.ser[[i]][[1]]$freeze_time)))
                            #Multiple studies found, result in sam conclusion (non-identical)   
                        } else {
                            #All studies vote for instability
                            if(all(unlist(fc.test))){
                                return(data.frame(analyte = cls$an_name[i],
                                                  status = flags[1],
                                                  temp_1 = mycond[[1]],
                                                  ttc = "< 60",
                                                  ttf = sum(cc.con.ser[[i]][[1]]$freeze_time)))
                                #All studies vote for stability
                            }  else {
                                return(data.frame(analyte = cls$an_name[i],
                                                  status = flags[3],
                                                  temp_1 = mycond[[1]],
                                                  ttc = "< 60",
                                                  ttf = sum(cc.con.ser[[i]][[1]]$freeze_time)))
                                
                            }
                        }
                        #Only one study found
                    } else if(length(fc.test) == 1){
                        #Study votes for instability
                        if(unlist(fc.test)){
                            return(data.frame(analyte = cls$an_name[i],
                                              status = flags[1],
                                              temp_1 = mycond[[1]],
                                              ttc = "< 60",
                                              ttf = sum(cc.con.ser[[i]][[1]]$freeze_time)))
                            #Study votes for stability
                        } else {
                            return(data.frame(analyte = cls$an_name[i],
                                              status = flags[3],
                                              temp_1 = mycond[[1]],
                                              ttc = "< 60",
                                              ttf = sum(cc.con.ser[[i]][[1]]$freeze_time)))
                        }
                    }
                }
            })
            
            cc = do.call(rbind,cc.test.ser)
            cc = cc[order(cc$analyte),]
            rownames(cc) = NULL
        }
        return(list(cc,exp_id.ser))
    })
    
    output$con_s_serum <- renderDataTable({datatable(own.prot_serum()[[1]], escape = F,
                                                     options = list(columnDefs = list(list(visible=FALSE, targets = 3:5)), rowCallback = JS(
                                                         "function(row, data) {",
                                                         "var full_text = 'Time to centr.: ' + data[4] + ' min ' + 'Time to freeze: ' + data[5] + ' min (' + data[3] + ')'",
                                                         "$('td', row).attr('title', full_text);",
                                                         "}")))})
    
    mult_names_samp_serum <- reactive({
        names <- input$look_samp_serum
        if (length(names > 1)){
            names <- paste("Currently looking at",paste(names, collapse = ", "))
        } else if (length(names == 1)){
            names <- paste("Currently looking at", names)
        } else {
            names <- c("Please select at least one compound class")
        }
        return(names)
    })
    
    output$pr.s_serum <- renderText({mult_names_samp_serum()})
    
    sampref_serum <- reactive({
        cl <- own.prot_serum()[[2]]
        if(length(cl) == 0){
            s.s.ref = data.frame(Ref = NA,
                                 Citation = NA)
        } else {
            match.cl = which(ref$exp_id %in% cl)
            s.s.ref = ref[match.cl,]
            s.s.ref = data.frame(Ref = s.s.ref$ref,
                                 Citation = s.s.ref$ref_long)
        }
        return(s.s.ref)
    })
    
    output$samp.ref_serum <- renderDataTable(sampref_serum())
    
    samp.plot_serum <- reactive({
        cc <- own.prot_serum()[[1]]
        if(!is.null(input$look_samp_serum)){
            MyPie(cc$status)
        } else {
            MyPie(flags[4])
        }
        
    })
    
    output$samp.pie_serum <- renderPlot({samp.plot_serum()})
    
    detail_s_download_serum <- reactive({
        ttf = input$slide.time2_serum
        temp = input$slide.temp_serum
        
        cl <- paste(input$look_samp_serum, collapse = ",")
        cc = own.prot_serum()[[1]][,-4]
        
        colnames.cc = c("analyte","status","temperature (before freezing)[°C]","time to freeze [min]") %>% t() %>% as.data.frame()
        
        cc$status = recode(cc$status,
                           "<img src=\"flags/red.png\" height=\"24\"></img>" = "WARNING",
                           "<img src=\"flags/yel.png\" height=\"24\"></img>" = "INCONCLUSIVE",
                           "<img src=\"flags/gre.png\" height=\"24\"></img>" = "OK",
                           "<img src=\"flags/tra.png\" height=\"24\"></img>" = "")
        
        cc = unname(cc)
        
        refo = paste(sampref()[,1], collapse = "; ")
        refo = c("ref",refo,rep("",2)) %>% t() %>% as.data.frame()
        
        own.t = if(is.null(input$own.t.samp_serum)){c("20, 30")}else{paste(input$own.t.samp_serum, collapse = ", ")}
        
        query.df = data.frame(c("date","query","stability thresholds [%]","time to centrifugation [min]","time to freezing [min]","temp. (during time to centr.)[°C]","temp. (during time to freeze)[°C]"),
                              c(date(),cl,own.t,"<60",ttf,"room temperature",temp))
        
        query.df = cbind(query.df,matrix("",ncol = 2, nrow = 7)) %>% as.data.frame()
        
        empty.row = rep("",4) %>% t() %>% as.data.frame()
        
        col.row = c("analyte","status","temp. after centr. [°C]","time to freeze [°C]") %>% t() %>% as.data.frame()
        
        samp.ls = list(query.df,empty.row,col.row,cc,empty.row,refo)
        
        samp.ls = lapply(seq_along(samp.ls),function(i){colnames(samp.ls[[i]]) = make.names(c(1:4));return(samp.ls[[i]])})
        
        samp.down = do.call(rbind,samp.ls)
        
        return(samp.down)
    })
    
    output$download_s_serum <- downloadHandler(
        filename = function(){
            paste("ALISTER_Export_SampleSearch_",make.names(date()),".csv",collapse="")
        },
        content = function(file){
            write.csv(detail_s_download_serum(),file,row.names = F)
        }
    )
    
    # Analyte search - Serum####
    
    output$text.an_serum <- renderText({input$look_an_serum})
    
    observe({
        updateSliderInput(
            session = session,
            inputId = "an.slide.temp_serum",
            value = input$an.num.temp_serum
        )
    })
    
    observe({
        updateNumericInput(
            session = session,
            inputId = "an.num.temp_serum",
            value = input$an.slide.temp_serum
        )
    })
    
    output$own.t.an_serum <- renderUI({
        req(input$own.thresh.an_serum == TRUE)
        sliderInput("own.t.an_serum",
                    h5(strong("Stability Threshold [%]")),
                    min = 0,
                    max = 100,
                    step = 5,
                    value = c(20))
    })
    
    output$own.t.h.an_serum <- renderUI({
        req(input$own.thresh.an_serum == TRUE)
        helpText("ALISTER defines stability by setting cut-offs for fold changes specific
                 to analytes subjected to certain pre-analytical conditions. The selected 
                 threshold is the maximum allowed fold change occuring in serum during post-centrifugation
                 processing delay (default is 20%).")
    })
    
    ## Protocol assessment----
    own.prot.an_serum <- reactive({
        
        if(input$own.thresh.an_serum == TRUE){
            thresh1 = input$own.t.an_serum[1]/100
        } else {
            thresh1 = 0.2
        }
        
        myan.ser <- input$look_an_serum
        my.ser <- an[myan.ser == an[,2],]
        all_fc_my.ser <- fc[which(!is.na(match(fc$an_id,my.ser$an_id))),]
        all_con_my.ser <- con[match(unique(all_fc_my.ser$prean_id),con$prean_id),]
        all_con_my.ser <- all_con_my.ser[all_con_my.ser$matrix == "Serum",]
        all_con_my.ser$prean_temp = cut(all_con_my.ser$prean_temp, breaks = c(-Inf,8,Inf), labels = c("cooled","roomtemp"))
        
        if(!is.null(input$look_an_serum)){
            mycond <- list(input$an.slide.temp_serum,
                           input$an.slide.time2_serum)
            mycond[[1]] <- cut(mycond[[1]], breaks = c(-Inf,8,Inf), labels = c("cooled","roomtemp"))   
        }
        
        an.con.ser = all_con_my.ser[which(all_con_my.ser$prean_id %in% all_fc_my.ser$prean_id),]
        
        an.con.ser = an.con.ser[which(mycond[[1]] == an.con.ser$prean_temp & an.con.ser$freeze_time!=0),]
        
        an.con.ser = an.con.ser[which(an.con.ser$freeze_time == min(an.con.ser$freeze_time[which(an.con.ser$freeze_time >= mycond[[2]])],na.rm = T)),] %>% suppressWarnings()
        
        contain.exp.ser = unique(an.con.ser$exp_id) %>% as.list()
        
        cc.con.ser = lapply(contain.exp.ser,function(z){
            return(an.con.ser[which(an.con.ser$exp_id == z),])
        })
        
        if(any(sapply(cc.con.ser,function(x){x$freeze_time == 0}))){
            cc.con.ser = cc.con.ser[-which(sapply(cc.con.ser,function(x){x$freeze_time == 0}))]
        }
        
        #Merging with fold changes and reaching descision
        cc.test.ser = if(length(cc.con.ser) == 0){
            #No data found
            list(1,
                 data.frame("time_to_centrifugation" = NA,
                            'time_to_freeze' = NA,
                            'temp.' = NA,
                            'trend' = NA,
                            'ref' = NA)) %>% return()
        } else {
            #Match fold changes
            exp.test.an.ser = lapply(cc.con.ser,function(x){
                return(merge(x,all_fc_my.ser,by.x = "prean_id", by.y = "prean_id", all.x = T))
            })
            
            #Test conditions
            fc.test.ser = lapply(exp.test.an.ser,function(x){
                #Should return TRUE, if threshold is exceeded
                thresh1.test = abs(x$fc-1)>thresh1
                return(thresh1.test)
            })
            
            updown.test.ser = lapply(exp.test.an.ser,function(x){
                thresh.updown = sapply(seq_along(x$fc),function(y){if(x$fc[y]>(1+thresh1)){return(trend[1])}else if(x$fc[y]<(1-thresh1)){return(trend[2])}else{return(trend[3])}})
                return(thresh.updown)
            })
            
            exp.out.ser = do.call(rbind,exp.test.an.ser)
            
            if(length(fc.test.ser) > 1){
                #No identical outcomes between studies
                if(!Reduce(identical,fc.test.ser)){
                    list(3,
                         data.frame("time_to_centrifugation" = rep("< 60",nrow(exp.out.ser)),
                                    'time_to_freeze' = exp.out.ser$freeze_time,
                                    'temp.' = exp.out.ser$prean_temp,
                                    'trend' = unlist(updown.test.ser),
                                    'ref' = exp.out.ser$ref))
                } else {
                    #All studies vote for instability
                    if(all(unlist(fc.test.ser))){
                        list(4,
                             data.frame("time_to_centrifugation" = rep("< 60",nrow(exp.out.ser)),
                                        'time_to_freeze' = exp.out.ser$freeze_time,
                                        'temp.' = exp.out.ser$prean_temp,
                                        'trend' = unlist(updown.test.ser),
                                        'ref' = exp.out.ser$ref))
                        #All studies vote for stability
                    } else {
                        list(2,
                             data.frame("time_to_centrifugation" = rep("< 60",nrow(exp.out.ser)),
                                        'time_to_freeze' = exp.out.ser$freeze_time,
                                        'temp.' = exp.out.ser$prean_temp,
                                        'trend' = unlist(updown.test.ser),
                                        'ref' = exp.out.ser$ref))
                    }
                }
            } else if (length(fc.test.ser) == 1){
                #Study votes for instability
                if(unlist(fc.test.ser)){
                    list(4,
                         data.frame("time_to_centrifugation" = rep("< 60",nrow(exp.out.ser)),
                                    'time_to_freeze' = exp.out.ser$freeze_time,
                                    'temp.' = exp.out.ser$prean_temp,
                                    'trend' = unlist(updown.test.ser),
                                    'ref' = exp.out.ser$ref))
                    #Study votes for stability    
                } else {
                    list(2,
                         data.frame("time_to_centrifugation" = rep("< 60",nrow(exp.out.ser)),
                                    'time_to_freeze' = exp.out.ser$freeze_time,
                                    'temp.' = exp.out.ser$prean_temp,
                                    'trend' = unlist(updown.test.ser),
                                    'ref' = exp.out.ser$ref))
                }
            }
        }
        
        return(cc.test.ser)
    })
    
    prot.an_serum <- reactive({
        cc <- own.prot.an_serum()[[1]]
        text <- c("Our data does not seem to be able to approximate the protocol you entered.",
                  "The protocol seems to ensure analyte stability.",
                  "Our data comes to different conclusions regarding analyte stability for your protocol.",
                  "The analyte seems to be unstable under the suggested protocol.")
        cc <- text[cc]
        return(cc)
    })
    
    output$prot.an.rec_serum <- renderText({prot.an_serum()})
    
    prot.an.sym_serum <- reactive({
        cc <- own.prot.an_serum()[[1]]
        cc <- an.flag[cc]
        return(cc)
    })
    
    output$own.prot.an.sym_serum <- renderUI({img(src = prot.an.sym_serum(), height = "200px", align = "right")})
    
    detail_an_download_serum <- reactive({
        
        query = unlist(input$look_an_serum)
        own.t = if(is.null(input$own.t.an_serum)){c("20")}else{input$own.t.an_serum}
        
        ttc = "< 60"
        ttf = input$an.slide.time2_serum
        temp = input$an.slide.temp_serum
        
        status = recode(own.prot.an_serum()[[1]],
                        "1" = "WARNING",
                        "2" = "INCONCLUSIVE",
                        "3" = "OK", 
                        "4" = "NA")
        
        empty.row = rep("",5) %>% t() %>% as.data.frame()
        
        col.row = c("time to centr.[min]","time to freeze [min]","temp[°C]","trend","ref") %>% t() %>% as.data.frame()
        
        det = own.prot.an_serum()[[2]] %>% as.data.frame()
        
        det$trend = recode(det$trend,
                           "<img src=\"trend/trend_up.PNG\" height=\"24\"></img>" = "UP",
                           "<img src=\"trend/trend_down.PNG\" height=\"24\"></img>" = "DOWN",
                           "<img src=\"trend/trend_stable.PNG\" height=\"24\"></img>" = "STABLE")
        
        query.df = cbind(data.frame(c("date","query","stability thresholds [%]","","time to centrifugation [min]","time to freeze [min]","temp. (during time to centr.)[°C]","temp. (during time to centr.)[min]"),
                                    c(date(),query,own.t,"",ttc,ttf,"room temperature",temp)),
                         matrix("",ncol = 3, nrow = 8)) %>% as.data.frame()
        
        df.ls = list(query.df,empty.row,col.row,det)
        
        df.ls = lapply(seq_along(df.ls),function(i){colnames(df.ls[[i]]) = make.names(c(1:5));return(df.ls[[i]])})
        
        df.ls = do.call(rbind,df.ls) %>% as.data.frame()
        
        return(df.ls)
    })
    
    detail_view_serum <- reactive({
        detail_view <- own.prot.an_serum()[[2]]
        return(detail_view)
    })
    
    output$an.detail_serum <- renderDataTable({datatable(detail_view_serum(), escape = F, rownames = F)})
    
    anref_serum <- reactive({
        cc = own.prot.an_serum()[[2]]$ref
        if(length(cc) == 0){
            a.ser.ref = data.frame(Ref = NA,
                                   Citation = NA)
        } else {
            match.ref = which(ref$ref %in% cc)
            a.ser.ref = ref[match.ref,]
            a.ser.ref = data.frame(Ref = a.ser.ref$ref,
                                   Citation = a.ser.ref$ref_long)
        }
        return(a.ser.ref)
    })
    
    output$an.ref_serum <- renderDataTable(anref_serum())
    
    output$download_an_serum <- downloadHandler(
        filename = function(){
            paste("ALISTER_Export_AnalyteSearch_",make.names(date()),".csv",sep="")
        },
        content = function(file){
            write.csv(detail_an_download_serum(),file,row.names = F)
        }
    )
    
    # Data filtering mode - Serum####
    
    observe({
        updateSliderInput(
            session = session,
            inputId = "filt.slide.temp_serum",
            value = input$filt.num.temp_serum
        )
    })
    
    observe({
        updateNumericInput(
            session = session,
            inputId = "filt.num.temp_serum",
            value = input$filt.slide.temp_serum
        )
    })
    
    output$own.t.filt_serum <- renderUI({
        req(input$own.thresh.filt_serum == TRUE)
        sliderInput("own.t.filt_serum",
                    h5(strong("Stability Threshold [%]")),
                    min = 0,
                    max = 100,
                    step = 5,
                    value = c(20))
    })
    
    output$own.t.h.filt_serum <- renderUI({
        req(input$own.thresh.filt_serum == TRUE)
        helpText("ALISTER defines stability by setting cut-offs for fold changes specific
                 to analytes subjected to certain pre-analytical conditions. The lower
                 threshold poses as the critical cut-off for singular foldchanges (default is 20%). The upper
                 fold change is the critical cut-off for summed up fold changes (default is 30%).")
    })
    
    filt.status_serum <- reactive({
        if(is.null(input$csvtable_serum)){
            text <- c("Please upload a dataset for filtering")
            return(text)
        } else {
            text <- c("Your filtered data")
            return(text)
        }
    })
    
    output$upload.status_serum <- renderText({filt.status_serum()})
    
    uploadcsv_serum <- reactive({
        if(is.null(input$csvtable_serum)){
            return(NULL)
        } else {
            df <- read.table(input$csvtable_serum$datapath,
                             header = T,
                             sep = input$sep_serum,
                             check.names = F)
            #Omit columns without names
            if(any(sapply(colnames(df),nchar) == 0)){
                df <- df[,-which(sapply(colnames(df),nchar) == 0)]
            }
            return(df)
        }
    })
    
    matchfilt_serum <- reactive({
        if(is.null(input$csvtable_serum)){
            return(NULL)
        } else {
            df <- uploadcsv_serum()
            
            col.filt <- colnames(df)
            # col.parse <- parseLipidNames(col.filt)
            # col.parse <- col.parse$Normalized.Name[match(col.filt,col.parse$Original.Name)]
            col.parse = make_clean_names(col.filt)
            col.filt <- cbind(col.filt,col.parse)
            
            all.filt <- unlist(serum.an)
            # all.parse <- parseLipidNames(make_clean_names(all.filt))
            # all.parse <- all.parse$Normalized.Name[match(all.filt,all.parse$Original.Name)]
            all.parse = make_clean_names(all.filt)
            all.filt <- cbind(all.filt,all.parse)
            
            mn <- match(col.filt[,2],all.filt[,2], incomparables = col.filt[is.na(col.filt[,2]),2])
            mo <- match(col.filt[,1],all.filt[,1], incomparables = col.filt[is.na(col.filt[,1]),1])
            
            all.mat <- mo
            if(any(is.na(all.mat))){
                all.mat[is.na(all.mat)] <- mn[is.na(all.mat)]
            }
            names(all.mat) <- serum.an[all.mat]
            
            col.filt <- colnames(df)
            #col.parse <- parseLipidNames(col.filt)
            #col.parse <- col.parse$Normalized.Name[match(col.filt,col.parse$Original.Name)]
            col.parse = make_clean_names(col.filt)
            col.filt <- cbind(col.filt,col.parse)
            
            all.filt <- an$an_name
            # all.parse <- parseLipidNames(make_clean_names(all.filt))
            # all.parse <- all.parse$Normalized.Name[match(all.filt,all.parse$Original.Name)]
            all.parse = make_clean_names(all.filt)
            all.filt <- cbind(all.filt,all.parse)
            
            mn <- match(col.filt[,2],all.filt[,2], incomparables = col.filt[is.na(col.filt[,2]),2])
            mo <- match(col.filt[,1],all.filt[,1], incomparables = col.filt[is.na(col.filt[,1]),1])
            
            all.mat2 <- mo
            if(any(is.na(all.mat2))){
                all.mat2[is.na(all.mat2)] <- mn[is.na(all.mat2)]
            }
            names(all.mat2) <- an$an_name[all.mat2]
            
            all.mat[which(!is.na(all.mat))] = all.mat2[which(!is.na(all.mat))]
            
            filt.ls <- list(matched = na.omit(all.mat),
                            found = which(!is.na(all.mat)),
                            notfound = colnames(df)[which(is.na(all.mat))],
                            all.mat) 
            return(filt.ls)
        }
    })
    
    filt.table_serum <- reactive({
        
        if(is.null(input$csvtable_serum)){
            return(NULL)
            ## Data filtering mode one temp ---- 
        } else {
            filt.ls.ser <- matchfilt_serum()
            
            if(input$own.thresh.filt_serum == TRUE){
                thresh1 = input$own.t.filt_serum[1]/100
            } else {
                thresh1 = 0.2
            }
            
            mycond <- list(input$filt.slide.temp_serum,
                           input$filt.slide.time2_serum)
            
            mycond[[1]] <- cut(mycond[[1]], breaks = c(-Inf,8,Inf), labels = c("cooled","roomtemp"))
            
            #Reveive parsed lipid names    
            find.filt.ser <- filt.ls.ser[[1]]
            
            if(length(find.filt.ser) > 0){
                
                anid.filt.ser <- an$an_id[find.filt.ser]
                fc.filt.ser <- fc[which(!is.na(match(fc$an_id,anid.filt.ser))),]
                all_con_filt.ser <- con[match(unique(fc.filt.ser$prean_id),con$prean_id),]
                all_con_filt.ser <- all_con_filt.ser[all_con_filt.ser$matrix == "Serum",]
                all_con_filt.ser$prean_temp = cut(all_con_filt.ser$prean_temp, breaks = c(-Inf,8,Inf), labels = c("cooled","roomtemp"))
                
                filt.ind.ser = as.list(unique(fc.filt.ser$an_id))
                
                filt.fc.samp.ser = lapply(filt.ind.ser,function(x){return(fc.filt.ser[which(fc.filt.ser$an_id %in% x),])})
                
                filt.con.ser = lapply(filt.fc.samp.ser,function(x){
                    y = all_con_filt.ser[which(all_con_filt.ser$prean_id %in% x$prean_id),]
                    
                    y = y[which(mycond[[1]] == y$prean_temp & y$freeze_time!=0),]
                    
                    #This decides which experiments are used
                    #If conditions are matched exactly by one or more experiments, only those data will be used for decision making
                    #If that is not the case: If conditions are approximated by one or more experiments, only those data will be used for decision making
                    #If no data is available, no decision can be reached
                    y = y[which(y$freeze_time == min(y$freeze_time[which(y$freeze_time >= mycond[[2]])],na.rm = T)),] %>% suppressWarnings()
                    
                    contain.exp.ser = unique(y$exp_id) %>% as.list()
                    
                    y.ls = lapply(contain.exp.ser,function(z){
                        return(y[which(y$exp_id == z),])
                    })
                    
                    if(any(sapply(y.ls,function(x){sum(x$freeze_time) == 0}))){
                        y.ls = y.ls[-which(sapply(y.ls,function(x){sum(x$freeze_time) == 0}))]
                    }
                    
                    if(any(sapply(y.ls,nrow)!=1)){
                        y.ls = y.ls[-which(sapply(y.ls,nrow)!=1)]
                    }
                    return(y.ls)
                })
                
                exp_id.ser = do.call(rbind,lapply(filt.con.ser,function(x){do.call(rbind,x)}))$exp_id %>% unique()
                
                filt.test.ser = lapply(seq_along(filt.con.ser),function(i){
                    if(length(filt.con.ser[[i]]) == 0){
                        return(c(TRUE,flags[4], temp_1 = NA, ttc = NA, ttf = NA))
                    }else{
                        
                        #Match fold changes
                        exp.test.ser = lapply(filt.con.ser[[i]],function(x){
                            return(merge(x,filt.fc.samp.ser[[i]],by.x = "prean_id", by.y = "prean_id", all.x = T))
                        })
                        
                        #Test conditions
                        fc.test.ser = lapply(exp.test.ser,function(x){
                            #Should return TRUE, if threshold is exceeded
                            thresh1.test = abs(x$fc-1)>thresh1
                            return(thresh1.test)
                        })
                        
                        if(length(fc.test.ser) > 1){
                            #Multiple studies found, result in different conclusion
                            if(!Reduce(identical,fc.test.ser)){
                                return(c(TRUE,flags[2], temp_1 = as.character(mycond[[1]]), ttc = "< 60", ttf = unique(exp.test.ser[[1]]$freeze_time)))
                                #Multiple studies found, result in sam conclusion (non-identical)   
                            } else {
                                #All studies vote for instability
                                if(all(unlist(fc.test.ser))){
                                    return(c(F,flags[1], temp_1 = as.character(mycond[[1]]), ttc = "< 60", ttf = unique(exp.test.ser[[1]]$freeze_time)))
                                    #All studies vote for stability
                                }  else {
                                    return(c(T,flags[3], temp_1 = as.character(mycond[[1]]), ttc = "< 60", ttf = unique(exp.test.ser[[1]]$freeze_time)))
                                    
                                }
                            }
                            #Only one study found
                        } else if(length(fc.test.ser) == 1){
                            #Study votes for instability
                            if(unlist(fc.test.ser)){
                                return(c(F,flags[1], temp_1 = as.character(mycond[[1]]), ttc = "< 60", ttf = unique(exp.test.ser[[1]]$freeze_time)))
                                #Study votes for stability
                            } else {
                                return(c(T,flags[3],  temp_1 = as.character(mycond[[1]]), ttc = "< 60", ttf = unique(exp.test.ser[[1]]$freeze_time)))
                            }
                        }
                    }
                })
                
                fls.ser <- do.call(cbind,filt.test.ser)
                colnames(fls.ser) = names(find.filt.ser)
                
                nfls.ser = matrix(rep(c(TRUE, flags[4], temp_1 = NA, ttc = NA, ttf = NA),length(filt.ls.ser[[3]])), nrow = 5, ncol = length(filt.ls.ser[[3]]), byrow = F) %>% as.data.frame()
                colnames(nfls.ser) = filt.ls.ser[[3]]
                
                filt.ser <- cbind(nfls.ser,fls.ser)
                return(list(filt.ser,exp_id.ser))
                
            } else {
                nfls.ser <- filt.ls.ser[[3]]
                allfls.ser <- matrix(rep(c(TRUE, flags[6], temp_1 = NA, ttc = NA, ttf = NA),length(nfls.ser)), nrow = 5, ncol = length(nfls.ser), byrow = F) %>% as.data.frame()
                names(allfls.ser) <- nfls.ser
                return(list(allfls.ser,NA))
            }
        }
    })
    
    filt.dt_serum <- reactive({
        if(is.null(input$csvtable_serum)){
            return(NULL)
        } else {
            df <- uploadcsv_serum()
            filt.ser <- filt.table_serum()[[1]]
            
            filt.ser <- filt.ser[2:5,colnames(df)]
            if(any(duplicated(colnames(df)))){
                colnames(filt.ser)[which(duplicated(colnames(df)))] <- colnames(df)[which(duplicated(colnames(df)))]
            }
            dt <- rbind(filt.ser,df) %>% t()%>% as.data.frame()
            colnames(dt)[1] = c("status")
            return(dt)
        } 
    })
    
    output$csvfilt_serum <- renderDataTable({datatable(filt.dt_serum(), escape = F,
                                                       options = list(columnDefs = list(list(visible=FALSE, targets = 2:4)), rowCallback = JS(
                                                           "function(row, data) {",
                                                           "var full_text = 'Time to centr.: ' + data[3] + ' min ' + 'Time to freeze: ' + data[4] + ' min (' + data[2] + ')'",
                                                           "$('td', row).attr('title', full_text);",
                                                           "}")))})
    
    filt.plot_serum <- reactive({
        if(!is.null(input$csvtable_serum)){
            filt.ser <- filt.table_serum()[[1]]
            filt.x <- unlist(filt.ser[2,])
            MyPie(filt.x)
        } else {
            MyPie(flags[4])
        }
        
    })
    
    output$filt.pie_serum <- renderPlot({filt.plot_serum()})
    
    #Prepare filtered table for download
    filt.down_serum <- reactive({
        if(is.null(input$csvtable_serum)){
            return(NULL)
        } else {
            df <- uploadcsv_serum()
            filt <- filt.table_serum()[[1]]
            
            filt <- filt[1,colnames(df)]
            if(any(duplicated(colnames(df)))){
                colnames(filt)[which(duplicated(colnames(df)))] <- colnames(df)[which(duplicated(colnames(df)))]
            }
            filt <- which(as.logical(filt))
            df <- df[,filt]
            return(df)
        }
    })
    
    output$download_filt_serum <- downloadHandler(
        filename = function(){
            paste("ALISTER_Export_filtered_",make.names(date()),".csv",sep="")
        },
        content = function(file){
            write.csv(filt.down_serum(),file,row.names = F)
        }
    )
    
    filtref_serum <- reactive({
        cl.ser <- filt.table_serum()[[2]]
        if(length(cl.ser)<1){
            f.ser.ref = data.frame(Ref = NA,
                                   Citation = NA)
        } else {
            match.ref = which(ref$exp_id %in% cl.ser)
            m.ser.ref = ref[match.ref,]
            f.ser.ref = data.frame(Ref = m.ser.ref$ref,
                                   Citation = m.ser.ref$ref_long)
        }
        return(f.ser.ref)
    })
    
    output$filt.ref_serum <- renderDataTable(filtref_serum())
}
# Run the application 
shinyApp(ui = ui, server = server)
