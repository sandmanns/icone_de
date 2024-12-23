library(shiny)
library(shinyjs)
library(openxlsx)
library(timetools)
library(stringr)
library(igraph)
library(networkD3)
library(RColorBrewer)
library(dplyr)
library(htmlwidgets)
library(readxl)


function(input, output, session) {
  output$FA_UI2<-renderUI({NULL})
  output$FA_UI2a<-renderUI({NULL})
  output$FA_UI2b<-renderUI({NULL})
  output$FA_UI2c<-renderUI({NULL})
  output$FA_UI3<-renderUI({NULL})
  output$FA_UI3b<-renderUI({NULL})
  output$FA_UI4<-renderUI({NULL})
  output$FA_UI4b<-renderUI({NULL})
  output$FA_UI5<-renderUI({NULL})
  output$FA_UI5b<-renderUI({NULL})
  
  output$ST_UI2<-renderUI({NULL})
  output$ST_UI2a<-renderUI({NULL})
  output$ST_UI2b<-renderUI({NULL})
  output$ST_UI2c<-renderUI({NULL})
  output$ST_UI3<-renderUI({NULL})
  output$ST_UI3b<-renderUI({NULL})
  output$ST_UI4<-renderUI({NULL})
  output$ST_UI4b<-renderUI({NULL})
  output$ST_UI5<-renderUI({NULL})
  output$ST_UI5b<-renderUI({NULL})
  
  observe({
    if(input$FA_oder_Station=="Fachabteilungen"){
      output$FA_intro<-renderText({"Kein Input File mit Fachabteilungsinformationen hochgeladen."})
      output$ST_intro<-renderText({NULL})
      
      output$FA_add1a<-renderText({NULL})
      output$FA_add2a<-renderText({NULL})
      output$FA_add3a<-renderText({NULL})
      output$FA_add4a<-renderUI({NULL})
      output$FA_add5a<-renderUI({NULL})
      output$FA_add7a<-renderUI({NULL})
      output$FA_add8a<-renderUI({NULL})
      output$FA_add9a<-renderUI({NULL})
      output$FA_add10a<-renderUI({NULL})
      output$FA_add11a<-renderUI({NULL})
      
    }
    if(input$FA_oder_Station=="Stationen"){
      output$ST_intro<-renderText({"Kein Input File mit Stationsinformationen hochgeladen."})
      output$FA_intro<-renderText({NULL})
      
      output$FA_add1a<-renderText({NULL})
      output$FA_add2a<-renderText({NULL})
      output$FA_add3a<-renderText({NULL})
      output$FA_add4a<-renderUI({NULL})
      output$FA_add5a<-renderUI({NULL})
      output$FA_add7a<-renderUI({NULL})
      output$FA_add8a<-renderUI({NULL})
      output$FA_add9a<-renderUI({NULL})
      output$FA_add10a<-renderUI({NULL})
      output$FA_add11a<-renderUI({NULL})
    }
  })
  
  output$inputFileUI<-renderUI({
    if(input$ownData=="Eigene Daten hochladen"){
      fileInput('inputFile',label = "Datei hochladen (Fachabteilungen)")
    }else{
      NULL
    }
  })
  output$inputFileUIb<-renderUI({
    if(input$ownData=="Eigene Daten hochladen"){
      fileInput('inputFileb',label = "Datei hochladen (Stationen)")
    }else{
      NULL
    }
  })
  
  output$sepInputUI<-renderUI({
    if(input$ownData=="Eigene Daten hochladen"){
      radioButtons('sepInput',label = "Trennzeichen",
                   choices = c("Komma","Semikolon","Tab"),selected = character(0),
                   inline = T)
    }else{
      NULL
    }
  })
  output$sepInputUIb<-renderUI({
    if(input$ownData=="Eigene Daten hochladen"){
      radioButtons('sepInputb',label = "Trennzeichen",
                   choices = c("Komma","Semikolon","Tab"),selected = character(0),
                   inline = T)
    }else{
      NULL
    }
  })
  
  
  observeEvent(input$do_clear,{
    session$reload()
    
  })
  
  
  observeEvent(input$do_in,{
    if(input$ownData=="Demo Daten laden"){
      shinyjs::html("text", paste0("<br>Demo Files werden eingelesen.<br><br>"), add = FALSE)
      input1a<-read.table("www/Fachabteilung_V2.txt",sep="\t",header=T)
      input1b<-read.table("www/Station_V2.txt",sep="\t",header=T)
      input_temp<-T
      input_tempb<-T
      shinyjs::html("text", paste0("Input Files erfolgreich eingelesen.","<br>"), add = TRUE)  
    }else{
      shinyjs::html("text", paste0("<br>Input Files werden eingelesen.<br><br>"), add = FALSE)
      input_temp<-input$inputFile
      input_tempb<-input$inputFileb
      if(is.null(input_temp)&&is.null(input_tempb)){
        shinyjs::html("text", paste0("ERROR: Keine Input Files definiert.","<br>"), add = TRUE) 
        return()
      }
      if(!is.null(input_temp)&&is.null(input$sepInput)){
        shinyjs::html("text", paste0("ERROR: Kein Trennzeichen für Fachabteilungs-Datei definiert.","<br>"), add = TRUE) 
        return()
      }
      if(!is.null(input_tempb)&&is.null(input$sepInputb)){
        shinyjs::html("text", paste0("ERROR: Kein Trennzeichen für Stations-Datei definiert.","<br>"), add = TRUE) 
        return()
      }
      
      if(!is.null(input_temp)){
        if(input$sepInput=="Komma"){
          input1a<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep=",",stringsAsFactors = F)      
        }
        if(input$sepInput=="Semikolon"){
          input1a<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep=";",stringsAsFactors = F)      
        }
        if(input$sepInput=="Tab"){
          input1a<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep="\t",stringsAsFactors = F)    
        }        
      }
      if(!is.null(input_tempb)){
        if(input$sepInputb=="Komma"){
          input1b<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep=",",stringsAsFactors = F)      
        }
        if(input$sepInputb=="Semikolon"){
          input1b<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep=";",stringsAsFactors = F)      
        }
        if(input$sepInputb=="Tab"){
          input1b<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep="\t",stringsAsFactors = F)    
        }        
      }
      if(!is.null(input_temp)&&!is.null(input_tempb)){
        shinyjs::html("text", paste0("Input Files erfolgreich eingelesen.","<br>"), add = TRUE)        
      }else{
        shinyjs::html("text", paste0("Input File erfolgreich eingelesen.","<br>"), add = TRUE)
      }
    }
  
      ##Fachabteilungen
    if(!is.null(input_temp)){
      output$columnUI0<-renderText({"Fachabteilungen: Wählen Sie die Spalte, die Informationen enthält zu..."})
      
      output$columnUI1<-renderUI({
        selectInput('column1',label = HTML("...Fallnummer:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),choices = names(input1a))
      })
      
      if(input$ownData=="Demo Daten laden"){
        output$columnUI2<-renderUI({
          selectInput('column2',label = HTML("...Fachabteilung:&nbsp&nbsp"),
                      choices = names(input1a),selected = "Fachabteilung")
        })
      }else{
        output$columnUI2<-renderUI({
          selectInput('column2',label = HTML("...Fachabteilung:&nbsp&nbsp"),
                      choices = names(input1a))
        })
      }
      
      if(input$ownData=="Demo Daten laden"){
        output$columnUI3<-renderUI({
          selectInput('column3',label = HTML("...Aufnahme:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),
                      choices = names(input1a),selected="Aufnahme")
        })
      }else{
        output$columnUI3<-renderUI({
          selectInput('column3',label = HTML("...Aufnahme:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),
                      choices = names(input1a))
        })
      }
      
      if(input$ownData=="Demo Daten laden"){
        output$columnUI4<-renderUI({
          selectInput('column4',label = HTML("...Entlassung:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),
                      choices = names(input1a),selected="Entlassung")
        })
      }else{
        output$columnUI4<-renderUI({
          selectInput('column4',label = HTML("...Entlassung:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),choices = names(input1a))
        })
      }
      
      if(input$ownData=="Demo Daten laden"){
        output$columnUI5<-renderUI({
          selectInput('column5',label = HTML("...Hauptdiagnose:&nbsp"),
                      choices = names(input1a),selected="Hauptdiagnose")
        })
      }else{
        output$columnUI5<-renderUI({
          selectInput('column5',label = HTML("...Hauptdiagnose:&nbsp"),choices = names(input1a))
        })
      }
      
      output$columnUI6<-renderText({"Hinweise:\n Die Zeit der Aufnahme und Entlassung ist zu kodieren als JJJJ-MM-TT Std:Min:Sek.\n
      Die Hauptdiagnose ist über ICD-Codes zu definieren. Es wird die Angabe 
    eines konkreten Codes erwartet, nicht nur die eines Kapitels."}) 
    }
    
      ##Stationen
      if(!is.null(input_tempb)){
          output$columnUI0b<-renderText({"Stationen: Wählen Sie die Spalte, die Informationen enthält zu..."})
          
          output$columnUI1b<-renderUI({
              selectInput('column1b',label = HTML("...Fallnummer:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),choices = names(input1b))
          })
          
          if(input$ownData=="Demo Daten laden"){
              output$columnUI2b<-renderUI({
                  selectInput('column2b',label = HTML("...Station:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),
                              choices = names(input1b),selected = "Station")
              })
          }else{
              output$columnUI2b<-renderUI({
                  selectInput('column2b',label = HTML("...Station:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),
                              choices = names(input1b))
              })
          }
          
          if(input$ownData=="Demo Daten laden"){
              output$columnUI3b<-renderUI({
                  selectInput('column3b',label = HTML("...Aufnahme:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),
                              choices = names(input1b),selected="Aufnahme")
              })
          }else{
              output$columnUI3b<-renderUI({
                  selectInput('column3b',label = HTML("...Aufnahme:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),
                              choices = names(input1b))
              })
          }
          
          if(input$ownData=="Demo Daten laden"){
              output$columnUI4b<-renderUI({
                  selectInput('column4b',label = HTML("...Entlassung:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),
                              choices = names(input1b),selected="Entlassung")
              })
          }else{
              output$columnUI4b<-renderUI({
                  selectInput('column4b',label = HTML("...Entlassung:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),choices = names(input1b))
              })
          }
          
          if(input$ownData=="Demo Daten laden"){
              output$columnUI5b<-renderUI({
                  selectInput('column5b',label = HTML("...Hauptdiagnose:&nbsp"),
                              choices = names(input1b),selected="Hauptdiagnose")
              })
          }else{
              output$columnUI5b<-renderUI({
                  selectInput('column5b',label = HTML("...Hauptdiagnose:&nbsp"),choices = names(input1b))
              })
          }
          
          output$columnUI6b<-renderText({"Hinweise:\n Die Zeit der Aufnahme und Entlassung ist zu kodieren als JJJJ-MM-TT Std:Min:Sek.\n
      Die Hauptdiagnose ist über ICD-Codes zu definieren. Es wird die Angabe 
    eines konkreten Codes erwartet, nicht nur die eines Kapitels."}) 
      }

  
  output$do_in2UI<-renderUI({
    actionButton('do_in2',"Input konfigurieren",class = "btn-primary")
  })

    
  
  observeEvent(input$do_in2,{
    shinyjs::html("text", paste0("<br>Input Files werden konfiguriert.<br><br>"), add = FALSE)
    
      if(input$ownData=="Demo Daten laden"){
          input1a<-read.table("www/Fachabteilung_V2.txt",sep="\t",header=T)
          input1b<-read.table("www/Station_V2.txt",sep="\t",header=T)
          input_temp<-NULL
          input_tempb<-NULL
      }else{
          input_temp<-input$inputFile
          input_tempb<-input$inputFileb
          input1a<-NULL
          input1b<-NULL
          
          if(!is.null(input_temp)){
              if(input$sepInput=="Komma"){
                  input1a<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep=",",stringsAsFactors = F)      
              }
              if(input$sepInput=="Semikolon"){
                  input1a<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep=";",stringsAsFactors = F)      
              }
              if(input$sepInput=="Tab"){
                  input1a<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep="\t",stringsAsFactors = F)    
              }        
          }
          if(!is.null(input_tempb)){
              if(input$sepInputb=="Komma"){
                  input1b<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep=",",stringsAsFactors = F)      
              }
              if(input$sepInputb=="Semikolon"){
                  input1b<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep=";",stringsAsFactors = F)      
              }
              if(input$sepInputb=="Tab"){
                  input1b<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep="\t",stringsAsFactors = F)    
              }        
          }
      } 
      
    icd_ref<-data.frame(Kapitel=c(1,1,2,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,19,20,20,20,20,21,22),
                        Buchstabe=c("A","B","C","D","D","E","F","G","H","H","I","J","K","L","M","N","O","P","Q","R","S","T","V","W","X","Y","Z","U"),
                        Start=c(0,0,0,0,50,0,0,0,0,60,0,0,0,0,0,0,0,0,0,0,0,0,1,49,19,9,0,0),
                        Ende=c(99,99,97,48,90,90,99,99,59,95,99,99,93,99,99,99,99,96,99,99,99,98,99,94,84,84,99,99))
    
      if(!is.null(input1a)){
        spalten<-c(input$column1,input$column2,input$column3,input$column4,input$column5)
        spalten_table<-table(spalten)
        if(sum(as.numeric(spalten_table)>1)>0){
          shinyjs::html("text", paste0("ERROR: Spalten ",names(spalten_table)[which(as.numeric(spalten_table)>1)],
                                       " mehr als 1x für Input Fachabteilungen gewählt.","<br>"), add = TRUE) 
          return()
        }
        
        input_neu<-data.frame(Patientennummer=input1a[,names(input1a)==input$column1],
                              Fallnummer=input1a[,names(input1a)==input$column1],
                              Abteilung=input1a[,names(input1a)==input$column2],
                              Aufnahme=input1a[,names(input1a)==input$column3],
                              Entlassung=input1a[,names(input1a)==input$column4],
                              ICDfull=input1a[,names(input1a)==input$column5])
        input1a<-input_neu
        
        helper<-data.frame(Buchstabe=substring(input1a$ICDfull,1,1),
                           Zahl=as.numeric(substring(input1a$ICDfull,2,3)),
                           ICDfull=input1a$ICDfull)
        result <- helper %>%
          inner_join(icd_ref, by = "Buchstabe",relationship="many-to-many") %>%
          filter(Zahl >= Start & Zahl <= Ende) %>%
          select(ICDfull, Kapitel) 
        input1a$ICDKapitel<-result$Kapitel
        
        input1a$Aufnahme<-as.POSIXct(input1a$Aufnahme,format="%Y-%m-%d %H:%M:%OS",tz="UTC")
        input1a$Entlassung<-as.POSIXct(input1a$Entlassung,format="%Y-%m-%d %H:%M:%OS",tz="UTC")
      }

    if(!is.null(input1b)){
      spalten<-c(input$column1b,input$column2b,input$column3b,input$column4b,input$column5b)
      spalten_table<-table(spalten)
      if(sum(as.numeric(spalten_table)>1)>0){
        shinyjs::html("text", paste0("ERROR: Spalten ",names(spalten_table)[which(as.numeric(spalten_table)>1)],
                                     " mehr als 1x für Input Stationen gewählt.","<br>"), add = TRUE) 
        return()
      } 
      
      input_neu<-data.frame(Patientennummer=input1b[,names(input1b)==input$column1b],
                            Fallnummer=input1b[,names(input1b)==input$column1b],
                            Abteilung=input1b[,names(input1b)==input$column2b],
                            Aufnahme=input1b[,names(input1b)==input$column3b],
                            Entlassung=input1b[,names(input1b)==input$column4b],
                            ICDfull=input1b[,names(input1b)==input$column5b])
      input1b<-input_neu
      
      helper<-data.frame(Buchstabe=substring(input1b$ICDfull,1,1),
                         Zahl=as.numeric(substring(input1b$ICDfull,2,3)),
                         ICDfull=input1b$ICDfull)
      result <- helper %>%
        inner_join(icd_ref, by = "Buchstabe",relationship="many-to-many") %>%
        filter(Zahl >= Start & Zahl <= Ende) %>%
        select(ICDfull, Kapitel) 
      input1b$ICDKapitel<-result$Kapitel
      
      input1b$Aufnahme<-as.POSIXct(input1b$Aufnahme,format="%Y-%m-%d %H:%M:%OS",tz="UTC")
      input1b$Entlassung<-as.POSIXct(input1b$Entlassung,format="%Y-%m-%d %H:%M:%OS",tz="UTC") 
    }

    shinyjs::html("text", paste0("<br>Input Files erfolgreich konfiguriert.<br><br>"), add = TRUE)
    

    output$FA_UI2<-renderUI({NULL})
    output$FA_UI2a<-renderUI({NULL})
    output$FA_UI2b<-renderUI({NULL})
    output$FA_UI2c<-renderUI({NULL})
    output$FA_UI3<-renderUI({NULL})
    output$FA_UI3b<-renderUI({NULL})
    output$FA_UI4<-renderUI({NULL})
    output$FA_UI4b<-renderUI({NULL})
    output$FA_UI5<-renderUI({NULL})
    output$FA_UI5b<-renderUI({NULL})
    
    output$ST_UI2<-renderUI({NULL})
    output$ST_UI2a<-renderUI({NULL})
    output$ST_UI2b<-renderUI({NULL})
    output$ST_UI2c<-renderUI({NULL})
    output$ST_UI3<-renderUI({NULL})
    output$ST_UI3b<-renderUI({NULL})
    output$ST_UI4<-renderUI({NULL})
    output$ST_UI4b<-renderUI({NULL})
    output$ST_UI5<-renderUI({NULL})
    output$ST_UI5b<-renderUI({NULL})
    
    output$text_analyse1<-renderUI({NULL})
    output$text_analyse2<-renderUI({NULL})
    #output$plotBasic<-renderPlot({NULL})
    output$force<-renderForceNetwork({NULL})
    
    output$FA_erweitert_UI1<-renderUI({NULL})
    output$FA_erweitert_UI1b<-renderUI({NULL})
    output$FA_erweitert_UI2<-renderUI({NULL})
    
    output$FA_erweitert_UI2<-renderUI({NULL})
    
    
    ###########################pro Fachabteilungen
    output$FA_button1<-renderUI({actionBttn("do_eigeneFA",label = "Starte Analyse",style = "gradient",color = "primary",block = T,disabled=T)})
    
    observe({
        if(input$FA_oder_Station=="Fachabteilungen"&&!is.null(input1a)){
          output$FA_UI2<-renderUI({NULL})
          output$FA_UI2a<-renderUI({NULL})
          output$FA_UI2b<-renderUI({NULL})
          output$FA_UI2c<-renderUI({NULL})
          output$FA_UI3<-renderUI({NULL})
          output$FA_UI4<-renderUI({NULL})
          output$FA_UI4b<-renderUI({NULL})
          output$FA_UI5<-renderUI({NULL})
          output$FA_UI5b<-renderUI({NULL})
          output$FA_intro<-renderText({NULL})
          
          output$ST_UI2<-renderUI({NULL})
          output$ST_UI2a<-renderUI({NULL})
          output$ST_UI2b<-renderUI({NULL})
          output$ST_UI2c<-renderUI({NULL})
          output$ST_UI3<-renderUI({NULL})
          output$ST_UI4<-renderUI({NULL})
          output$ST_UI4b<-renderUI({NULL})
          output$ST_UI5<-renderUI({NULL})
          output$ST_UI5b<-renderUI({NULL})
          output$ST_intro<-renderText({NULL})
          
          output$FA_add1a<-renderText({"Auswahl Fall"})
          output$FA_add2a<-renderText({"Auswahl Beobachtungszeitraum"})
          output$FA_add3a<-renderText({"Kontaktanalyse"})
          output$FA_add4a<-renderUI({pickerInput("Min_Inkubation_Tag",label = HTML("Latenzzeit:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
                                                 choices = c("0 Tage","1 Tag",paste0(c(2:30)," Tage")),
                                                 options = list(title="Tage",size=5),inline = T,width = "fit")})
          output$FA_add5a<-renderUI({pickerInput("Min_Inkubation_Stunde",label = "",
                                                 choices = c("0 Stunden","1 Stunde",paste0(c(2:23)," Stunden")),
                                                 options = list(title="Stunden",size=5),inline = T,width = "fit")})
          output$FA_add6a<-renderUI({materialSwitch("Fall_Inkubation","Latenzzeit auch für Fall im Fokus?",value = FALSE,status = "primary")})
          output$FA_add7a<-renderUI({pickerInput("Min_Kontakt_Tag",label = HTML("Minimale Expositionszeit:&nbsp;"),
                                                 choices = c("0 Tage","1 Tag",paste0(c(2:30)," Tage")),
                                                 options = list(title="Tage",size=5),inline = T,width = "fit")})
          output$FA_add8a<-renderUI({pickerInput("Min_Kontakt_Stunde",label = "",
                                                 choices = c("0 Stunden","1 Stunde",paste0(c(2:23)," Stunden")),
                                                 options = list(title="Stunden",size=5),inline = T,width = "fit")})
          output$FA_add9a<-renderUI({pickerInput("Min_Ansteckend_Tag",label = HTML("Infektiöse Phase:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
                                                 choices = c("0 Tage","1 Tag",paste0(c(2:30)," Tage")),
                                                 options = list(title="Tage",size=5),inline = T,width = "fit")})
          output$FA_add10a<-renderUI({pickerInput("Min_Ansteckend_Stunde",label = "",
                                                  choices = c("0 Stunden","1 Stunde",paste0(c(2:23)," Stunden")),
                                                  options = list(title="Stunden",size=5),inline = T,width = "fit")})
          output$FA_add11a<-renderUI({sliderTextInput("Grad_Intern",label = "Maximale Länge der Infektionskette",
                                                      choices = c(1:10,"unbegrenzt"),selected = "1",grid=FALSE)})
          
            output$FA_erweitert_UI2<-renderUI({NULL})
            #input1<-readxl::read_excel("www/Verlegungshistorien_2023_2024-03-04_01.xlsx",sheet=2)
            input1<-input1a
            input1<-input1[input1$Abteilung!="Patient_abwesend",]
            input1<-input1[order(input1$Fallnummer),]
            fa<-sort(unique(input1$Abteilung))
            
            output$FA_UI2<-renderUI({
                pickerInput('FA_Fokus',label = "Fachabteilung im Fokus",choices = fa,selected = fa[1],options = list(`live-search` = TRUE))
            })
            output$ST_UI2<-renderUI({NULL})
            
            observe({
                if(!is.null(input$FA_Fokus)){
                    input1_fa<-input1[input1$Abteilung==input$FA_Fokus,]
                    #input1_fa<-input1[input1$Abteilung==fa[1],]
                    
                    output$FA_UI2a<-renderUI({
                        prettyRadioButtons(inputId = "switch99",label = "Hauptdiagnose nach ICD-Code filtern?",
                                           choices = c("nein","ICD-Kapitel","exakter ICD-Code"),selected = "nein",
                                           inline=T,outline = T,bigger = T,
                                           status = "primary",
                                           icon=icon("check")
                        )
                    })
                    
                    observe({
                        if(!is.null(input$switch99)){
                            output$FA_UI3<-renderUI({NULL})
                            output$FA_UI3b<-renderUI({NULL})
                            output$FA_UI4<-renderUI({NULL})
                            output$FA_UI4b<-renderUI({NULL})
                            output$FA_UI5<-renderUI({NULL})
                            output$FA_UI5b<-renderUI({NULL})
                            
                            if(input$switch99=="nein"){
                                output$FA_UI2b<-renderUI({NULL})
                                output$FA_UI2c<-renderUI({NULL})
                                output$FA_UI3b<-renderUI({NULL})
                    
                                fall<-sort(unique(input1_fa$Fallnummer))
                                output$FA_UI3<-renderUI({
                                    pickerInput('Fall_Fokus_FA',label = "Fall im Fokus",choices = fall,options = list(`live-search` = TRUE))
                                })
                                output$ST_UI3<-renderUI({NULL})
                                
                                observe({
                                    if(!is.null(input$Fall_Fokus_FA)){
                                        input1_fall<-as.data.frame(input1_fa[input1_fa$Fallnummer==input$Fall_Fokus_FA,])
                                        
                                        ausgeschlossen<-c()
                                        if(nrow(input1_fall)>1){
                                            for(i in 1:(nrow(input1_fall)-1)){
                                                anfang<-as.Date(input1_fall$Entlassung[i],format="%Y-%m-%d")+1
                                                ende<-as.Date(input1_fall$Aufnahme[i+1],format="%Y-%m-%d")-1
                                                if(anfang==ende){
                                                    ausgeschlossen<-c(as.Date(ausgeschlossen),anfang)
                                                }
                                                if(anfang<ende){
                                                    ausgeschlossen<-c(as.Date(ausgeschlossen),as.Date(anfang:ende))
                                                }
                                            }
                                            output$FA_UI4<-renderUI({dateInput("FA_Start","Beginn des Beobachtungszeitraums",
                                                                               value=as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                               min = as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                               max = as.Date(max(input1_fall$Entlassung),format="%Y-%m-%d"),
                                                                               weekstart = 1,language = "de",format="yyyy-mm-dd",
                                                                               datesdisabled = as.character(ausgeschlossen))})
                                            output$FA_UI4b<-renderUI({h5(paste0("Bedingt durch den ausgewählten Fall können als Beginn nur Tage zwischen ",
                                                                                as.Date(min(input1_fall$Aufnahme)),
                                                                                " und ",as.Date(max(input1_fall$Entlassung))," gewählt werden.\n",
                                                                                "Tage mit Abwesenheit des Falls auf der Fachabteilung ",input$FA_Fokus,
                                                                                " stehen nicht zur Auswahl."))})
                                        }
                                        if(nrow(input1_fall)==1){   
                                            output$FA_UI4<-renderUI({dateInput("FA_Start","Beginn des Beobachtungszeitraums",
                                                                               value=as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                               min = as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                               max = as.Date(max(input1_fall$Entlassung),format="%Y-%m-%d"),
                                                                               weekstart = 1,language = "de",format="yyyy-mm-dd")})
                                            output$FA_UI4b<-renderUI({h5(paste0("Bedingt durch den ausgewählten Fall können als Beginn nur Tage zwischen ",
                                                                                as.Date(min(input1_fall$Aufnahme)),
                                                                                " und ",as.Date(max(input1_fall$Entlassung))," gewählt werden.\n",
                                                                                "Tage mit Abwesenheit des Falls auf der Fachabteilung ",input$FA_Fokus,
                                                                                " stehen nicht zur Auswahl."))})
                                        }
                                        
                                        
                                        output$ST_UI4<-renderUI({NULL})
                                        output$ST_UI4b<-renderUI({NULL})
                                        
                                        observe({
                                            if(!is.null(input$FA_Start)){
                                                #if(input$FA_Start!=as.Date(max(input1_fa$Entlassung))){
                                                    output$FA_UI5<-renderUI({dateInput("FA_Ende","Ende des Beobachtungszeitraums",
                                                                                       value=input$FA_Start+30,
                                                                                       min = input$FA_Start,
                                                                                       max = as.Date(max(input1_fa$Entlassung)),
                                                                                       weekstart = 1,language = "de",format="yyyy-mm-dd")})
                                                    
                                                #}else{
                                                #    output$FA_UI5<-renderUI({dateInput("FA_Ende","Ende des Beobachtungszeitraums",
                                                #                                       value=input$FA_Start,
                                                #                                       min = input$FA_Start,
                                                #                                       max = input$FA_Start,
                                                #                                       weekstart = 1,language = "de",format="yyyy-mm-dd")})
                                                #}
                                                output$FA_UI5b<-renderUI({h5(paste0("Bedingt durch die ausgewählte Fachabteilung können Zeiträume bis maximal ",
                                                                                    as.Date(max(input1_fa$Entlassung),format="%Y-%m-%d"),
                                                                                    " gewählt werden. Standard ist ein Zeitraum von 30 Tagen."))})
                                                
                                                output$ST_UI5<-renderUI({NULL})
                                                output$ST_UI5b<-renderUI({NULL})
                                            }
                                        })
                                    }
                                })
                            }else{
                                if(input$switch99=="ICD-Kapitel"){
                                    output$FA_UI2b<-renderUI({
                                        pickerInput('FA_ICDKapitel',
                                                    choices = c(1:22), 
                                                    choicesOpt = list(
                                                        content = c(HTML('<b>I</b>: A00-B99'),
                                                                    HTML('<b>II</b>: C00-D48'),
                                                                    HTML('<b>III</b>: D50-D90'),
                                                                    HTML('<b>IV</b>: E00-E90'),
                                                                    HTML('<b>V</b>: F00-F99'),
                                                                    HTML('<b>VI</b>: G00-G99'),
                                                                    HTML('<b>VII</b>: H00-H59'),
                                                                    HTML('<b>VIII</b>: H60-H95'),
                                                                    HTML('<b>IX</b>: I00-I99'),
                                                                    HTML('<b>X</b>: J00-J99'),
                                                                    HTML('<b>XI</b>: K00-K93'),
                                                                    HTML('<b>XII</b>: L00-L99'),
                                                                    HTML('<b>XIII</b>: M00-M99'),
                                                                    HTML('<b>XIV</b>: N00-N99'),
                                                                    HTML('<b>XV</b>: O00-O99'),
                                                                    HTML('<b>XVI</b>: P00-P96'),
                                                                    HTML('<b>XVII</b>: Q00-Q99'),
                                                                    HTML('<b>XVIII</b>: R00-R99'),
                                                                    HTML('<b>XIX</b>: S00-T98'),
                                                                    HTML('<b>XX</b>: V01-Y84'),
                                                                    HTML('<b>XXI</b>: Z00-Z99'),
                                                                    HTML('<b>XXII</b>: U00-U99'))
                                                    ),
                                                    options = list(`actions-box` = TRUE,
                                                                   `deselect-all-text` = "Auswahl aufheben",
                                                                   `select-all-text` = "Alle auswählen",
                                                                   `none-selected-text` = "Nichts ausgewählt",
                                                                   sanitize=FALSE),multiple=TRUE)
                                    })
                                    output$FA_UI2c<-renderUI({NULL})
                                    output$FA_UI3b<-renderUI({NULL})
                                    
                                    observe({
                                        if(!is.null(input$FA_ICDKapitel)){
                                            input1_fa<-as.data.frame(input1_fa[input1_fa$ICDKapitel%in%input$FA_ICDKapitel,])
                                            output$FA_UI3b<-renderUI({NULL})
                                            
                                            if(nrow(input1_fa)>0){
                                            fall<-sort(unique(input1_fa$Fallnummer))
                                            output$FA_UI3<-renderUI({
                                                pickerInput('Fall_Fokus_FA',label = "Fall im Fokus",choices = fall,options = list(`live-search` = TRUE))
                                            })
                                            output$ST_UI3<-renderUI({NULL})
                                            
                                            observe({
                                                if(!is.null(input$Fall_Fokus_FA)){
                                                    input1_fall<-as.data.frame(input1_fa[input1_fa$Fallnummer==input$Fall_Fokus_FA,])
                                                    
                                                    ausgeschlossen<-c()
                                                    if(nrow(input1_fall)>1){
                                                        for(i in 1:(nrow(input1_fall)-1)){
                                                            anfang<-as.Date(input1_fall$Entlassung[i],format="%Y-%m-%d")+1
                                                            ende<-as.Date(input1_fall$Aufnahme[i+1],format="%Y-%m-%d")-1
                                                            if(anfang==ende){
                                                                ausgeschlossen<-c(as.Date(ausgeschlossen),anfang)
                                                            }
                                                            if(anfang<ende){
                                                                ausgeschlossen<-c(as.Date(ausgeschlossen),as.Date(anfang:ende))
                                                            }
                                                        }
                                                        output$FA_UI4<-renderUI({dateInput("FA_Start","Beginn des Beobachtungszeitraums",
                                                                                           value=as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                                           min = as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                                           max = as.Date(max(input1_fall$Entlassung),format="%Y-%m-%d"),
                                                                                           weekstart = 1,language = "de",format="yyyy-mm-dd",
                                                                                           datesdisabled = as.character(ausgeschlossen))})
                                                        output$FA_UI4b<-renderUI({h5(paste0("Bedingt durch den ausgewählten Fall können als Beginn nur Tage zwischen ",
                                                                                            as.Date(min(input1_fall$Aufnahme)),
                                                                                            " und ",as.Date(max(input1_fall$Entlassung))," gewählt werden.\n",
                                                                                            "Tage mit Abwesenheit des Falls auf der Fachabteilung ",input$FA_Fokus,
                                                                                            " stehen nicht zur Auswahl."))})
                                                    }
                                                    if(nrow(input1_fall)==1){   
                                                        output$FA_UI4<-renderUI({dateInput("FA_Start","Beginn des Beobachtungszeitraums",
                                                                                           value=as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                                           min = as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                                           max = as.Date(max(input1_fall$Entlassung),format="%Y-%m-%d"),
                                                                                           weekstart = 1,language = "de",format="yyyy-mm-dd")})
                                                        output$FA_UI4b<-renderUI({h5(paste0("Bedingt durch den ausgewählten Fall können als Beginn nur Tage zwischen ",
                                                                                            as.Date(min(input1_fall$Aufnahme)),
                                                                                            " und ",as.Date(max(input1_fall$Entlassung))," gewählt werden.\n",
                                                                                            "Tage mit Abwesenheit des Falls auf der Fachabteilung ",input$FA_Fokus,
                                                                                            " stehen nicht zur Auswahl."))})
                                                    }
                                                    
                                                    
                                                    output$ST_UI4<-renderUI({NULL})
                                                    output$ST_UI4b<-renderUI({NULL})
                                                    
                                                    observe({
                                                        if(!is.null(input$FA_Start)){
                                                            #if(input$FA_Start!=as.Date(max(input1_fa$Entlassung))){
                                                                output$FA_UI5<-renderUI({dateInput("FA_Ende","Ende des Beobachtungszeitraums",
                                                                                                   value=min(as.Date(max(input1_fa$Entlassung)),(input$FA_Start+30)),
                                                                                                   min = input$FA_Start,
                                                                                                   max = as.Date(max(input1_fa$Entlassung)),
                                                                                                   weekstart = 1,language = "de",format="yyyy-mm-dd")})
                                                                
                                                            #}else{
                                                            #    output$FA_UI5<-renderUI({dateInput("FA_Ende","Ende des Beobachtungszeitraums",
                                                            #                                       value=input$FA_Start,
                                                            #                                       min = input$FA_Start,
                                                            #                                       max = input$FA_Start,
                                                            #                                       weekstart = 1,language = "de",format="yyyy-mm-dd")})
                                                            #}
                                                            output$FA_UI5b<-renderUI({h5(paste0("Bedingt durch die ausgewählte Fachabteilung können Zeiträume bis maximal ",
                                                                                                as.Date(max(input1_fa$Entlassung),format="%Y-%m-%d"),
                                                                                                " gewählt werden. Standard ist ein Zeitraum von 30 Tagen."))})
                                                            output$ST_UI5<-renderUI({NULL})
                                                            output$ST_UI5b<-renderUI({NULL})
                                                        }
                                                    })
                                                }
                                            })
                                            }else{
                                                output$FA_UI3b<-renderUI({h5(paste0("Für die ausgewählte Fachabteilung gibt es mit dem von Ihnen definierten ICD-Filter 
                                                                 keine Fälle. Wählen Sie eine andere Fachabteilung und/oder ändern Sie Ihren ICD-Filter, um 
                                                                 einen Beobachtungszeitraum wählen zu können."))})
                                                output$FA_UI4<-renderUI({NULL})
                                                output$FA_UI4b<-renderUI({NULL})
                                                output$FA_UI5<-renderUI({NULL})
                                            }
                                        }else{
                                            output$FA_UI3<-renderUI({NULL})
                                            output$FA_UI3b<-renderUI({NULL})
                                            output$FA_UI4<-renderUI({NULL})
                                            output$FA_UI4b<-renderUI({NULL})
                                            output$FA_UI5<-renderUI({NULL})
                                            output$FA_UI5b<-renderUI({NULL})
                                        }
                                    })
                                }else{
                                    output$FA_UI2c<-renderUI({
                                        textInput("FA_ICDExakt",value = NULL,placeholder = "z.B. A00 oder A00.0",label = NULL)
                                    })
                                    output$FA_UI2b<-renderUI({NULL})
                                    output$FA_UI3b<-renderUI({NULL})
                                    
                                    observe({
                                        if(!is.null(input$FA_ICDExakt)&&input$FA_ICDExakt!=""&&nchar(input$FA_ICDExakt)>=3){
                                            output$FA_UI3b<-renderUI({NULL})
                                            
                                            user_icd<-gsub(".","\\.",input$FA_ICDExakt,fixed=T)
                                            input1_fa<-input1_fa[grep(paste0("^",user_icd),input1_fa$ICDfull),]
                                            
                                            if(nrow(input1_fa)>0){
                                            fall<-sort(unique(input1_fa$Fallnummer))
                                            output$FA_UI3<-renderUI({
                                                pickerInput('Fall_Fokus_FA',label = "Fall im Fokus",choices = fall,options = list(`live-search` = TRUE))
                                            })
                                            output$ST_UI3<-renderUI({NULL})
                                            
                                            observe({
                                                if(!is.null(input$Fall_Fokus_FA)){
                                                    input1_fall<-as.data.frame(input1_fa[input1_fa$Fallnummer==input$Fall_Fokus_FA,])
                                                    
                                                    ausgeschlossen<-c()
                                                    if(nrow(input1_fall)>1){
                                                        for(i in 1:(nrow(input1_fall)-1)){
                                                            anfang<-as.Date(input1_fall$Entlassung[i],format="%Y-%m-%d")+1
                                                            ende<-as.Date(input1_fall$Aufnahme[i+1],format="%Y-%m-%d")-1
                                                            if(anfang==ende){
                                                                ausgeschlossen<-c(as.Date(ausgeschlossen),anfang)
                                                            }
                                                            if(anfang<ende){
                                                                ausgeschlossen<-c(as.Date(ausgeschlossen),as.Date(anfang:ende))
                                                            }
                                                        }
                                                        output$FA_UI4<-renderUI({dateInput("FA_Start","Beginn des Beobachtungszeitraums",
                                                                                           value=as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                                           min = as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                                           max = as.Date(max(input1_fall$Entlassung),format="%Y-%m-%d"),
                                                                                           weekstart = 1,language = "de",format="yyyy-mm-dd",
                                                                                           datesdisabled = as.character(ausgeschlossen))})
                                                        output$FA_UI4b<-renderUI({h5(paste0("Bedingt durch den ausgewählten Fall können als Beginn nur Tage zwischen ",
                                                                                            as.Date(min(input1_fall$Aufnahme)),
                                                                                            " und ",as.Date(max(input1_fall$Entlassung))," gewählt werden.\n",
                                                                                            "Tage mit Abwesenheit des Falls auf der Fachabteilung ",input$FA_Fokus,
                                                                                            " stehen nicht zur Auswahl."))})
                                                    }
                                                    if(nrow(input1_fall)==1){   
                                                        output$FA_UI4<-renderUI({dateInput("FA_Start","Beginn des Beobachtungszeitraums",
                                                                                           value=as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                                           min = as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                                           max = as.Date(max(input1_fall$Entlassung),format="%Y-%m-%d"),
                                                                                           weekstart = 1,language = "de",format="yyyy-mm-dd")})
                                                        output$FA_UI4b<-renderUI({h5(paste0("Bedingt durch den ausgewählten Fall können als Beginn nur Tage zwischen ",
                                                                                            as.Date(min(input1_fall$Aufnahme)),
                                                                                            " und ",as.Date(max(input1_fall$Entlassung))," gewählt werden.\n",
                                                                                            "Tage mit Abwesenheit des Falls auf der Fachabteilung ",input$FA_Fokus,
                                                                                            " stehen nicht zur Auswahl."))})
                                                    }
                                                    
                                                    
                                                    output$ST_UI4<-renderUI({NULL})
                                                    output$ST_UI4b<-renderUI({NULL})
                                                    
                                                    observe({
                                                        if(!is.null(input$FA_Start)){
                                                            #if(input$FA_Start!=as.Date(max(input1_fa$Entlassung))){
                                                                output$FA_UI5<-renderUI({dateInput("FA_Ende","Ende des Beobachtungszeitraums",
                                                                                                   value=min(as.Date(max(input1_fa$Entlassung)),(input$FA_Start+30)),
                                                                                                   min = input$FA_Start,
                                                                                                   max = as.Date(max(input1_fa$Entlassung)),
                                                                                                   weekstart = 1,language = "de",format="yyyy-mm-dd")})
                                                                
                                                            #}else{
                                                            #    output$FA_UI5<-renderUI({dateInput("FA_Ende","Ende des Beobachtungszeitraums",
                                                            #                                       value=input$FA_Start,
                                                            #                                       min = input$FA_Start,
                                                            #                                       max = input$FA_Start,
                                                            #                                       weekstart = 1,language = "de",format="yyyy-mm-dd")})
                                                            #}
                                                            output$FA_UI5b<-renderUI({h5(paste0("Bedingt durch die ausgewählte Fachabteilung können Zeiträume bis maximal ",
                                                                                                as.Date(max(input1_fa$Entlassung),format="%Y-%m-%d"),
                                                                                                " gewählt werden. Standard ist ein Zeitraum von 30 Tagen."))})
                                                            
                                                            output$ST_UI5<-renderUI({NULL})
                                                            output$ST_UI5b<-renderUI({NULL})
                                                        }
                                                    })
                                                }
                                            })
                                            }else{
                                                output$FA_UI3b<-renderUI({h5(paste0("Für die ausgewählte Fachabteilung gibt es mit dem von Ihnen definierten ICD-Filter 
                                                                 keine Fälle. Wählen Sie eine andere Fachabteilung und/oder ändern Sie Ihren ICD-Filter, um 
                                                                 einen Beobachtungszeitraum wählen zu können."))})
                                                output$FA_UI4<-renderUI({NULL})
                                                output$FA_UI4b<-renderUI({NULL})
                                                output$FA_UI5<-renderUI({NULL})
                                            }
                                        }else{
                                            output$FA_UI3<-renderUI({NULL})
                                            output$FA_UI3b<-renderUI({NULL})
                                            output$FA_UI4<-renderUI({NULL})
                                            output$FA_UI4b<-renderUI({NULL})
                                            output$FA_UI5<-renderUI({NULL})
                                            output$FA_UI5b<-renderUI({NULL})
                                        }
                                    })
                                }
                            }
                        }
                    })
                                
                }
            })
        }
      if(input$FA_oder_Station=="Fachabteilungen"&&is.null(input1a)){
          output$FA_UI2<-renderUI({NULL})
          output$FA_UI2a<-renderUI({NULL})
          output$FA_UI2b<-renderUI({NULL})
          output$FA_UI2c<-renderUI({NULL})
          output$FA_UI3<-renderUI({NULL})
          output$FA_UI4<-renderUI({NULL})
          output$FA_UI4b<-renderUI({NULL})
          output$FA_UI5<-renderUI({NULL})
          output$FA_UI5b<-renderUI({NULL})
          output$FA_intro<-renderText({"Kein Input File mit Fachabteilungsinformationen hochgeladen."})
          
          output$ST_UI2<-renderUI({NULL})
          output$ST_UI2a<-renderUI({NULL})
          output$ST_UI2b<-renderUI({NULL})
          output$ST_UI2c<-renderUI({NULL})
          output$ST_UI3<-renderUI({NULL})
          output$ST_UI4<-renderUI({NULL})
          output$ST_UI4b<-renderUI({NULL})
          output$ST_UI5<-renderUI({NULL})
          output$ST_UI5b<-renderUI({NULL})
          output$ST_intro<-renderText({NULL})
          
          output$FA_add1a<-renderText({NULL})
          output$FA_add2a<-renderText({NULL})
          output$FA_add3a<-renderText({NULL})
          output$FA_add4a<-renderUI({NULL})
          output$FA_add5a<-renderUI({NULL})
          output$FA_add7a<-renderUI({NULL})
          output$FA_add8a<-renderUI({NULL})
          output$FA_add9a<-renderUI({NULL})
          output$FA_add10a<-renderUI({NULL})
          output$FA_add11a<-renderUI({NULL})
        }
    })
    
    
    #####################pro Stationen
    observe({
        if(input$FA_oder_Station=="Stationen"&&!is.null(input1b)){
          output$FA_UI2<-renderUI({NULL})
          output$FA_UI2a<-renderUI({NULL})
          output$FA_UI2b<-renderUI({NULL})
          output$FA_UI2c<-renderUI({NULL})
          output$FA_UI3<-renderUI({NULL})
          output$FA_UI3b<-renderUI({NULL})
          output$FA_UI4<-renderUI({NULL})
          output$FA_UI4b<-renderUI({NULL})
          output$FA_UI5<-renderUI({NULL})
          output$FA_UI5b<-renderUI({NULL})
          output$FA_intro<-renderText({NULL})
          
          output$ST_UI2<-renderUI({NULL})
          output$ST_UI2a<-renderUI({NULL})
          output$ST_UI2b<-renderUI({NULL})
          output$ST_UI2c<-renderUI({NULL})
          output$ST_UI3<-renderUI({NULL})
          output$ST_UI3b<-renderUI({NULL})
          output$ST_UI4<-renderUI({NULL})
          output$ST_UI4b<-renderUI({NULL})
          output$ST_UI5<-renderUI({NULL})
          output$ST_UI5b<-renderUI({NULL})
          output$ST_intro<-renderText({NULL})
          
          output$FA_add1a<-renderText({"Auswahl Fall"})
          output$FA_add2a<-renderText({"Auswahl Beobachtungszeitraum"})
          output$FA_add3a<-renderText({"Kontaktanalyse"})
          output$FA_add4a<-renderUI({pickerInput("Min_Inkubation_Tag",label = HTML("Latenzzeit:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
                                                 choices = c("0 Tage","1 Tag",paste0(c(2:30)," Tage")),
                                                 options = list(title="Tage",size=5),inline = T,width = "fit")})
          output$FA_add5a<-renderUI({pickerInput("Min_Inkubation_Stunde",label = "",
                                                 choices = c("0 Stunden","1 Stunde",paste0(c(2:23)," Stunden")),
                                                 options = list(title="Stunden",size=5),inline = T,width = "fit")})
          output$FA_add6a<-renderUI({materialSwitch("Fall_Inkubation","Latenzzeit auch für Fall im Fokus?",value = FALSE,status = "primary")})
          output$FA_add7a<-renderUI({pickerInput("Min_Kontakt_Tag",label = HTML("Minimale Expositionszeit:&nbsp;"),
                                                 choices = c("0 Tage","1 Tag",paste0(c(2:30)," Tage")),
                                                 options = list(title="Tage",size=5),inline = T,width = "fit")})
          output$FA_add8a<-renderUI({pickerInput("Min_Kontakt_Stunde",label = "",
                                                 choices = c("0 Stunden","1 Stunde",paste0(c(2:23)," Stunden")),
                                                 options = list(title="Stunden",size=5),inline = T,width = "fit")})
          output$FA_add9a<-renderUI({pickerInput("Min_Ansteckend_Tag",label = HTML("Infektiöse Phase:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
                                                 choices = c("0 Tage","1 Tag",paste0(c(2:30)," Tage")),
                                                 options = list(title="Tage",size=5),inline = T,width = "fit")})
          output$FA_add10a<-renderUI({pickerInput("Min_Ansteckend_Stunde",label = "",
                                                  choices = c("0 Stunden","1 Stunde",paste0(c(2:23)," Stunden")),
                                                  options = list(title="Stunden",size=5),inline = T,width = "fit")})
          output$FA_add11a<-renderUI({sliderTextInput("Grad_Intern",label = "Maximale Länge der Infektionskette",
                                                      choices = c(1:10,"unbegrenzt"),selected = "1",grid=FALSE)})
          
            output$FA_erweitert_UI2<-renderUI({NULL})
            #input1<-readxl::read_excel("www/Verlegungshistorien_2023_2024-03-04_01.xlsx",sheet=4)
            input1<-input1b
            names(input1)[3]<-"Abteilung"
            input1<-input1[input1$Abteilung!="Patient_abwesend",]
            input1<-input1[order(input1$Fallnummer),]
            fa<-sort(unique(input1$Abteilung))
            
            output$ST_UI2<-renderUI({
                pickerInput('ST_Fokus',label = "Station im Fokus",choices = fa,selected=fa[1],options = list(`live-search` = TRUE))
            })
            output$FA_UI2<-renderUI({NULL})
            
            observe({
                if(!is.null(input$ST_Fokus)){
                    input1_fa<-input1[input1$Abteilung==input$ST_Fokus,]
                    
                    output$ST_UI2a<-renderUI({
                        prettyRadioButtons(inputId = "switch99c",label = "Hauptdiagnose nach ICD-Code filtern?",
                                           choices = c("nein","ICD-Kapitel","exakter ICD-Code"),selected = "nein",
                                           inline=T,outline = T,bigger = T,
                                           status = "primary",
                                           icon=icon("check")
                        )
                    })
                    
                    observe({
                        if(!is.null(input$switch99c)){
                            output$ST_UI3<-renderUI({NULL})
                            output$ST_UI3b<-renderUI({NULL})
                            output$ST_UI4<-renderUI({NULL})
                            output$ST_UI4b<-renderUI({NULL})
                            output$ST_UI5<-renderUI({NULL})
                            output$ST_UI5b<-renderUI({NULL})
                            
                            if(input$switch99c=="nein"){
                                output$ST_UI2b<-renderUI({NULL})
                                output$ST_UI2c<-renderUI({NULL})
                                output$ST_UI3b<-renderUI({NULL})
                    
                                fall<-sort(unique(input1_fa$Fallnummer))
                                output$ST_UI3<-renderUI({
                                    pickerInput('Fall_Fokus_ST',label = "Fall im Fokus",choices = fall,options = list(`live-search` = TRUE))
                                })
                                output$FA_UI3<-renderUI({NULL})
                                
                                observe({
                                    if(!is.null(input$Fall_Fokus_ST)){
                                        input1_fall<-as.data.frame(input1_fa[input1_fa$Fallnummer==input$Fall_Fokus_ST,])
                                        #input1_fall<-as.data.frame(input1_fa[input1_fa$Fallnummer==fall[1],])
                                        
                                        ausgeschlossen<-c()
                                        if(nrow(input1_fall)>1){
                                            for(i in 1:(nrow(input1_fall)-1)){
                                                anfang<-as.Date(input1_fall$Entlassung[i],format="%Y-%m-%d")+1
                                                ende<-as.Date(input1_fall$Aufnahme[i+1],format="%Y-%m-%d")-1
                                                if(anfang==ende){
                                                    ausgeschlossen<-c(as.Date(ausgeschlossen),anfang)
                                                }
                                                if(anfang<ende){
                                                    ausgeschlossen<-c(as.Date(ausgeschlossen),as.Date(anfang:ende))
                                                }
                                            }
                                            output$ST_UI4<-renderUI({dateInput("ST_Start","Beginn des Beobachtungszeitraums",
                                                                               value=as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                               min = as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                               max = as.Date(max(input1_fall$Entlassung),format="%Y-%m-%d"),
                                                                               weekstart = 1,language = "de",format="yyyy-mm-dd",
                                                                               datesdisabled = as.character(ausgeschlossen))})
                                            output$ST_UI4b<-renderUI({h5(paste0("Bedingt durch den ausgewählten Fall können als Beginn nur Tage zwischen ",
                                                                                as.Date(min(input1_fall$Aufnahme)),
                                                                                " und ",as.Date(max(input1_fall$Entlassung))," gewählt werden.\n",
                                                                                "Tage mit Abwesenheit des Falls auf der Station ",input$ST_Fokus,
                                                                                " stehen nicht zur Auswahl."))})
                                        }
                                        if(nrow(input1_fall)==1){              
                                            output$ST_UI4<-renderUI({dateInput("ST_Start","Beginn des Beobachtungszeitraums",
                                                                               value=as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                               min = as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                               max = as.Date(max(input1_fall$Entlassung),format="%Y-%m-%d"),
                                                                               weekstart = 1,language = "de",format="yyyy-mm-dd")})
                                            output$ST_UI4b<-renderUI({h5(paste0("Bedingt durch den ausgewählten Fall können als Beginn nur Tage zwischen ",
                                                                                as.Date(min(input1_fall$Aufnahme)),
                                                                                " und ",as.Date(max(input1_fall$Entlassung))," gewählt werden.\n",
                                                                                "Tage mit Abwesenheit des Falls auf der Station ",input$ST_Fokus,
                                                                                " stehen nicht zur Auswahl."))})
                                        }
                                        
                                        
                                        output$FA_UI4<-renderUI({NULL})
                                        output$FA_UI4b<-renderUI({NULL})
                                        
                                        observe({
                                            if(!is.null(input$ST_Start)){
                                                #if(input$ST_Start!=as.Date(max(input1_fa$Entlassung))){
                                                    output$ST_UI5<-renderUI({dateInput("ST_Ende","Ende des Beobachtungszeitraums",
                                                                                       value=input$ST_Start+30,
                                                                                       min = input$ST_Start,
                                                                                       max = as.Date(max(input1_fa$Entlassung)),
                                                                                       weekstart = 1,language = "de",format="yyyy-mm-dd")})                                    
                                                #}else{
                                                #    output$ST_UI5<-renderUI({dateInput("ST_Ende","Ende des Beobachtungszeitraums",
                                                #                                       value=input$ST_Start,
                                                #                                       min = input$ST_Start,
                                                #                                       max = input$ST_Start,
                                                #                                       weekstart = 1,language = "de",format="yyyy-mm-dd")})
                                                #}
                                                
                                                output$FA_UI5b<-renderUI({h5(paste0("Bedingt durch die ausgewählte Station können Zeiträume bis maximal ",
                                                                                    as.Date(max(input1_fa$Entlassung),format="%Y-%m-%d"),
                                                                                    " gewählt werden. Standard ist ein Zeitraum von 30 Tagen."))})
                                                
                                                output$FA_UI5<-renderUI({NULL})
                                                output$FA_UI5b<-renderUI({NULL})
                                            }
                                        })
                                    }
                                })
                            }else{
                                if(input$switch99c=="ICD-Kapitel"){
                                    output$ST_UI2b<-renderUI({
                                        pickerInput('ST_ICDKapitel',
                                                    choices = c(1:22), 
                                                    choicesOpt = list(
                                                        content = c(HTML('<b>I</b>: A00-B99'),
                                                                    HTML('<b>II</b>: C00-D48'),
                                                                    HTML('<b>III</b>: D50-D90'),
                                                                    HTML('<b>IV</b>: E00-E90'),
                                                                    HTML('<b>V</b>: F00-F99'),
                                                                    HTML('<b>VI</b>: G00-G99'),
                                                                    HTML('<b>VII</b>: H00-H59'),
                                                                    HTML('<b>VIII</b>: H60-H95'),
                                                                    HTML('<b>IX</b>: I00-I99'),
                                                                    HTML('<b>X</b>: J00-J99'),
                                                                    HTML('<b>XI</b>: K00-K93'),
                                                                    HTML('<b>XII</b>: L00-L99'),
                                                                    HTML('<b>XIII</b>: M00-M99'),
                                                                    HTML('<b>XIV</b>: N00-N99'),
                                                                    HTML('<b>XV</b>: O00-O99'),
                                                                    HTML('<b>XVI</b>: P00-P96'),
                                                                    HTML('<b>XVII</b>: Q00-Q99'),
                                                                    HTML('<b>XVIII</b>: R00-R99'),
                                                                    HTML('<b>XIX</b>: S00-T98'),
                                                                    HTML('<b>XX</b>: V01-Y84'),
                                                                    HTML('<b>XXI</b>: Z00-Z99'),
                                                                    HTML('<b>XXII</b>: U00-U99'))
                                                    ),
                                                    options = list(`actions-box` = TRUE,
                                                                   `deselect-all-text` = "Auswahl aufheben",
                                                                   `select-all-text` = "Alle auswählen",
                                                                   `none-selected-text` = "Nichts ausgewählt",
                                                                   sanitize=FALSE),multiple=TRUE)
                                    })
                                    output$ST_UI2c<-renderUI({NULL})
                                    output$ST_UI3b<-renderUI({NULL})
                                    
                                    observe({
                                        if(!is.null(input$ST_ICDKapitel)){
                                            input1_fa<-input1_fa[input1_fa$ICDKapitel%in%input$ST_ICDKapitel,]
                                            output$ST_UI3b<-renderUI({NULL})
                                            if(nrow(input1_fa)>0){
                                                fall<-sort(unique(input1_fa$Fallnummer))
                                                output$ST_UI3<-renderUI({
                                                    pickerInput('Fall_Fokus_ST',label = "Fall im Fokus",choices = fall,options = list(`live-search` = TRUE))
                                                })
                                                output$FA_UI3<-renderUI({NULL})
                                                
                                                observe({
                                                    if(!is.null(input$Fall_Fokus_ST)){
                                                        input1_fall<-as.data.frame(input1_fa[input1_fa$Fallnummer==input$Fall_Fokus_ST,])
                                                        #input1_fall<-as.data.frame(input1_fa[input1_fa$Fallnummer==fall[1],])
                                                        
                                                        ausgeschlossen<-c()
                                                        if(nrow(input1_fall)>1){
                                                            for(i in 1:(nrow(input1_fall)-1)){
                                                                anfang<-as.Date(input1_fall$Entlassung[i],format="%Y-%m-%d")+1
                                                                ende<-as.Date(input1_fall$Aufnahme[i+1],format="%Y-%m-%d")-1
                                                                if(anfang==ende){
                                                                    ausgeschlossen<-c(as.Date(ausgeschlossen),anfang)
                                                                }
                                                                if(anfang<ende){
                                                                    ausgeschlossen<-c(as.Date(ausgeschlossen),as.Date(anfang:ende))
                                                                }
                                                            }
                                                            output$ST_UI4<-renderUI({dateInput("ST_Start","Beginn des Beobachtungszeitraums",
                                                                                               value=as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                                               min = as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                                               max = as.Date(max(input1_fall$Entlassung),format="%Y-%m-%d"),
                                                                                               weekstart = 1,language = "de",format="yyyy-mm-dd",
                                                                                               datesdisabled = as.character(ausgeschlossen))})
                                                            output$ST_UI4b<-renderUI({h5(paste0("Bedingt durch den ausgewählten Fall können als Beginn nur Tage zwischen ",
                                                                                                as.Date(min(input1_fall$Aufnahme)),
                                                                                                " und ",as.Date(max(input1_fall$Entlassung))," gewählt werden.\n",
                                                                                                "Tage mit Abwesenheit des Falls auf der Station ",input$ST_Fokus,
                                                                                                " stehen nicht zur Auswahl."))})
                                                        }
                                                        if(nrow(input1_fall)==1){              
                                                            output$ST_UI4<-renderUI({dateInput("ST_Start","Beginn des Beobachtungszeitraums",
                                                                                               value=as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                                               min = as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                                               max = as.Date(max(input1_fall$Entlassung),format="%Y-%m-%d"),
                                                                                               weekstart = 1,language = "de",format="yyyy-mm-dd")})
                                                            output$ST_UI4b<-renderUI({h5(paste0("Bedingt durch den ausgewählten Fall können als Beginn nur Tage zwischen ",
                                                                                                as.Date(min(input1_fall$Aufnahme)),
                                                                                                " und ",as.Date(max(input1_fall$Entlassung))," gewählt werden.\n",
                                                                                                "Tage mit Abwesenheit des Falls auf der Station ",input$ST_Fokus,
                                                                                                " stehen nicht zur Auswahl."))})
                                                        }
                                                        
                                                        
                                                        output$FA_UI4<-renderUI({NULL})
                                                        output$FA_UI4b<-renderUI({NULL})
                                                        
                                                        observe({
                                                            if(!is.null(input$ST_Start)){
                                                                #if(input$ST_Start!=as.Date(max(input1_fa$Entlassung))){
                                                                    output$ST_UI5<-renderUI({dateInput("ST_Ende","Ende des Beobachtungszeitraums",
                                                                                                       value=min(as.Date(max(input1_fa$Entlassung)),(input$ST_Start+30)),
                                                                                                       min = input$ST_Start,
                                                                                                       max = as.Date(max(input1_fa$Entlassung)),
                                                                                                       weekstart = 1,language = "de",format="yyyy-mm-dd")})                                    
                                                                #}else{
                                                                #    output$ST_UI5<-renderUI({dateInput("ST_Ende","Ende des Beobachtungszeitraums",
                                                                #                                       value=input$ST_Start,
                                                                #                                       min = input$ST_Start,
                                                                #                                       max = input$ST_Start,
                                                                #                                       weekstart = 1,language = "de",format="yyyy-mm-dd")})
                                                                #}
                                                                
                                                                output$FA_UI5b<-renderUI({h5(paste0("Bedingt durch die ausgewählte Station können Zeiträume bis maximal ",
                                                                                                    as.Date(max(input1_fa$Entlassung),format="%Y-%m-%d"),
                                                                                                    " gewählt werden. Standard ist ein Zeitraum von 30 Tagen."))})
                                                                
                                                                output$FA_UI5<-renderUI({NULL})
                                                                output$FA_UI5b<-renderUI({NULL})
                                                            }
                                                        })
                                                    }
                                                })
                                            }else{
                                                output$ST_UI3b<-renderUI({h5(paste0("Für die ausgewählte Station gibt es mit dem von Ihnen definierten ICD-Filter 
                                                                 keine Fälle. Wählen Sie eine andere Station und/oder ändern Sie Ihren ICD-Filter, um 
                                                                 einen Beobachtungszeitraum wählen zu können."))})
                                                output$ST_UI4<-renderUI({NULL})
                                                output$ST_UI4b<-renderUI({NULL})
                                                output$ST_UI5<-renderUI({NULL})
                                            }
                                        }else{
                                            output$ST_UI3<-renderUI({NULL})
                                            output$ST_UI3b<-renderUI({NULL})
                                            output$ST_UI4<-renderUI({NULL})
                                            output$ST_UI4b<-renderUI({NULL})
                                            output$ST_UI5<-renderUI({NULL})
                                            output$ST_UI5b<-renderUI({NULL})
                                        }
                                    })
                                }else{
                                    output$ST_UI2c<-renderUI({
                                        textInput("ST_ICDExakt",value = NULL,placeholder = "z.B. A00 oder A00.0",label = NULL)
                                    })
                                    output$ST_UI2b<-renderUI({NULL})
                                    output$ST_UI3b<-renderUI({NULL})
                                    
                                    observe({
                                        if(!is.null(input$ST_ICDExakt)&&input$ST_ICDExakt!=""&&nchar(input$ST_ICDExakt)>=3){
                                            output$ST_UI3b<-renderUI({NULL})
                                            
                                            user_icd<-gsub(".","\\.",input$ST_ICDExakt,fixed=T)
                                            input1_fa<-input1_fa[grep(paste0("^",user_icd),input1_fa$ICDfull),]
                                            
                                            if(nrow(input1_fa)>0){
                                                fall<-sort(unique(input1_fa$Fallnummer))
                                                output$ST_UI3<-renderUI({
                                                    pickerInput('Fall_Fokus_ST',label = "Fall im Fokus",choices = fall,options = list(`live-search` = TRUE))
                                                })
                                                output$FA_UI3<-renderUI({NULL})
                                                
                                                observe({
                                                    if(!is.null(input$Fall_Fokus_ST)){
                                                        input1_fall<-as.data.frame(input1_fa[input1_fa$Fallnummer==input$Fall_Fokus_ST,])
                                                        #input1_fall<-as.data.frame(input1_fa[input1_fa$Fallnummer==fall[1],])
                                                        
                                                        ausgeschlossen<-c()
                                                        if(nrow(input1_fall)>1){
                                                            for(i in 1:(nrow(input1_fall)-1)){
                                                                anfang<-as.Date(input1_fall$Entlassung[i],format="%Y-%m-%d")+1
                                                                ende<-as.Date(input1_fall$Aufnahme[i+1],format="%Y-%m-%d")-1
                                                                if(anfang==ende){
                                                                    ausgeschlossen<-c(as.Date(ausgeschlossen),anfang)
                                                                }
                                                                if(anfang<ende){
                                                                    ausgeschlossen<-c(as.Date(ausgeschlossen),as.Date(anfang:ende))
                                                                }
                                                            }
                                                            output$ST_UI4<-renderUI({dateInput("ST_Start","Beginn des Beobachtungszeitraums",
                                                                                               value=as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                                               min = as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                                               max = as.Date(max(input1_fall$Entlassung),format="%Y-%m-%d"),
                                                                                               weekstart = 1,language = "de",format="yyyy-mm-dd",
                                                                                               datesdisabled = as.character(ausgeschlossen))})
                                                            output$ST_UI4b<-renderUI({h5(paste0("Bedingt durch den ausgewählten Fall können als Beginn nur Tage zwischen ",
                                                                                                as.Date(min(input1_fall$Aufnahme)),
                                                                                                " und ",as.Date(max(input1_fall$Entlassung))," gewählt werden.\n",
                                                                                                "Tage mit Abwesenheit des Falls auf der Station ",input$ST_Fokus,
                                                                                                " stehen nicht zur Auswahl."))})
                                                        }
                                                        if(nrow(input1_fall)==1){              
                                                            output$ST_UI4<-renderUI({dateInput("ST_Start","Beginn des Beobachtungszeitraums",
                                                                                               value=as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                                               min = as.Date(min(input1_fall$Aufnahme),format="%Y-%m-%d"),
                                                                                               max = as.Date(max(input1_fall$Entlassung),format="%Y-%m-%d"),
                                                                                               weekstart = 1,language = "de",format="yyyy-mm-dd")})
                                                            output$ST_UI4b<-renderUI({h5(paste0("Bedingt durch den ausgewählten Fall können als Beginn nur Tage zwischen ",
                                                                                                as.Date(min(input1_fall$Aufnahme)),
                                                                                                " und ",as.Date(max(input1_fall$Entlassung))," gewählt werden.\n",
                                                                                                "Tage mit Abwesenheit des Falls auf der Station ",input$ST_Fokus,
                                                                                                " stehen nicht zur Auswahl."))})
                                                        }
                                                        
                                                        
                                                        output$FA_UI4<-renderUI({NULL})
                                                        output$FA_UI4b<-renderUI({NULL})
                                                        
                                                        observe({
                                                            if(!is.null(input$ST_Start)){
                                                                #if(input$ST_Start!=as.Date(max(input1_fa$Entlassung))){
                                                                    output$ST_UI5<-renderUI({dateInput("ST_Ende","Ende des Beobachtungszeitraums",
                                                                                                       value=min(as.Date(max(input1_fa$Entlassung)),(input$ST_Start+30)),
                                                                                                       min = input$ST_Start,
                                                                                                       max = as.Date(max(input1_fa$Entlassung)),
                                                                                                       weekstart = 1,language = "de",format="yyyy-mm-dd")})                                    
                                                                #}else{
                                                                #    output$ST_UI5<-renderUI({dateInput("ST_Ende","Ende des Beobachtungszeitraums",
                                                                #                                       value=input$ST_Start,
                                                                #                                       min = input$ST_Start,
                                                                #                                       max = input$ST_Start,
                                                                #                                       weekstart = 1,language = "de",format="yyyy-mm-dd")})
                                                                #}
                                                                
                                                                output$FA_UI5b<-renderUI({h5(paste0("Bedingt durch die ausgewählte Station können Zeiträume bis maximal ",
                                                                                                    as.Date(max(input1_fa$Entlassung),format="%Y-%m-%d"),
                                                                                                    " gewählt werden. Standard ist ein Zeitraum von 30 Tagen."))})
                                                                
                                                                output$FA_UI5<-renderUI({NULL})
                                                                output$FA_UI5b<-renderUI({NULL})
                                                            }
                                                        })
                                                    }
                                                })
                                            }else{
                                                output$ST_UI3b<-renderUI({h5(paste0("Für die ausgewählte Station gibt es mit dem von Ihnen definierten ICD-Filter 
                                                                 keine Fälle. Wählen Sie eine andere Station und/oder ändern Sie Ihren ICD-Filter, um 
                                                                 einen Beobachtungszeitraum wählen zu können."))})
                                                output$ST_UI4<-renderUI({NULL})
                                                output$ST_UI4b<-renderUI({NULL})
                                                output$ST_UI5<-renderUI({NULL})
                                            }
                                        }else{
                                            #output$ST_UI2<-renderUI({NULL})
                                            output$ST_UI3<-renderUI({NULL})
                                            output$ST_UI3b<-renderUI({NULL})
                                            output$ST_UI4<-renderUI({NULL})
                                            output$ST_UI4b<-renderUI({NULL})
                                            output$ST_UI5<-renderUI({NULL})
                                            output$ST_UI5b<-renderUI({NULL})
                                        }
                                    })
                                } 
                            }
                        }
                    })
                }
            })
        }
      if(input$FA_oder_Station=="Stationen"&&is.null(input1b)){
          output$FA_UI2<-renderUI({NULL})
          output$FA_UI2a<-renderUI({NULL})
          output$FA_UI2b<-renderUI({NULL})
          output$FA_UI2c<-renderUI({NULL})
          output$FA_UI3<-renderUI({NULL})
          output$FA_UI3b<-renderUI({NULL})
          output$FA_UI4<-renderUI({NULL})
          output$FA_UI4b<-renderUI({NULL})
          output$FA_UI5<-renderUI({NULL})
          output$FA_UI5b<-renderUI({NULL})
          output$FA_intro<-renderText({NULL})
          
          output$ST_UI2<-renderUI({NULL})
          output$ST_UI2a<-renderUI({NULL})
          output$ST_UI2b<-renderUI({NULL})
          output$ST_UI2c<-renderUI({NULL})
          output$ST_UI3<-renderUI({NULL})
          output$ST_UI3b<-renderUI({NULL})
          output$ST_UI4<-renderUI({NULL})
          output$ST_UI4b<-renderUI({NULL})
          output$ST_UI5<-renderUI({NULL})
          output$ST_UI5b<-renderUI({NULL})
          output$ST_intro<-renderText({"Kein Input File mit Stationsinformationen hochgeladen."})
          
          output$FA_add1a<-renderText({NULL})
          output$FA_add2a<-renderText({NULL})
          output$FA_add3a<-renderText({NULL})
          output$FA_add4a<-renderUI({NULL})
          output$FA_add5a<-renderUI({NULL})
          output$FA_add7a<-renderUI({NULL})
          output$FA_add8a<-renderUI({NULL})
          output$FA_add9a<-renderUI({NULL})
          output$FA_add10a<-renderUI({NULL})
          output$FA_add11a<-renderUI({NULL})
        }
    })
    
    observe({
        if((!is.null(input$FA_Ende)||!is.null(input$ST_Ende))&&(!is.null(input$Fall_Fokus_FA)||!is.null(input$Fall_Fokus_ST))&&
            (input$Min_Inkubation_Tag!=""||input$Min_Inkubation_Stunde!="")&&
            (input$Min_Kontakt_Tag!=""||input$Min_Kontakt_Stunde!="")&&
            (input$Min_Ansteckend_Tag!=""||input$Min_Ansteckend_Stunde!="")){
          if(input$FA_oder_Station=="Fachabteilungen"&&!is.null(input1a)){
            shinyjs::enable("do_eigeneFA")
          }
          if(input$FA_oder_Station=="Fachabteilungen"&&is.null(input1a)){
            shinyjs::disable("do_eigeneFA")
          }
          if(input$FA_oder_Station=="Stationen"&&!is.null(input1b)){
            shinyjs::enable("do_eigeneFA")
          }
          if(input$FA_oder_Station=="Stationen"&&is.null(input1b)){
            shinyjs::disable("do_eigeneFA")
          }
                
        }else{
            shinyjs::disable("do_eigeneFA")
        }
    })
    
    button_do_erweiterteFA <- reactiveVal(FALSE)
    button_do_erweiterteFA2 <- reactiveVal(FALSE)
    
    rv<-reactiveValues(nodes1=NULL,links1=NULL,backup_all=NULL,fall_ist=NULL,fa_ist=NULL,FA_Start=NULL,FA_Ende=NULL)
    
    ##1. Analyse: Fokus auf FA im Fokus
    ##Ketten ausgehend vom Fall im Fokus
    observeEvent(input$do_eigeneFA,{
      updateTabsetPanel(session,"main",
                        selected="Ergebnisse")

        output$force<-renderForceNetwork({NULL})
        output$FA_erweitert_UI2<-renderUI({NULL})
        output$FA_erweitert_UI3<-renderUI({NULL})
        
        if(input$FA_oder_Station=="Fachabteilungen"){      
          shinyjs::html("text", paste0("<br>Analysiere Kontakte für Fachabteilung im Fokus.<br><br>"), add = FALSE)
          
            input1<-input1a
            input1<-input1[input1$Abteilung!="Patient_abwesend",]
            input1<-input1[order(input1$Fallnummer),]
            fa<-sort(unique(input1$Abteilung))
            
            input1_fa<-input1[input1$Abteilung==input$FA_Fokus,]
            
            FA_ICD<-23
            
            if(input$switch99=="ICD-Kapitel"){
                input1_fa<-input1_fa[input1_fa$ICDKapitel%in%input$FA_ICDKapitel,]
                FA_ICD<-input$FA_ICDKapitel
            }
            if(input$switch99=="exakter ICD-Code"){
                user_icd<-gsub(".","\\.",input$FA_ICDExakt,fixed=T)
                input1_fa<-input1_fa[grep(paste0("^",user_icd),input1_fa$ICDfull),]
                FA_ICD<-input$FA_ICDExakt
            }
            
            fall<-sort(unique(input1_fa$Fallnummer))
            
            if(!is.null(input$Fall_Fokus_FA)){
                input1_fall<-as.data.frame(input1_fa[input1_fa$Fallnummer==input$Fall_Fokus_FA,])
                #input1_fall<-as.data.frame(input1_fa[input1_fa$Fallnummer==fall[1],])
                
                ausgeschlossen<-c()
                if(nrow(input1_fall)>1){
                    for(i in 1:(nrow(input1_fall)-1)){
                        anfang<-as.Date(input1_fall$Entlassung[i],format="%Y-%m-%d")+1
                        ende<-as.Date(input1_fall$Aufnahme[i+1],format="%Y-%m-%d")-1
                        if(anfang==ende){
                            ausgeschlossen<-c(as.Date(ausgeschlossen),anfang)
                        }
                        if(anfang<ende){
                            ausgeschlossen<-c(as.Date(ausgeschlossen),as.Date(anfang:ende))
                        }
                    }
                }
            }
        }
        
        if(input$FA_oder_Station=="Stationen"){
          shinyjs::html("text", paste0("<br>Analysiere Kontakte für Station im Fokus.<br><br>"), add = FALSE)
          
            input1<-input1b
            names(input1)[3]<-"Abteilung"
            input1<-input1[input1$Abteilung!="Patient_abwesend",]
            input1<-input1[order(input1$Fallnummer),]
            fa<-sort(unique(input1$Abteilung))
            
            input1_fa<-input1[input1$Abteilung==input$ST_Fokus,]
            
            FA_ICD<-23
            
            if(input$switch99c=="ICD-Kapitel"){
                input1_fa<-input1_fa[input1_fa$ICDKapitel%in%input$ST_ICDKapitel,]
                FA_ICD<-input$ST_ICDKapitel
            }
            if(input$switch99c=="exakter ICD-Code"){
                user_icd<-gsub(".","\\.",input$ST_ICDExakt,fixed=T)
                input1_fa<-input1_fa[grep(paste0("^",user_icd),input1_fa$ICDfull),] 
                FA_ICD<-input$ST_ICDExakt
            }
            
            fall<-sort(unique(input1_fa$Fallnummer))
            
            input1_fall<-as.data.frame(input1_fa[input1_fa$Fallnummer==input$Fall_Fokus_ST,])
            #input1_fall<-as.data.frame(input1_fa[input1_fa$Fallnummer==fall[1],])
            
            ausgeschlossen<-c()
            if(nrow(input1_fall)>1){
                for(i in 1:(nrow(input1_fall)-1)){
                    anfang<-as.Date(input1_fall$Entlassung[i],format="%Y-%m-%d")+1
                    ende<-as.Date(input1_fall$Aufnahme[i+1],format="%Y-%m-%d")-1
                    if(anfang==ende){
                        ausgeschlossen<-c(as.Date(ausgeschlossen),anfang)
                    }
                    if(anfang<ende){
                        ausgeschlossen<-c(as.Date(ausgeschlossen),as.Date(anfang:ende))
                    }
                }
            }
        }
        
        FA_Fokus<-input$FA_Fokus
        Fall_Fokus_FA<-input$Fall_Fokus_FA
        ST_Fokus<-input$ST_Fokus
        Fall_Fokus_ST<-input$Fall_Fokus_ST

        if(input$FA_oder_Station=="Fachabteilungen"){
            FA_Start<-input$FA_Start
            FA_Ende<-input$FA_Ende
            fall_ist<-Fall_Fokus_FA
            fa_ist<-FA_Fokus
            ST_Fokus<-NULL
            Fall_Fokus_ST<-NULL
        }else{
            FA_Start<-input$ST_Start
            FA_Ende<-input$ST_Ende
            fall_ist<-Fall_Fokus_ST
            fa_ist<-ST_Fokus
            FA_Fokus<-NULL
            Fall_Fokus_FA<-NULL
        }
        
        if(sum(as.numeric(FA_Start)==as.numeric(as.Date(input1_fall$Aufnahme,format="%Y-%m-%d")))>=1){
            FA_Start<-min(input1_fall$Aufnahme[which(as.numeric(FA_Start)==as.numeric(as.Date(input1_fall$Aufnahme,format="%Y-%m-%d")))])
        }else{
            FA_Start<-as.POSIXct(paste0(FA_Start," 00:00:01"),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
        }
        FA_Ende<-as.POSIXct(paste0(FA_Ende," 23:59:59"),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
        
        if(input$Min_Inkubation_Tag==""){
            min_inkubation_tag<-0
        }else{
            min_inkubation_tag<-as.numeric(strsplit(input$Min_Inkubation_Tag," ")[[1]][1])
        }
        if(input$Min_Inkubation_Stunde==""){
            min_inkubation_stunde<-0
        }else{
            min_inkubation_stunde<-as.numeric(strsplit(input$Min_Inkubation_Stunde," ")[[1]][1])
        }
        inkubationszeit<-min_inkubation_tag+min_inkubation_stunde/24
        
        if(input$Min_Kontakt_Tag==""){
            min_kontakt_tag<-0
        }else{
            min_kontakt_tag<-as.numeric(strsplit(input$Min_Kontakt_Tag," ")[[1]][1])
        }
        if(input$Min_Kontakt_Stunde==""){
            min_kontakt_stunde<-0
        }else{
            min_kontakt_stunde<-as.numeric(strsplit(input$Min_Kontakt_Stunde," ")[[1]][1])
        }
        kontaktzeit<-min_kontakt_tag+min_kontakt_stunde/24
        
        if(input$Min_Ansteckend_Tag==""){
            min_ansteckend_tag<-0
        }else{
            min_ansteckend_tag<-as.numeric(strsplit(input$Min_Ansteckend_Tag," ")[[1]][1])
        }
        if(input$Min_Ansteckend_Stunde==""){
            min_ansteckend_stunde<-0
        }else{
            min_ansteckend_stunde<-as.numeric(strsplit(input$Min_Ansteckend_Stunde," ")[[1]][1])
        }
        ansteckendzeit<-min_ansteckend_tag+min_ansteckend_stunde/24
        
        ##Filter Zeitraum
        if(inkubationszeit==0){
            output$text_analyse1<-renderUI({"Bitte wählen Sie eine minimale Latenzzeit."})
            output$text_analyse2<-renderUI({NULL})
            #output$plotBasic<-renderPlot({NULL})
            output$force<-renderForceNetwork({NULL})
            return()
        }
        if(kontaktzeit==0){
            output$text_analyse1<-renderUI({"Bitte wählen Sie eine minimale Expositionszeit."})
            output$text_analyse2<-renderUI({NULL})
            #output$plotBasic<-renderPlot({NULL})
            output$force<-renderForceNetwork({NULL})
            return()
        }
        if(ansteckendzeit==0){
            output$text_analyse1<-renderUI({"Bitte wählen Sie eine Zeitangabe für die infektiöse Phase."})
            output$text_analyse2<-renderUI({NULL})
            #output$plotBasic<-renderPlot({NULL})
            output$force<-renderForceNetwork({NULL})
            return()
        }
        if((FA_Ende-FA_Start)<(kontaktzeit+inkubationszeit)){
            output$text_analyse1<-renderUI(HTML("Der gewählte Beobachtungszeitraum ist kürzer als die Latenzzeit + minimale Expositionszeit.<br>
        Bitte wählen Sie einen längeren Beobachtungszeitraum, eine kürzere Latenzzeit oder eine kürzere minimale Expositionszeit, damit eine Ansteckung möglich ist."))
            output$text_analyse2<-renderUI({NULL})
            #output$plotBasic<-renderPlot({NULL})
            output$force<-renderForceNetwork({NULL})
            return()
        }
        
        ##Für Fall bestimmen, wann er da ist und wann er davon ansteckend ist
        input1_fall_filtered<-input1_fall[as.numeric(input1_fall$Aufnahme)<=as.numeric(FA_Ende)
                                          &as.numeric(input1_fall$Entlassung)>=as.numeric(FA_Start),]
        if(input$Fall_Inkubation==F){
            input1_fall_filtered$Aufnahme[1]<-max(input1_fall_filtered$Aufnahme[1],FA_Start)
        }else{
            input1_fall_filtered$Aufnahme[1]<-max(as.POSIXct(as.numeric(input1_fall_filtered$Aufnahme[1])+inkubationszeit*60*60*24,tz="UTC"),FA_Start)
        }
        ##sobald nicht mehr ansteckend, sind alle weiteren aufenthalte + kontakte auf der FA egal -> filtern
        input1_fall_filtered<-as.data.frame(input1_fall_filtered[input1_fall_filtered$Aufnahme<=as.POSIXct(as.numeric(input1_fall_filtered$Aufnahme[1])+ansteckendzeit*60*60*24,tz="UTC"),])
        for(zeile in nrow(input1_fall_filtered)){
            input1_fall_filtered$Entlassung[zeile]<-min(input1_fall_filtered$Entlassung[zeile],
                                                        as.POSIXct(as.numeric(input1_fall_filtered$Aufnahme[zeile])+ansteckendzeit*60*60*24,tz="UTC"))
        }
        input1_fall_filtered$Dauer<-(as.numeric(input1_fall_filtered$Entlassung)-as.numeric(input1_fall_filtered$Aufnahme))/(60*60*24)
        
        if(input$Fall_Inkubation==F){
            ##Fall im Fokus ist direkt ansteckend -> Latenzzeit egal
            if(max(input1_fall_filtered$Dauer)<kontaktzeit&&input$FA_oder_Station=="Fachabteilungen"){
                fall_tage<-floor(max(input1_fall_filtered$Dauer))
                fall_stunden<-(max(input1_fall_filtered$Dauer)-floor(max(input1_fall_filtered$Dauer)))*24
                if(fall_tage==1){
                    output$text_analyse1<-renderUI(HTML(paste0("In dem betrachteten Beobachtungszeitraum beträgt der längste Aufenthalt
          von Fall ",Fall_Fokus_FA," auf Fachabteilung ",FA_Fokus," ",fall_tage," Tag und ",round(fall_stunden,2)," Stunden. <br>
      Die gewählte minimale Expositionszeit beträgt jedoch ",min_kontakt_tag, " Tage und ",min_kontakt_stunde," Stunden. <br>
        Bitte wählen Sie eine kürzere minimale Expositionszeit.")))
                    output$text_analyse2<-renderUI({NULL})
                    output$force<-renderForceNetwork({NULL})
                    return()
                    
                }else{
                    output$text_analyse1<-renderUI(HTML(paste0("In dem betrachteten Beobachtungszeitraum beträgt der längste Aufenthalt
          von Fall ",Fall_Fokus_FA," auf Fachabteilung ",FA_Fokus," ",fall_tage," Tage und ",round(fall_stunden,2)," Stunden. <br>
      Die gewählte minimale Expositionszeit beträgt jedoch ",min_kontakt_tag, " Tage und ",min_kontakt_stunde," Stunden. <br>
        Bitte wählen Sie eine kürzere minimale Expositionszeit.")))
                    output$text_analyse2<-renderUI({NULL})
                    output$force<-renderForceNetwork({NULL})
                    return()
                }
            }
            
            if(max(input1_fall_filtered$Dauer)<kontaktzeit&&input$FA_oder_Station=="Stationen"){
                fall_tage<-floor(max(input1_fall_filtered$Dauer))
                fall_stunden<-(max(input1_fall_filtered$Dauer)-floor(max(input1_fall_filtered$Dauer)))*24
                if(fall_tage==1){
                    output$text_analyse1<-renderUI(HTML(paste0("In dem betrachteten Beobachtungszeitraum beträgt der längste Aufenthalt von Fall ",
                                                               Fall_Fokus_ST," auf Station ",ST_Fokus," ",fall_tage," Tag und ",round(fall_stunden,2)," Stunden. <br>
      Die gewählte minimale Expositionszeit beträgt jedoch ",min_kontakt_tag, " Tage und ",min_kontakt_stunde," Stunden. <br>
        Bitte wählen Sie eine kürzere minimale Expositionszeit.")))
                    output$text_analyse2<-renderUI({NULL})
                    output$force<-renderForceNetwork({NULL})
                    return()
                }else{
                    output$text_analyse1<-renderUI(HTML(paste0("In dem betrachteten Beobachtungszeitraum beträgt der längste Aufenthalt von Fall ",
                                                               Fall_Fokus_ST," auf Station ",ST_Fokus," ",fall_tage," Tage und ",round(fall_stunden,2)," Stunden. <br>
      Die gewählte minimale Expositionszeit beträgt jedoch ",min_kontakt_tag, " Tage und ",min_kontakt_stunde," Stunden. <br>
        Bitte wählen Sie eine kürzere minimale Expositionszeit.")))
                    output$text_analyse2<-renderUI({NULL})
                    output$force<-renderForceNetwork({NULL})
                    return()
                }
            }
        }else{
            ##Fall im Fokus muss erst noch ansteckend werden -> Latenzzeit draufrechnen
            if(max(input1_fall_filtered$Dauer)<(kontaktzeit)&&input$FA_oder_Station=="Fachabteilungen"){
                fall_tage<-floor(max(input1_fall_filtered$Dauer))
                fall_stunden<-(max(input1_fall_filtered$Dauer)-floor(max(input1_fall_filtered$Dauer)))*24
                if(fall_tage==1){
                    output$text_analyse1<-renderUI(HTML(paste0("In dem betrachteten Beobachtungszeitraum beträgt der längste Aufenthalt
          von Fall ",Fall_Fokus_FA," auf Fachabteilung ",FA_Fokus," ",fall_tage," Tag und ",round(fall_stunden,2)," Stunden. <br>
      Die gewählte Latenzzeit beträgt jedoch ",min_inkubation_tag," Tage und ",min_inkubation_stunde," Stunden, ",
      "die minimale Expositionszeit ",min_kontakt_tag, " Tage und ",min_kontakt_stunde," Stunden. <br>
        Bitte wählen Sie eine kürzere Latenzzeit oder eine kürzere minimale Expositionszeit.")))
                    output$text_analyse2<-renderUI({NULL})
                    output$force<-renderForceNetwork({NULL})
                    return()
                    
                }else{
                    output$text_analyse1<-renderUI(HTML(paste0("In dem betrachteten Beobachtungszeitraum beträgt der längste Aufenthalt
          von Fall ",Fall_Fokus_FA," auf Fachabteilung ",FA_Fokus," ",fall_tage," Tage und ",round(fall_stunden,2)," Stunden. <br>
      Die gewählte Latenzzeit beträgt jedoch ",min_inkubation_tag," Tage und ",min_inkubation_stunde," Stunden, ",
      "die minimale Expositionszeit ",min_kontakt_tag, " Tage und ",min_kontakt_stunde," Stunden. <br>
        Bitte wählen Sie eine kürzere Latenzzeit oder eine kürzere minimale Expositionszeit.")))
                    output$text_analyse2<-renderUI({NULL})
                    output$force<-renderForceNetwork({NULL})
                    return()
                }
            }
            
            if(max(input1_fall_filtered$Dauer)<kontaktzeit&&input$FA_oder_Station=="Stationen"){
                fall_tage<-floor(max(input1_fall_filtered$Dauer))
                fall_stunden<-(max(input1_fall_filtered$Dauer)-floor(max(input1_fall_filtered$Dauer)))*24
                if(fall_tage==1){
                    output$text_analyse1<-renderUI(HTML(paste0("In dem betrachteten Beobachtungszeitraum beträgt der längste Aufenthalt von Fall ",
                                                               Fall_Fokus_ST," auf Station ",ST_Fokus," ",fall_tage," Tag und ",round(fall_stunden,2)," Stunden. <br>
      Die gewählte Latenzzeit beträgt jedoch ",min_inkubation_tag," Tage und ",min_inkubation_stunde," Stunden, ",
      "die minimale Expositionszeit ",min_kontakt_tag, " Tage und ",min_kontakt_stunde," Stunden. <br>
        Bitte wählen Sie eine kürzere Latenzzeit oder eine kürzere minimale Expositionszeit.")))
                    output$text_analyse2<-renderUI({NULL})
                    output$force<-renderForceNetwork({NULL})
                    return()
                }else{
                    output$text_analyse1<-renderUI(HTML(paste0("In dem betrachteten Beobachtungszeitraum beträgt der längste Aufenthalt von Fall ",
                                                               Fall_Fokus_ST," auf Station ",ST_Fokus," ",fall_tage," Tage und ",round(fall_stunden,2)," Stunden. <br>
      Die gewählte Latenzzeit beträgt jedoch ",min_inkubation_tag," Tage und ",min_inkubation_stunde," Stunden, ",
      "die minimale Expositionszeit ",min_kontakt_tag, " Tage und ",min_kontakt_stunde," Stunden. <br>
        Bitte wählen Sie eine kürzere Latenzzeit oder eine kürzere minimale Expositionszeit.")))
                    output$text_analyse2<-renderUI({NULL})
                    output$force<-renderForceNetwork({NULL})
                    return()
                }
            }
        }
        
        ##dann dafür alle potenziellen Kontakte bestimmen
        input1_fa_filtered<-input1_fa[as.numeric(input1_fa$Aufnahme)<=as.numeric(max(input1_fall_filtered$Entlassung))
                                      &as.numeric(input1_fa$Entlassung)>=as.numeric(min(input1_fall_filtered$Aufnahme)),]
        
        if(sum(input1_fa_filtered$Fallnummer!=fall_ist)==0){
            if(max(input1_fall_filtered$Dauer)<kontaktzeit&&input$FA_oder_Station=="Fachabteilungen"){
                output$text_analyse1<-renderUI(HTML(paste0("In dem betrachteten Beobachtungszeitraum weist Fall ",fall_ist,
                       " auf Fachabteilung ",fa_ist," basierend auf der von Ihnen gewählten Konfiguration keine Kontakte auf, die mutmaßlich zu einer Ansteckung führen.")))
            }else{
                output$text_analyse1<-renderUI(HTML(paste0("In dem betrachteten Beobachtungszeitraum weist Fall ",fall_ist,
                        " auf Station ",fa_ist," basierend auf der von Ihnen gewählten Konfiguration keine Kontakte auf, die mutmaßlich zu einer Ansteckung führen.")))
            }
            output$text_analyse2<-renderUI({NULL})
            output$force<-renderForceNetwork({NULL})
            return()
        }
        
        for(i in 1:nrow(input1_fa_filtered)){
            input1_fa_filtered$Aufnahme[i]<-max(input1_fa_filtered$Aufnahme[i],FA_Start)
            input1_fa_filtered$Entlassung[i]<-min(input1_fa_filtered$Entlassung[i],FA_Ende)
        }
        input1_fa_filtered$Dauer<-(as.numeric(input1_fa_filtered$Entlassung)-as.numeric(input1_fa_filtered$Aufnahme))/(60*60*24)
        
        
        ##vorläufig: jeder Kontakt muss Min-Zeit erfüllen, wenn 2x Kontakt und keiner erfüllt es einzeln, aber in der Summe schon,
        ##wird es aktuell ausgeschlossen
        ##Option zum Aufsummieren ist aber möglich - wenn gewünscht
        if(input$Fall_Inkubation==F){
            input1_fa_filtered2<-input1_fa_filtered[input1_fa_filtered$Dauer>=kontaktzeit,]
        }else{
            input1_fa_filtered2<-input1_fa_filtered[input1_fa_filtered$Dauer>=(kontaktzeit+inkubationszeit),]
        }
        
        
        if(sum(input1_fa_filtered2$Fallnummer==unique(input1_fall_filtered$Fallnummer))==nrow(input1_fa_filtered2)){
            if(input$FA_oder_Station=="Fachabteilungen"){
                output$text_analyse1<-renderUI(HTML(paste0("In dem betrachteten Beobachtungszeitraum hat kein anderer Fall auf Fachabteilung ",
                                                           FA_Fokus," Kontakt zu Fall ",Fall_Fokus_FA," mit der geforderten minimale Expositionszeit.")))
                output$text_analyse2<-renderUI({NULL})
                #output$plotBasic<-renderPlot({NULL})
                output$force<-renderForceNetwork({NULL})
                return()
            }
            if(input$FA_oder_Station=="Stationen"){
                output$text_analyse1<-renderUI(HTML(paste0("In dem betrachteten Beobachtungszeitraum hat kein anderer Fall auf Station ",
                                                           ST_Fokus," Kontakt zu Fall ",Fall_Fokus_ST," mit der geforderten minimale Expositionszeit.")))
                output$text_analyse2<-renderUI({NULL})
                #output$plotBasic<-renderPlot({NULL})
                output$force<-renderForceNetwork({NULL})
                return()
            }
        }
        output$text_analyse1<-renderUI({NULL})
        output$text_analyse2<-renderUI({NULL})
        
        
        ###Netzwerk
        farben<-c("cadetblue3","firebrick","darkgoldenrod1","aquamarine","darksalmon","lightgoldenrod4",colorRampPalette(c("red","blue"))(100))
        
        temp_intervalle<-TimeIntervalDataFrame(start=input1_fa_filtered2$Aufnahme,end=input1_fa_filtered2$Entlassung)
        temp_overlapping<-overlapping(temp_intervalle,idx = T)
        
        if(input$Grad_Intern=="unbegrenzt"){
            kette_soll<-100
        }else{
            kette_soll<-as.numeric(input$Grad_Intern)
        }
        #kette_soll<-1

        kette<-1
        ids_in<-c(unique(input1_fall_filtered$Fallnummer))
        
        progress <- shiny::Progress$new()
        progress$set(message = "Bestimme Kontakte", value = 0)
        progress$inc(0/kette_soll,detail=paste0(kette,". Grades"))
        #while(kette<=kette_soll){
         #   message("Kette: ",kette)
            temp_overlapping2<-rbind(temp_overlapping[temp_overlapping$i%in%which(input1_fa_filtered2$Fallnummer%in%ids_in),],
                                     temp_overlapping[temp_overlapping$j%in%which(input1_fa_filtered2$Fallnummer%in%ids_in),])
            
            if(nrow(temp_overlapping2)==0){
                if(input$FA_oder_Station=="Fachabteilungen"){
                    output$text_analyse1<-renderUI(HTML(paste0("Für die gewählte Konfiguration gibt es keine Kontakte für Fall ",
                                                               Fall_Fokus_FA,".<br>Bitte wählen Sie einen anderen ICD-Filter, eine kürzere Latenzzeit oder eine kürzere minimale Expositionszeit.",
                                                               "<br>Beachten Sie, dass ein gewählter ICD-Filter auf alle Fälle angewandt wird.")))
                }else{
                    output$text_analyse1<-renderUI(HTML(paste0("Für die gewählte Konfiguration gibt es keine Kontakte für Fall ",
                                                               Fall_Fokus_ST,".<br>Bitte wählen Sie einen anderen ICD-Filter, eine kürzere Latenzzeit oder eine kürzere minimale Expositionszeit.",
                                                               "<br>Beachten Sie, dass ein gewählter ICD-Filter auf alle Fälle angewandt wird.")))
                }
                output$text_analyse2<-renderUI({NULL})
                output$force<-renderForceNetwork({NULL})
                return()
            }
            ##Start: Kettenlänge 1
            temp_overlapping2$Patientennummer_i<-input1_fa_filtered2$Patientennummer[temp_overlapping2$i]
            temp_overlapping2$Fallnummer_i<-input1_fa_filtered2$Fallnummer[temp_overlapping2$i]
            temp_overlapping2$Patientennummer_j<-input1_fa_filtered2$Patientennummer[temp_overlapping2$j]
            temp_overlapping2$Fallnummer_j<-input1_fa_filtered2$Fallnummer[temp_overlapping2$j]
            
            temp_overlapping2$Start_i<-input1_fa_filtered2$Aufnahme[temp_overlapping2$i]
            temp_overlapping2$Ende_i<-input1_fa_filtered2$Entlassung[temp_overlapping2$i]
            temp_overlapping2$Start_j<-input1_fa_filtered2$Aufnahme[temp_overlapping2$j]
            temp_overlapping2$Ende_j<-input1_fa_filtered2$Entlassung[temp_overlapping2$j]
            
            helper<-cbind(as.numeric(input1_fa_filtered2$Aufnahme[temp_overlapping2$i]),as.numeric(input1_fa_filtered2$Aufnahme[temp_overlapping2$j]))
            helper_start<-apply(helper,1,max)
            helper<-cbind(as.numeric(input1_fa_filtered2$Entlassung[temp_overlapping2$i]),as.numeric(input1_fa_filtered2$Entlassung[temp_overlapping2$j]))
            helper_ende<-apply(helper,1,min)
            temp_overlapping2$Dauer<-(helper_ende-helper_start)/(60*60*24)
            
            ##Filtern auf Mindest-Kontaktzeit
            if(input$Fall_Inkubation==F){
                temp_overlapping2<-temp_overlapping2[temp_overlapping2$Dauer>=kontaktzeit,]
            }else{
                temp_overlapping2<-temp_overlapping2[temp_overlapping2$Dauer>=(kontaktzeit+inkubationszeit),]
            }
            
            ##prüfen, dass keine Kombination 2x oder mehr vorkommt
            help_test<-paste(temp_overlapping2$Fallnummer_i,temp_overlapping2$Fallnummer_j,sep = "_")
            help_test_table<-table(help_test)
            help_test_table<-help_test_table[help_test_table>1]
            
            if(length(help_test_table)>0){
                help_test<-str_split_fixed(names(help_test_table),"_",Inf)
                
                temp_overlapping_neu<-temp_overlapping2
                temp_overlapping_neu$Erster<-TRUE
                for(k in 1:nrow(help_test)){
                    temp_overlapping_neu$Dauer[temp_overlapping_neu$Fallnummer_i==help_test[k,1]&
                                                   temp_overlapping_neu$Fallnummer_j==help_test[k,2]]<-sum(temp_overlapping_neu$Dauer[temp_overlapping_neu$Fallnummer_i==help_test[k,1]&
                                                                                                                                          temp_overlapping_neu$Fallnummer_j==help_test[k,2]])

                    temp_overlapping_neu[temp_overlapping_neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu$Fallnummer_j==help_test[k,2],]$Erster[temp_overlapping_neu$Start_i[temp_overlapping_neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu$Fallnummer_j==help_test[k,2]]!=min(temp_overlapping_neu$Start_i[temp_overlapping_neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu$Fallnummer_j==help_test[k,2]])|
                                                                                                                                                          temp_overlapping_neu$Start_j[temp_overlapping_neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu$Fallnummer_j==help_test[k,2]]!=min(temp_overlapping_neu$Start_j[temp_overlapping_neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu$Fallnummer_j==help_test[k,2]])]<-F
                }
                temp_overlapping_neu<-temp_overlapping_neu[temp_overlapping_neu$Erster==T,]
                temp_overlapping_neu$Infiziert<-as.POSIXct(ifelse(temp_overlapping_neu$Fallnummer_i==ids_in,
                                                                   as.numeric(temp_overlapping_neu$Start_j+kontaktzeit*60*60*24),
                                                                   as.numeric(temp_overlapping_neu$Start_i+kontaktzeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                temp_overlapping_neu$Ansteckend<-as.POSIXct(ifelse(temp_overlapping_neu$Fallnummer_i==ids_in,
                                                                   as.numeric(temp_overlapping_neu$Start_j+kontaktzeit*60*60*24+inkubationszeit*60*60*24),
                                                                   as.numeric(temp_overlapping_neu$Start_i+kontaktzeit*60*60*24+inkubationszeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                temp_overlapping_neu$AnsteckendBis<-as.POSIXct(ifelse(temp_overlapping_neu$Fallnummer_i==ids_in,
                                                                   as.numeric(temp_overlapping_neu$Start_j+ansteckendzeit*60*60*24+kontaktzeit*60*60*24+inkubationszeit*60*60*24),
                                                                   as.numeric(temp_overlapping_neu$Start_i+ansteckendzeit*60*60*24+kontaktzeit*60*60*24+inkubationszeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                temp_overlapping_neu<-temp_overlapping_neu[,-c(1:2,7:10,12)]
                temp_overlapping_neu<-unique(temp_overlapping_neu)
            }else{
                temp_overlapping2$Infiziert<-as.POSIXct(ifelse(temp_overlapping2$Fallnummer_i==ids_in,
                                                                  as.numeric(temp_overlapping2$Start_j+kontaktzeit*60*60*24),
                                                                  as.numeric(temp_overlapping2$Start_i+kontaktzeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                temp_overlapping2$Ansteckend<-as.POSIXct(ifelse(temp_overlapping2$Fallnummer_i==ids_in,
                                                                   as.numeric(temp_overlapping2$Start_j+kontaktzeit*60*60*24+inkubationszeit*60*60*24),
                                                                   as.numeric(temp_overlapping2$Start_i+kontaktzeit*60*60*24+inkubationszeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                temp_overlapping2$AnsteckendBis<-as.POSIXct(ifelse(temp_overlapping2$Fallnummer_i==ids_in,
                                                                as.numeric(temp_overlapping2$Start_j+ansteckendzeit*60*60*24+kontaktzeit*60*60*24+inkubationszeit*60*60*24),
                                                                as.numeric(temp_overlapping2$Start_i+ansteckendzeit*60*60*24+kontaktzeit*60*60*24+inkubationszeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                temp_overlapping_neu<-temp_overlapping2[,-c(1:2,7:10)]
            }
            temp_overlapping_neu$Grad<-1
            backup_all<-temp_overlapping_neu
            backup_all$RichtigeID<-ifelse(backup_all$Fallnummer_i==ids_in,backup_all$Fallnummer_j,backup_all$Fallnummer_i)
            backup_all$Anstecker<-ifelse(backup_all$Fallnummer_i==ids_in,backup_all$Fallnummer_i,backup_all$Fallnummer_j)
            
            kette<-kette+1

            ##von jedem 1. Kontakt von Fall im Fokus aus neu analysieren
            while(kette<=kette_soll){
                progress$inc(1/kette_soll,detail=paste0(kette,". Grades"))
                
                ids_neu<-backup_all$RichtigeID[!backup_all$RichtigeID%in%ids_in]
                progress2 <- shiny::Progress$new()
                progress2$set(message = "Analysiere ID", value = 0)
                for(id in ids_neu){
                    progress2$inc(1/length(ids_neu),detail=id)
                    
                    FA_Start_Neu<-backup_all$Ansteckend[backup_all$RichtigeID==id]
                    ##Für neuen Fall bestimmen, wann er da ist und wann er davon ansteckend ist
                    input1_fall_filtered_Neu<-input1_fa[input1_fa$Fallnummer==id
                                                    &as.numeric(input1_fa$Aufnahme)<=as.numeric(FA_Ende)
                                                      &as.numeric(input1_fa$Entlassung)>=as.numeric(FA_Start_Neu),]
                    ##wenn entlassen bevor ansteckend -> filtern
                    if(nrow(input1_fall_filtered_Neu)>0){
                      input1_fall_filtered_Neu$Aufnahme[1]<-FA_Start_Neu
                      ##sobald nicht mehr ansteckend, sind alle weiteren aufenthalte + kontakte auf der FA egal -> filtern
                      input1_fall_filtered_Neu<-as.data.frame(input1_fall_filtered_Neu[input1_fall_filtered_Neu$Aufnahme<=input1_fall_filtered_Neu$Aufnahme[1],])
                      if(nrow(input1_fall_filtered_Neu)>0){
                        for(zeile in nrow(input1_fall_filtered_Neu)){
                          input1_fall_filtered_Neu$Entlassung[zeile]<-min(input1_fall_filtered_Neu$Entlassung[zeile],
                                                                          as.POSIXct(as.numeric(input1_fall_filtered_Neu$Aufnahme[zeile])+ansteckendzeit*60*60*24,tz="UTC"))
                        }
                      }
                      input1_fall_filtered_Neu$Dauer<-(as.numeric(input1_fall_filtered_Neu$Entlassung)-as.numeric(input1_fall_filtered_Neu$Aufnahme))/(60*60*24)
                      
                      if(sum(input1_fall_filtered_Neu$Dauer>=kontaktzeit)>0){
                        ##dann dafür alle potenziellen Kontakte bestimmen
                        input1_fa_filtered_Neu<-input1_fa[as.numeric(input1_fa$Aufnahme)<=as.numeric(max(input1_fall_filtered_Neu$Entlassung))
                                                          &as.numeric(input1_fa$Entlassung)>=as.numeric(min(input1_fall_filtered_Neu$Aufnahme)),]
                        if(nrow(input1_fa_filtered_Neu)!=0){
                          for(i in 1:nrow(input1_fa_filtered_Neu)){
                            input1_fa_filtered_Neu$Aufnahme[i]<-max(input1_fa_filtered_Neu$Aufnahme[i],FA_Start_Neu)
                            input1_fa_filtered_Neu$Entlassung[i]<-min(input1_fa_filtered_Neu$Entlassung[i],FA_Ende)
                          }
                          input1_fa_filtered_Neu$Dauer<-(as.numeric(input1_fa_filtered_Neu$Entlassung)-as.numeric(input1_fa_filtered_Neu$Aufnahme))/(60*60*24)
                          ##ab Start ist id ansteckend, jetzt nur noch betrachten, wo Aufenthalt anderer Fälle lang genug für Ansteckung ist
                          input1_fa_filtered2_Neu<-input1_fa_filtered_Neu[input1_fa_filtered_Neu$Dauer>=kontaktzeit,]
                          
                          if(sum(input1_fa_filtered2_Neu$Fallnummer!=id)!=0){
                            temp_intervalle_Neu<-TimeIntervalDataFrame(start=input1_fa_filtered2_Neu$Aufnahme,end=input1_fa_filtered2_Neu$Entlassung)
                            temp_overlapping_Neu<-overlapping(temp_intervalle_Neu,idx = T)
                            message(id)
                            
                            temp_overlapping2_Neu<-rbind(temp_overlapping_Neu[temp_overlapping_Neu$i%in%which(input1_fa_filtered2_Neu$Fallnummer%in%id),],
                                                         temp_overlapping_Neu[temp_overlapping_Neu$j%in%which(input1_fa_filtered2_Neu$Fallnummer%in%id),])
                            if(nrow(temp_overlapping2_Neu)>0){
                              ##Kettenlänge 2
                              temp_overlapping2_Neu$Patientennummer_i<-input1_fa_filtered2_Neu$Patientennummer[temp_overlapping2_Neu$i]
                              temp_overlapping2_Neu$Fallnummer_i<-input1_fa_filtered2_Neu$Fallnummer[temp_overlapping2_Neu$i]
                              temp_overlapping2_Neu$Patientennummer_j<-input1_fa_filtered2_Neu$Patientennummer[temp_overlapping2_Neu$j]
                              temp_overlapping2_Neu$Fallnummer_j<-input1_fa_filtered2_Neu$Fallnummer[temp_overlapping2_Neu$j]
                              
                              temp_overlapping2_Neu$Start_i<-input1_fa_filtered2_Neu$Aufnahme[temp_overlapping2_Neu$i]
                              temp_overlapping2_Neu$Ende_i<-input1_fa_filtered2_Neu$Entlassung[temp_overlapping2_Neu$i]
                              temp_overlapping2_Neu$Start_j<-input1_fa_filtered2_Neu$Aufnahme[temp_overlapping2_Neu$j]
                              temp_overlapping2_Neu$Ende_j<-input1_fa_filtered2_Neu$Entlassung[temp_overlapping2_Neu$j]
                              
                              helper<-cbind(as.numeric(input1_fa_filtered2_Neu$Aufnahme[temp_overlapping2_Neu$i]),as.numeric(input1_fa_filtered2_Neu$Aufnahme[temp_overlapping2_Neu$j]))
                              helper_start<-apply(helper,1,max)
                              helper<-cbind(as.numeric(input1_fa_filtered2_Neu$Entlassung[temp_overlapping2_Neu$i]),as.numeric(input1_fa_filtered2_Neu$Entlassung[temp_overlapping2_Neu$j]))
                              helper_ende<-apply(helper,1,min)
                              temp_overlapping2_Neu$Dauer<-(helper_ende-helper_start)/(60*60*24)
                              
                              ##Filtern auf Mindest-Kontaktzeit
                              temp_overlapping2_Neu<-temp_overlapping2_Neu[temp_overlapping2_Neu$Dauer>=kontaktzeit,]
                              
                              if(nrow(temp_overlapping2_Neu)>0){
                                ##prüfen, dass keine Kombination 2x oder mehr vorkommt
                                help_test<-paste(temp_overlapping2_Neu$Fallnummer_i,temp_overlapping2_Neu$Fallnummer_j,sep = "_")
                                help_test_table<-table(help_test)
                                help_test_table<-help_test_table[help_test_table>1]
                                
                                if(length(help_test_table)>0){
                                  help_test<-str_split_fixed(names(help_test_table),"_",Inf)
                                  
                                  temp_overlapping_neu_Neu<-temp_overlapping2_Neu
                                  temp_overlapping_neu_Neu$Erster<-TRUE
                                  for(k in 1:nrow(help_test)){
                                    temp_overlapping_neu_Neu$Dauer[temp_overlapping_neu_Neu$Fallnummer_i==help_test[k,1]&
                                                                     temp_overlapping_neu_Neu$Fallnummer_j==help_test[k,2]]<-sum(temp_overlapping_neu_Neu$Dauer[temp_overlapping_neu_Neu$Fallnummer_i==help_test[k,1]&
                                                                                                                                                                  temp_overlapping_neu_Neu$Fallnummer_j==help_test[k,2]])
                                    
                                    temp_overlapping_neu_Neu[temp_overlapping_neu_Neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu_Neu$Fallnummer_j==help_test[k,2],]$Erster[temp_overlapping_neu_Neu$Start_i[temp_overlapping_neu_Neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu_Neu$Fallnummer_j==help_test[k,2]]!=min(temp_overlapping_neu_Neu$Start_i[temp_overlapping_neu_Neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu_Neu$Fallnummer_j==help_test[k,2]])|
                                                                                                                                                                                    temp_overlapping_neu_Neu$Start_j[temp_overlapping_neu_Neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu_Neu$Fallnummer_j==help_test[k,2]]!=min(temp_overlapping_neu_Neu$Start_j[temp_overlapping_neu_Neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu_Neu$Fallnummer_j==help_test[k,2]])]<-F
                                  }
                                  temp_overlapping_neu_Neu<-temp_overlapping_neu_Neu[temp_overlapping_neu_Neu$Erster==T,]
                                  temp_overlapping_neu_Neu$Infiziert<-as.POSIXct(ifelse(temp_overlapping_neu_Neu$Fallnummer_i==id,
                                                                                        as.numeric(temp_overlapping_neu_Neu$Start_j+kontaktzeit*60*60*24),
                                                                                        as.numeric(temp_overlapping_neu_Neu$Start_i+kontaktzeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                  temp_overlapping_neu_Neu$Ansteckend<-as.POSIXct(ifelse(temp_overlapping_neu_Neu$Fallnummer_i==id,
                                                                                         as.numeric(temp_overlapping_neu_Neu$Start_j+kontaktzeit*60*60*24+inkubationszeit*60*60*24),
                                                                                         as.numeric(temp_overlapping_neu_Neu$Start_i+kontaktzeit*60*60*24+inkubationszeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                  temp_overlapping_neu_Neu$AnsteckendBis<-as.POSIXct(ifelse(temp_overlapping_neu_Neu$Fallnummer_i==id,
                                                                                            as.numeric(temp_overlapping_neu_Neu$Start_j+ansteckendzeit*60*60*24+kontaktzeit*60*60*24+inkubationszeit*60*60*24),
                                                                                            as.numeric(temp_overlapping_neu_Neu$Start_i+ansteckendzeit*60*60*24+kontaktzeit*60*60*24+inkubationszeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                  temp_overlapping_neu_Neu<-temp_overlapping_neu_Neu[,-c(1:2,7:10,12)]
                                  temp_overlapping_neu_Neu<-unique(temp_overlapping_neu_Neu)
                                }else{
                                  temp_overlapping2_Neu$Infiziert<-as.POSIXct(ifelse(temp_overlapping2_Neu$Fallnummer_i==id,
                                                                                     as.numeric(temp_overlapping2_Neu$Start_j+kontaktzeit*60*60*24),
                                                                                     as.numeric(temp_overlapping2_Neu$Start_i+kontaktzeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                  temp_overlapping2_Neu$Ansteckend<-as.POSIXct(ifelse(temp_overlapping2_Neu$Fallnummer_i==id,
                                                                                      as.numeric(temp_overlapping2_Neu$Start_j+kontaktzeit*60*60*24+inkubationszeit*60*60*24),
                                                                                      as.numeric(temp_overlapping2_Neu$Start_i+kontaktzeit*60*60*24+inkubationszeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                  temp_overlapping2_Neu$AnsteckendBis<-as.POSIXct(ifelse(temp_overlapping2_Neu$Fallnummer_i==id,
                                                                                         as.numeric(temp_overlapping2_Neu$Start_j+ansteckendzeit*60*60*24+kontaktzeit*60*60*24+inkubationszeit*60*60*24),
                                                                                         as.numeric(temp_overlapping2_Neu$Start_i+ansteckendzeit*60*60*24+kontaktzeit*60*60*24+inkubationszeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                  temp_overlapping_neu_Neu<-temp_overlapping2_Neu[,-c(1:2,7:10)]
                                }
                                
                                temp_overlapping_neu_Neu$Grad<-kette
                                temp_overlapping_neu_Neu$RichtigeID<-ifelse(temp_overlapping_neu_Neu$Fallnummer_i==id,temp_overlapping_neu_Neu$Fallnummer_j,temp_overlapping_neu_Neu$Fallnummer_i)
                                temp_overlapping_neu_Neu$Anstecker<-ifelse(temp_overlapping_neu_Neu$Fallnummer_i==id,temp_overlapping_neu_Neu$Fallnummer_i,temp_overlapping_neu_Neu$Fallnummer_j)
                                ##überprüfen, ob zeit bis zur Ansteckung mit höherem Grad evt schneller ist als mit niedrigerem?
                                for(zeile in 1:nrow(backup_all)){
                                  if(sum(backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID)>0&&
                                     backup_all$Infiziert[zeile]>temp_overlapping_neu_Neu$Infiziert[backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID]){
                                    backup_all$Infiziert[zeile]<-temp_overlapping_neu_Neu$Infiziert[backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID]
                                    backup_all$Ansteckend[zeile]<-temp_overlapping_neu_Neu$Ansteckend[backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID]
                                    backup_all$AnsteckendBis[zeile]<-temp_overlapping_neu_Neu$AnsteckendBis[backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID]
                                    backup_all$Dauer[zeile]<-temp_overlapping_neu_Neu$Dauer[backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID]
                                    backup_all$Grad[zeile]<-temp_overlapping_neu_Neu$Grad[backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID]
                                    backup_all$Anstecker[zeile]<-temp_overlapping_neu_Neu$Anstecker[backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID]
                                  }
                                }
                                
                                neue_zeilen<-unique(c(temp_overlapping_neu_Neu$Fallnummer_i,temp_overlapping_neu_Neu$Fallnummer_j))[!unique(c(temp_overlapping_neu_Neu$Fallnummer_i,temp_overlapping_neu_Neu$Fallnummer_j))%in%unique(c(backup_all$Fallnummer_i,backup_all$Fallnummer_j))]
                                if(length(neue_zeilen)>0){
                                  backup_temp<-temp_overlapping_neu_Neu[temp_overlapping_neu_Neu$Fallnummer_i%in%neue_zeilen|temp_overlapping_neu_Neu$Fallnummer_j%in%neue_zeilen,]
                                  backup_temp$RichtigeID<-ifelse(backup_temp$Fallnummer_i==id,backup_temp$Fallnummer_j,backup_temp$Fallnummer_i)
                                  backup_temp$Anstecker<-ifelse(backup_temp$Fallnummer_i==id,backup_temp$Fallnummer_i,backup_temp$Fallnummer_j)
                                  backup_all<-rbind(backup_all,backup_temp)
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                }
                progress2$close()
                if(sum(backup_all$Grad==kette)==0){
                    kette<-kette_soll
                }
                kette<-kette+1
            }
            progress$close()
            
            ##alles notwendige steht in backup_all
            ##nur noch Knoten für Fall im Fokus hinzufügen, auch für andere FAs wo der war
            backup_all$Group<-fa_ist
            backup_all<-rbind(backup_all,NA)
            backup_all$Patientennummer_i[nrow(backup_all)]<-unique(input1_fall_filtered$Patientennummer)
            backup_all$Fallnummer_i[nrow(backup_all)]<-unique(input1_fall_filtered$Fallnummer)
            backup_all$Patientennummer_j[nrow(backup_all)]<-unique(input1_fall_filtered$Patientennummer)
            backup_all$Fallnummer_j[nrow(backup_all)]<-unique(input1_fall_filtered$Fallnummer)
            backup_all$Infiziert[nrow(backup_all)]<-min(input1_fall_filtered$Aufnahme,FA_Start)
            backup_all$Ansteckend[nrow(backup_all)]<-min(input1_fall_filtered$Aufnahme)
            backup_all$AnsteckendBis[nrow(backup_all)]<-min(max(input1_fall_filtered$Entlassung),as.POSIXct(as.numeric(backup_all$Ansteckend[nrow(backup_all)]+ansteckendzeit*60*60*24),tz="UTC"))
            backup_all$Grad[nrow(backup_all)]<-0
            backup_all$RichtigeID[nrow(backup_all)]<-unique(input1_fall_filtered$Fallnummer)
            backup_all$RichtigeID2<-backup_all$RichtigeID
            backup_all$Anstecker[nrow(backup_all)]<-"-"
            backup_all$Group[nrow(backup_all)]<-fa_ist
            
            backup_all$Dauer<-(as.numeric(backup_all$AnsteckendBis)-as.numeric(backup_all$Ansteckend))/(60*60*24)
            for(i in 1:nrow(backup_all)){
                investigate<-input1_fa[input1_fa$Fallnummer==backup_all$RichtigeID2[i],]
                investigate<-as.data.frame(investigate[investigate$Entlassung>=backup_all$Ansteckend[i]&investigate$Aufnahme<=backup_all$AnsteckendBis[i],])
                investigate$Aufnahme[investigate$Aufnahme<backup_all$Ansteckend[i]]<-backup_all$Ansteckend[i]
                if(nrow(investigate)>0){
                    backup_all$AnsteckendBis[i]<-min(backup_all$AnsteckendBis[i],max(investigate$Entlassung))
                    backup_all$Dauer[i]<-(as.numeric(backup_all$AnsteckendBis[i])-as.numeric(backup_all$Ansteckend[i]))/(60*60*24)
                    if(nrow(investigate)>1){
                        backup_all$Dauer[i]<-backup_all$Dauer[i]-sum((as.numeric(investigate$Aufnahme[2:nrow(investigate)])-as.numeric(investigate$Entlassung[1:(nrow(investigate)-1)]))/(60*60*24))
                    }
                }else{
                    backup_all$Dauer[i]<--1##später extra bezeichnen
                }
            }
            backup_all<-backup_all[backup_all$Dauer>0,]
                
            ##fall_ist auf anderen FAs
            fall_add<-input1[input1$Fallnummer==fall_ist&input1$Abteilung!=fa_ist&
                                 input1$Aufnahme>=min(input1_fall_filtered$Aufnahme)&
                                 input1$Aufnahme<=max(input1_fall_filtered$Entlassung,as.POSIXct(as.numeric(input1_fall_filtered$Aufnahme)+ansteckendzeit*60*60*24,tz="UTC"))&
                                 as.numeric(input1$Entlassung-input1$Aufnahme)/(60*60*24)>=kontaktzeit,]
            if(nrow(fall_add)>0){
                andere_fas<-unique(fall_add$Abteilung)
                for(zeile in 1:length(andere_fas)){
                    backup_all<-rbind(backup_all,NA)
                    backup_all$Patientennummer_i[nrow(backup_all)]<-unique(fall_add$Patientennummer[fall_add$Abteilung==andere_fas[zeile]])
                    backup_all$Fallnummer_i[nrow(backup_all)]<-unique(fall_add$Fallnummer[fall_add$Abteilung==andere_fas[zeile]])
                    backup_all$Patientennummer_j[nrow(backup_all)]<-unique(fall_add$Patientennummer[fall_add$Abteilung==andere_fas[zeile]])
                    backup_all$Fallnummer_j[nrow(backup_all)]<-paste0(andere_fas[zeile]," ",unique(fall_add$Fallnummer[fall_add$Abteilung==andere_fas[zeile]]))
                    backup_all$Infiziert[nrow(backup_all)]<-backup_all$Infiziert[backup_all$Fallnummer_i==backup_all$Fallnummer_i[nrow(backup_all)]&backup_all$Fallnummer_j==backup_all$Fallnummer_i[nrow(backup_all)]]
                    backup_all$Ansteckend[nrow(backup_all)]<-min(fall_add$Aufnahme[fall_add$Abteilung==andere_fas[zeile]])
                    backup_all$AnsteckendBis[nrow(backup_all)]<-max(fall_add$Entlassung[fall_add$Abteilung==andere_fas[zeile]])
                    backup_all$Dauer[nrow(backup_all)]<-(as.numeric(backup_all$AnsteckendBis[nrow(backup_all)])-as.numeric(backup_all$Ansteckend[nrow(backup_all)]))/(60*60*24)
                    investigate<-fall_add[fall_add$Abteilung==andere_fas[zeile],]
                    if(nrow(investigate)>1){
                        backup_all$Dauer[nrow(backup_all)]<-backup_all$Dauer[nrow(backup_all)]-as.numeric(investigate$Aufnahme[2:nrow(investigate)]-investigate$Entlassung[1:(nrow(investigate)-1)])/(60*60*24)
                    }
                    backup_all$Grad[nrow(backup_all)]<-0
                    backup_all$RichtigeID[nrow(backup_all)]<-paste0(fall_add$Abteilung[zeile]," ",fall_add$Fallnummer[zeile])
                    backup_all$RichtigeID2[nrow(backup_all)]<-fall_add$Fallnummer[zeile]
                    backup_all$Anstecker[nrow(backup_all)]<-"-"
                    backup_all$Group[nrow(backup_all)]<-fall_add$Abteilung[zeile]
                }
            }
            
            if(nrow(backup_all)==1){
                if(input$FA_oder_Station=="Fachabteilungen"){
                    output$text_analyse1<-renderUI(HTML(paste0("Für die gewählte Konfiguration gibt es keine Kontakte für Fall ",
                                                               Fall_Fokus_FA,".<br>Bitte wählen Sie einen anderen ICD-Filter, eine kürzere Latenzzeit oder eine kürzere minimale Expositionszeit.",
                                                               "<br>Beachten Sie, dass ein gewählter ICD-Filter auf alle Fälle angewandt wird.")))
                }else{
                    output$text_analyse1<-renderUI(HTML(paste0("Für die gewählte Konfiguration gibt es keine Kontakte für Fall ",
                                                               Fall_Fokus_ST,".<br>Bitte wählen Sie einen anderen ICD-Filter, eine kürzere Latenzzeit oder eine kürzere minimale Expositionszeit.",
                                                               "<br>Beachten Sie, dass ein gewählter ICD-Filter auf alle Fälle angewandt wird.")))
                }
                output$text_analyse2<-renderUI({NULL})
                output$force<-renderForceNetwork({NULL})
                return()
            }
            
            backup_all$ICDfull<-input1$ICDfull[match(backup_all$RichtigeID2,input1$Fallnummer)]
            
            ##Links und Nodes definieren
            links1<-backup_all[,c("Anstecker","RichtigeID")]
            links1$Anstecker[links1$Anstecker=="-"]<-fall_ist
            names(links1)<-c("Source","Target")
            links1$Group<-backup_all$Group
            links1$Anstecker<-backup_all$Anstecker
            links1$Farbe<-"#d1d1d1"
            links1$Farbe[links1$Anstecker=="-"]<-"black"
            
            nodes1<-data.frame(NodeID=backup_all$RichtigeID,ID=seq(0,(nrow(backup_all)-1)))
            links1$Source1<-nodes1$ID[match(links1$Source,nodes1$NodeID)]
            links1$Target1<-nodes1$ID[match(links1$Target,nodes1$NodeID)]
            
            nodes1$NodeSize<-nodes1$Dauer<-as.numeric(backup_all$Dauer)
            nodes1$NodeSizeb<-2+8*nodes1$NodeSize/max(nodes1$NodeSize)
            nodes1$Group<-backup_all$Group
            nodes1$ICDfull<-backup_all$ICDfull
            nodes1$Grad<-backup_all$Grad
            nodes1$Infiziert<-backup_all$Infiziert
            nodes1$Ansteckend<-backup_all$Ansteckend
            nodes1$AnsteckendBis<-backup_all$AnsteckendBis
            nodes1$Anstecker<-backup_all$Anstecker
            nodes1$RichtigeID<-backup_all$RichtigeID
            nodes1$ID2<-backup_all$RichtigeID2
            
            nodes1$Kontakte<-0
            nodes1$AndereFAs<-""
            for(i in 1:nrow(nodes1)){
                if(nodes1$Grad[i]==kette_soll||nodes1$RichtigeID[i]!=nodes1$ID2[i]){
                    nodes1$Kontakte[i]<-"noch nicht analysiert"
                }else{
                    nodes1$Kontakte[i]<-sum(nodes1$Anstecker==nodes1$NodeID[i])
                }
                if(nodes1$ID2[i]!=fall_ist){
                    nodes1$AndereFAs[i]<-ifelse(length(unique(input1$Abteilung[input1$Fallnummer==nodes1$ID2[i]&input1$Abteilung!=nodes1$Group[i]&input1$Aufnahme>=nodes1$Ansteckend[i]&input1$Aufnahme<=nodes1$AnsteckendBis[i]]))==0,
                                                "-",
                                                paste0(unique(input1$Abteilung[input1$Fallnummer==nodes1$ID2[i]&input1$Abteilung!=nodes1$Group[i]&input1$Aufnahme>=nodes1$Ansteckend[i]&input1$Aufnahme<=nodes1$AnsteckendBis[i]]),collapse=", "))
                }else{
                    help_fa<-unique(nodes1$Group[nodes1$ID2==fall_ist])
                    nodes1$AndereFAs[i]<-ifelse(length(help_fa)==1,
                                                "-",
                                                paste0(help_fa[help_fa!=nodes1$Group[i]],collapse=", "))
                }
            }

        
            color_scale_dark <- c("#0048ba","#cc5500","#1b4d3e","#9a0e2a","#aa7a04","#9a235e","#3e6717","#874747","#9b5206","#5c4eb2")
            color_scale_light <- c("#9df6fd","#ffa332","#2feeb5","#f6a99b","#eecf83","#f5a9ce","#9af1b7","#dbb3b3","#e3c6a7","#d0cbf3")
            nodes1$Farbe<-"black"
            nodes1$NodeSizeb[nodes1$ID2==fall_ist&nodes1$Group!=fa_ist]<-12
            nodes1$Farbe[nodes1$ID2==fall_ist&nodes1$Group!=fa_ist]<-color_scale_dark[2:length(unique(nodes1$Group))]
            farben_grad<-colorRampPalette(c(color_scale_dark[1],color_scale_light[1]))(max(nodes1$Grad))
            for(kette in 1:kette_soll){
                nodes1$Farbe[nodes1$ID2!=fall_ist&nodes1$Group==fa_ist&nodes1$Grad==kette]<-farben_grad[kette]
            }
            nodes1$NodeSizeb[nodes1$Farbe=="black"]<-12
            nodes1$Group.Farbe<-paste0(nodes1$Group,",",nodes1$Farbe)
            
            #links1$Grad<-nodes1$Grad[match(links1$Target,nodes1$NodeID)]
            
            #links1$linkDistance<-50*(kette-links1$Grad+1)
            #links1$linkDistance[links1$Farbe=="black"&links1$Source!=links1$Target]<-500
            
            #links1$linkDistance<-1
            #counts_temp <- as.data.frame(table(links1$Source1))
            #counts_temp$Var1<-as.numeric(as.character(counts_temp$Var1))
            
            #links1$linkDistance[!is.na(match(links1$Target1,counts_temp$Var1))]<-counts_temp$Freq[match(counts_temp$Var1,links1$Target1[!is.na(match(links1$Target1,counts_temp$Var1))])]+1
            #links1$linkDistance<-1+links1$linkDistance/max(links1$linkDistance)


            group_color_pairs <- paste0('\'', unique(nodes1$Group.Farbe), '\'', collapse = ', ')
            color_pairs <- paste0('\'', unique(nodes1$Farbe), '\'', collapse = ', ')
            
            JS_color <- sprintf("
                     d3.scaleOrdinal()
                     .domain([%s])
                     .range([%s]);
                     ", group_color_pairs, color_pairs)
        
        
        #if(input$FA_oder_Station=="Fachabteilungen"){
        #  script <-'alert("Fachabteilung: " + (d.Group) + "\\n \\n Fall: " + (d.NodeID) + "\\n Hauptdiagnose: " + (d.ICDfull) + "\\n Aufenthaltsdauer auf Fachabteilung: " + (d.Dauer) + " Tage" + "\\n Registrierte Kontakte auf Fachabteilung: " + (d.Kontakte));'
        #}else{
        #  script <-'alert("Station: " + (d.Group) + "\\n \\n Fall: " + (d.NodeID) + "\\n Hauptdiagnose: " + (d.ICDfull) + "\\n Aufenthaltsdauer auf Station: " + (d.Dauer) + " Tage" + "\\n Registrierte Kontakte auf Station: " + (d.Kontakte));'
        #}
        script <- 'alert(d.Test)'
        
        fn<-forceNetwork(Links = links1,Nodes = nodes1,NodeID="RichtigeID",Group = "Group.Farbe",
                     Source="Source1",Target="Target1",zoom=T,opacityNoHover = 0.2,opacity = 1,Nodesize = "NodeSizeb",
                     radiusCalculation = JS("d.nodesize"),charge = -60,colourScale = JS(JS_color),
                     linkColour = links1$Farbe,#Value="linkDistance",#linkWidth = 1,
                     #linkDistance = JS("function(d) { return d.value; }"),
                     clickAction = script)

        fn$x$nodes$NodeID<-nodes1$ID2
        fn$x$nodes$ICDfull<-nodes1$ICDfull
        fn$x$nodes$Group2<-nodes1$Group
        fn$x$nodes$Dauer<-as.character(round(as.numeric(nodes1$Dauer),2))
        fn$x$nodes$Kontakte<-nodes1$Kontakte
        
        fn$x$nodes$Infiziert<-nodes1$Infiziert
        fn$x$nodes$Ansteckend<-nodes1$Ansteckend
        fn$x$nodes$AnsteckendBis<-nodes1$AnsteckendBis
        fn$x$nodes$Grad<-nodes1$Grad
        fn$x$nodes$Anstecker<-nodes1$Anstecker
        fn$x$nodes$Angesteckt<-nodes1$Angesteckt
        fn$x$nodes$Group.Farbe<-nodes1$Group.Farbe
        fn$x$nodes$AndereFAs<-nodes1$AndereFAs
        
        if(!is.null(FA_Fokus)){
            fn$x$nodes$Test<-paste0("Fachabteilung: ",fn$x$nodes$Group2,
                                    "\n\nFall: ",fn$x$nodes$NodeID,
                                    "\nHauptdiagnose: ",fn$x$nodes$ICDfull,
                                    "\n\nInfektion:",
                                    "\n     Am: ",fn$x$nodes$Infiziert,
                                    "\n     Durch: ",fn$x$nodes$Anstecker," (Infektion ",fn$x$nodes$Grad,". Grades)",
                                    "\n\nAnsteckend auf Fachabteilung ",fn$x$nodes$Group2,":",
                                    ifelse(fn$x$nodes$Dauer==-1,"\nnoch vor infektiöser Phase wieder von Fachabteilung entlassen",
                                           paste0("\n     Von ",fn$x$nodes$Ansteckend," bis ",fn$x$nodes$AnsteckendBis,
                                           "\n     Aufenthaltsdauer: ",fn$x$nodes$Dauer," Tage",
                                           "\n     Fälle angesteckt: ",fn$x$nodes$Kontakte)),
                                    "\n\nWeitere besuchte Fachabteilungen während ansteckend: ",fn$x$nodes$AndereFAs)
        }else{
            fn$x$nodes$Test<-paste0("Station: ",fn$x$nodes$Group2,
                                    "\n\nFall: ",fn$x$nodes$NodeID,
                                    "\nHauptdiagnose: ",fn$x$nodes$ICDfull,
                                    "\n\nInfektion: ",
                                    "\n     Am: ",fn$x$nodes$Infiziert,
                                    "\n     Durch: ",fn$x$nodes$Anstecker," (Infektion ",fn$x$nodes$Grad,". Grades)",
                                    "\n\nAnsteckend auf Station ",fn$x$nodes$Group2,":",
                                    ifelse(fn$x$nodes$Dauer==-1,"\nnoch vor infektiöser Phase wieder von Station entlassen",
                                           paste0("\n     Von ",fn$x$nodes$Ansteckend," bis ",fn$x$nodes$AnsteckendBis,
                                                  "\n     Aufenthaltsdauer: ",fn$x$nodes$Dauer," Tage",
                                                  "\n     Fälle angesteckt: ",fn$x$nodes$Kontakte)),
                                    "\n\nWeitere besuchte Stationen während ansteckend: ",fn$x$nodes$AndereFAs)
        }

        nodes1$FarbeAussen<-nodes1$Farbe
        nodes1$FarbeAussen[nodes1$AndereFAs!="-"]<-"#141414"
        nodes1$FarbeAussen[nodes1$Grad==0&nodes1$Group==fa_ist]<-farben_grad[1]
        fn$x$nodes$FarbeAussen<-nodes1$FarbeAussen
        fn$x$nodes$borderWidth<-1.5
        fn$x$nodes$borderWidth[fn$x$nodes$Grad==0]<-7
        
        
        fn <- onRender(fn, '
                function(el, x) {
                    // Convert R data frame to JavaScript array of objects
                var nodeColors = HTMLWidgets.dataframeToD3(x.nodes);
                var nodeData = HTMLWidgets.dataframeToD3(x.nodes);
                d3.selectAll(".node").select("circle")
                .style("stroke", function(d, i) {
                    return nodeColors[i].FarbeAussen;  // Use the AB column for node stroke color
                })
                .style("stroke-width", function(d, i) {
                    return nodeData[i].borderWidth + "px";  // Use the borderWidth column for node stroke width
                 });
              }
            ')
        
        output$force <- renderForceNetwork({
          fn})
        
        
        #test.layout <- layout_with_graphopt(net2.bp_all)
        #set.seed(123)
        #test.layout <- layout_nicely(net2.bp_all,dim = 2)
        #set.seed(123)
        #test.layout <- layout_with_graphopt(net2.bp_all,charge = 0.1)
        #message(length(vertex_size_all))
        #output$plotBasic<-renderPlot({
        #    plot(net2.bp_all,vertex.size=4*vertex_size_all/as.numeric(FA_Ende-FA_Start),
        #         edge.width=1*edge_size_all/as.numeric(FA_Ende-FA_Start),edge.color=edge_color_all,
        #         vertex.color=vertex_color_all,layout=test.layout,
        #         vertex.frame.color=vertex_color_all,vertex.label.dist=label_distance_all)
        #},width = 1500,height = 1500)
        #width = max(1000,length(vertex_size_all)*12),height = max(1000,length(vertex_size_all)*12))
        
        icd_help_out<-c("I: A00-B99",
                        "II: C00-D48",
                        "III: D50-D90",
                        "IV: E00-E90",
                        "V: F00-F99",
                        "VI: G00-G99",
                        "VII: H00-H59",
                        "VIII: H60-H95",
                        "IX: I00-I99",
                        "X: J00-J99",
                        "XI: K00-K93",
                        "XII: L00-L99",
                        "XIII: M00-M99",
                        "XIV: N00-N99",
                        "XV: O00-O99",
                        "XVI: P00-P96",
                        "XVII: Q00-Q99",
                        "XVIII: R00-R99",
                        "XIX: S00-T98",
                        "XX: V01-Y84",
                        "XXI: Z00-Z99",
                        "XXII: U00-U99",
                        "nein")
        
        risiko_andere<-nodes1$AndereFAs[nodes1$AndereFAs!="-"]
        risiko_andere2<-str_split_fixed(risiko_andere,pattern = ",",n = Inf)
        risiko_andere2<-sub("^ ","",risiko_andere2,fixed=F)
        risiko_andere2<-as.vector(risiko_andere2)
        risiko_andere2<-risiko_andere2[risiko_andere2!=""]
        risiko_andere3<-risiko_andere2[risiko_andere2!=fa_ist]
        risiko<-as.data.frame(table(risiko_andere3))
        risiko<-risiko[order(risiko$Freq,decreasing = T),]
        if(length(risiko)>0){
            risiko_out<-paste0(risiko$risiko_andere3," (",risiko$Freq,ifelse(risiko$Freq==1," Verlegung)"," Verlegungen)"))
        }else{
            risiko_out<-"-"
        }
        
        
        
        if(input$FA_oder_Station=="Fachabteilungen"){
            if(kette_soll!=100){
                output$text_analyse2<-renderUI(HTML(paste0("Fachabteilung: ",FA_Fokus,
                                                           "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA_ICD)],collapse=", "),
                                                           "<br>Fall im Fokus: ",Fall_Fokus_FA,
                                                           "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                           "<br><br>Latenzzeit: ",min_inkubation_tag,ifelse(min_inkubation_tag==1," Tag "," Tage "),
                                                           " und ",min_inkubation_stunde,ifelse(min_inkubation_stunde==1," Stunde"," Stunden"),
                                                           "<br>Minimale Expositionszeit: ",min_kontakt_tag,ifelse(min_kontakt_tag==1," Tag "," Tage "),
                                                           " und ",min_kontakt_stunde,ifelse(min_kontakt_stunde==1," Stunde"," Stunden"),
                                                           "<br>Infektiöse Phase: ",min_ansteckend_tag,ifelse(min_ansteckend_tag==1," Tag "," Tage "),
                                                           " und ",min_ansteckend_stunde,ifelse(min_ansteckend_stunde==1," Stunde"," Stunden"),
                                                           "<br>Maximale Länge der Infektions-Kette: ",kette_soll,
                                                           "<br><br>Mutmaßlich infizierte Fälle auf Fachabteilung ",FA_Fokus,": ",nrow(nodes1[nodes1$Group==FA_Fokus,]),
                                                           "<br>Infektionsrisiko für ",ifelse(length(risiko)==0,0,nrow(risiko)),
                                                           "/",(length(unique(input1$Abteilung))-1)," weitere Fachabteilungen: ",paste0(risiko_out,collapse=", "),
                                                           "<br><br>Analyse erfolgreich durchgeführt")))
            }else{
                output$text_analyse2<-renderUI(HTML(paste0("Fachabteilung: ",FA_Fokus,
                                                           "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA_ICD)],collapse=", "),
                                                           "<br>Fall im Fokus: ",Fall_Fokus_FA,
                                                           "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                           "<br><br>Latenzzeit: ",min_inkubation_tag,ifelse(min_inkubation_tag==1," Tag "," Tage "),
                                                           " und ",min_inkubation_stunde,ifelse(min_inkubation_stunde==1," Stunde"," Stunden"),
                                                           "<br>Minimale Expositionszeit: ",min_kontakt_tag,ifelse(min_kontakt_tag==1," Tag "," Tage "),
                                                           " und ",min_kontakt_stunde,ifelse(min_kontakt_stunde==1," Stunde"," Stunden"),
                                                           "<br>Infektiöse Phase: ",min_ansteckend_tag,ifelse(min_ansteckend_tag==1," Tag "," Tage "),
                                                           " und ",min_ansteckend_stunde,ifelse(min_ansteckend_stunde==1," Stunde"," Stunden"),
                                                           "<br>Maximale Länge der Infektions-Kette: unbegrenzt (hier: ",max(nodes1$Grad),")",
                                                           "<br><br>Mutmaßlich infizierte Fälle auf Fachabteilung ",FA_Fokus,": ",nrow(nodes1[nodes1$Group==FA_Fokus,]),
                                                           "<br>Infektionsrisiko für ",ifelse(length(risiko)==0,0,nrow(risiko)),
                                                           "/",(length(unique(input1$Abteilung))-1)," weitere Fachabteilungen: ",paste0(risiko_out,collapse=", "),
                                                           "<br><br>Analyse erfolgreich durchgeführt")))
            }
            if(length(unique(fn$x$nodes$Group2))>1){
                output$FA_erweitert_UI2<-renderUI({actionBttn("do_erweiterteFA",
                                                              label = tagList("+ weitere besuchte Fachabteilungen",tags$br(),
                                                                              paste0("(Fall: ",fall_ist,", Länge Infektionskette: ",input$Grad_Intern,")")),
                                                              style = "gradient",color = "primary",block=T)})
            }else{
                output$FA_erweitert_UI2<-renderUI({NULL})
            }
            if(length(unique(fn$x$nodes$AndereFAs[!is.na(fn$x$nodes$AndereFAs)&fn$x$nodes$AndereFAs!="-"]))>0){
                output$FA_erweitert_UI3<-renderUI({actionBttn("do_erweiterteFA2",
                                                              label = tagList("+ weitere besuchte Fachabteilungen",tags$br(),
                                                                              paste0("(Fall: alle, Länge Infektionskette: 1)")),
                                                              style = "gradient",color = "primary",block=T)})
            }else{
                output$FA_erweitert_UI3<-renderUI({NULL})
            }
            
        }else{
            if(kette_soll!=100){
                output$text_analyse2<-renderUI(HTML(paste0("Station: ",ST_Fokus,
                                                           "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA_ICD)],collapse=", "),
                                                           "<br>Fall im Fokus: ",Fall_Fokus_ST,
                                                           "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                           "<br><br>Latenzzeit: ",min_inkubation_tag,ifelse(min_inkubation_tag==1," Tag "," Tage "),
                                                           " und ",min_inkubation_stunde,ifelse(min_inkubation_stunde==1," Stunde"," Stunden"),
                                                           "<br>Minimale Expositionszeit: ",min_kontakt_tag,ifelse(min_kontakt_tag==1," Tag "," Tage "),
                                                           " und ",min_kontakt_stunde,ifelse(min_kontakt_stunde==1," Stunde"," Stunden"),
                                                           "<br>Infektiöse Phase: ",min_ansteckend_tag,ifelse(min_ansteckend_tag==1," Tag "," Tage "),
                                                           " und ",min_ansteckend_stunde,ifelse(min_ansteckend_stunde==1," Stunde"," Stunden"),
                                                           "<br>Maximale Länge der Infektions-Kette: ",kette_soll,
                                                           "<br><br>Mutmaßlich infizierte Fälle auf Station ",FA_Fokus,": ",nrow(nodes1[nodes1$Group==ST_Fokus,]),
                                                           "<br>Infektionsrisiko für ",ifelse(length(risiko)==0,0,nrow(risiko)),
                                                           "/",(length(unique(input1$Abteilung))-1)," weitere Stationen: ",paste0(risiko_out,collapse=", "),
                                                           "<br><br>Analyse erfolgreich durchgeführt")))
            }else{
                output$text_analyse2<-renderUI(HTML(paste0("Station: ",ST_Fokus,
                                                           "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA_ICD)],collapse=", "),
                                                           "<br>Fall im Fokus: ",Fall_Fokus_ST,
                                                           "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                           "<br><br>Latenzzeit: ",min_inkubation_tag,ifelse(min_inkubation_tag==1," Tag "," Tage "),
                                                           " und ",min_inkubation_stunde,ifelse(min_inkubation_stunde==1," Stunde"," Stunden"),
                                                           "<br>Minimale Expositionszeit: ",min_kontakt_tag,ifelse(min_kontakt_tag==1," Tag "," Tage "),
                                                           " und ",min_kontakt_stunde,ifelse(min_kontakt_stunde==1," Stunde"," Stunden"),
                                                           "<br>Infektiöse Phase: ",min_ansteckend_tag,ifelse(min_ansteckend_tag==1," Tag "," Tage "),
                                                           " und ",min_ansteckend_stunde,ifelse(min_ansteckend_stunde==1," Stunde"," Stunden"),
                                                           "<br>Maximale Länge der Infektions-Kette: unbegrenzt (hier: ",max(nodes1$Grad),")",
                                                           "<br><br>Mutmaßlich infizierte Fälle auf Station ",FA_Fokus,": ",nrow(nodes1[nodes1$Group==ST_Fokus,]),
                                                           "<br>Infektionsrisiko für ",ifelse(length(risiko)==0,0,nrow(risiko)),
                                                           "/",(length(unique(input1$Abteilung))-1)," weitere Stationen: ",paste0(risiko_out,collapse=", "),
                                                           "<br><br>Analyse erfolgreich durchgeführt")))
            }
            if(length(unique(fn$x$nodes$Group2))>1){
                output$FA_erweitert_UI2<-renderUI({actionBttn("do_erweiterteFA",
                                                              label = tagList("+ weitere besuchte Stationen",tags$br(),
                                                                              paste0("(Fall: ",fall_ist,", Länge Infektionskette: ",input$Grad_Intern,")")),
                                                              style = "gradient",color = "primary",block=T)})
            }else{
                output$FA_erweitert_UI2<-renderUI({NULL})
            }
            if(length(unique(fn$x$nodes$AndereFAs[!is.na(fn$x$nodes$AndereFAs)&fn$x$nodes$AndereFAs!="-"]))>0){
                output$FA_erweitert_UI3<-renderUI({actionBttn("do_erweiterteFA2",
                                                              label = tagList("+ weitere besuchte Stationen",tags$br(),
                                                                              paste0("(Fall: alle, Länge Infektionskette: 1)")),
                                                              style = "gradient",color = "primary",block=T)})
            }else{
                output$FA_erweitert_UI3<-renderUI({NULL})
            }
        }

        button_do_erweiterteFA(FALSE)
        button_do_erweiterteFA2(FALSE)
        
        rv$nodes1<-nodes1
        rv$links1<-links1
        rv$backup_all<-backup_all
        rv$fall_ist<-fall_ist
        rv$fa_ist<-fa_ist
        rv$FA_Start<-FA_Start
        rv$FA_Ende<-FA_Ende
        
        shinyjs::html("text", paste0("<br>Analyse erfolgreich durchgeführt<br><br>"), add = TRUE)


        ##erweitert nur für Fall im Fokus: analoge analyse durchführen, nur für andere FAs
        observeEvent(input$do_erweiterteFA,ignoreInit = TRUE,{
          
            if(!button_do_erweiterteFA()){
                button_do_erweiterteFA(TRUE)
              button_do_erweiterteFA2(FALSE)
            
              nodes1<-rv$nodes1
              links1<-rv$links1
              backup_all<-rv$backup_all
              fall_ist<-rv$fall_ist
              fa_ist<-rv$fa_ist
              
              nodes1_backup<-nodes1
              links1_backup<-links1
              backup_all_backup<-backup_all
              
              fall_ist_backup<-fall_ist
              fa_ist_backup<-fa_ist

              FA_Start<-rv$FA_Start
              FA_Ende<-rv$FA_Ende
            
            if(input$Grad_Intern=="unbegrenzt"){
              kette_soll<-100
            }else{
              kette_soll<-as.numeric(input$Grad_Intern)
            }
            
            output$force<-renderForceNetwork({NULL})
            
            support_abteilungen<-unique(unique(nodes1$Group[nodes1$ID2==fall_ist]))
            support_abteilungen<-support_abteilungen[support_abteilungen!=fa_ist_backup]
            
            for(neue_abteilung in support_abteilungen){
                if(input$FA_oder_Station=="Fachabteilungen"){
                  shinyjs::html("text", paste0("<br>Erweitere Analyse für weitere Fachabteilungen (Fall im Fokus).<br><br>"), add = FALSE)
                  
                    input1<-input1a
                    input1<-input1[input1$Abteilung!="Patient_abwesend",]
                    input1<-input1[order(input1$Fallnummer),]
                    fa<-sort(unique(input1$Abteilung))
                    
                    input1_fa<-input1[input1$Abteilung==neue_abteilung,]
                    
                    FA_ICD<-23
                    
                    if(input$switch99=="ICD-Kapitel"){
                        input1_fa<-input1_fa[input1_fa$ICDKapitel%in%input$FA_ICDKapitel,]
                        FA_ICD<-input$FA_ICDKapitel
                    }
                    if(input$switch99=="exakter ICD-Code"){
                        user_icd<-gsub(".","\\.",input$FA_ICDExakt,fixed=T)
                        input1_fa<-input1_fa[grep(paste0("^",user_icd),input1_fa$ICDfull),]
                        FA_ICD<-input$FA_ICDExakt
                    }
                    
                    fall<-sort(unique(input1_fa$Fallnummer))
                    
                    if(!is.null(input$Fall_Fokus_FA)){
                        input1_fall<-as.data.frame(input1_fa[input1_fa$Fallnummer==input$Fall_Fokus_FA&
                                                                 as.numeric(input1_fa$Aufnahme)>=as.numeric(FA_Start)&
                                                                 as.numeric(input1_fa$Aufnahme)<=as.numeric(FA_Ende),])
                        #input1_fall<-as.data.frame(input1_fa[input1_fa$Fallnummer==fall[1],])
                        
                        ausgeschlossen<-c()
                        if(nrow(input1_fall)>1){
                            for(i in 1:(nrow(input1_fall)-1)){
                                anfang<-as.Date(input1_fall$Entlassung[i],format="%Y-%m-%d")+1
                                ende<-as.Date(input1_fall$Aufnahme[i+1],format="%Y-%m-%d")-1
                                if(anfang==ende){
                                    ausgeschlossen<-c(as.Date(ausgeschlossen),anfang)
                                }
                                if(anfang<ende){
                                    ausgeschlossen<-c(as.Date(ausgeschlossen),as.Date(anfang:ende))
                                }
                            }
                        }
                    }
                }
                
                if(input$FA_oder_Station=="Stationen"){
                  shinyjs::html("text", paste0("<br>Erweitere Analyse für weitere Stationen (Fall im Fokus).<br><br>"), add = FALSE)
                  
                    input1<-input1b
                    names(input1)[3]<-"Abteilung"
                    input1<-input1[input1$Abteilung!="Patient_abwesend",]
                    input1<-input1[order(input1$Fallnummer),]
                    fa<-sort(unique(input1$Abteilung))
                    
                    input1_fa<-input1[input1$Abteilung==neue_abteilung,]
                    
                    FA_ICD<-23
                    
                    if(input$switch99c=="ICD-Kapitel"){
                        input1_fa<-input1_fa[input1_fa$ICDKapitel%in%input$ST_ICDKapitel,]
                        FA_ICD<-input$ST_ICDKapitel
                    }
                    if(input$switch99c=="exakter ICD-Code"){
                        user_icd<-gsub(".","\\.",input$ST_ICDExakt,fixed=T)
                        input1_fa<-input1_fa[grep(paste0("^",user_icd),input1_fa$ICDfull),] 
                        FA_ICD<-input$ST_ICDExakt
                    }
                    
                    fall<-sort(unique(input1_fa$Fallnummer))
                    
                    input1_fall<-as.data.frame(input1_fa[input1_fa$Fallnummer==input$Fall_Fokus_ST&
                                                             as.numeric(input1_fa$Aufnahme)>=as.numeric(FA_Start)&
                                                             as.numeric(input1_fa$Aufnahme)<=as.numeric(FA_Ende),])
                    
                    ausgeschlossen<-c()
                    if(nrow(input1_fall)>1){
                        for(i in 1:(nrow(input1_fall)-1)){
                            anfang<-as.Date(input1_fall$Entlassung[i],format="%Y-%m-%d")+1
                            ende<-as.Date(input1_fall$Aufnahme[i+1],format="%Y-%m-%d")-1
                            if(anfang==ende){
                                ausgeschlossen<-c(as.Date(ausgeschlossen),anfang)
                            }
                            if(anfang<ende){
                                ausgeschlossen<-c(as.Date(ausgeschlossen),as.Date(anfang:ende))
                            }
                        }
                    }
                }
                
                FA_Fokus<-neue_abteilung
                Fall_Fokus_FA<-input$Fall_Fokus_FA
                ST_Fokus<-neue_abteilung
                Fall_Fokus_ST<-input$Fall_Fokus_ST
                
                if(input$FA_oder_Station=="Fachabteilungen"){
                    FA_Start<-input$FA_Start
                    FA_Ende<-input$FA_Ende
                    fall_ist<-Fall_Fokus_FA
                    fa_ist<-FA_Fokus
                    ST_Fokus<-NULL
                    Fall_Fokus_ST<-NULL
                }else{
                    FA_Start<-input$ST_Start
                    FA_Ende<-input$ST_Ende
                    fall_ist<-Fall_Fokus_ST
                    fa_ist<-ST_Fokus
                    FA_Fokus<-NULL
                    Fall_Fokus_FA<-NULL
                }
                
                
                if(sum(as.numeric(FA_Start)==as.numeric(as.Date(input1_fall$Aufnahme,format="%Y-%m-%d")))>=1){
                    FA_Start<-min(input1_fall$Aufnahme[which(as.numeric(FA_Start)==as.numeric(as.Date(input1_fall$Aufnahme,format="%Y-%m-%d")))])
                }else{
                    FA_Start<-as.POSIXct(paste0(FA_Start," 00:00:01"),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                }
                FA_Ende<-as.POSIXct(paste0(FA_Ende," 23:59:59"),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                
                ##Für Fall bestimmen, wann er da ist und wann er davon ansteckend ist
                input1_fall_filtered<-input1_fall[as.numeric(input1_fall$Aufnahme)<=as.numeric(FA_Ende)
                                                  &as.numeric(input1_fall$Entlassung)>=as.numeric(FA_Start),]
                
                input1_fall_filtered$Aufnahme[1]<-max(input1_fall_filtered$Aufnahme[1],FA_Start)
                
                ##sobald nicht mehr ansteckend, sind alle weiteren aufenthalte + kontakte auf der FA egal -> filtern
                input1_fall_filtered<-as.data.frame(input1_fall_filtered[input1_fall_filtered$Aufnahme<=as.POSIXct(as.numeric(input1_fall_filtered$Aufnahme[1])+ansteckendzeit*60*60*24,tz="UTC"),])
                for(zeile in nrow(input1_fall_filtered)){
                    input1_fall_filtered$Entlassung[zeile]<-min(input1_fall_filtered$Entlassung[zeile],
                                                                as.POSIXct(as.numeric(input1_fall_filtered$Aufnahme[zeile])+ansteckendzeit*60*60*24,tz="UTC"))
                }
                input1_fall_filtered$Dauer<-(as.numeric(input1_fall_filtered$Entlassung)-as.numeric(input1_fall_filtered$Aufnahme))/(60*60*24)
                
                
                
                ##dann dafür alle potenziellen Kontakte bestimmen
                input1_fa_filtered<-input1_fa[as.numeric(input1_fa$Aufnahme)<=as.numeric(max(input1_fall_filtered$Entlassung))
                                              &as.numeric(input1_fa$Entlassung)>=as.numeric(min(input1_fall_filtered$Aufnahme)),]
                
                if(sum(input1_fa_filtered$Fallnummer!=fall_ist)>0){
                    for(i in 1:nrow(input1_fa_filtered)){
                        input1_fa_filtered$Aufnahme[i]<-max(input1_fa_filtered$Aufnahme[i],FA_Start)
                        input1_fa_filtered$Entlassung[i]<-min(input1_fa_filtered$Entlassung[i],FA_Ende)
                    }
                    input1_fa_filtered$Dauer<-(as.numeric(input1_fa_filtered$Entlassung)-as.numeric(input1_fa_filtered$Aufnahme))/(60*60*24)
                    
                    
                    ##vorläufig: jeder Kontakt muss Min-Zeit erfüllen, wenn 2x Kontakt und keiner erfüllt es einzeln, aber in der Summe schon,
                    ##wird es aktuell ausgeschlossen
                    ##Option zum Aufsummieren ist aber möglich - wenn gewünscht
                    input1_fa_filtered2<-input1_fa_filtered[input1_fa_filtered$Dauer>=kontaktzeit,]
                    
                    if(sum(input1_fa_filtered2$Fallnummer==unique(input1_fall_filtered$Fallnummer))!=nrow(input1_fa_filtered2)){
                        ###Netzwerk
                        farben<-c("cadetblue3","firebrick","darkgoldenrod1","aquamarine","darksalmon","lightgoldenrod4",colorRampPalette(c("red","blue"))(100))
                        
                        temp_intervalle<-TimeIntervalDataFrame(start=input1_fa_filtered2$Aufnahme,end=input1_fa_filtered2$Entlassung)
                        temp_overlapping<-overlapping(temp_intervalle,idx = T)
                        
                        
                        kette<-1
                        ids_in<-c(unique(input1_fall_filtered$Fallnummer))
                        
                        progress <- shiny::Progress$new()
                        progress$set(message = paste0(neue_abteilung,": Bestimme Kontakte"), value = 0)
                        progress$inc(0/kette_soll,detail=paste0(kette,". Grades"))
                        #while(kette<=kette_soll){
                        #   message("Kette: ",kette)
                        temp_overlapping2<-rbind(temp_overlapping[temp_overlapping$i%in%which(input1_fa_filtered2$Fallnummer%in%ids_in),],
                                                 temp_overlapping[temp_overlapping$j%in%which(input1_fa_filtered2$Fallnummer%in%ids_in),])
                        
                        if(nrow(temp_overlapping2)>0){
                            ##Start: Kettenlänge 1
                            temp_overlapping2$Patientennummer_i<-input1_fa_filtered2$Patientennummer[temp_overlapping2$i]
                            temp_overlapping2$Fallnummer_i<-input1_fa_filtered2$Fallnummer[temp_overlapping2$i]
                            temp_overlapping2$Patientennummer_j<-input1_fa_filtered2$Patientennummer[temp_overlapping2$j]
                            temp_overlapping2$Fallnummer_j<-input1_fa_filtered2$Fallnummer[temp_overlapping2$j]
                            
                            temp_overlapping2$Start_i<-input1_fa_filtered2$Aufnahme[temp_overlapping2$i]
                            temp_overlapping2$Ende_i<-input1_fa_filtered2$Entlassung[temp_overlapping2$i]
                            temp_overlapping2$Start_j<-input1_fa_filtered2$Aufnahme[temp_overlapping2$j]
                            temp_overlapping2$Ende_j<-input1_fa_filtered2$Entlassung[temp_overlapping2$j]
                            
                            helper<-cbind(as.numeric(input1_fa_filtered2$Aufnahme[temp_overlapping2$i]),as.numeric(input1_fa_filtered2$Aufnahme[temp_overlapping2$j]))
                            helper_start<-apply(helper,1,max)
                            helper<-cbind(as.numeric(input1_fa_filtered2$Entlassung[temp_overlapping2$i]),as.numeric(input1_fa_filtered2$Entlassung[temp_overlapping2$j]))
                            helper_ende<-apply(helper,1,min)
                            temp_overlapping2$Dauer<-(helper_ende-helper_start)/(60*60*24)
                            
                            ##Filtern auf Mindest-Kontaktzeit
                            if(input$Fall_Inkubation==F){
                                temp_overlapping2<-temp_overlapping2[temp_overlapping2$Dauer>=kontaktzeit,]
                            }else{
                                temp_overlapping2<-temp_overlapping2[temp_overlapping2$Dauer>=(kontaktzeit+inkubationszeit),]
                            }
                            
                            ##prüfen, dass keine Kombination 2x oder mehr vorkommt
                            help_test<-paste(temp_overlapping2$Fallnummer_i,temp_overlapping2$Fallnummer_j,sep = "_")
                            help_test_table<-table(help_test)
                            help_test_table<-help_test_table[help_test_table>1]
                            
                            if(length(help_test_table)>0){
                                help_test<-str_split_fixed(names(help_test_table),"_",Inf)
                                
                                temp_overlapping_neu<-temp_overlapping2
                                temp_overlapping_neu$Erster<-TRUE
                                for(k in 1:nrow(help_test)){
                                    temp_overlapping_neu$Dauer[temp_overlapping_neu$Fallnummer_i==help_test[k,1]&
                                                                   temp_overlapping_neu$Fallnummer_j==help_test[k,2]]<-sum(temp_overlapping_neu$Dauer[temp_overlapping_neu$Fallnummer_i==help_test[k,1]&
                                                                                                                                                          temp_overlapping_neu$Fallnummer_j==help_test[k,2]])
                                    
                                    temp_overlapping_neu[temp_overlapping_neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu$Fallnummer_j==help_test[k,2],]$Erster[temp_overlapping_neu$Start_i[temp_overlapping_neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu$Fallnummer_j==help_test[k,2]]!=min(temp_overlapping_neu$Start_i[temp_overlapping_neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu$Fallnummer_j==help_test[k,2]])|
                                                                                                                                                                          temp_overlapping_neu$Start_j[temp_overlapping_neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu$Fallnummer_j==help_test[k,2]]!=min(temp_overlapping_neu$Start_j[temp_overlapping_neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu$Fallnummer_j==help_test[k,2]])]<-F
                                }
                                temp_overlapping_neu<-temp_overlapping_neu[temp_overlapping_neu$Erster==T,]
                                temp_overlapping_neu$Infiziert<-as.POSIXct(ifelse(temp_overlapping_neu$Fallnummer_i==ids_in,
                                                                                  as.numeric(temp_overlapping_neu$Start_j+kontaktzeit*60*60*24),
                                                                                  as.numeric(temp_overlapping_neu$Start_i+kontaktzeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                temp_overlapping_neu$Ansteckend<-as.POSIXct(ifelse(temp_overlapping_neu$Fallnummer_i==ids_in,
                                                                                   as.numeric(temp_overlapping_neu$Start_j+kontaktzeit*60*60*24+inkubationszeit*60*60*24),
                                                                                   as.numeric(temp_overlapping_neu$Start_i+kontaktzeit*60*60*24+inkubationszeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                temp_overlapping_neu$AnsteckendBis<-as.POSIXct(ifelse(temp_overlapping_neu$Fallnummer_i==ids_in,
                                                                                      as.numeric(temp_overlapping_neu$Start_j+ansteckendzeit*60*60*24+kontaktzeit*60*60*24+inkubationszeit*60*60*24),
                                                                                      as.numeric(temp_overlapping_neu$Start_i+ansteckendzeit*60*60*24+kontaktzeit*60*60*24+inkubationszeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                temp_overlapping_neu<-temp_overlapping_neu[,-c(1:2,7:10,12)]
                                temp_overlapping_neu<-unique(temp_overlapping_neu)
                            }else{
                                temp_overlapping2$Infiziert<-as.POSIXct(ifelse(temp_overlapping2$Fallnummer_i==ids_in,
                                                                               as.numeric(temp_overlapping2$Start_j+kontaktzeit*60*60*24),
                                                                               as.numeric(temp_overlapping2$Start_i+kontaktzeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                temp_overlapping2$Ansteckend<-as.POSIXct(ifelse(temp_overlapping2$Fallnummer_i==ids_in,
                                                                                as.numeric(temp_overlapping2$Start_j+kontaktzeit*60*60*24+inkubationszeit*60*60*24),
                                                                                as.numeric(temp_overlapping2$Start_i+kontaktzeit*60*60*24+inkubationszeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                temp_overlapping2$AnsteckendBis<-as.POSIXct(ifelse(temp_overlapping2$Fallnummer_i==ids_in,
                                                                                   as.numeric(temp_overlapping2$Start_j+ansteckendzeit*60*60*24+kontaktzeit*60*60*24+inkubationszeit*60*60*24),
                                                                                   as.numeric(temp_overlapping2$Start_i+ansteckendzeit*60*60*24+kontaktzeit*60*60*24+inkubationszeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                temp_overlapping_neu<-temp_overlapping2[,-c(1:2,7:10)]
                            }
                            temp_overlapping_neu$Grad<-1
                            backup_all<-temp_overlapping_neu
                            backup_all$RichtigeID<-ifelse(backup_all$Fallnummer_i==ids_in,backup_all$Fallnummer_j,backup_all$Fallnummer_i)
                            backup_all$Anstecker<-ifelse(backup_all$Fallnummer_i==ids_in,backup_all$Fallnummer_i,backup_all$Fallnummer_j)
                            
                            backup_all<-backup_all[!backup_all$RichtigeID%in%nodes1$ID2,]
                            if(nrow(backup_all)>0){
                            
                            kette<-kette+1
                            
                            ##von jedem 1. Kontakt von Fall im Fokus aus neu analysieren
                            while(kette<=kette_soll){
                                progress$inc(1/kette_soll,detail=paste0(kette,". Grades"))
                                
                                ids_neu<-backup_all$RichtigeID[!backup_all$RichtigeID%in%ids_in]
                                progress2 <- shiny::Progress$new()
                                progress2$set(message = "Analysiere ID", value = 0)
                                for(id in ids_neu){
                                    message(id)
                                    progress2$inc(1/length(ids_neu),detail=id)
                                    
                                    FA_Start_Neu<-backup_all$Ansteckend[backup_all$RichtigeID==id]
                                    ##Für neuen Fall bestimmen, wann er da ist und wann er davon ansteckend ist
                                    input1_fall_filtered_Neu<-input1_fa[input1_fa$Fallnummer==id
                                                                        &as.numeric(input1_fa$Aufnahme)<=as.numeric(FA_Ende)
                                                                        &as.numeric(input1_fa$Entlassung)>=as.numeric(FA_Start_Neu),]
                                    ##wenn entlassen bevor ansteckend -> filtern
                                    if(nrow(input1_fall_filtered_Neu)>0){
                                      input1_fall_filtered_Neu$Aufnahme[1]<-FA_Start_Neu
                                      ##sobald nicht mehr ansteckend, sind alle weiteren aufenthalte + kontakte auf der FA egal -> filtern
                                      input1_fall_filtered_Neu<-as.data.frame(input1_fall_filtered_Neu[input1_fall_filtered_Neu$Aufnahme<=input1_fall_filtered_Neu$Aufnahme[1],])
                                      if(nrow(input1_fall_filtered_Neu)>0){
                                        for(zeile in nrow(input1_fall_filtered_Neu)){
                                          input1_fall_filtered_Neu$Entlassung[zeile]<-min(input1_fall_filtered_Neu$Entlassung[zeile],
                                                                                          as.POSIXct(as.numeric(input1_fall_filtered_Neu$Aufnahme[zeile])+ansteckendzeit*60*60*24,tz="UTC"))
                                        }
                                      }
                                    input1_fall_filtered_Neu$Dauer<-(as.numeric(input1_fall_filtered_Neu$Entlassung)-as.numeric(input1_fall_filtered_Neu$Aufnahme))/(60*60*24)
                                    
                                    if(sum(input1_fall_filtered_Neu$Dauer>=kontaktzeit)>0){
                                        ##dann dafür alle potenziellen Kontakte bestimmen
                                        input1_fa_filtered_Neu<-input1_fa[as.numeric(input1_fa$Aufnahme)<=as.numeric(max(input1_fall_filtered_Neu$Entlassung))
                                                                          &as.numeric(input1_fa$Entlassung)>=as.numeric(min(input1_fall_filtered_Neu$Aufnahme))
                                                                          &!input1_fa$Fallnummer%in%nodes1$ID2,]#Vereinfachung! später besser machen
                                        ##aktuelle Annahme: ein fall besucht 2 abteilungen, ist aber nicht fall im fokus, eine Infektion kann auf beiden abteilungen
                                        ##passiert sein, da voraussetzungen gegeben sind - annahme: infektion findet immer auf der abteilung im fokus statt, kein
                                        ##vergleich zu anderer abteilung, falls da vll doch früher -> sehr spezieller fall, erst mal egal
                                        ##falls >1 weitere abteilung: alphabetisch behandelt, erst mal immer nodes1 genommen
                                        if(nrow(input1_fa_filtered_Neu)!=0){
                                            for(i in 1:nrow(input1_fa_filtered_Neu)){
                                                input1_fa_filtered_Neu$Aufnahme[i]<-max(input1_fa_filtered_Neu$Aufnahme[i],FA_Start_Neu)
                                                input1_fa_filtered_Neu$Entlassung[i]<-min(input1_fa_filtered_Neu$Entlassung[i],FA_Ende)
                                            }
                                            input1_fa_filtered_Neu$Dauer<-(as.numeric(input1_fa_filtered_Neu$Entlassung)-as.numeric(input1_fa_filtered_Neu$Aufnahme))/(60*60*24)
                                            ##ab Start ist id ansteckend, jetzt nur noch betrachten, wo Aufenthalt anderer Fälle lang genug für Ansteckung ist
                                            input1_fa_filtered2_Neu<-input1_fa_filtered_Neu[input1_fa_filtered_Neu$Dauer>=kontaktzeit,]
                                            
                                            if(sum(input1_fa_filtered2_Neu$Fallnummer!=id)!=0){
                                                temp_intervalle_Neu<-TimeIntervalDataFrame(start=input1_fa_filtered2_Neu$Aufnahme,end=input1_fa_filtered2_Neu$Entlassung)
                                                temp_overlapping_Neu<-overlapping(temp_intervalle_Neu,idx = T)
                                                
                                                temp_overlapping2_Neu<-rbind(temp_overlapping_Neu[temp_overlapping_Neu$i%in%which(input1_fa_filtered2_Neu$Fallnummer%in%id),],
                                                                             temp_overlapping_Neu[temp_overlapping_Neu$j%in%which(input1_fa_filtered2_Neu$Fallnummer%in%id),])
                                                if(nrow(temp_overlapping2_Neu)>0){
                                                    ##Kettenlänge 2
                                                    temp_overlapping2_Neu$Patientennummer_i<-input1_fa_filtered2_Neu$Patientennummer[temp_overlapping2_Neu$i]
                                                    temp_overlapping2_Neu$Fallnummer_i<-input1_fa_filtered2_Neu$Fallnummer[temp_overlapping2_Neu$i]
                                                    temp_overlapping2_Neu$Patientennummer_j<-input1_fa_filtered2_Neu$Patientennummer[temp_overlapping2_Neu$j]
                                                    temp_overlapping2_Neu$Fallnummer_j<-input1_fa_filtered2_Neu$Fallnummer[temp_overlapping2_Neu$j]
                                                    
                                                    temp_overlapping2_Neu$Start_i<-input1_fa_filtered2_Neu$Aufnahme[temp_overlapping2_Neu$i]
                                                    temp_overlapping2_Neu$Ende_i<-input1_fa_filtered2_Neu$Entlassung[temp_overlapping2_Neu$i]
                                                    temp_overlapping2_Neu$Start_j<-input1_fa_filtered2_Neu$Aufnahme[temp_overlapping2_Neu$j]
                                                    temp_overlapping2_Neu$Ende_j<-input1_fa_filtered2_Neu$Entlassung[temp_overlapping2_Neu$j]
                                                    
                                                    helper<-cbind(as.numeric(input1_fa_filtered2_Neu$Aufnahme[temp_overlapping2_Neu$i]),as.numeric(input1_fa_filtered2_Neu$Aufnahme[temp_overlapping2_Neu$j]))
                                                    helper_start<-apply(helper,1,max)
                                                    helper<-cbind(as.numeric(input1_fa_filtered2_Neu$Entlassung[temp_overlapping2_Neu$i]),as.numeric(input1_fa_filtered2_Neu$Entlassung[temp_overlapping2_Neu$j]))
                                                    helper_ende<-apply(helper,1,min)
                                                    temp_overlapping2_Neu$Dauer<-(helper_ende-helper_start)/(60*60*24)
                                                    
                                                    ##Filtern auf Mindest-Kontaktzeit
                                                    temp_overlapping2_Neu<-temp_overlapping2_Neu[temp_overlapping2_Neu$Dauer>=kontaktzeit,]
                                                    
                                                    if(nrow(temp_overlapping2_Neu)>0){
                                                        ##prüfen, dass keine Kombination 2x oder mehr vorkommt
                                                        help_test<-paste(temp_overlapping2_Neu$Fallnummer_i,temp_overlapping2_Neu$Fallnummer_j,sep = "_")
                                                        help_test_table<-table(help_test)
                                                        help_test_table<-help_test_table[help_test_table>1]
                                                        
                                                        if(length(help_test_table)>0){
                                                            help_test<-str_split_fixed(names(help_test_table),"_",Inf)
                                                            
                                                            temp_overlapping_neu_Neu<-temp_overlapping2_Neu
                                                            temp_overlapping_neu_Neu$Erster<-TRUE
                                                            for(k in 1:nrow(help_test)){
                                                                temp_overlapping_neu_Neu$Dauer[temp_overlapping_neu_Neu$Fallnummer_i==help_test[k,1]&
                                                                                                   temp_overlapping_neu_Neu$Fallnummer_j==help_test[k,2]]<-sum(temp_overlapping_neu_Neu$Dauer[temp_overlapping_neu_Neu$Fallnummer_i==help_test[k,1]&
                                                                                                                                                                                                  temp_overlapping_neu_Neu$Fallnummer_j==help_test[k,2]])
                                                                
                                                                temp_overlapping_neu_Neu[temp_overlapping_neu_Neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu_Neu$Fallnummer_j==help_test[k,2],]$Erster[temp_overlapping_neu_Neu$Start_i[temp_overlapping_neu_Neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu_Neu$Fallnummer_j==help_test[k,2]]!=min(temp_overlapping_neu_Neu$Start_i[temp_overlapping_neu_Neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu_Neu$Fallnummer_j==help_test[k,2]])|
                                                                                                                                                                                                                  temp_overlapping_neu_Neu$Start_j[temp_overlapping_neu_Neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu_Neu$Fallnummer_j==help_test[k,2]]!=min(temp_overlapping_neu_Neu$Start_j[temp_overlapping_neu_Neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu_Neu$Fallnummer_j==help_test[k,2]])]<-F
                                                            }
                                                            temp_overlapping_neu_Neu<-temp_overlapping_neu_Neu[temp_overlapping_neu_Neu$Erster==T,]
                                                            temp_overlapping_neu_Neu$Infiziert<-as.POSIXct(ifelse(temp_overlapping_neu_Neu$Fallnummer_i==id,
                                                                                                                  as.numeric(temp_overlapping_neu_Neu$Start_j+kontaktzeit*60*60*24),
                                                                                                                  as.numeric(temp_overlapping_neu_Neu$Start_i+kontaktzeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                                            temp_overlapping_neu_Neu$Ansteckend<-as.POSIXct(ifelse(temp_overlapping_neu_Neu$Fallnummer_i==id,
                                                                                                                   as.numeric(temp_overlapping_neu_Neu$Start_j+kontaktzeit*60*60*24+inkubationszeit*60*60*24),
                                                                                                                   as.numeric(temp_overlapping_neu_Neu$Start_i+kontaktzeit*60*60*24+inkubationszeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                                            temp_overlapping_neu_Neu$AnsteckendBis<-as.POSIXct(ifelse(temp_overlapping_neu_Neu$Fallnummer_i==id,
                                                                                                                      as.numeric(temp_overlapping_neu_Neu$Start_j+ansteckendzeit*60*60*24+kontaktzeit*60*60*24+inkubationszeit*60*60*24),
                                                                                                                      as.numeric(temp_overlapping_neu_Neu$Start_i+ansteckendzeit*60*60*24+kontaktzeit*60*60*24+inkubationszeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                                            temp_overlapping_neu_Neu<-temp_overlapping_neu_Neu[,-c(1:2,7:10,12)]
                                                            temp_overlapping_neu_Neu<-unique(temp_overlapping_neu_Neu)
                                                        }else{
                                                            temp_overlapping2_Neu$Infiziert<-as.POSIXct(ifelse(temp_overlapping2_Neu$Fallnummer_i==id,
                                                                                                               as.numeric(temp_overlapping2_Neu$Start_j+kontaktzeit*60*60*24),
                                                                                                               as.numeric(temp_overlapping2_Neu$Start_i+kontaktzeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                                            temp_overlapping2_Neu$Ansteckend<-as.POSIXct(ifelse(temp_overlapping2_Neu$Fallnummer_i==id,
                                                                                                                as.numeric(temp_overlapping2_Neu$Start_j+kontaktzeit*60*60*24+inkubationszeit*60*60*24),
                                                                                                                as.numeric(temp_overlapping2_Neu$Start_i+kontaktzeit*60*60*24+inkubationszeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                                            temp_overlapping2_Neu$AnsteckendBis<-as.POSIXct(ifelse(temp_overlapping2_Neu$Fallnummer_i==id,
                                                                                                                   as.numeric(temp_overlapping2_Neu$Start_j+ansteckendzeit*60*60*24+kontaktzeit*60*60*24+inkubationszeit*60*60*24),
                                                                                                                   as.numeric(temp_overlapping2_Neu$Start_i+ansteckendzeit*60*60*24+kontaktzeit*60*60*24+inkubationszeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                                            temp_overlapping_neu_Neu<-temp_overlapping2_Neu[,-c(1:2,7:10)]
                                                        }
                                                        
                                                        temp_overlapping_neu_Neu$Grad<-kette
                                                        temp_overlapping_neu_Neu$RichtigeID<-ifelse(temp_overlapping_neu_Neu$Fallnummer_i==id,temp_overlapping_neu_Neu$Fallnummer_j,temp_overlapping_neu_Neu$Fallnummer_i)
                                                        temp_overlapping_neu_Neu$Anstecker<-ifelse(temp_overlapping_neu_Neu$Fallnummer_i==id,temp_overlapping_neu_Neu$Fallnummer_i,temp_overlapping_neu_Neu$Fallnummer_j)
                                                        ##überprüfen, ob zeit bis zur Ansteckung mit höherem Grad evt schneller ist als mit niedrigerem?
                                                        for(zeile in 1:nrow(backup_all)){
                                                            if(sum(backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID)>0&&
                                                               backup_all$Infiziert[zeile]>temp_overlapping_neu_Neu$Infiziert[backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID]){
                                                                backup_all$Infiziert[zeile]<-temp_overlapping_neu_Neu$Infiziert[backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID]
                                                                backup_all$Ansteckend[zeile]<-temp_overlapping_neu_Neu$Ansteckend[backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID]
                                                                backup_all$AnsteckendBis[zeile]<-temp_overlapping_neu_Neu$AnsteckendBis[backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID]
                                                                backup_all$Dauer[zeile]<-temp_overlapping_neu_Neu$Dauer[backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID]
                                                                backup_all$Grad[zeile]<-temp_overlapping_neu_Neu$Grad[backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID]
                                                                backup_all$Anstecker[zeile]<-temp_overlapping_neu_Neu$Anstecker[backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID]
                                                            }
                                                        }
                                                        
                                                        neue_zeilen<-unique(c(temp_overlapping_neu_Neu$Fallnummer_i,temp_overlapping_neu_Neu$Fallnummer_j))[!unique(c(temp_overlapping_neu_Neu$Fallnummer_i,temp_overlapping_neu_Neu$Fallnummer_j))%in%unique(c(backup_all$Fallnummer_i,backup_all$Fallnummer_j))]
                                                        if(length(neue_zeilen)>0){
                                                            backup_temp<-temp_overlapping_neu_Neu[temp_overlapping_neu_Neu$Fallnummer_i%in%neue_zeilen|temp_overlapping_neu_Neu$Fallnummer_j%in%neue_zeilen,]
                                                            backup_temp$RichtigeID<-ifelse(backup_temp$Fallnummer_i==id,backup_temp$Fallnummer_j,backup_temp$Fallnummer_i)
                                                            backup_temp$Anstecker<-ifelse(backup_temp$Fallnummer_i==id,backup_temp$Fallnummer_i,backup_temp$Fallnummer_j)
                                                            backup_all<-rbind(backup_all,backup_temp)
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    }
                                }
                                progress2$close()
                                if(sum(backup_all$Grad==kette)==0){
                                    kette<-kette_soll
                                }
                                kette<-kette+1
                            }
                            progress$close()
                            
                            ##alles notwendige steht in backup_all
                            ##nur noch Knoten für Fall im Fokus hinzufügen, auch für andere FAs wo der war
                            backup_all$Group<-fa_ist
                            backup_all<-rbind(backup_all,NA)
                            backup_all$Patientennummer_i[nrow(backup_all)]<-unique(input1_fall_filtered$Patientennummer)
                            backup_all$Fallnummer_i[nrow(backup_all)]<-unique(input1_fall_filtered$Fallnummer)
                            backup_all$Patientennummer_j[nrow(backup_all)]<-unique(input1_fall_filtered$Patientennummer)
                            backup_all$Fallnummer_j[nrow(backup_all)]<-unique(input1_fall_filtered$Fallnummer)
                            backup_all$Infiziert[nrow(backup_all)]<-min(input1_fall_filtered$Aufnahme,FA_Start)
                            backup_all$Ansteckend[nrow(backup_all)]<-min(input1_fall_filtered$Aufnahme)
                            backup_all$AnsteckendBis[nrow(backup_all)]<-min(max(input1_fall_filtered$Entlassung),as.POSIXct(as.numeric(backup_all$Ansteckend[nrow(backup_all)]+ansteckendzeit*60*60*24),tz="UTC"))
                            backup_all$Grad[nrow(backup_all)]<-0
                            backup_all$RichtigeID[nrow(backup_all)]<-unique(input1_fall_filtered$Fallnummer)
                            backup_all$RichtigeID2<-backup_all$RichtigeID
                            backup_all$Anstecker[nrow(backup_all)]<-"-"
                            backup_all$Group[nrow(backup_all)]<-fa_ist
                            
                            backup_all$Dauer<-(as.numeric(backup_all$AnsteckendBis)-as.numeric(backup_all$Ansteckend))/(60*60*24)
                            for(i in 1:nrow(backup_all)){
                                investigate<-input1_fa[input1_fa$Fallnummer==backup_all$RichtigeID2[i],]
                                investigate<-as.data.frame(investigate[investigate$Entlassung>=backup_all$Ansteckend[i]&investigate$Aufnahme<=backup_all$AnsteckendBis[i],])
                                investigate$Aufnahme[investigate$Aufnahme<backup_all$Ansteckend[i]]<-backup_all$Ansteckend[i]
                                if(nrow(investigate)>0){
                                    backup_all$AnsteckendBis[i]<-min(backup_all$AnsteckendBis[i],max(investigate$Entlassung))
                                    backup_all$Dauer[i]<-(as.numeric(backup_all$AnsteckendBis[i])-as.numeric(backup_all$Ansteckend[i]))/(60*60*24)
                                    if(nrow(investigate)>1){
                                        backup_all$Dauer[i]<-backup_all$Dauer[i]-sum((as.numeric(investigate$Aufnahme[2:nrow(investigate)])-as.numeric(investigate$Entlassung[1:(nrow(investigate)-1)]))/(60*60*24))
                                    }
                                }else{
                                    backup_all$Dauer[i]<--1##später extra bezeichnen
                                }
                            }
                            
                            backup_all$ICDfull<-input1$ICDfull[match(backup_all$RichtigeID2,input1$Fallnummer)]
                            
                            ##Links und Nodes definieren
                            links1<-backup_all[,c("Anstecker","RichtigeID")]
                            links1$Anstecker[links1$Anstecker=="-"]<-fall_ist
                            names(links1)<-c("Source","Target")
                            links1$Group<-backup_all$Group
                            links1$Anstecker<-backup_all$Anstecker
                            links1$Farbe<-"#d1d1d1"
                            links1$Farbe[links1$Anstecker=="-"]<-"black"
                            
                            nodes1<-data.frame(NodeID=backup_all$RichtigeID,ID=seq(0,(nrow(backup_all)-1)))
                            links1$Source1<-nodes1$ID[match(links1$Source,nodes1$NodeID)]
                            links1$Target1<-nodes1$ID[match(links1$Target,nodes1$NodeID)]
                            
                            nodes1$NodeSize<-nodes1$Dauer<-as.numeric(backup_all$Dauer)
                            nodes1$NodeSizeb<-2+8*nodes1$NodeSize/max(nodes1$NodeSize)
                            nodes1$Group<-backup_all$Group
                            nodes1$ICDfull<-backup_all$ICDfull
                            nodes1$Grad<-backup_all$Grad
                            nodes1$Infiziert<-backup_all$Infiziert
                            nodes1$Ansteckend<-backup_all$Ansteckend
                            nodes1$AnsteckendBis<-backup_all$AnsteckendBis
                            nodes1$Anstecker<-backup_all$Anstecker
                            nodes1$RichtigeID<-backup_all$RichtigeID
                            nodes1$ID2<-backup_all$RichtigeID2
                            
                            nodes1$Kontakte<-0
                            nodes1$AndereFAs<-""
                            for(i in 1:nrow(nodes1)){
                                if(nodes1$Grad[i]==kette_soll){
                                    nodes1$Kontakte[i]<-"noch nicht analysiert"
                                }else{
                                    nodes1$Kontakte[i]<-sum(nodes1$Anstecker==nodes1$NodeID[i])
                                }
                                if(nodes1$ID2[i]!=fall_ist){
                                    nodes1$AndereFAs[i]<-ifelse(length(unique(input1$Abteilung[input1$Fallnummer==nodes1$ID2[i]&input1$Abteilung!=nodes1$Group[i]&input1$Aufnahme>=nodes1$Ansteckend[i]&input1$Aufnahme<=nodes1$AnsteckendBis[i]]))==0,
                                                                "-",
                                                                paste0(unique(input1$Abteilung[input1$Fallnummer==nodes1$ID2[i]&input1$Abteilung!=nodes1$Group[i]&input1$Aufnahme>=nodes1$Ansteckend[i]&input1$Aufnahme<=nodes1$AnsteckendBis[i]]),collapse=", "))
                                }else{
                                    help_fa<-c(fa_ist_backup,support_abteilungen[support_abteilungen!=neue_abteilung])
                                    nodes1$AndereFAs[i]<-paste0(help_fa,collapse=", ")
                                }
                            }
                            
                            
                            color_scale_dark <- c("#0048ba","#cc5500","#1b4d3e","#9a0e2a","#aa7a04","#9a235e","#3e6717","#874747","#9b5206","#5c4eb2")
                            color_scale_light <- c("#9df6fd","#ffa332","#2feeb5","#f6a99b","#eecf83","#f5a9ce","#9af1b7","#dbb3b3","#e3c6a7","#d0cbf3")
                            nodes1$Farbe<-"black"
                            nodes1$NodeSizeb[nodes1$ID2==fall_ist&nodes1$Group!=fa_ist]<-12
                            #nodes1$Farbe[nodes1$ID2==fall_ist&nodes1$Group!=fa_ist]<-color_scale_dark[2:length(unique(nodes1$Group))]
                            #farben_grad<-colorRampPalette(c(color_scale_dark[1+which(neue_abteilung==nodes1_backup$Group[nodes1_backup$Group!=fa_ist_backup])],
                            #                                color_scale_light[1+which(neue_abteilung==nodes1_backup$Group[nodes1_backup$Group!=fa_ist_backup])]))(max(nodes1$Grad))################
                            farben_grad<-colorRampPalette(c(color_scale_dark[1+which(neue_abteilung==support_abteilungen)],
                                                            color_scale_light[1+which(neue_abteilung==support_abteilungen)]))(max(nodes1$Grad))################
                            for(kette in 1:kette_soll){
                                nodes1$Farbe[nodes1$ID2!=fall_ist&nodes1$Group==fa_ist&nodes1$Grad==kette]<-farben_grad[kette]
                            }
                            nodes1$NodeSizeb[nodes1$Farbe=="black"]<-12
                            nodes1$Group.Farbe<-paste0(nodes1$Group,",",nodes1$Farbe)
                            
                            nodes1$NodeID<-paste0(nodes1$Group," ",nodes1$NodeID)
                            nodes1$RichtigeID<-paste0(nodes1$Group," ",nodes1$RichtigeID)
                            nodes1$ID2<-paste0(nodes1$Group," ",nodes1$ID2)
                            nodes1$Anstecker<-ifelse(nodes1$Anstecker!="-",paste0(nodes1$Group," ",nodes1$Anstecker),"-")
                            
                            nodes1$FarbeAussen<-nodes1$Farbe
                            nodes1$FarbeAussen[nodes1$AndereFAs!="-"]<-"#141414"
                            nodes1$FarbeAussen[nodes1$Grad==0]<-farben_grad[1]
                            
                            links1$Source<-paste0(fa_ist," ",links1$Source)
                            links1$Target<-paste0(fa_ist," ",links1$Target)
                            
                            nodes1_backup<-nodes1_backup[nodes1_backup$Group!=neue_abteilung,]
                            
                            nodes1_backup<-unique(rbind(nodes1_backup,nodes1))
                            links1_backup<-unique(rbind(links1_backup,links1))
                            
                            nodes1_backup$Farbe[nodes1_backup$ID2==fall_ist_backup]<-"black"
                            
                            nodes1_backup$ID<-seq(0,(nrow(nodes1_backup)-1))
                            links1_backup$Source1<-nodes1_backup$ID[match(links1_backup$Source,nodes1_backup$NodeID)]
                            links1_backup$Target1<-nodes1_backup$ID[match(links1_backup$Target,nodes1_backup$NodeID)]
                            }
                        }
                    }
                }
            }
            
            nodes1<-nodes1_backup
            links1<-links1_backup
            
            #links1$linkDistance<-100
            #links1$linkDistance[links1$Farbe=="black"&links1$Source!=links1$Target]<-1000
            
            help_farbe<-nodes1[,c("Group.Farbe","Farbe")]
            help_farbe<-unique(help_farbe)
            group_color_pairs <- paste0('\'', help_farbe$Group.Farbe, '\'', collapse = ', ')
            color_pairs <- paste0('\'', help_farbe$Farbe, '\'', collapse = ', ')
            
            JS_color <- sprintf("
                     d3.scaleOrdinal()
                     .domain([%s])
                     .range([%s]);
                     ", group_color_pairs, color_pairs)
            
            
            #if(input$FA_oder_Station=="Fachabteilungen"){
            #  script <-'alert("Fachabteilung: " + (d.Group) + "\\n \\n Fall: " + (d.NodeID) + "\\n Hauptdiagnose: " + (d.ICDfull) + "\\n Aufenthaltsdauer auf Fachabteilung: " + (d.Dauer) + " Tage" + "\\n Registrierte Kontakte auf Fachabteilung: " + (d.Kontakte));'
            #}else{
            #  script <-'alert("Station: " + (d.Group) + "\\n \\n Fall: " + (d.NodeID) + "\\n Hauptdiagnose: " + (d.ICDfull) + "\\n Aufenthaltsdauer auf Station: " + (d.Dauer) + " Tage" + "\\n Registrierte Kontakte auf Station: " + (d.Kontakte));'
            #}
            script <- 'alert(d.Test)'
            
            fn<-forceNetwork(Links = links1,Nodes = nodes1,NodeID="RichtigeID",Group = "Group.Farbe",
                             Source="Source1",Target="Target1",zoom=T,opacityNoHover = 0.2,opacity = 1,Nodesize = "NodeSizeb",
                             radiusCalculation = JS("d.nodesize"),charge = -160,colourScale = JS(JS_color),
                             linkColour = links1$Farbe,#linkWidth = 1,Value="linkDistance",
                             #linkDistance = JS("function(d) { return d.value; }"),
                             clickAction = script)
            
            fn$x$nodes$NodeID<-nodes1$ID2
            fn$x$nodes$ICDfull<-nodes1$ICDfull
            fn$x$nodes$Group2<-nodes1$Group
            fn$x$nodes$Dauer<-as.character(round(as.numeric(nodes1$Dauer),2))
            fn$x$nodes$Kontakte<-nodes1$Kontakte
            
            fn$x$nodes$Infiziert<-nodes1$Infiziert
            fn$x$nodes$Ansteckend<-nodes1$Ansteckend
            fn$x$nodes$AnsteckendBis<-nodes1$AnsteckendBis
            fn$x$nodes$Grad<-nodes1$Grad
            fn$x$nodes$Anstecker<-nodes1$Anstecker
            fn$x$nodes$Angesteckt<-nodes1$Angesteckt
            fn$x$nodes$Group.Farbe<-nodes1$Group.Farbe
            fn$x$nodes$AndereFAs<-nodes1$AndereFAs
            
            if(!is.null(FA_Fokus)){
                fn$x$nodes$Test<-paste0("Fachabteilung: ",fn$x$nodes$Group2,
                                        "\n\nFall: ",fn$x$nodes$NodeID,
                                        "\nHauptdiagnose: ",fn$x$nodes$ICDfull,
                                        "\n\nInfektion:",
                                        "\n     Am: ",fn$x$nodes$Infiziert,
                                        "\n     Durch: ",fn$x$nodes$Anstecker," (Infektion ",fn$x$nodes$Grad,". Grades)",
                                        "\n\nAnsteckend auf Fachabteilung: ",
                                        ifelse(fn$x$nodes$Dauer==-1,"\nnoch vor infektiöser Phase wieder von Fachabteilung entlassen",
                                               paste0("\n     Von ",fn$x$nodes$Ansteckend," bis ",fn$x$nodes$AnsteckendBis,
                                                      "\n     Aufenthaltsdauer: ",fn$x$nodes$Dauer," Tage",
                                                      "\n     Fälle angesteckt: ",fn$x$nodes$Kontakte)),
                                        "\n\nWeitere besuchte Fachabteilungen während ansteckend: ",fn$x$nodes$AndereFAs)
            }else{
                fn$x$nodes$Test<-paste0("Station: ",fn$x$nodes$Group2,
                                        "\n\nFall: ",fn$x$nodes$NodeID,
                                        "\nHauptdiagnose: ",fn$x$nodes$ICDfull,
                                        "\n\nInfektion: ",
                                        "\n     Am: ",fn$x$nodes$Infiziert,
                                        "\n     Durch: ",fn$x$nodes$Anstecker," (Infektion ",fn$x$nodes$Grad,". Grades)",
                                        "\n\nAnsteckend auf Station: ",
                                        ifelse(fn$x$nodes$Dauer==-1,"\nnoch vor infektiöser Phase wieder von Station entlassen",
                                               paste0("\n     Von ",fn$x$nodes$Ansteckend," bis ",fn$x$nodes$AnsteckendBis,
                                                      "\n     Aufenthaltsdauer: ",fn$x$nodes$Dauer," Tage",
                                                      "\n     Fälle angesteckt: ",fn$x$nodes$Kontakte)),
                                        "\n\nWeitere besuchte Stationen während ansteckend: ",fn$x$nodes$AndereFAs)
            }
            
            #nodes1$FarbeAussen<-nodes1$Farbe
            #nodes1$FarbeAussen[nodes1$AndereFAs!="-"]<-"#141414"
            fn$x$nodes$FarbeAussen<-nodes1$FarbeAussen
            fn$x$nodes$borderWidth<-1.5
            fn$x$nodes$borderWidth[fn$x$nodes$Grad==0]<-7
            
            fn <- onRender(fn, '
                function(el, x) {
                    // Convert R data frame to JavaScript array of objects
                var nodeColors = HTMLWidgets.dataframeToD3(x.nodes);
                var nodeData = HTMLWidgets.dataframeToD3(x.nodes);
                d3.selectAll(".node").select("circle")
                .style("stroke", function(d, i) {
                    return nodeColors[i].FarbeAussen;  // Use the AB column for node stroke color
                })
                .style("stroke-width", function(d, i) {
                    return nodeData[i].borderWidth + "px";  // Use the borderWidth column for node stroke width
                 });
              }
            ')

            
            output$force <- renderForceNetwork({
                fn})
            
            
            icd_help_out<-c("I: A00-B99",
                            "II: C00-D48",
                            "III: D50-D90",
                            "IV: E00-E90",
                            "V: F00-F99",
                            "VI: G00-G99",
                            "VII: H00-H59",
                            "VIII: H60-H95",
                            "IX: I00-I99",
                            "X: J00-J99",
                            "XI: K00-K93",
                            "XII: L00-L99",
                            "XIII: M00-M99",
                            "XIV: N00-N99",
                            "XV: O00-O99",
                            "XVI: P00-P96",
                            "XVII: Q00-Q99",
                            "XVIII: R00-R99",
                            "XIX: S00-T98",
                            "XX: V01-Y84",
                            "XXI: Z00-Z99",
                            "XXII: U00-U99",
                            "nein")
            
            eigene_fa<-c(paste0("<br><br>Mutmaßlich infizierte Fälle<br>&nbsp; &nbsp; &nbsp; &nbsp; auf ",ifelse(input$FA_oder_Station=="Fachabteilungen","Fachabteilung ","Station "),
                                fa_ist_backup,": ",nrow(nodes1[nodes1$Group==fa_ist_backup,])))
            for(neue_abteilung in support_abteilungen){
                eigene_fa<-c(eigene_fa,
                             paste0("<br>&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; auf ",neue_abteilung,": ",nrow(nodes1[nodes1$Group==neue_abteilung,])))
            }
            risiko_andere<-nodes1$AndereFAs[nodes1$AndereFAs!="-"]
            risiko_andere2<-str_split_fixed(risiko_andere,pattern = ",",n = Inf)
            risiko_andere2<-sub("^ ","",risiko_andere2,fixed=F)
            risiko_andere2<-as.vector(risiko_andere2)
            risiko_andere2<-risiko_andere2[risiko_andere2!=""]
            risiko_andere3<-risiko_andere2[!risiko_andere2%in%c(fa_ist_backup,support_abteilungen)]
            risiko<-as.data.frame(table(risiko_andere3))
            risiko<-risiko[order(risiko$Freq,decreasing = T),]
            if(length(risiko)>0){
                risiko_out<-paste0(risiko$risiko_andere3," (",risiko$Freq,ifelse(risiko$Freq==1," Verlegung)"," Verlegungen)"))
            }else{
                risiko_out<-"-"
            }
            
            
            if(input$FA_oder_Station=="Fachabteilungen"){
                if(kette_soll!=100){
                    output$text_analyse2<-renderUI(HTML(paste0("Fachabteilung: ",fa_ist_backup,
                                                               "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA_ICD)],collapse=", "),
                                                               "<br>Fall im Fokus: ",fall_ist_backup,
                                                               "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                               "<br><br>Latenzzeit: ",min_inkubation_tag,ifelse(min_inkubation_tag==1," Tag "," Tage "),
                                                               " und ",min_inkubation_stunde,ifelse(min_inkubation_stunde==1," Stunde"," Stunden"),
                                                               "<br>Minimale Expositionszeit: ",min_kontakt_tag,ifelse(min_kontakt_tag==1," Tag "," Tage "),
                                                               " und ",min_kontakt_stunde,ifelse(min_kontakt_stunde==1," Stunde"," Stunden"),
                                                               "<br>Infektiöse Phase: ",min_ansteckend_tag,ifelse(min_ansteckend_tag==1," Tag "," Tage "),
                                                               " und ",min_ansteckend_stunde,ifelse(min_ansteckend_stunde==1," Stunde"," Stunden"),
                                                               "<br>Maximale Länge der Infektions-Kette: ",kette_soll,
                                                               paste0(eigene_fa,collapse=""),
                                                              # "<br><br>Mutmaßlich infizierte Fälle auf Fachabteilung ",fa_ist_backup,": ",nrow(nodes1[nodes1$Group==fa_ist_backup,]),
                                                               "<br><br>Infektionsrisiko für ",ifelse(length(risiko)==0,0,nrow(risiko)),
                                                               "/",(length(unique(input1$Abteilung))-length(eigene_fa)),
                                                               " weitere Fachabteilungen: ",paste0(risiko_out,collapse=", "),
                                                               "<br><br>Analyse erfolgreich durchgeführt")))
                }else{
                    output$text_analyse2<-renderUI(HTML(paste0("Fachabteilung: ",fa_ist_backup,
                                                               "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA_ICD)],collapse=", "),
                                                               "<br>Fall im Fokus: ",fall_ist_backup,
                                                               "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                               "<br><br>Latenzzeit: ",min_inkubation_tag,ifelse(min_inkubation_tag==1," Tag "," Tage "),
                                                               " und ",min_inkubation_stunde,ifelse(min_inkubation_stunde==1," Stunde"," Stunden"),
                                                               "<br>Minimale Expositionszeit: ",min_kontakt_tag,ifelse(min_kontakt_tag==1," Tag "," Tage "),
                                                               " und ",min_kontakt_stunde,ifelse(min_kontakt_stunde==1," Stunde"," Stunden"),
                                                               "<br>Infektiöse Phase: ",min_ansteckend_tag,ifelse(min_ansteckend_tag==1," Tag "," Tage "),
                                                               " und ",min_ansteckend_stunde,ifelse(min_ansteckend_stunde==1," Stunde"," Stunden"),
                                                               "<br>Maximale Länge der Infektions-Kette: unbegrenzt (hier: ",max(nodes1$Grad),")",
                                                               paste0(eigene_fa,collapse=""),
                                                               #"<br><br>Mutmaßlich infizierte Fälle auf Fachabteilung ",fa_ist_backup,": ",nrow(nodes1[nodes1$Group==fa_ist_backup,]),
                                                               "<br><br>Infektionsrisiko für ",ifelse(length(risiko)==0,0,nrow(risiko)),
                                                               "/",(length(unique(input1$Abteilung))-length(eigene_fa)),
                                                               " weitere Fachabteilungen: ",paste0(risiko_out,collapse=", "),
                                                               "<br><br>Analyse erfolgreich durchgeführt")))
                }
            }else{
                if(kette_soll!=100){
                    output$text_analyse2<-renderUI(HTML(paste0("Station: ",fa_ist_backup,
                                                               "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA_ICD)],collapse=", "),
                                                               "<br>Fall im Fokus: ",fall_ist_backup,
                                                               "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                               "<br><br>Latenzzeit: ",min_inkubation_tag,ifelse(min_inkubation_tag==1," Tag "," Tage "),
                                                               " und ",min_inkubation_stunde,ifelse(min_inkubation_stunde==1," Stunde"," Stunden"),
                                                               "<br>Minimale Expositionszeit: ",min_kontakt_tag,ifelse(min_kontakt_tag==1," Tag "," Tage "),
                                                               " und ",min_kontakt_stunde,ifelse(min_kontakt_stunde==1," Stunde"," Stunden"),
                                                               "<br>Infektiöse Phase: ",min_ansteckend_tag,ifelse(min_ansteckend_tag==1," Tag "," Tage "),
                                                               " und ",min_ansteckend_stunde,ifelse(min_ansteckend_stunde==1," Stunde"," Stunden"),
                                                               "<br>Maximale Länge der Infektions-Kette: ",kette_soll,
                                                               paste0(eigene_fa,collapse=""),
                                                               #"<br><br>Mutmaßlich infizierte Fälle auf Station ",fa_ist_backup,": ",nrow(nodes1[nodes1$Group==fa_ist_backup,]),
                                                               "<br><br>Infektionsrisiko für ",ifelse(length(risiko)==0,0,nrow(risiko)),
                                                               "/",(length(unique(input1$Abteilung))-length(eigene_fa)),
                                                               " weitere Stationen: ",paste0(risiko_out,collapse=", "),
                                                               "<br><br>Analyse erfolgreich durchgeführt")))
                }else{
                    output$text_analyse2<-renderUI(HTML(paste0("Station: ",fa_ist_backup,
                                                               "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA_ICD)],collapse=", "),
                                                               "<br>Fall im Fokus: ",fall_ist_backup,
                                                               "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                               "<br><br>Latenzzeit: ",min_inkubation_tag,ifelse(min_inkubation_tag==1," Tag "," Tage "),
                                                               " und ",min_inkubation_stunde,ifelse(min_inkubation_stunde==1," Stunde"," Stunden"),
                                                               "<br>Minimale Expositionszeit: ",min_kontakt_tag,ifelse(min_kontakt_tag==1," Tag "," Tage "),
                                                               " und ",min_kontakt_stunde,ifelse(min_kontakt_stunde==1," Stunde"," Stunden"),
                                                               "<br>Infektiöse Phase: ",min_ansteckend_tag,ifelse(min_ansteckend_tag==1," Tag "," Tage "),
                                                               " und ",min_ansteckend_stunde,ifelse(min_ansteckend_stunde==1," Stunde"," Stunden"),
                                                               "<br>Maximale Länge der Infektions-Kette: unbegrenzt (hier: ",max(nodes1$Grad),")",
                                                               paste0(eigene_fa,collapse=""),
                                                               #"<br><br>Mutmaßlich infizierte Fälle auf Station ",fa_ist_backup,": ",nrow(nodes1[nodes1$Group==fa_ist_backup,]),
                                                               "<br><br>Infektionsrisiko für ",ifelse(length(risiko)==0,0,nrow(risiko)),
                                                               "/",(length(unique(input1$Abteilung))-length(eigene_fa)),
                                                               " weitere Stationen: ",paste0(risiko_out,collapse=", "),
                                                               "<br><br>Analyse erfolgreich durchgeführt")))
                }
            }
            
            
            
            
            
            
            
            
            
            
            }
          shinyjs::html("text", paste0("<br>Analyse erfolgreich durchgeführt<br><br>"), add = TRUE)
        })

        
        ##erweitert für alle, aber nur Kettenlänge 1, weil sonst zu unübersichtlich
        observeEvent(input$do_erweiterteFA2,ignoreInit = TRUE,{
            if(!button_do_erweiterteFA2()){
                button_do_erweiterteFA2(TRUE)
              button_do_erweiterteFA(FALSE)
                
              nodes1<-rv$nodes1
              links1<-rv$links1
              backup_all<-rv$backup_all
              fall_ist<-rv$fall_ist
              fa_ist<-rv$fa_ist
              
            nodes1_backup<-nodes1
            links1_backup<-links1
            backup_all_backup<-backup_all
            
            fall_ist_backup<-fall_ist
            fa_ist_backup<-fa_ist
            
            FA_Start<-rv$FA_Start
            FA_Ende<-rv$FA_Ende
            
            if(input$Grad_Intern=="unbegrenzt"){
                kette_soll<-100
            }else{
                kette_soll<-as.numeric(input$Grad_Intern)
            }
            
            output$force<-renderForceNetwork({NULL})
            
            ##erst mal die einfachen, später sonderfall
            neue_ids<-nodes1_backup$RichtigeID[nodes1_backup$Grad!=0&nodes1_backup$AndereFAs!="-"]
            
            progress <- shiny::Progress$new()
            progress$set(message = "Bestimme Kontakte", value = 0)
            for(neue_id in neue_ids){
                progress$inc(1/length(neue_ids),detail=neue_id)
                
                if(input$FA_oder_Station=="Fachabteilungen"){
                  shinyjs::html("text", paste0("<br>Erweitere Analyse für weitere Fachabteilungen (alle Fälle).<br><br>"), add = FALSE)
                  
                    input1<-input1a
                    input1<-input1[input1$Abteilung!="Patient_abwesend",]
                    input1<-input1[order(input1$Fallnummer),]
                    fa<-sort(unique(input1$Abteilung))
                    
                    input1_fa<-input1[input1$Abteilung==fa_ist_backup,]
                    
                    FA_ICD<-23
                    
                    if(input$switch99=="ICD-Kapitel"){
                        input1_fa<-input1_fa[input1_fa$ICDKapitel%in%input$FA_ICDKapitel,]
                        FA_ICD<-input$FA_ICDKapitel
                    }
                    if(input$switch99=="exakter ICD-Code"){
                        user_icd<-gsub(".","\\.",input$FA_ICDExakt,fixed=T)
                        input1_fa<-input1_fa[grep(paste0("^",user_icd),input1_fa$ICDfull),]
                        FA_ICD<-input$FA_ICDExakt
                    }
                    
                    fall<-sort(unique(input1_fa$Fallnummer))
                    
                    input1_fall<-as.data.frame(input1_fa[input1_fa$Fallnummer==neue_id&
                                                             as.numeric(input1_fa$Aufnahme)>=as.numeric(FA_Start)&
                                                             as.numeric(input1_fa$Aufnahme)<=as.numeric(FA_Ende),])
                    #input1_fall<-as.data.frame(input1_fa[input1_fa$Fallnummer==fall[1],])
                    
                    ausgeschlossen<-c()
                    if(nrow(input1_fall)>1){
                        for(i in 1:(nrow(input1_fall)-1)){
                            anfang<-as.Date(input1_fall$Entlassung[i],format="%Y-%m-%d")+1
                            ende<-as.Date(input1_fall$Aufnahme[i+1],format="%Y-%m-%d")-1
                            if(anfang==ende){
                                ausgeschlossen<-c(as.Date(ausgeschlossen),anfang)
                            }
                            if(anfang<ende){
                                ausgeschlossen<-c(as.Date(ausgeschlossen),as.Date(anfang:ende))
                            }
                        }
                    }
                    
                }
                
                if(input$FA_oder_Station=="Stationen"){
                  shinyjs::html("text", paste0("<br>Erweitere Analyse für weitere Stationen (alle Fälle).<br><br>"), add = FALSE)
                  
                    input1<-input1b
                    names(input1)[3]<-"Abteilung"
                    input1<-input1[input1$Abteilung!="Patient_abwesend",]
                    input1<-input1[order(input1$Fallnummer),]
                    fa<-sort(unique(input1$Abteilung))
                    
                    input1_fa<-input1[input1$Abteilung==fa_ist_backup,]
                    
                    FA_ICD<-23
                    
                    if(input$switch99c=="ICD-Kapitel"){
                        input1_fa<-input1_fa[input1_fa$ICDKapitel%in%input$ST_ICDKapitel,]
                        FA_ICD<-input$ST_ICDKapitel
                    }
                    if(input$switch99c=="exakter ICD-Code"){
                        user_icd<-gsub(".","\\.",input$ST_ICDExakt,fixed=T)
                        input1_fa<-input1_fa[grep(paste0("^",user_icd),input1_fa$ICDfull),] 
                        FA_ICD<-input$ST_ICDExakt
                    }
                    
                    fall<-sort(unique(input1_fa$Fallnummer))
                    
                    input1_fall<-as.data.frame(input1_fa[input1_fa$Fallnummer==neue_id&
                                                             as.numeric(input1_fa$Aufnahme)>=as.numeric(FA_Start)&
                                                             as.numeric(input1_fa$Aufnahme)<=as.numeric(FA_Ende),])
                    
                    ausgeschlossen<-c()
                    if(nrow(input1_fall)>1){
                        for(i in 1:(nrow(input1_fall)-1)){
                            anfang<-as.Date(input1_fall$Entlassung[i],format="%Y-%m-%d")+1
                            ende<-as.Date(input1_fall$Aufnahme[i+1],format="%Y-%m-%d")-1
                            if(anfang==ende){
                                ausgeschlossen<-c(as.Date(ausgeschlossen),anfang)
                            }
                            if(anfang<ende){
                                ausgeschlossen<-c(as.Date(ausgeschlossen),as.Date(anfang:ende))
                            }
                        }
                    }
                }
                
                FA_Fokus<-input$FA_Fokus
                Fall_Fokus_FA<-neue_id
                ST_Fokus<-input$ST_Fokus
                Fall_Fokus_ST<-neue_id
                
                if(input$FA_oder_Station=="Fachabteilungen"){
                    fall_ist<-Fall_Fokus_FA
                    fa_ist<-FA_Fokus
                    ST_Fokus<-NULL
                    Fall_Fokus_ST<-NULL
                }else{
                    fall_ist<-Fall_Fokus_ST
                    fa_ist<-ST_Fokus
                    FA_Fokus<-NULL
                    Fall_Fokus_FA<-NULL
                }
                
                if(sum(as.numeric(FA_Start)==as.numeric(as.Date(input1_fall$Aufnahme,format="%Y-%m-%d")))>=1){
                    FA_Start<-min(input1_fall$Aufnahme[which(as.numeric(FA_Start)==as.numeric(as.Date(input1_fall$Aufnahme,format="%Y-%m-%d")))])
                }else{
                    FA_Start<-as.POSIXct(paste0(FA_Start," 00:00:01"),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                }
                FA_Ende<-as.POSIXct(paste0(FA_Ende," 23:59:59"),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                
                
                id<-neue_id
                
                FA_Start_Neu<-backup_all$Ansteckend[backup_all$RichtigeID==id]
                Ansteckend_bis<-as.POSIXct(as.numeric(backup_all$Ansteckend[backup_all$RichtigeID==id]+ansteckendzeit*60*60*24,tz="UTC"))
                
                ##Für neuen Fall bestimmen, wann er da ist und wann er davon ansteckend ist
                input1_fall_filtered_Neu<-as.data.frame(input1[input1$Fallnummer==id&input1$Abteilung!=fa_ist
                                                    &as.numeric(input1$Aufnahme)<=as.numeric(Ansteckend_bis)
                                                    &as.numeric(input1$Entlassung)>=as.numeric(FA_Start_Neu),])

                for(zeile in nrow(input1_fall_filtered_Neu)){
                    input1_fall_filtered_Neu$Entlassung[zeile]<-min(input1_fall_filtered_Neu$Entlassung[zeile],
                                                                    Ansteckend_bis)
                }
                input1_fall_filtered_Neu$Dauer<-(as.numeric(input1_fall_filtered_Neu$Entlassung)-as.numeric(input1_fall_filtered_Neu$Aufnahme))/(60*60*24)
                
                if(sum(input1_fall_filtered_Neu$Dauer>=kontaktzeit)>0){
                    ##dann dafür alle potenziellen Kontakte bestimmen
                    input1_fa_filtered_Neu<-input1[input1$Abteilung%in%input1_fall_filtered_Neu$Abteilung
                                                   &as.numeric(input1$Aufnahme)<=as.numeric(max(input1_fall_filtered_Neu$Entlassung))
                                                      &as.numeric(input1$Entlassung)>=as.numeric(min(input1_fall_filtered_Neu$Aufnahme)),]
                    if(nrow(input1_fa_filtered_Neu)!=0){
                        for(i in 1:nrow(input1_fa_filtered_Neu)){
                            input1_fa_filtered_Neu$Aufnahme[i]<-max(input1_fa_filtered_Neu$Aufnahme[i],FA_Start_Neu)
                            input1_fa_filtered_Neu$Entlassung[i]<-min(input1_fa_filtered_Neu$Entlassung[i],Ansteckend_bis)
                        }
                        input1_fa_filtered_Neu$Dauer<-(as.numeric(input1_fa_filtered_Neu$Entlassung)-as.numeric(input1_fa_filtered_Neu$Aufnahme))/(60*60*24)
                        ##ab Start ist id ansteckend, jetzt nur noch betrachten, wo Aufenthalt anderer Fälle lang genug für Ansteckung ist
                        input1_fa_filtered2_Neu<-input1_fa_filtered_Neu[input1_fa_filtered_Neu$Dauer>=kontaktzeit,]
                        
                        if(sum(input1_fa_filtered2_Neu$Fallnummer!=id)!=0){
                            temp_intervalle_Neu<-TimeIntervalDataFrame(start=input1_fa_filtered2_Neu$Aufnahme,end=input1_fa_filtered2_Neu$Entlassung)
                            temp_overlapping_Neu<-overlapping(temp_intervalle_Neu,idx = T)
                            
                            temp_overlapping2_Neu<-rbind(temp_overlapping_Neu[temp_overlapping_Neu$i%in%which(input1_fa_filtered2_Neu$Fallnummer%in%id),],
                                                         temp_overlapping_Neu[temp_overlapping_Neu$j%in%which(input1_fa_filtered2_Neu$Fallnummer%in%id),])
                            if(nrow(temp_overlapping2_Neu)>0){
                                ##Kettenlänge 2
                                temp_overlapping2_Neu$Patientennummer_i<-input1_fa_filtered2_Neu$Patientennummer[temp_overlapping2_Neu$i]
                                temp_overlapping2_Neu$Fallnummer_i<-input1_fa_filtered2_Neu$Fallnummer[temp_overlapping2_Neu$i]
                                temp_overlapping2_Neu$Patientennummer_j<-input1_fa_filtered2_Neu$Patientennummer[temp_overlapping2_Neu$j]
                                temp_overlapping2_Neu$Fallnummer_j<-input1_fa_filtered2_Neu$Fallnummer[temp_overlapping2_Neu$j]
                                
                                temp_overlapping2_Neu$Start_i<-input1_fa_filtered2_Neu$Aufnahme[temp_overlapping2_Neu$i]
                                temp_overlapping2_Neu$Ende_i<-input1_fa_filtered2_Neu$Entlassung[temp_overlapping2_Neu$i]
                                temp_overlapping2_Neu$Start_j<-input1_fa_filtered2_Neu$Aufnahme[temp_overlapping2_Neu$j]
                                temp_overlapping2_Neu$Ende_j<-input1_fa_filtered2_Neu$Entlassung[temp_overlapping2_Neu$j]
                                
                                temp_overlapping2_Neu$Group<-ifelse(temp_overlapping2_Neu$Fallnummer_i==id,
                                                                    input1_fa_filtered2_Neu$Abteilung[temp_overlapping2_Neu$j],
                                                                    input1_fa_filtered2_Neu$Abteilung[temp_overlapping2_Neu$i])
                                temp_overlapping2_Neu$ICDfull<-ifelse(temp_overlapping2_Neu$Fallnummer_i==id,
                                                                      input1_fa_filtered2_Neu$ICDfull[temp_overlapping2_Neu$j],
                                                                      input1_fa_filtered2_Neu$ICDfull[temp_overlapping2_Neu$i])
                                
                                helper<-cbind(as.numeric(input1_fa_filtered2_Neu$Aufnahme[temp_overlapping2_Neu$i]),as.numeric(input1_fa_filtered2_Neu$Aufnahme[temp_overlapping2_Neu$j]))
                                helper_start<-apply(helper,1,max)
                                helper<-cbind(as.numeric(input1_fa_filtered2_Neu$Entlassung[temp_overlapping2_Neu$i]),as.numeric(input1_fa_filtered2_Neu$Entlassung[temp_overlapping2_Neu$j]))
                                helper_ende<-apply(helper,1,min)
                                temp_overlapping2_Neu$Dauer<-(helper_ende-helper_start)/(60*60*24)
                                
                                ##Filtern auf Mindest-Kontaktzeit
                                temp_overlapping2_Neu<-temp_overlapping2_Neu[temp_overlapping2_Neu$Dauer>=kontaktzeit,]
                                
                                if(nrow(temp_overlapping2_Neu)>0){
                                    ##prüfen, dass keine Kombination 2x oder mehr vorkommt
                                    help_test<-paste(temp_overlapping2_Neu$Fallnummer_i,temp_overlapping2_Neu$Fallnummer_j,sep = "_")
                                    help_test_table<-table(help_test)
                                    help_test_table<-help_test_table[help_test_table>1]
                                    
                                    if(length(help_test_table)>0){
                                        help_test<-str_split_fixed(names(help_test_table),"_",Inf)
                                        
                                        temp_overlapping_neu_Neu<-temp_overlapping2_Neu
                                        temp_overlapping_neu_Neu$Erster<-TRUE
                                        for(k in 1:nrow(help_test)){
                                            temp_overlapping_neu_Neu$Dauer[temp_overlapping_neu_Neu$Fallnummer_i==help_test[k,1]&
                                                                               temp_overlapping_neu_Neu$Fallnummer_j==help_test[k,2]]<-sum(temp_overlapping_neu_Neu$Dauer[temp_overlapping_neu_Neu$Fallnummer_i==help_test[k,1]&
                                                                                                                                                                              temp_overlapping_neu_Neu$Fallnummer_j==help_test[k,2]])
                                            
                                            temp_overlapping_neu_Neu[temp_overlapping_neu_Neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu_Neu$Fallnummer_j==help_test[k,2],]$Erster[temp_overlapping_neu_Neu$Start_i[temp_overlapping_neu_Neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu_Neu$Fallnummer_j==help_test[k,2]]!=min(temp_overlapping_neu_Neu$Start_i[temp_overlapping_neu_Neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu_Neu$Fallnummer_j==help_test[k,2]])|
                                                                                                                                                                                              temp_overlapping_neu_Neu$Start_j[temp_overlapping_neu_Neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu_Neu$Fallnummer_j==help_test[k,2]]!=min(temp_overlapping_neu_Neu$Start_j[temp_overlapping_neu_Neu$Fallnummer_i==help_test[k,1]&temp_overlapping_neu_Neu$Fallnummer_j==help_test[k,2]])]<-F
                                        }
                                        temp_overlapping_neu_Neu<-temp_overlapping_neu_Neu[temp_overlapping_neu_Neu$Erster==T,]
                                        temp_overlapping_neu_Neu$Infiziert<-as.POSIXct(ifelse(temp_overlapping_neu_Neu$Fallnummer_i==id,
                                                                                              as.numeric(temp_overlapping_neu_Neu$Start_j+kontaktzeit*60*60*24),
                                                                                              as.numeric(temp_overlapping_neu_Neu$Start_i+kontaktzeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                        temp_overlapping_neu_Neu$Ansteckend<-as.POSIXct(ifelse(temp_overlapping_neu_Neu$Fallnummer_i==id,
                                                                                               as.numeric(temp_overlapping_neu_Neu$Start_j+kontaktzeit*60*60*24+inkubationszeit*60*60*24),
                                                                                               as.numeric(temp_overlapping_neu_Neu$Start_i+kontaktzeit*60*60*24+inkubationszeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                        temp_overlapping_neu_Neu$AnsteckendBis<-as.POSIXct(ifelse(temp_overlapping_neu_Neu$Fallnummer_i==id,
                                                                                                  as.numeric(temp_overlapping_neu_Neu$Start_j+ansteckendzeit*60*60*24+kontaktzeit*60*60*24+inkubationszeit*60*60*24),
                                                                                                  as.numeric(temp_overlapping_neu_Neu$Start_i+ansteckendzeit*60*60*24+kontaktzeit*60*60*24+inkubationszeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                        temp_overlapping_neu_Neu<-temp_overlapping_neu_Neu[,-c(1:2,7:10,14)]
                                        temp_overlapping_neu_Neu<-unique(temp_overlapping_neu_Neu)
                                    }else{
                                        temp_overlapping2_Neu$Infiziert<-as.POSIXct(ifelse(temp_overlapping2_Neu$Fallnummer_i==id,
                                                                                           as.numeric(temp_overlapping2_Neu$Start_j+kontaktzeit*60*60*24),
                                                                                           as.numeric(temp_overlapping2_Neu$Start_i+kontaktzeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                        temp_overlapping2_Neu$Ansteckend<-as.POSIXct(ifelse(temp_overlapping2_Neu$Fallnummer_i==id,
                                                                                            as.numeric(temp_overlapping2_Neu$Start_j+kontaktzeit*60*60*24+inkubationszeit*60*60*24),
                                                                                            as.numeric(temp_overlapping2_Neu$Start_i+kontaktzeit*60*60*24+inkubationszeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                        temp_overlapping2_Neu$AnsteckendBis<-as.POSIXct(ifelse(temp_overlapping2_Neu$Fallnummer_i==id,
                                                                                               as.numeric(temp_overlapping2_Neu$Start_j+ansteckendzeit*60*60*24+kontaktzeit*60*60*24+inkubationszeit*60*60*24),
                                                                                               as.numeric(temp_overlapping2_Neu$Start_i+ansteckendzeit*60*60*24+kontaktzeit*60*60*24+inkubationszeit*60*60*24)),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
                                        temp_overlapping_neu_Neu<-temp_overlapping2_Neu[,-c(1:2,7:10)]
                                    }
                                    temp_overlapping_neu_Neu$Grad<-backup_all$Grad[backup_all$RichtigeID==id]+1
                                    temp_overlapping_neu_Neu$RichtigeID<-ifelse(temp_overlapping_neu_Neu$Fallnummer_i==id,temp_overlapping_neu_Neu$Fallnummer_j,temp_overlapping_neu_Neu$Fallnummer_i)
                                    temp_overlapping_neu_Neu$Anstecker<-ifelse(temp_overlapping_neu_Neu$Fallnummer_i==id,temp_overlapping_neu_Neu$Fallnummer_i,temp_overlapping_neu_Neu$Fallnummer_j)
                                    
                                    ##Vereinfachend: Fall ausschließen, dass A steckt B, A steckt C an, B und C beide auf anderer FA, dort steckt B C früher an als A
                                    ids_raus<-backup_all$RichtigeID[backup_all$RichtigeID%in%temp_overlapping_neu_Neu$RichtigeID&backup_all$Group==fa_ist]
                                    temp_overlapping_neu_Neu<-temp_overlapping_neu_Neu[!temp_overlapping_neu_Neu$RichtigeID%in%ids_raus,]
                                    
                                    if(nrow(temp_overlapping_neu_Neu)>0){
                                        temp_overlapping_neu_Neu$RichtigeID2<-temp_overlapping_neu_Neu$RichtigeID
                                        temp_overlapping_neu_Neu$RichtigeID<-paste0(temp_overlapping_neu_Neu$Group," ",temp_overlapping_neu_Neu$RichtigeID)
                                        temp_overlapping_neu_Neu$Fallnummer_i<-ifelse(temp_overlapping_neu_Neu$Fallnummer_i==id,temp_overlapping_neu_Neu$Fallnummer_i,paste0(temp_overlapping_neu_Neu$Group," ",temp_overlapping_neu_Neu$Fallnummer_i))
                                        temp_overlapping_neu_Neu$Fallnummer_j<-ifelse(temp_overlapping_neu_Neu$Fallnummer_j==id,temp_overlapping_neu_Neu$Fallnummer_j,paste0(temp_overlapping_neu_Neu$Group," ",temp_overlapping_neu_Neu$Fallnummer_j))
                                        
                                        temp_overlapping_neu_Neu<-temp_overlapping_neu_Neu[,c(1:4,7:13,5,14,6)]
                                        
                                        ##überprüfen, ob zeit bis zur Ansteckung mit höherem Grad evt schneller ist als mit niedrigerem?
                                        for(zeile in 1:nrow(backup_all)){
                                            if(sum(backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID)>0&&
                                               backup_all$Infiziert[zeile]>temp_overlapping_neu_Neu$Infiziert[backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID]){
                                                backup_all$Infiziert[zeile]<-temp_overlapping_neu_Neu$Infiziert[backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID]
                                                backup_all$Ansteckend[zeile]<-temp_overlapping_neu_Neu$Ansteckend[backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID]
                                                backup_all$AnsteckendBis[zeile]<-temp_overlapping_neu_Neu$AnsteckendBis[backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID]
                                                backup_all$Dauer[zeile]<-temp_overlapping_neu_Neu$Dauer[backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID]
                                                backup_all$Grad[zeile]<-temp_overlapping_neu_Neu$Grad[backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID]
                                                backup_all$Anstecker[zeile]<-temp_overlapping_neu_Neu$Anstecker[backup_all$RichtigeID[zeile]==temp_overlapping_neu_Neu$RichtigeID]
                                            }
                                        }
                                        
                                        neue_zeilen<-unique(c(temp_overlapping_neu_Neu$Fallnummer_i,temp_overlapping_neu_Neu$Fallnummer_j))[!unique(c(temp_overlapping_neu_Neu$Fallnummer_i,temp_overlapping_neu_Neu$Fallnummer_j))%in%unique(c(backup_all$Fallnummer_i,backup_all$Fallnummer_j))]
                                        if(length(neue_zeilen)>0){
                                            backup_temp<-temp_overlapping_neu_Neu[temp_overlapping_neu_Neu$Fallnummer_i%in%neue_zeilen|temp_overlapping_neu_Neu$Fallnummer_j%in%neue_zeilen,]
                                            
                                            #backup_temp$Group<-temp_overlapping2_Neu$Abteilung[match(backup_temp$RichtigeID,temp_overlapping2_Neu$)]
                                            backup_all<-rbind(backup_all,backup_temp)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                
            }
                
            progress$close()

            
            ##Links und Nodes definieren
            links1<-backup_all[,c("Anstecker","RichtigeID")]
            links1$Anstecker[links1$Anstecker=="-"]<-fall_ist_backup
            names(links1)<-c("Source","Target")
            links1$Group<-backup_all$Group
            links1$Anstecker<-backup_all$Anstecker
            links1$Farbe<-"#d1d1d1"
            links1$Farbe[links1$Anstecker=="-"]<-"black"
            
            nodes1<-data.frame(NodeID=backup_all$RichtigeID,ID=seq(0,(nrow(backup_all)-1)))
            links1$Source1<-nodes1$ID[match(links1$Source,nodes1$NodeID)]
            links1$Target1<-nodes1$ID[match(links1$Target,nodes1$NodeID)]
            
            nodes1$NodeSize<-nodes1$Dauer<-as.numeric(backup_all$Dauer)
            nodes1$NodeSizeb<-2+8*nodes1$NodeSize/max(nodes1$NodeSize)
            nodes1$Group<-backup_all$Group
            nodes1$ICDfull<-backup_all$ICDfull
            nodes1$Grad<-backup_all$Grad
            nodes1$Infiziert<-backup_all$Infiziert
            nodes1$Ansteckend<-backup_all$Ansteckend
            nodes1$AnsteckendBis<-backup_all$AnsteckendBis
            nodes1$Anstecker<-backup_all$Anstecker
            nodes1$RichtigeID<-backup_all$RichtigeID
            nodes1$ID2<-backup_all$RichtigeID2
            
            input1$Abteilung[input1$Fallnummer==nodes1$ID2[i]&input1$Abteilung!=nodes1$Group[i]&input1$Aufnahme>=nodes1$Ansteckend[i]&input1$Aufnahme<=nodes1$AnsteckendBis[i]]
            
            nodes1$Kontakte<-0
            nodes1$AndereFAs<-""
            progress <- shiny::Progress$new()
            progress$set(message = "Annotiere Fälle", value = 0)
            for(i in 1:nrow(nodes1)){
              progress$inc(1/nrow(nodes1))
              
                if(nodes1$Grad[i]==kette_soll||nodes1$Group[i]!=fa_ist){
                    nodes1$Kontakte[i]<-"noch nicht analysiert"
                }else{
                    nodes1$Kontakte[i]<-sum(nodes1$Anstecker==nodes1$NodeID[i]&nodes1$Group==fa_ist)
                }
              nodes1$AndereFAs[i]<-ifelse(length(unique(input1$Abteilung[input1$Fallnummer==nodes1$ID2[i]&input1$Abteilung!=nodes1$Group[i]&input1$Aufnahme>=nodes1$Ansteckend[i]&input1$Aufnahme<=nodes1$AnsteckendBis[i]]))==0,
                                          "-",
                                          NA)
              if(is.na(nodes1$AndereFAs[i])){
                  if(nodes1$Group[i]==fa_ist&&nodes1$NodeID[i]!=fall_ist_backup){
                      abteilungen<-unique(nodes1$Group[nodes1$Anstecker==nodes1$NodeID[i]])
                      abteilungen<-abteilungen[abteilungen!=fa_ist]
                      for(a in abteilungen){
                          nodes1$AndereFAs[i]<-paste0(nodes1$AndereFAs[i],", ",a," (",sum(nodes1$Anstecker==nodes1$NodeID[i]&nodes1$Group==a)," angesteckt)")
                      }
                      nodes1$AndereFAs[i]<-gsub("NA, ","",nodes1$AndereFAs[i])
                      ##immer noch NA, weil niemals Anstecker
                      if(is.na(nodes1$AndereFAs[i])){
                          nodes1$AndereFAs[i]<-"-"
                      }
                  }else{
                      nodes1$AndereFAs[i]<-paste0(unique(input1$Abteilung[input1$Fallnummer==nodes1$ID2[i]&input1$Abteilung!=nodes1$Group[i]&input1$Aufnahme>=nodes1$Ansteckend[i]&input1$Aufnahme<=nodes1$AnsteckendBis[i]]),collapse=", ")
                  }##################warum klappt das nicht für fall im fokus??
              }
            }
            progress$close()

            
            color_scale_dark <- c("#0048ba","#cc5500","#1b4d3e","#9a0e2a","#aa7a04","#9a235e","#3e6717","#874747","#9b5206","#5c4eb2")
            color_scale_light <- c("#9df6fd","#ffa332","#2feeb5","#f6a99b","#eecf83","#f5a9ce","#9af1b7","#dbb3b3","#e3c6a7","#d0cbf3")
            nodes1$Farbe<-"black"
            nodes1$NodeSizeb[nodes1$ID2==fall_ist_backup&nodes1$Group!=fa_ist]<-12
            #nodes1$Farbe[nodes1$ID2==fall_ist&nodes1$Group!=fa_ist]<-color_scale_dark[2:length(unique(nodes1$Group))]
            
            abts_temp<-unique(nodes1$Group)
            abts<-c(fa_ist,abts_temp[abts_temp!=fa_ist])
            ##1. FA im Fokus
            farben_grad<-colorRampPalette(c(color_scale_dark[1],
                                            color_scale_light[1]))(max(nodes1$Grad[nodes1$Group==fa_ist_backup]))
            for(kette in 1:kette_soll){
              nodes1$Farbe[nodes1$ID2!=fall_ist_backup&nodes1$Group==fa_ist&nodes1$Grad==kette]<-farben_grad[kette]
            }
            
            color_scale_extended<-c(color_scale_dark,colorRampPalette(brewer.pal(9, "Set1"))(length(fa)))
            for(abt in 2:length(abts)){
                nodes1$Farbe[nodes1$Group==abts[abt]]<-color_scale_extended[abt]
            }

            nodes1$NodeSizeb[nodes1$Farbe=="black"]<-12

            nodes1$FarbeAussen<-nodes1$Farbe
            nodes1$FarbeAussen[nodes1$AndereFAs!="-"&nodes1$Grad!=0]<-"#141414"
            nodes1$FarbeAussen[nodes1$Grad==0&nodes1$Group==fa_ist]<-farben_grad[1]
            
            nodes1$Farbe[nodes1$Grad==0]<-"black"
            nodes1$Group.Farbe<-paste0(nodes1$Group,",",nodes1$Farbe)
            
            
            #links1$linkDistance<-100
            #links1$linkDistance[links1$Farbe=="black"&links1$Source!=links1$Target]<-1000
            
            help_farbe<-nodes1[,c("Group.Farbe","Farbe")]
            help_farbe<-unique(help_farbe)
            group_color_pairs <- paste0('\'', help_farbe$Group.Farbe, '\'', collapse = ', ')
            color_pairs <- paste0('\'', help_farbe$Farbe, '\'', collapse = ', ')
            
            JS_color <- sprintf("
                     d3.scaleOrdinal()
                     .domain([%s])
                     .range([%s]);
                     ", group_color_pairs, color_pairs)
            
            
            #if(input$FA_oder_Station=="Fachabteilungen"){
            #  script <-'alert("Fachabteilung: " + (d.Group) + "\\n \\n Fall: " + (d.NodeID) + "\\n Hauptdiagnose: " + (d.ICDfull) + "\\n Aufenthaltsdauer auf Fachabteilung: " + (d.Dauer) + " Tage" + "\\n Registrierte Kontakte auf Fachabteilung: " + (d.Kontakte));'
            #}else{
            #  script <-'alert("Station: " + (d.Group) + "\\n \\n Fall: " + (d.NodeID) + "\\n Hauptdiagnose: " + (d.ICDfull) + "\\n Aufenthaltsdauer auf Station: " + (d.Dauer) + " Tage" + "\\n Registrierte Kontakte auf Station: " + (d.Kontakte));'
            #}
            script <- 'alert(d.Test)'
            
            fn<-forceNetwork(Links = links1,Nodes = nodes1,NodeID="RichtigeID",Group = "Group.Farbe",
                             Source="Source1",Target="Target1",zoom=T,opacityNoHover = 0.2,opacity = 1,Nodesize = "NodeSizeb",
                             radiusCalculation = JS("d.nodesize"),charge = -60,colourScale = JS(JS_color),
                             linkColour = links1$Farbe,#linkWidth = 1,Value="linkDistance",
                             #linkDistance = JS("function(d) { return d.value; }"),
                             clickAction = script)
            
            fn$x$nodes$NodeID<-nodes1$RichtigeID
            fn$x$nodes$ICDfull<-nodes1$ICDfull
            fn$x$nodes$Group2<-nodes1$Group
            fn$x$nodes$Dauer<-as.character(round(as.numeric(nodes1$Dauer),2))
            fn$x$nodes$Kontakte<-nodes1$Kontakte
            
            fn$x$nodes$Infiziert<-nodes1$Infiziert
            fn$x$nodes$Ansteckend<-nodes1$Ansteckend
            fn$x$nodes$AnsteckendBis<-nodes1$AnsteckendBis
            fn$x$nodes$Grad<-nodes1$Grad
            fn$x$nodes$Anstecker<-nodes1$Anstecker
            fn$x$nodes$Angesteckt<-nodes1$Angesteckt
            fn$x$nodes$Group.Farbe<-nodes1$Group.Farbe
            fn$x$nodes$AndereFAs<-nodes1$AndereFAs
            
            if(!is.null(FA_Fokus)){
                fn$x$nodes$Test<-paste0("Fachabteilung: ",fn$x$nodes$Group2,
                                        "\n\nFall: ",fn$x$nodes$NodeID,
                                        "\nHauptdiagnose: ",fn$x$nodes$ICDfull,
                                        "\n\nInfektion:",
                                        "\n     Am: ",fn$x$nodes$Infiziert,
                                        "\n     Durch: ",fn$x$nodes$Anstecker," (Infektion ",fn$x$nodes$Grad,". Grades)",
                                        "\n\nAnsteckend auf Fachabteilung ",fn$x$nodes$Group2,":",
                                        ifelse(fn$x$nodes$Dauer==-1,"\nnoch vor infektiöser Phase wieder von Fachabteilung entlassen",
                                               paste0("\n     Von ",fn$x$nodes$Ansteckend," bis ",fn$x$nodes$AnsteckendBis,
                                                      "\n     Aufenthaltsdauer: ",fn$x$nodes$Dauer," Tage",
                                                      "\n     Fälle angesteckt: ",fn$x$nodes$Kontakte)),
                                        "\n\nWeitere besuchte Fachabteilungen während ansteckend: ",fn$x$nodes$AndereFAs)
            }else{
                fn$x$nodes$Test<-paste0("Station: ",fn$x$nodes$Group2,
                                        "\n\nFall: ",fn$x$nodes$NodeID,
                                        "\nHauptdiagnose: ",fn$x$nodes$ICDfull,
                                        "\n\nInfektion: ",
                                        "\n     Am: ",fn$x$nodes$Infiziert,
                                        "\n     Durch: ",fn$x$nodes$Anstecker," (Infektion ",fn$x$nodes$Grad,". Grades)",
                                        "\n\nAnsteckend auf Station ",fn$x$nodes$Group2,":",
                                        ifelse(fn$x$nodes$Dauer==-1,"\nnoch vor infektiöser Phase wieder von Station entlassen",
                                               paste0("\n     Von ",fn$x$nodes$Ansteckend," bis ",fn$x$nodes$AnsteckendBis,
                                                      "\n     Aufenthaltsdauer: ",fn$x$nodes$Dauer," Tage",
                                                      "\n     Fälle angesteckt: ",fn$x$nodes$Kontakte)),
                                        "\n\nWeitere besuchte Stationen während ansteckend: ",fn$x$nodes$AndereFAs)
            }
            
            #nodes1$FarbeAussen<-nodes1$Farbe
            #nodes1$FarbeAussen[nodes1$AndereFAs!="-"]<-"#141414"
            fn$x$nodes$FarbeAussen<-nodes1$FarbeAussen
            fn$x$nodes$borderWidth<-1.5
            fn$x$nodes$borderWidth[fn$x$nodes$Grad==0]<-7
            
            fn <- onRender(fn, '
                function(el, x) {
                    // Convert R data frame to JavaScript array of objects
                var nodeColors = HTMLWidgets.dataframeToD3(x.nodes);
                var nodeData = HTMLWidgets.dataframeToD3(x.nodes);
                d3.selectAll(".node").select("circle")
                .style("stroke", function(d, i) {
                    return nodeColors[i].FarbeAussen;  // Use the AB column for node stroke color
                })
                .style("stroke-width", function(d, i) {
                    return nodeData[i].borderWidth + "px";  // Use the borderWidth column for node stroke width
                 });
              }
            ')
            
            
            output$force <- renderForceNetwork({
                fn})
            
            
            icd_help_out<-c("I: A00-B99",
                            "II: C00-D48",
                            "III: D50-D90",
                            "IV: E00-E90",
                            "V: F00-F99",
                            "VI: G00-G99",
                            "VII: H00-H59",
                            "VIII: H60-H95",
                            "IX: I00-I99",
                            "X: J00-J99",
                            "XI: K00-K93",
                            "XII: L00-L99",
                            "XIII: M00-M99",
                            "XIV: N00-N99",
                            "XV: O00-O99",
                            "XVI: P00-P96",
                            "XVII: Q00-Q99",
                            "XVIII: R00-R99",
                            "XIX: S00-T98",
                            "XX: V01-Y84",
                            "XXI: Z00-Z99",
                            "XXII: U00-U99",
                            "nein")
            
            risiko_andere<-nodes1$Group
            risiko_andere2<-risiko_andere[risiko_andere!=fa_ist_backup]
            risiko<-as.data.frame(table(risiko_andere2))
            
            risiko_andere<-nodes1$AndereFAs[nodes1$AndereFAs!="-"]
            risiko_andere<-str_split_fixed(risiko_andere,pattern = " \\(",n = Inf)[,1]
            risiko_andere2<-str_split_fixed(risiko_andere,pattern = ",",n = Inf)
            risiko_andere2<-sub("^ ","",risiko_andere2,fixed=F)
            #risiko_andere2<-gsub(" S","S",risiko_andere2,fixed=T)
            risiko_andere2<-as.vector(risiko_andere2)
            risiko_andere2<-risiko_andere2[risiko_andere2!=""]
            risiko_andere3<-risiko_andere2[risiko_andere2!=fa_ist_backup]
            risiko2<-as.data.frame(table(risiko_andere3))
            
            risiko_all<-data.frame(Abteilung=unique(c(risiko$risiko_andere2,risiko2$risiko_andere3)),Infiziert=NA,Verlegt=NA)
            risiko_all$Infiziert<-risiko$Freq[match(risiko_all$Abteilung,risiko$risiko_andere2)]
            risiko_all$Verlegt<-risiko2$Freq[match(risiko_all$Abteilung,risiko2$risiko_andere3)]
            risiko_all<-risiko_all[order(risiko_all$Infiziert,risiko_all$Verlegt,decreasing = T),]
            if(length(risiko_all)>0){
                risiko_out<-paste0(risiko$risiko_andere3," (",risiko$Freq,ifelse(risiko$Freq==1," Verlegung)"," Verlegungen)"))
                risiko_out<-paste0(risiko_all$Abteilung," (",ifelse(!is.na(risiko_all$Infiziert),paste0(risiko_all$Infiziert,ifelse(risiko_all$Infiziert==1," Infektion"," Infektionen")),""))
                risiko_out<-paste0(risiko_out,
                                      ifelse(!is.na(risiko_all$Verlegt)&!is.na(risiko_all$Infiziert),", ",""))
                risiko_out<-paste0(risiko_out,
                                      ifelse(!is.na(risiko_all$Verlegt),paste0(risiko_all$Verlegt,ifelse(risiko_all$Verlegt==1," Verlegung)"," Verlegungen)")),")"))
            }else{
                risiko_out<-"-"
            }


            
            if(input$FA_oder_Station=="Fachabteilungen"){
                if(kette_soll!=100){
                    output$text_analyse2<-renderUI(HTML(paste0("Fachabteilung: ",fa_ist_backup,
                                                               "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA_ICD)],collapse=", "),
                                                               "<br>Fall im Fokus: ",fall_ist_backup,
                                                               "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                               "<br><br>Latenzzeit: ",min_inkubation_tag,ifelse(min_inkubation_tag==1," Tag "," Tage "),
                                                               " und ",min_inkubation_stunde,ifelse(min_inkubation_stunde==1," Stunde"," Stunden"),
                                                               "<br>Minimale Expositionszeit: ",min_kontakt_tag,ifelse(min_kontakt_tag==1," Tag "," Tage "),
                                                               " und ",min_kontakt_stunde,ifelse(min_kontakt_stunde==1," Stunde"," Stunden"),
                                                               "<br>Infektiöse Phase: ",min_ansteckend_tag,ifelse(min_ansteckend_tag==1," Tag "," Tage "),
                                                               " und ",min_ansteckend_stunde,ifelse(min_ansteckend_stunde==1," Stunde"," Stunden"),
                                                               "<br>Maximale Länge der Infektions-Kette: ",kette_soll,
                                                               "<br><br>Mutmaßlich infizierte Fälle auf Fachabteilung ",fa_ist_backup,": ",nrow(nodes1[nodes1$Group==fa_ist_backup,]),
                                                               "<br>Infektionsrisiko für ",ifelse(length(risiko_all)==0,0,nrow(risiko_all)),
                                                               "/",(length(unique(input1$Abteilung))-1),
                                                               " weitere Fachabteilungen: ",paste0(risiko_out,collapse=", "),
                                                               "<br><br>Analyse erfolgreich durchgeführt")))
                }else{
                    output$text_analyse2<-renderUI(HTML(paste0("Fachabteilung: ",fa_ist_backup,
                                                               "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA_ICD)],collapse=", "),
                                                               "<br>Fall im Fokus: ",fall_ist_backup,
                                                               "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                               "<br><br>Latenzzeit: ",min_inkubation_tag,ifelse(min_inkubation_tag==1," Tag "," Tage "),
                                                               " und ",min_inkubation_stunde,ifelse(min_inkubation_stunde==1," Stunde"," Stunden"),
                                                               "<br>Minimale Expositionszeit: ",min_kontakt_tag,ifelse(min_kontakt_tag==1," Tag "," Tage "),
                                                               " und ",min_kontakt_stunde,ifelse(min_kontakt_stunde==1," Stunde"," Stunden"),
                                                               "<br>Infektiöse Phase: ",min_ansteckend_tag,ifelse(min_ansteckend_tag==1," Tag "," Tage "),
                                                               " und ",min_ansteckend_stunde,ifelse(min_ansteckend_stunde==1," Stunde"," Stunden"),
                                                               "<br>Maximale Länge der Infektions-Kette: unbegrenzt (hier: ",max(nodes1$Grad),")",
                                                               "<br><br>Mutmaßlich infizierte Fälle auf Fachabteilung ",fa_ist_backup,": ",nrow(nodes1[nodes1$Group==fa_ist_backup,]),
                                                               "<br>Infektionsrisiko für ",ifelse(length(risiko_all)==0,0,nrow(risiko_all)),
                                                               "/",(length(unique(input1$Abteilung))-1),
                                                               " weitere Fachabteilungen: ",paste0(risiko_out,collapse=", "),
                                                               "<br><br>Analyse erfolgreich durchgeführt")))
                }
            }else{
                if(kette_soll!=100){
                    output$text_analyse2<-renderUI(HTML(paste0("Station: ",fa_ist_backup,
                                                               "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA_ICD)],collapse=", "),
                                                               "<br>Fall im Fokus: ",fall_ist_backup,
                                                               "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                               "<br><br>Latenzzeit: ",min_inkubation_tag,ifelse(min_inkubation_tag==1," Tag "," Tage "),
                                                               " und ",min_inkubation_stunde,ifelse(min_inkubation_stunde==1," Stunde"," Stunden"),
                                                               "<br>Minimale Expositionszeit: ",min_kontakt_tag,ifelse(min_kontakt_tag==1," Tag "," Tage "),
                                                               " und ",min_kontakt_stunde,ifelse(min_kontakt_stunde==1," Stunde"," Stunden"),
                                                               "<br>Infektiöse Phase: ",min_ansteckend_tag,ifelse(min_ansteckend_tag==1," Tag "," Tage "),
                                                               " und ",min_ansteckend_stunde,ifelse(min_ansteckend_stunde==1," Stunde"," Stunden"),
                                                               "<br>Maximale Länge der Infektions-Kette: ",kette_soll,
                                                               "<br><br>Mutmaßlich infizierte Fälle auf Station ",fa_ist_backup,": ",nrow(nodes1[nodes1$Group==fa_ist_backup,]),
                                                               "<br>Infektionsrisiko für ",ifelse(length(risiko_all)==0,0,nrow(risiko_all)),
                                                               "/",(length(unique(input1$Abteilung))-1),
                                                               " weitere Stationen: ",paste0(risiko_out,collapse=", "),
                                                               "<br><br>Analyse erfolgreich durchgeführt")))
                }else{
                    output$text_analyse2<-renderUI(HTML(paste0("Station: ",fa_ist_backup,
                                                               "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA_ICD)],collapse=", "),
                                                               "<br>Fall im Fokus: ",fall_ist_backup,
                                                               "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                               "<br><br>Latenzzeit: ",min_inkubation_tag,ifelse(min_inkubation_tag==1," Tag "," Tage "),
                                                               " und ",min_inkubation_stunde,ifelse(min_inkubation_stunde==1," Stunde"," Stunden"),
                                                               "<br>Minimale Expositionszeit: ",min_kontakt_tag,ifelse(min_kontakt_tag==1," Tag "," Tage "),
                                                               " und ",min_kontakt_stunde,ifelse(min_kontakt_stunde==1," Stunde"," Stunden"),
                                                               "<br>Infektiöse Phase: ",min_ansteckend_tag,ifelse(min_ansteckend_tag==1," Tag "," Tage "),
                                                               " und ",min_ansteckend_stunde,ifelse(min_ansteckend_stunde==1," Stunde"," Stunden"),
                                                               "<br>Maximale Länge der Infektions-Kette: unbegrenzt (hier: ",max(nodes1$Grad),")",
                                                               "<br><br>Mutmaßlich infizierte Fälle auf Station ",fa_ist_backup,": ",nrow(nodes1[nodes1$Group==fa_ist_backup,]),
                                                               "<br>Infektionsrisiko für ",ifelse(length(risiko_all)==0,0,nrow(risiko_all)),
                                                               "/",(length(unique(input1$Abteilung))-1),
                                                               " weitere Stationen: ",paste0(risiko_out,collapse=", "),
                                                               "<br><br>Analyse erfolgreich durchgeführt")))
                }
            }
            
            
            
            
            
            
            }
          shinyjs::html("text", paste0("<br>Analyse erfolgreich durchgeführt<br><br>"), add = TRUE)
            
        })
        
    })

    
    observeEvent(input$Grad_Intern,{
        output$FA_erweitert_UI2<-renderUI({NULL})
        output$FA_erweitert_UI3<-renderUI({NULL})
    })
    observeEvent(input$Min_Ansteckend_Stunde,{
        output$FA_erweitert_UI2<-renderUI({NULL})
        output$FA_erweitert_UI3<-renderUI({NULL})
    })
    observeEvent(input$Min_Ansteckend_Tag,{
        output$FA_erweitert_UI2<-renderUI({NULL})
        output$FA_erweitert_UI3<-renderUI({NULL})
    })
    observeEvent(input$Min_Kontakt_Stunde,{
        output$FA_erweitert_UI2<-renderUI({NULL})
        output$FA_erweitert_UI3<-renderUI({NULL})
    })
    observeEvent(input$Min_Kontakt_Tag,{
        output$FA_erweitert_UI2<-renderUI({NULL})
        output$FA_erweitert_UI3<-renderUI({NULL})
    })
    observeEvent(input$Min_Inkubation_Stunde,{
        output$FA_erweitert_UI2<-renderUI({NULL})
        output$FA_erweitert_UI3<-renderUI({NULL})
    })
    observeEvent(input$Min_Inkubation_Tag,{
        output$FA_erweitert_UI2<-renderUI({NULL})
        output$FA_erweitert_UI3<-renderUI({NULL})
    })
    observeEvent(input$Fall_Inkubation,{
        output$FA_erweitert_UI2<-renderUI({NULL})
        output$FA_erweitert_UI3<-renderUI({NULL})
    })
    
    observeEvent(input$FA_Fokus,{
        output$FA_erweitert_UI2<-renderUI({NULL})
        output$FA_erweitert_UI3<-renderUI({NULL})
    })
    observeEvent(input$switch99,{
        output$FA_erweitert_UI2<-renderUI({NULL})
        output$FA_erweitert_UI3<-renderUI({NULL})
    })
    observeEvent(input$Fall_Fokus_FA,{
        output$FA_erweitert_UI2<-renderUI({NULL})
        output$FA_erweitert_UI3<-renderUI({NULL})
    })
    observeEvent(input$FA_Start,{
        output$FA_erweitert_UI2<-renderUI({NULL})
        output$FA_erweitert_UI3<-renderUI({NULL})
    })
    observeEvent(input$FA_Ende,{
        output$FA_erweitert_UI2<-renderUI({NULL})
        output$FA_erweitert_UI3<-renderUI({NULL})
    })
    observeEvent(input$FA_ICDKapitel,{
        output$FA_erweitert_UI2<-renderUI({NULL})
        output$FA_erweitert_UI3<-renderUI({NULL})
    })
    observeEvent(input$FA_ICDExakt,{
        output$FA_erweitert_UI2<-renderUI({NULL})
        output$FA_erweitert_UI3<-renderUI({NULL})
    })

    observeEvent(input$ST_Fokus,{
        output$FA_erweitert_UI2<-renderUI({NULL})
        output$FA_erweitert_UI3<-renderUI({NULL})
    })
    observeEvent(input$switch99c,{
        output$FA_erweitert_UI2<-renderUI({NULL})
        output$FA_erweitert_UI3<-renderUI({NULL})
    })
    observeEvent(input$Fall_Fokus_ST,{
        output$FA_erweitert_UI2<-renderUI({NULL})
        output$FA_erweitert_UI3<-renderUI({NULL})
    })
    observeEvent(input$ST_Start,{
        output$FA_erweitert_UI2<-renderUI({NULL})
        output$FA_erweitert_UI3<-renderUI({NULL})
    })
    observeEvent(input$ST_Ende,{
        output$FA_erweitert_UI2<-renderUI({NULL})
        output$FA_erweitert_UI3<-renderUI({NULL})
    })
    observeEvent(input$ST_ICDKapitel,{
        output$FA_erweitert_UI2<-renderUI({NULL})
        output$FA_erweitert_UI3<-renderUI({NULL})
    })
    observeEvent(input$ST_ICDExakt,{
        output$FA_erweitert_UI2<-renderUI({NULL})
        output$FA_erweitert_UI3<-renderUI({NULL})
    })

    
    
    
   

    
    })
  })
    
    session$onSessionEnded(function(){
      stopApp()
    })
    
    
}












