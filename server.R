server <- function(input, output, session) {
  
  vFootballers <- c("Toporek", "Kruczyk", "Segit", "Rucki", "Matulewicz", "Dubel")
  
  output$redDef <- renderUI({
    selectInput(
      "redDefSelect",
      "Obrona czerwonych",
      choices = c(Wybierz='', setdiff(vFootballers, union(input$redAtkSelect, union(input$blueDefSelect, input$blueAtkSelect)))),#c(Wybierz='', vFootballers),
      selected = ifelse(!is.null(input[["redDefSelect"]]), input[["redDefSelect"]], ""), #NULL,
      selectize = TRUE
    )
  })
  
  output$redAtk <- renderUI({
    selectInput(
      "redAtkSelect",
      "Napad czerwonych",
      choices = c(Wybierz='', setdiff(vFootballers, union(input$redDefSelect, union(input$blueDefSelect, input$blueAtkSelect)))),
      selected = ifelse(!is.null(input[["redAtkSelect"]]), input[["redAtkSelect"]], ""), #NULL,
      selectize = TRUE
    )
  })
  
  output$blueDef <- renderUI({
    selectInput(
      "blueDefSelect",
      "Obrona niebieskich",
      choices = c(Wybierz='', setdiff(vFootballers, union(input$redAtkSelect, union(input$redDefSelect, input$blueAtkSelect)))),
      selected = ifelse(!is.null(input[["blueDefSelect"]]), input[["blueDefSelect"]], ""), #NULL,
      selectize = TRUE
    )
  })
  
  output$blueAtk <- renderUI({
    selectInput(
      "blueAtkSelect",
      "Napad niebieskich",
      choices = c(Wybierz='', setdiff(vFootballers, union(input$redAtkSelect, union(input$blueDefSelect, input$redDefSelect)))),
      selected = ifelse(!is.null(input[["blueAtkSelect"]]), input[["blueAtkSelect"]], ""), #NULL,
      selectize = TRUE
    )
  })

#####
#
#####
  
  observeEvent(input$RankingMainTable_rows_selected, {
    updateSelectInput(
      session,
      "redAtkSelect",
      "Napad czerwonych",
      choices = c(Wybierz='', setdiff(vFootballers, union(input$redDefSelect, union(input$blueDefSelect, input$blueAtkSelect)))),
      selected = rFootball$vTableM[input$RankingMainTable_rows_selected, "Red.Atk"]
    )
    updateSelectInput(
      session,
      "redDefSelect",
      "Obrona czerwonych",
      choices = c(Wybierz='', setdiff(vFootballers, union(input$redAtkSelect, union(input$blueDefSelect, input$blueAtkSelect)))),
      selected = rFootball$vTableM[input$RankingMainTable_rows_selected, "Red.Def"]
    )
    updateSelectInput(
      session,
      "blueDefSelect",
      "Obrona niebieskich",
      choices = c(Wybierz='', setdiff(vFootballers, union(input$redAtkSelect, union(input$redDefSelect, input$blueAtkSelect)))),
      selected = rFootball$vTableM[input$RankingMainTable_rows_selected, "Blue.Def"]
    )
    updateSelectInput(
      session,
      "blueAtkSelect",
      "Napad niebieskich",
      choices = c(Wybierz='', setdiff(vFootballers, union(input$redAtkSelect, union(input$blueDefSelect, input$redDefSelect)))),
      selected = rFootball$vTableM[input$RankingMainTable_rows_selected, "Blue.Atk"]
    )
  })
  
  
#####
#  
#####
  
  output$wynikUI <- renderUI({
    fluidRow(
      column(width = 3,
             br(),
        radioButtons(
          "redORblue",
          "Wygrani i wynik przegranych",
          choices = list("Niebiescy" = "blue",
                         "Czerwoni" = "red"),
          inline = TRUE,
          width = '100%'
        )       
      ),
      column(width = 4,
             br(),
        sliderInput(
          "looserScore",
          NULL,#"Wynik przegranych",
          min = 0,
          max = 9,
          value = 5,
          step = 1
        )       
      ),
      column(width = 5,
        selectInput(
          inputId = "sklady",
          label = "Gramy?",
          choices = vFootballers,
          selected = vFootballers,
          multiple = TRUE
        ),
        actionButton(
          "zapiszMecz",
          label = "Zapisz wynik meczu",
          icon = icon("check"),
          width = '100%'
        )       
      )
    )
  })
  
#####
#
#####
  
  output$strzelcyUI <- renderUI({
    req(input$blueAtkSelect)
    req(input$blueDefSelect)
    req(input$redAtkSelect)
    req(input$redDefSelect)
    
    column(width = 12,
    fluidRow(
      column(width = 12,
        numericInput(
          "blueDefScore",
          label = input$blueDefSelect,
          min = 0,
          max = 10,
          step = 1,
          value = 5
        )       
      )
    ),
    fluidRow(
      column(width = 12,
             numericInput(
               "blueAtkScore",
               label = input$blueAtkSelect,
               min = 0,
               max = 10,
               step = 1,
               value = 5
             )       
      )
    ),
    fluidRow(
      column(width = 12,
             numericInput(
               "redDefScore",
               label = input$redDefSelect,
               min = 0,
               max = 10,
               step = 1,
               value = 5
             )       
      )
    ),
    fluidRow(
      column(width = 12,
             numericInput(
               "redAtkScore",
               label = input$redAtkSelect,
               min = 0,
               max = 10,
               step = 1,
               value = 5
             )       
      )
    )
    )
  })  
  
  
#####
#
#####
  
  rFootball <- reactiveValues()
  
  rFootball$table <- read.csv("./football.csv", header = TRUE, sep = ";")
  rFootball$vTableM <- data.frame()
  load("./QuantFootball.rda")
  
  observe({
    req(input$sklady)
    vPlayers <- input$sklady
    rFootball$vTableM <- subset(rFootball$table[,1:6], Red.Def %in% vPlayers & Blue.Def %in% vPlayers & Red.Atk %in% vPlayers & Blue.Atk %in% vPlayers)
  })
  
  output$RankingMainTable <- renderDataTable(server = FALSE, {
    req(input$sklady)
    req(rFootball$vTableM)

    datatable(
     #rFootball$table,
     rFootball$vTableM,
     filter = "bottom",
     selection = 'single',
     rownames = FALSE,
     colnames = c("Obrona niebieskich" = "Blue.Def",
                  "Atak niebieskich" = "Blue.Atk",
                  "Obrona czerwonych" = "Red.Def",
                  "Atak czerwonych" = "Red.Atk",
                  "Gole niebiescy" = "Score.Blue",
                  "Gole czerwoni" = "Score.Red"),
     options = list(
      auto.width = TRUE,
      pageLength = 10,
      lengthMenu = c(10, 30, 60)
     )
    )
  })
  
  
  
#####
#
#####

  
  observeEvent(input$zapiszMecz, {
    vUklad <- intersect(which(rFootball$table$Blue.Def == input$blueDefSelect),
                        intersect(which(rFootball$table$Blue.Atk == input$blueAtkSelect),
                        intersect(which(rFootball$table$Red.Def == input$redDefSelect),
                        which(rFootball$table$Red.Atk == input$redAtkSelect))))
    
    if(!is.na(rFootball$table[vUklad, "Score.Blue"])) {
      return()
    }
    
    vLooser <- c(-2, -1, 0, 1)
    vWinner <- c(5, 4, 3, 2)
    
    # dodanie punktow zwyciezcom i przegranym
    if(input$redORblue == "blue") {
      rFootball$table[vUklad,"Score.Blue"] <- 10
      rFootball$table[vUklad,"Score.Red"] <- input$looserScore
      if(input$looserScore == 0) {
        rFootball$table[vUklad,"Points.Blue"] <- vWinner[1]
        rFootball$table[vUklad,"Points.Red"] <- vLooser[1]
      } else if(input$looserScore == 9) {
        rFootball$table[vUklad,"Points.Blue"] <- vWinner[4]
        rFootball$table[vUklad,"Points.Red"] <- vLooser[4]
      } else if(input$looserScore <= 4) {
        rFootball$table[vUklad,"Points.Blue"] <- vWinner[2]
        rFootball$table[vUklad,"Points.Red"] <- vLooser[2]
      } else if(input$looserScore > 4) {
        rFootball$table[vUklad,"Points.Blue"] <- vWinner[3]
        rFootball$table[vUklad,"Points.Red"] <- vLooser[3]
      }
    } else if(input$redORblue == "red") {
      rFootball$table[vUklad,"Score.Red"] <- 10
      rFootball$table[vUklad,"Score.Blue"] <- input$looserScore
      if(input$looserScore == 0) {
        rFootball$table[vUklad,"Points.Blue"] <- vLooser[1]
        rFootball$table[vUklad,"Points.Red"] <- vWinner[1]
      } else if(input$looserScore == 9) {
        rFootball$table[vUklad,"Points.Blue"] <- vLooser[4]
        rFootball$table[vUklad,"Points.Red"] <- vWinner[4]
      } else if(input$looserScore <= 4) {
        rFootball$table[vUklad,"Points.Blue"] <- vLooser[2]
        rFootball$table[vUklad,"Points.Red"] <- vWinner[2]
      } else if(input$looserScore > 4) {
        rFootball$table[vUklad,"Points.Blue"] <- vLooser[3]
        rFootball$table[vUklad,"Points.Red"] <- vWinner[3]
      }
    }
    
    load("./QuantFootball.rda")
   
    vGracze <- union(input$blueAtkSelect, union(input$redAtkSelect, union(input$blueDefSelect, input$redDefSelect)))
    
    # liczba rozegranych spotkan (niewazne ktory zawodnik zostanie wziety)
    vLen <- length(QuantFootball[[1]]$mecze)
    
    for(i in 1:6) {
      # niegrajacym przepisuje to samo
      if(!(vFootballers[i] %in% vGracze)) {
        QuantFootball[[i]]$mecze[[vLen+1]] <- QuantFootball[[i]]$mecze[[vLen]]
        QuantFootball[[i]]$punkty[[vLen+1]] <- QuantFootball[[i]]$punkty[[vLen]]
        QuantFootball[[i]]$bramkiObrona[[vLen+1]] <- QuantFootball[[i]]$bramkiObrona[[vLen]]
        QuantFootball[[i]]$bramkiAtak[[vLen+1]] <- QuantFootball[[i]]$bramkiAtak[[vLen]]
      }
    }
   
    # bramki i mecze
    QuantFootball[[input$blueDefSelect]]$mecze[[vLen+1]] <- QuantFootball[[input$blueDefSelect]]$mecze[[vLen]] + 1
    QuantFootball[[input$blueDefSelect]]$bramkiObrona[[vLen+1]] <- QuantFootball[[input$blueDefSelect]]$bramkiObrona[[vLen]] + input$blueDefScore
    QuantFootball[[input$blueDefSelect]]$bramkiAtak[[vLen+1]] <- QuantFootball[[input$blueDefSelect]]$bramkiAtak[[vLen]]
    
    QuantFootball[[input$blueAtkSelect]]$mecze[[vLen+1]] <- QuantFootball[[input$blueAtkSelect]]$mecze[[vLen]] + 1
    QuantFootball[[input$blueAtkSelect]]$bramkiObrona[[vLen+1]] <- QuantFootball[[input$blueAtkSelect]]$bramkiObrona[[vLen]]
    QuantFootball[[input$blueAtkSelect]]$bramkiAtak[[vLen+1]] <- QuantFootball[[input$blueAtkSelect]]$bramkiAtak[[vLen]] + input$blueAtkScore
    
    QuantFootball[[input$redDefSelect]]$mecze[vLen+1] <- QuantFootball[[input$redDefSelect]]$mecze[[vLen]] + 1
    QuantFootball[[input$redDefSelect]]$bramkiObrona[vLen+1] <- QuantFootball[[input$redDefSelect]]$bramkiObrona[[vLen]] + input$redDefScore
    QuantFootball[[input$redDefSelect]]$bramkiAtak[vLen+1] <- QuantFootball[[input$redDefSelect]]$bramkiAtak[[vLen]]
    
    QuantFootball[[input$redAtkSelect]]$mecze[[vLen+1]] <- QuantFootball[[input$redAtkSelect]]$mecze[[vLen]] + 1
    QuantFootball[[input$redAtkSelect]]$bramkiObrona[[vLen+1]] <- QuantFootball[[input$redAtkSelect]]$bramkiObrona[[vLen]]
    QuantFootball[[input$redAtkSelect]]$bramkiAtak[[vLen+1]] <- QuantFootball[[input$redAtkSelect]]$bramkiAtak[[vLen]] + input$redAtkScore
    
    if(input$looserScore == 9) {
      QuantFootball[[input$blueDefSelect]]$punkty[[vLen+1]] <- QuantFootball[[input$blueDefSelect]]$punkty[[vLen]] + ifelse(input$redORblue == "blue", vWinner[4], vLooser[4])
      QuantFootball[[input$blueAtkSelect]]$punkty[[vLen+1]] <- QuantFootball[[input$blueAtkSelect]]$punkty[[vLen]] + ifelse(input$redORblue == "blue", vWinner[4], vLooser[4])
      QuantFootball[[input$redDefSelect]]$punkty[[vLen+1]] <- QuantFootball[[input$redDefSelect]]$punkty[[vLen]] + ifelse(input$redORblue == "red", vWinner[4], vLooser[4])
      QuantFootball[[input$redAtkSelect]]$punkty[[vLen+1]] <- QuantFootball[[input$redAtkSelect]]$punkty[[vLen]] + ifelse(input$redORblue == "red", vWinner[4], vLooser[4])
    } else if(input$looserScore == 0) {
      QuantFootball[[input$blueDefSelect]]$punkty[[vLen+1]] <- QuantFootball[[input$blueDefSelect]]$punkty[[vLen]] + ifelse(input$redORblue == "blue", vWinner[1], vLooser[1])
      QuantFootball[[input$blueAtkSelect]]$punkty[[vLen+1]] <- QuantFootball[[input$blueAtkSelect]]$punkty[[vLen]] + ifelse(input$redORblue == "blue", vWinner[1], vLooser[1])
      QuantFootball[[input$redDefSelect]]$punkty[[vLen+1]] <- QuantFootball[[input$redDefSelect]]$punkty[[vLen]] + ifelse(input$redORblue == "red", vWinner[1], vLooser[1])
      QuantFootball[[input$redAtkSelect]]$punkty[[vLen+1]] <- QuantFootball[[input$redAtkSelect]]$punkty[[vLen]] + ifelse(input$redORblue == "red", vWinner[1], vLooser[1])
    } else if(input$looserScore <= 4) {
      QuantFootball[[input$blueDefSelect]]$punkty[[vLen+1]] <- QuantFootball[[input$blueDefSelect]]$punkty[[vLen]] + ifelse(input$redORblue == "blue", vWinner[2], vLooser[2])
      QuantFootball[[input$blueAtkSelect]]$punkty[[vLen+1]] <- QuantFootball[[input$blueAtkSelect]]$punkty[[vLen]] + ifelse(input$redORblue == "blue", vWinner[2], vLooser[2])
      QuantFootball[[input$redDefSelect]]$punkty[[vLen+1]] <- QuantFootball[[input$redDefSelect]]$punkty[[vLen]] + ifelse(input$redORblue == "red", vWinner[2], vLooser[2])
      QuantFootball[[input$redAtkSelect]]$punkty[[vLen+1]] <- QuantFootball[[input$redAtkSelect]]$punkty[[vLen]] + ifelse(input$redORblue == "red", vWinner[2], vLooser[2])
    } else if(input$looserScore > 4) {
      QuantFootball[[input$blueDefSelect]]$punkty[[vLen+1]] <- QuantFootball[[input$blueDefSelect]]$punkty[[vLen]] + ifelse(input$redORblue == "blue", vWinner[3], vLooser[3])
      QuantFootball[[input$blueAtkSelect]]$punkty[[vLen+1]] <- QuantFootball[[input$blueAtkSelect]]$punkty[[vLen]] + ifelse(input$redORblue == "blue", vWinner[3], vLooser[3])
      QuantFootball[[input$redDefSelect]]$punkty[[vLen+1]] <- QuantFootball[[input$redDefSelect]]$punkty[[vLen]] + ifelse(input$redORblue == "red", vWinner[3], vLooser[3])
      QuantFootball[[input$redAtkSelect]]$punkty[[vLen+1]] <- QuantFootball[[input$redAtkSelect]]$punkty[[vLen]] + ifelse(input$redORblue == "red", vWinner[3], vLooser[3])
    }

    save(QuantFootball, file = "./QuantFootball.rda")
    write.csv2(rFootball$table, "./football.csv", row.names = FALSE)
    
    updateSelectInput(
      session,
      "redDefSelect",
      "Obrona czerwonych",
      choices = c(Wybierz='', setdiff(vFootballers, union(input$redAtkSelect, union(input$blueDefSelect, input$blueAtkSelect)))),
      selected = NULL
    )
    updateSelectInput(
      session,
      "redAtkSelect",
      "Napad czerwonych",
      choices = c(Wybierz='', setdiff(vFootballers, union(input$redDefSelect, union(input$blueDefSelect, input$blueAtkSelect)))),
      selected = NULL
    )
    updateSelectInput(
      session,
      "blueDefSelect",
      "Obrona niebieskich",
      choices = c(Wybierz='', setdiff(vFootballers, union(input$redAtkSelect, union(input$redDefSelect, input$blueAtkSelect)))),
      selected = NULL
    )
    updateSelectInput(
      session,
      "blueAtkSelect",
      "Napad niebieskich",
      choices = c(Wybierz='', setdiff(vFootballers, union(input$redAtkSelect, union(input$blueDefSelect, input$redDefSelect)))),
      selected = NULL
    )

  })
  
  
  
#####
#
#####
  
  output$punktacja <- renderDataTable({
    
    vName <- c("Sromotna Klęska", "Blamaż i Kretes", "Porażka i Honorowa przegrana", "Nieistotna statystycznie przegrana")
    vScore <- c("0", "1-4", "5-8", "9")
    vLooser <- c(-2, -1, 0, 1)
    vWinner <- c(5, 4, 3, 2)
    
    vInfoTable <- data.frame(cbind(vName, vScore, vLooser, vWinner))
    colnames(vInfoTable) <- c("Opis", "Wynik przegranych", "Punkty przegranych", "Punkty wygranych")
    
    datatable(
      vInfoTable,
        options = list(
          paging = FALSE,
          searching = FALSE,
          scrollX = TRUE,
          info = FALSE,
          autoWidth = TRUE
        ),
      rownames=FALSE
    )
  })
  

  output$TabelaPunkty <- renderDataTable({
    req(QuantFootball)
    req(rFootball$table)
    
    load("./QuantFootball.rda")
    
    vLen <- length(QuantFootball[[1]]$mecze)
    
    vM <- NULL
    for(i in 1:length(vFootballers)) {vM <- c(vM, QuantFootball[[vFootballers[i]]]$mecze[[vLen]])}
    
    vS <- NULL
    for(i in 1:length(vFootballers)) {vS <- c(vS, QuantFootball[[vFootballers[i]]]$punkty[[vLen]])}
    
    vT <- NULL
    for(i in 1:length(vFootballers)) {vT <- c(vT, (QuantFootball[[vFootballers[i]]]$punkty[[vLen]])/(QuantFootball[[vFootballers[i]]]$mecze[[vLen]]))}
    
    vD <- NULL
    for(i in 1:length(vFootballers)) {vD <- c(vD, QuantFootball[[vFootballers[i]]]$bramkiObrona[[vLen]])}
    
    vA <- NULL
    for(i in 1:length(vFootballers)) {vA <- c(vA, QuantFootball[[vFootballers[i]]]$bramkiAtak[[vLen]])}
    
    vW <- lapply(vFootballers, function(i) {
      length(union(intersect(union(which(na.omit(rFootball$table)$Blue.Def == i), which(na.omit(rFootball$table)$Blue.Atk == i)),
                             which(na.omit(rFootball$table)$Score.Blue == 10)),
                   intersect(union(which(na.omit(rFootball$table)$Red.Def == i), which(na.omit(rFootball$table)$Red.Atk == i)),
                             which(na.omit(rFootball$table)$Score.Red == 10))
              )
      )
    })
    
    
    vNazwisko <- vFootballers
    vMecze <- unlist(vM) 
    vWin <-  paste0(round(100*(unlist(vW)/unlist(vM)),0), "%", " [", unlist(vW), "]")
    vSuma <- unlist(vS)
    vSrednia <- round(unlist(vT),2)
    vObrona <- unlist(vD)
    vAtak <- unlist(vA)
    vGole <- vObrona + vAtak
    
    vTabela <- data.frame(cbind(vNazwisko, vMecze, vWin, vSuma, vSrednia, vGole))
    colnames(vTabela) <- c("Gracz", "Mecze", "Wygrane", "Punkty", "Średnia", "Bramki")
    
    datatable(
      vTabela,        
      options = list(
        paging = FALSE,
        searching = FALSE,
        scrollX = TRUE,
        info = FALSE,
        autoWidth = TRUE
      ),
      rownames=FALSE
    )
  })
  
  
  
  output$TabelaPara <- renderDataTable({
    req(QuantFootball)
    req(rFootball$table)
    
    load("./QuantFootball.rda")
    
    vLooser <- c(-2, -1, 0, 1)
    vWinner <- c(5, 4, 3, 2)
    
    vTabelaBestTeam <- unique(rFootball$table[,1:2])
    colnames(vTabelaBestTeam) <- c("Obrona", "Napad")
   
    vTabelaBestTeam$Mecze <- apply(vTabelaBestTeam, 1, function(i) {
      return(nrow(na.omit(rFootball$table[intersect(which(rFootball$table$Blue.Def == i[1]), which(rFootball$table$Blue.Atk == i[2])),])) + 
               nrow(na.omit(rFootball$table[intersect(which(rFootball$table$Red.Def == i[1]), which(rFootball$table$Red.Atk == i[2])),])))
    })
    
    vTabelaBestTeam <- vTabelaBestTeam[which(vTabelaBestTeam$Mecze > 0),]
    
    vTabelaBestTeam$Wygrane <- apply(vTabelaBestTeam, 1, function(i) {
      return(nrow(na.omit(rFootball$table[intersect(intersect(which(rFootball$table$Blue.Def == i[1]),
                                                              which(rFootball$table$Blue.Atk == i[2])),
                                                    which(rFootball$table$Score.Blue == 10)),])) + 
               nrow(na.omit(rFootball$table[intersect(intersect(which(rFootball$table$Red.Def == i[1]),
                                                                which(rFootball$table$Red.Atk == i[2])),
                                                      which(rFootball$table$Score.Red == 10)),])))
    })
    
    vTabelaBestTeam$Punkty <- apply(vTabelaBestTeam, 1, function(i) {
      return(sum(na.omit(rFootball$table[intersect(which(rFootball$table$Blue.Def == i[1]), which(rFootball$table$Blue.Atk == i[2])),])$Points.Blue) + 
               sum(na.omit(rFootball$table[intersect(which(rFootball$table$Red.Def == i[1]), which(rFootball$table$Red.Atk == i[2])),])$Points.Red))
    })
    
    vTabelaBestTeam$Srednia <- round(vTabelaBestTeam$Punkty/vTabelaBestTeam$Mecze, 2)
    
    vTabelaBestTeam <- vTabelaBestTeam[order(vTabelaBestTeam$Srednia, vTabelaBestTeam$Punkty, vTabelaBestTeam$Wygrane, decreasing = TRUE),]
    
    datatable(
      vTabelaBestTeam[1,],        
      options = list(
        paging = FALSE,
        searching = FALSE,
        scrollX = TRUE,
        info = FALSE,
        autoWidth = TRUE
      ),
      rownames=FALSE
    )
  })
  
  
  
  output$TabelaObrona <- renderDataTable({
    req(QuantFootball)
    req(rFootball$table)
    
    load("./QuantFootball.rda")
    
    vLooser <- c(-2, -1, 0, 1)
    vWinner <- c(5, 4, 3, 2)
    
    vLen <- length(QuantFootball[[1]]$mecze)
    vD <- NULL
    for(i in 1:length(vFootballers)) {vD <- c(vD, QuantFootball[[vFootballers[i]]]$bramkiObrona[[vLen]])}
  
    vM <- lapply(vFootballers, function(i) {
      length(union(which(na.omit(rFootball$table)$Blue.Def == i), which(na.omit(rFootball$table)$Red.Def == i)))
    })
    vW <- lapply(vFootballers, function(i) {
      length(union(intersect(which(na.omit(rFootball$table)$Blue.Def == i),
                             which(na.omit(rFootball$table)$Score.Blue == 10)),
                   intersect(which(na.omit(rFootball$table)$Red.Def == i),
                             which(na.omit(rFootball$table)$Score.Red == 10))
                   )
             )
    })
    vS <- lapply(vFootballers, function(i) {
      vPlayed <- union(which(na.omit(rFootball$table)$Blue.Def == i), which(na.omit(rFootball$table)$Red.Def == i))
      vWin <- union(intersect(which(na.omit(rFootball$table)$Blue.Def == i),
                              which(na.omit(rFootball$table)$Score.Blue == 10)),
                    intersect(which(na.omit(rFootball$table)$Red.Def == i),
                              which(na.omit(rFootball$table)$Score.Red == 10))
                    )

      vScoreS <- lapply(vPlayed, function(j) {
        if(min(na.omit(rFootball$table)[j, c("Score.Blue", "Score.Red")]) == 0) {
          if(j %in% vWin) {
            return(vWinner[1])
          } else {
            return(vLooser[1])
          }
        } else if(min(na.omit(rFootball$table)[j, c("Score.Blue", "Score.Red")]) == 9) {
          if(j %in% vWin) {
            return(vWinner[4])
          } else {
            return(vLooser[4])
          }
        } else if(min(na.omit(rFootball$table)[j, c("Score.Blue", "Score.Red")]) <= 4) {
          if(j %in% vWin) {
            return(vWinner[2])
          } else {
            return(vLooser[2])
          }
        } else if(min(na.omit(rFootball$table)[j, c("Score.Blue", "Score.Red")]) > 4) {
          if(j %in% vWin) {
            return(vWinner[3])
          } else {
            return(vLooser[3])
          }
        }
      })
      return(sum(unlist(vScoreS)))
    })
    
    
    vNazwisko <- vFootballers
    vMecze <- unlist(vM) 
    vWin <- paste0(round(100*(unlist(vW)/unlist(vM)),0), "%", " [", unlist(vW), "]")
    vSuma <- unlist(vS)
    vSrednia <- round(vSuma/vMecze, 2)
    vObrona <- unlist(vD)
    vObrSrednia <- vObrona/vMecze
    
    vTabela <- data.frame(cbind(vNazwisko, vMecze, vWin, vSuma, vSrednia, vObrona, vObrSrednia))
    colnames(vTabela) <- c("Gracz", "Mecze", "Wygrane", "Punkty", "Średnia", "Bramki", "Bramki/mecz")
    
    datatable(
      vTabela,        
      options = list(
        paging = FALSE,
        searching = FALSE,
        scrollX = TRUE,
        info = FALSE,
        autoWidth = TRUE
      ),
      rownames=FALSE
    )
  })
  
  
  
  
  output$TabelaAtak <- renderDataTable({
    req(QuantFootball)
    req(rFootball$table)
    
    load("./QuantFootball.rda")
    
    vLooser <- c(-2, -1, 0, 1)
    vWinner <- c(5, 4, 3, 2)
    
    vLen <- length(QuantFootball[[1]]$mecze)
    vA <- NULL
    for(i in 1:length(vFootballers)) {vA <- c(vA, QuantFootball[[vFootballers[i]]]$bramkiAtak[[vLen]])}
    
    vM <- lapply(vFootballers, function(i) {
      length(union(which(na.omit(rFootball$table)$Blue.Atk == i), which(na.omit(rFootball$table)$Red.Atk == i)))
    })
    vW <- lapply(vFootballers, function(i) {
      length(union(intersect(which(na.omit(rFootball$table)$Blue.Atk == i),
                             which(na.omit(rFootball$table)$Score.Blue == 10)),
                   intersect(which(na.omit(rFootball$table)$Red.Atk == i),
                             which(na.omit(rFootball$table)$Score.Red == 10))
      )
      )
    })
    vS <- lapply(vFootballers, function(i) {
      vPlayed <- union(which(na.omit(rFootball$table)$Blue.Atk == i), which(na.omit(rFootball$table)$Red.Atk == i))
      vWin <- union(intersect(which(na.omit(rFootball$table)$Blue.Atk == i),
                              which(na.omit(rFootball$table)$Score.Blue == 10)),
                    intersect(which(na.omit(rFootball$table)$Red.Atk == i),
                              which(na.omit(rFootball$table)$Score.Red == 10))
      )
      
      vScoreS <- lapply(vPlayed, function(j) {
        if(min(na.omit(rFootball$table)[j, c("Score.Blue", "Score.Red")]) == 0) {
          if(j %in% vWin) {
            return(vWinner[1])
          } else {
            return(vLooser[1])
          }
        } else if(min(na.omit(rFootball$table)[j, c("Score.Blue", "Score.Red")]) == 9) {
          if(j %in% vWin) {
            return(vWinner[4])
          } else {
            return(vLooser[4])
          }
        } else if(min(na.omit(rFootball$table)[j, c("Score.Blue", "Score.Red")]) <= 4) {
          if(j %in% vWin) {
            return(vWinner[2])
          } else {
            return(vLooser[2])
          }
        } else if(min(na.omit(rFootball$table)[j, c("Score.Blue", "Score.Red")]) > 4) {
          if(j %in% vWin) {
            return(vWinner[3])
          } else {
            return(vLooser[3])
          }
        }
      })
      return(sum(unlist(vScoreS)))
    })
    
    
    vNazwisko <- vFootballers
    vMecze <- unlist(vM) 
    vWin <- paste0(round(100*(unlist(vW)/unlist(vM)),0), "%", " [", unlist(vW), "]")
    vSuma <- unlist(vS)
    vSrednia <- round(vSuma/vMecze, 2)
    vAtak <- unlist(vA)
    vAtkSrednia <- vAtak/vMecze
    
    vTabela <- data.frame(cbind(vNazwisko, vMecze, vWin, vSuma, vSrednia, vAtak, vAtkSrednia))
    colnames(vTabela) <- c("Gracz", "Mecze", "Wygrane", "Punkty", "Średnia", "Bramki", "Bramki/mecz")
    
    datatable(
      vTabela,        
      options = list(
        paging = FALSE,
        searching = FALSE,
        scrollX = TRUE,
        info = FALSE,
        autoWidth = TRUE
      ),
      rownames=FALSE
    )
  })
  
  
#####  
#
#####  
  output$TabelaWykres <- renderChart2({
    req(QuantFootball)
    req(length(QuantFootball$Toporek$punkty)>1)
    req(rFootball$table)
    
    load("./QuantFootball.rda")
    
    vTable <- cbind("Toporek" = QuantFootball$Toporek$punkty[-1]/QuantFootball$Toporek$mecze[-1],
                    "Kruczyk" = QuantFootball$Kruczyk$punkty[-1]/QuantFootball$Kruczyk$mecze[-1],
                    "Segit" = QuantFootball$Segit$punkty[-1]/QuantFootball$Segit$mecze[-1],
                    "Rucki" = QuantFootball$Rucki$punkty[-1]/QuantFootball$Rucki$mecze[-1],
                    "Matulewicz" = QuantFootball$Matulewicz$punkty[-1]/QuantFootball$Matulewicz$mecze[-1],
                    "Dubel" = QuantFootball$Dubel$punkty[-1]/QuantFootball$Dubel$mecze[-1])
    
    vTable <- melt(vTable)
    colnames(vTable) <- c("Kolejka", "Gracz", "Wynik")
    vTable$Wynik <- round(vTable$Wynik,2)
    
    nY2Y <- nPlot(
      Wynik ~ Kolejka,
      group = "Gracz",
      data = vTable,
      type = "lineChart",
      height = 550,
      width = session$clientData[["output_TabelaWidthHelp_width"]]
    )

    nY2Y$chart(
      width = session$clientData[["output_TabelaWykresWidthHelp_width"]]
    )
    
    nY2Y$xAxis(axisLabel = 'Średnia punktów / Mecz')
    
    return(nY2Y)
  })
  
}