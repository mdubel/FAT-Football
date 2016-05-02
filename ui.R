library(shiny)
library(shinydashboard)
library(DT)
library(rCharts)
library(reshape)

ui <- dashboardPage(
  dashboardHeader(
    title = "QUANT Team"
    ),
  dashboardSidebar(    
    sidebarMenu(
      menuItem("Mecz", tabName = "mecz", icon = icon("futbol-o")),
      menuItem("Rankingi", tabName = "rankingi", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "mecz",
        column(width = 7,
          fluidRow(
            column(width = 6,
            box(
              fluidRow(
                column(width = 6,
                  uiOutput("blueDef")       
                ),
                column(width = 6,
                  uiOutput("blueAtk")     
                )
              ),
              title = "Niebiescy",
              status = "primary",
              solidHeader = TRUE,
              width = 12
            )
            ),
            column(width = 6,
            box(
              fluidRow(
                column(width = 6,
                       uiOutput("redDef")       
                ),
                column(width = 6,
                       uiOutput("redAtk")     
                )
              ),
              title = "Czerwoni",
              status = "danger",
              solidHeader = TRUE,
              width = 12
            )
          )
          ),
          fluidRow(
            column(width=12,
            box(
              uiOutput("wynikUI"),
              title = "Wynik",
              status = "success",
              solidHeader = FALSE,
              width = 12
            )
            )
          )
        ),
        column(width = 5,
          box(
            uiOutput("strzelcyUI"),
            title = "Strzelcy",
            status = "warning",
            solidHeader = FALSE,
            width = 12
          )       
        ),
        dataTableOutput("RankingMainTable")
      ),
      tabItem(
        tabName = "rankingi",
        plotOutput("TabelaWidthHelp", height = 0),
        fluidRow(
          box(
            title = NULL,
            solidHeader = FALSE,
            status = "primary",
            width = 12,
            dataTableOutput("punktacja")
          )
        ),
        fluidRow(
          column(width = 6,
                 br(),
            fluidRow(
              box(
               title = "NAJLEPSZA EKIPA",
               solidHeader = TRUE,
               status = "primary",
               width = 12,
               dataTableOutput("TabelaPara")
              )
            ),
            fluidRow(     
              box(
                title = "RAZEM",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                dataTableOutput("TabelaPunkty")
              )
            )
          ),
          column(width = 6,
             br(),
             box(
               title = NULL,
               solidHeader = FALSE,
               status = "primary",
               width = 12,
               plotOutput("TabelaWykresWidthHelp", height = 0),
               showOutput("TabelaWykres", "nvd3")
             )    
          )
        ),
        fluidRow(
          column(width = 6,
            box(
              title = "OBRONA",
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              collapsible = TRUE,
              collapsed = FALSE,
              dataTableOutput("TabelaObrona")
            )
          ),
          column(width = 6,
             box(
               title = "ATAK",
               solidHeader = TRUE,
               status = "primary",
               width = 12,
               collapsible = TRUE,
               collapsed = FALSE,
               dataTableOutput("TabelaAtak")
             )
          )
        )
      )
    )
  )
)