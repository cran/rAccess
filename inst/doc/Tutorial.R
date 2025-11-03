## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(rAccess)

## ----results = FALSE----------------------------------------------------------
library(shiny)

ui <- function() {
  navbarPage(
    "Demo!",
    tabPanel(
      "Plot",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            "plotType", "Plot type",
            c("Scatter" = "p", "Line" = "l")
          )
        ),
        mainPanel(
          plotOutput("plot")
        )
      )
    ),
    tabPanel(
      "Summary",
      verbatimTextOutput("summary")
    ),
    navbarMenu(
      "More",
      tabPanel(
        "Table",
        DT::dataTableOutput("table")
      ),
      tabPanel(
        "About",
        fluidRow(
          column(
            6,
            h1("this is a sample app")
          )
        )
      )
    )
  )
}

server <- function(input, output, session) {
  output$plot <- renderPlot({
    plot(cars, type = input$plotType)
  })

  output$summary <- renderPrint({
    summary(cars)
  })

  output$table <- DT::renderDataTable({
    DT::datatable(cars)
  })
}

shinyApp(ui, server)

## ----results= FALSE-----------------------------------------------------------
library(DT)
library(pins)
library(shiny)
library(rAccess)

ui <- navbarPage(
  id = "mainpage",
  title = "Demo!",
  tabPanel(
    "Plot",
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "plotType", "Plot type",
          c("Scatter" = "p", "Line" = "l")
        )
      ),
      mainPanel(
        plotOutput("plot")
      )
    )
  ),
  tabPanel(
    "Summary",
    verbatimTextOutput("summary")
  ),
  navbarMenu(
    "More",
    tabPanel(
      "Table",
      DT::dataTableOutput("table")
    ),
    tabPanel(
      "About",
      fluidRow(
        column(
          6,
          h1("this is a sample app")
        )
      )
    )
  ),
  tabPanel(
    "Access Control",
    rAccess::module_iam_ui("iam")
  )
)

server <- function(input, output, session) {
  # Add sample rAccess config file to the root directory
  if (!file.exists("sample_rAccess.yml")) {
    rAccess::use_config("sample_rAccess.yml")
  }

  # Create new instance of rAccess
  user_id <- ifelse(!exists("session$user"), "UserID", session$user)
  newIAM <- rAccess$new(
    user = user_id,
    config = "sample_rAccess.yml"
  )

  if (newIAM$no_admin() || newIAM$is_admin()) {
    showTab("mainpage", target = "Access Control")
  } else {
    hideTab("mainpage", target = "Access Control")
  }

  rAccess::module_iam_server("iam", newIAM)

  # Get panels with access
  user_access_list <- newIAM$get_user_accesslist()

  # Show/Hide: Access Control Panel
  if (newIAM$no_admin() || newIAM$is_admin()) {
    showTab("mainpage", target = "Access Control")
  } else {
    hideTab("mainpage", target = "Access Control")
    print("YOU DO NOT HAVE ADMIN ACCESS")
  }

  # Show/Hide: plot tab
  if (!"plot" %in% user_access_list[["sum"]]) {
    hideTab("mainpage", target = "Plot")
  } else {
    showTab("mainpage", target = "Plot")
  }

  # Show/hide the Summary Tab
  if (!"summary" %in% user_access_list[["sum"]]) {
    hideTab("mainpage", target = "Summary")
  } else {
    showTab("mainpage", target = "Summary")
  }

  # Show/hide the Table tab
  if (!"view" %in% user_access_list[["data"]]) {
    hideTab("mainpage", target = "Table")
  } else {
    showTab("mainpage", target = "Table")
  }

  # ----------------------------------------------------------------------------

  output$plot <- renderPlot({
    plot(cars, type = input$plotType)
  })

  output$summary <- renderPrint({
    summary(cars)
  })

  output$table <- DT::renderDataTable({
    DT::datatable(cars)
  })
}

shinyApp(ui, server)

## -----------------------------------------------------------------------------
pin_board <- pins::board_folder(path = "./data/")
get_accesshistory(pin_board, "demo")

## -----------------------------------------------------------------------------
get_accesslist(pin_board, "demo", datemin = "2023-06-29")

## -----------------------------------------------------------------------------
get_admins(pin_board, "demo", "ADMIN")

## -----------------------------------------------------------------------------
get_board(pin_board, "demo")

