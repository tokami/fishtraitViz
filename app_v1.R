
## packages -----------------------------------------------------
library(shiny)
library(fishtraitR)  ## needs phylosem@dev
library(reshape2)
library(bslib)


## ui -----------------------------------------------------------
ui <- page_sidebar(

  theme = bs_theme(bootswatch = "sandstone"),  ## themes: https://bootswatch.com/

  title = "fishtraitViz",

  sidebar = sidebar(
    width = 400,

      h4("Taxa"),

    p("Select the taxa you are interested in. You can select up to 8 taxa at once; selecting more may affect the app's ability to display the information properly. Note that only the first 100 of the 35000 taxa are initally displayed as options. Start typing a taxon name to search for additional taxa. Use the remove buttons to remove individual selections or the 'Remove all taxa' button below to remove all."),

      selectizeInput(
        inputId = "taxa",
        label = "Select taxa",
        choices = NULL,
        multiple = TRUE,
        options = list(
          placeholder = "Type taxa...",
          maxOptions = 100,  ## TODO: 40000 works, but slows everything down... all species can still be found by start typing the name
          maxItems = 8,
          plugins = list("remove_button"),
          create = FALSE
        )
      ),

      actionButton("remove_all_taxa", "Remove all taxa"),

      br(),

      h4("Traits"),

      p("Select the traits you are interested in. You can select as many traits as are available in the database. Note that the available traits might differ between database versions. Use the remove buttons to remove individual selections or the buttons below to select all and remove all traits from the selection."),

      selectizeInput(
        inputId = "traits",
        label = "Select traits",
        choices = NULL,
        multiple = TRUE,
        options = list(
          placeholder = "Type traits...",
          maxOptions = 100,
          plugins = list("remove_button"),
          create = FALSE
        )
      ),

      actionButton("select_all_traits", "Select all traits"),
      actionButton("remove_all_traits", "Remove all traits"),

      br(),

      h4("Database version"),

      p("By default the latest version of the fishtrait database is used, but you can also select an older version."),

      selectInput("record_id",
                  label = "Select database",
                  choices = c("latest","..."), ## TODO: show all versions
                  selected = "latest")

      ## TODO: make sure that latest version is used (caching on server)

    ),

      navset_tab(

        nav_panel(
          "Estimates",

          br(),

          h3("Estimates"),

          p("Explore the predicted mean values of selected traits across selected taxa. Traits are shown in rows and taxa in columns. Values are rounded to three significant figures. The shown values can be downloaded as a csv file in the 'DOWNLOAD' tab."),

          div(style = "display: flex; justify-content: center;",
              tableOutput("est_table"))
        ),

        nav_panel(
          "Standard deviations",

          br(),

          h3("Standard deviations"),

          p("Explore the predicted standard deviations of selected traits across selected taxa. Traits are shown in rows and taxa in columns. Values are rounded to three significant figures. The shown values can be downloaded as a csv file in the 'DOWNLOAD' tab."),

          div(style = "display: flex; justify-content: center;",
              tableOutput("sd_table"))
        ),

        nav_panel(
          "Univariate plots",

          br(),

          h3("Univariate plots"),

          p("Explore the distribution of each traits visually, and optionally compare distributions across different taxa. A subset of traits can be chosen."),

          selectizeInput(
            inputId = "traits_uni",
            label = "Optionally subset traits",
            choices = NULL,
            multiple = TRUE,
            width = "80%",
            options = list(
              placeholder = "Type traits...",
              maxOptions = 500,
              plugins = list("remove_button"),
              create = FALSE
            )
          ),

          div(style = "display: flex; justify-content: center;",
              plotOutput("plot_uni"))

        ),

        nav_panel(
          "Bivariate plots",

          br(),

          h3("Bivariate plots"),

          p("Explore the covariance between two traits visually, and optionally compare covariances across different taxa. All pairwise combinations between a maximum of 4 traits can be displayed at once. The covariance matrices can be downloaded as a zip archive in the 'DOWNLOAD' tab."),

          selectizeInput(
            inputId = "traits_bi",
            label = "Select up to 4 traits",
            choices = NULL,
            multiple = TRUE,
            width = "80%",
            options = list(
              placeholder = "Type traits...",
              maxOptions = 500,
              maxItems = 4,
              plugins = list("remove_button"),
              create = FALSE
            )
          ),

          div(style = "display: flex; justify-content: center;",
              plotOutput("plot_bi"))

        ),

        nav_panel(
          "Download",

          br(),

          h3("Download"),

          br(),

          p("You can download the estimates and standard deviations (SD) for all your taxa and traits combined as a csv file in long format. The dataset has a column for the traits, one for the type of estimate ('est' or 'sd'), one for the taxa, and one with the value."),

          div(style = "display: flex; justify-content: center;",
              downloadButton("download_est",
                             label = "Estimates and SD")),

          br(), br(), br(),

          p("Additionally, you can download the covariance matrix between traits for each species. A csv file by species will be created and all files will be compiled in a zip file."),

          div(style = "display: flex; justify-content: center;",
              downloadButton("download_cov",
                             label = "Covariance matrices")),

          ),

        nav_panel(
          "Information",

          br(),

          h3("Information"),

          br(),

          p("Information to database, methods, reference to paper, link to fish-trait-network, to R code, to shiny code,  ..."),

          p("For more information on the fishtraitR package, see the ",
            a("github page",
              href = "https://github.com/fish-trait-network/fishtraitR",
              target = "_blank"),".")
        )

      )

)

## server ---------------------------------------------------------
server <- function(input, output, session) {

  ## record ID
  record_id <- reactive({
    input$record_id
  })

  ## get database
  dataset <- reactive({
    rec <- record_id()
    req(rec)
    path <- tryCatch({
      download_traits(input$record_id)
    }, error = function(x) {
      return(NULL)
    })
    fishtraitR:::get_dataset(record_id = rec)
  })


  ## taxa selector ----------------------------------------------------------
  all_taxa <- reactive({
    data <- dataset()
    req(data)
    sort(unique(dplyr::pull(dplyr::distinct(data, taxon), taxon)))
  })

  observe({
    updateSelectizeInput(
      session,
      "taxa",
      choices = all_taxa(),
      selected = character(0),
      server = TRUE
    )
  })

  observeEvent(input$remove_all_taxa, {
    updateSelectizeInput(
      session,
      "taxa",
      choices = all_taxa(),
      selected = character(0),
      server = TRUE
    )
  })


  ## traits selector --------------------------------------------------

  all_traits <- reactive({
    req(record_id())
    sort(unique(fishtraitR:::get_trait_names(record_id())))
  })

  observe({
    updateSelectizeInput(
      session,
      "traits",
      choices = all_traits(),
      selected = character(0),
      server = TRUE
    )
  })

  observeEvent(input$select_all_traits, {
    updateSelectizeInput(
      session,
      "traits",
      choices = all_traits(),
      selected = all_traits(),
      server = TRUE
    )
  })

  observeEvent(input$remove_all_traits, {
    updateSelectizeInput(
      session,
      "traits",
      choices = all_traits(),
      selected = character(0),
      server = TRUE
    )
  })

  observe({
    updateSelectizeInput(
      session,
      "traits_uni",
      choices = input$traits,
      selected = input$traits,
      server = TRUE
    )
  })

  observe({
    req(input$traits)
    maxi <- ifelse(length(input$traits < 4), length(input$traits), 4)
    updateSelectizeInput(
      session,
      "traits_bi",
      choices = input$traits,
      selected = input$traits[1:maxi],
      server = TRUE
    )
  })


  ## get results -------------------------------------------------------
  results <- reactive({
    req(dataset(), input$taxa, input$traits)
    get_traits(input$taxa, record_id())
  })

  get_est <- reactive({
    res <- results()
    req(res)
    ind <- which(all_traits() %in% input$traits)
    est <- as.data.frame(res$est[ind, , drop = FALSE])
    est <- cbind(input$traits, est)
    colnames(est) <- c("Traits", colnames(res$est))
    est
  })

  get_sd <- reactive({
    res <- results()
    req(res)
    ind <- which(all_traits() %in% input$traits)
    sd <- matrix(sapply(seq_len(dim(res$cov)[3]), function(i) {
      cov <- matrix(res$cov[ind, ind, i, drop = FALSE],
                    nrow = length(ind), ncol = length(ind))
      sqrt(diag(cov))
    }), nrow = length(input$traits), ncol = ncol(res$est))
    sd <- cbind(input$traits, as.data.frame(sd))
    colnames(sd) <- c("Traits", colnames(res$est))
    sd
  })

  get_cov <- reactive({
    res <- results()
    req(res)
    ind <- which(all_traits() %in% input$traits)
    cov <- list()
    for (i in 1:dim(res$cov)[3]) cov[[i]] <- res$cov[ind,ind,i]
    names(cov) <- attributes(res$cov)$dimnames[[3]]
    cov
  })

  ## estimates table
  output$est_table <- renderTable({

    validate(need(length(input$traits) >= 1,
                  "Please select at least one trait to display estimates."))

    validate(need(length(input$taxa) >= 1,
           "Please select at least one taxa to display estimates."))

    est <- get_est()
    est[-1] <- lapply(est[-1], signif, digits = 2)
    est
  })


  ## SD table
  output$sd_table <- renderTable({

    validate(need(length(input$traits) >= 1,
                  "Please select at least one trait to display SDs."))

    validate(need(length(input$taxa) >= 1,
           "Please select at least one taxa to display SDs."))

    sd <- get_sd()
    sd[-1] <- lapply(sd[-1], signif, digits = 2)
    sd
  })

  ## plots -------------------------------------------------------------
  output$plot_uni <- renderPlot({

    validate(need(length(input$traits) >= 1,
                  "Please select at least one trait to display univariate plots."))

    validate(need(length(input$taxa) >= 1,
           "Please select at least one taxa to display univariate plots."))

    res <- results()
    req(res, input$traits_uni)

    ntraits <- length(input$traits_uni)
    n_cols <- min(ntraits, 3)
    n_rows <- ceiling(ntraits / n_cols)
    par(mfrow = c(n_rows, n_cols), mar = c(5,5,3,2), oma = c(0,0,0,0),
        cex.axis = 1.3, cex.lab = 1.4, cex.main = 1.4, lwd = 1.5)
    for (i in 1:ntraits) {
      fishtraitR:::plot.get_traits(res, variables = input$traits_uni[i],
                                   plot_legend = as.logical(i == 1),
                                   legend_cex = 1.5)
    }

  }, height = function() {
    ntraits <- length(input$traits_uni)
    n_rows <- ceiling(ntraits / 3)
    max(400, n_rows * 400)
  }, width = 1200)


  output$plot_bi <- renderPlot({
    validate(need(length(input$traits) >= 1,
                  "Please select at least one trait to display bivariate plots."))

    validate(need(length(input$taxa) >= 1,
           "Please select at least one taxa to display bivariate plots."))

    res <- results()

    validate(need(length(input$traits_bi) >= 2,
           "Please select at least two traits to display bivariate plots (max 4)."))

    req(res)

    combs <- as.data.frame(t(combn(input$traits_bi, 2)))
    ncombs <- nrow(combs)
    ntraits <- length(input$traits_bi)
    n_cols <- min(ncombs, 3)
    n_rows <- ceiling(ncombs / n_cols)
    par(mfrow = c(n_rows, n_cols), mar = c(5,5,3,2), oma = c(0,0,0,0),
        cex.axis = 1.3, cex.lab = 1.4, cex.main = 1.4, lwd = 1.5)
    for (i in 1:ncombs) {
      fishtraitR:::plot.get_traits(res, variables = as.character(combs[i,]),
                                   plot_legend = as.logical(i == 1),
                                   legend_cex = 1.5)
    }

  }, height = function() {
    ncombs <- choose(length(input$traits_bi), 2)
    n_rows <- ceiling(ncombs / 3)
    max(400, n_rows * 400)
  }, width = 1200)


  ## traits_env <- get(".traits_env", envir = asNamespace("fishtraitR"))

  ## download -----------------------------------------------------------
  output$download_est <- downloadHandler(
    filename = function() {
      time_stamp <- format(Sys.time(), "%Y%m%d")
      paste0("fishtraits_est_", time_stamp, ".csv")
    },
    content = function(file) {
      est <- get_est()
      sd <- get_sd()
      res <- rbind(reshape2::melt(cbind(est, type = "est")),
                   reshape2::melt(cbind(sd, type = "sd")))
      colnames(res) <- c("trait", "type", "taxa", "value")
      write.csv(res, file, row.names = FALSE)
    }
  )

  output$download_cov <- downloadHandler(
    filename = function() {
      time_stamp <- format(Sys.time(), "%Y%m%d")
      paste0("fishtraits_cov_", time_stamp, ".zip")
    },
    content = function(file) {
      cov <- get_cov()

      tmp <- tempfile()
      dir.create(tmp)

      taxa_list <- gsub(" ", "_", names(cov))
      for (i in 1:length(taxa_list)) {
        write.csv(cov[[i]], file = file.path(tmp, paste0(taxa_list[i], ".csv")))
      }

      old_wd <- setwd(tmp)
      utils::zip(zipfile = file, files = list.files(tmp))
      setwd(old_wd)
    }
  )

}



## run app -------------------------------------------------------------
shinyApp(ui, server)
