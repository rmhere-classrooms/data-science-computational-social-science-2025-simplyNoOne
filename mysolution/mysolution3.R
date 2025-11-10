# ten plik to tylko wersja początkowa, faktyczna implementacja opublikowana w shiny znajduje się w folderze emailSpreadSim, ponieważ pojawiły się problemy z deployem aplikacji w tej formie

library(igraph)
library(shiny)
library(bslib)
library("lubridate")

# Aplikacja uruchomiana za pomocą shiny
server <- function(input, output) {
  # Przygotowanie grafu ważonego z danych
  data <- read.table("~/Studia/VII/Danologia/data-science-computational-social-science-2025-simplyNoOne/css/out.radoslaw_email_email", skip = 2, header = FALSE)

  data <- data[, 1:2]
  colnames(data) <- c("from", "to")

  graph <- graph_from_data_frame(data, directed = TRUE)
  graph <- simplify(graph, remove.multiple = TRUE, remove.loops = TRUE)

  cat("\nLiczba węzłów po simplify:", vcount(graph), "\n")
  cat("\nLiczba krawędzi po simplify:", ecount(graph), "\n")

  edges_count <- as.data.frame(table(data$from, data$to))
  colnames(edges_count) <- c("from", "to", "cntij")
  edges_count <- edges_count[edges_count$cntij > 0, ]

  total_sent <- aggregate(cntij ~ from, data = edges_count, FUN = sum)
  colnames(total_sent) <- c("from", "cnti")

  edges_count <- merge(edges_count, total_sent, by = "from")
  edges_count$weight <- edges_count$cntij / edges_count$cnti

  edges_count$from <- as.character(edges_count$from)
  edges_count$to <- as.character(edges_count$to)

  graph_weighted <- graph_from_data_frame(edges_count[, c("from", "to", "weight")], directed = TRUE)
  graph_weighted <- simplify(graph_weighted, remove.multiple = TRUE, remove.loops = TRUE)

  cat("Liczba węzłów w grafie ważonym:", vcount(graph_weighted), "\n")
  cat("Liczba krawędzi  w grafie ważonym:", ecount(graph_weighted), "\n")


  # Symulacja rozprzestrzeniania informacji - Independent Cascades Model

  # Funkcja symulująca proces independent cascades
  independent_cascades <- function(graph, initial_nodes) {
    n_nodes <- vcount(graph)
    activated <- logical(n_nodes)
    activated[initial_nodes] <- TRUE

    adj_list <- lapply(V(graph), function(v) neighbors(graph, v, mode = "out"))
    weights <- E(graph)$weight
    edge_df <- as.data.frame(get.edgelist(graph, names = FALSE))

    edge_key <- paste(edge_df$V1, edge_df$V2, sep = "-")
    weight_lookup <- setNames(weights, edge_key)

    newly_activated <- initial_nodes
    activation_history <- integer(0)
    total_activated <- length(initial_nodes)
    activation_history <- c(activation_history, total_activated)

    while (length(newly_activated) > 0) {
      next_wave <- integer(0)

      for (node in newly_activated) {
        nbrs <- adj_list[[node]]
        if (length(nbrs) == 0) next

        inactive_nbrs <- nbrs[!activated[nbrs]]
        if (length(inactive_nbrs) == 0) next

        keys <- paste(node, inactive_nbrs, sep = "-")
        probs <- weight_lookup[keys]

        successes <- runif(length(probs)) < (probs * (input$probability_multiplier / 100))
        activated[inactive_nbrs[successes]] <- TRUE
        next_wave <- c(next_wave, inactive_nbrs[successes])
      }

      newly_activated <- unique(next_wave)
      total_activated <- sum(activated)
      activation_history <- c(activation_history, total_activated)
    }

    activation_history
  }


  # Wybór strategii początkowych węzłów do aktywacji
  n_initial <- ceiling(0.05 * vcount(graph_weighted))

  outdegrees <- degree(graph_weighted, mode = "out")
  betweenness_values <- betweenness(graph_weighted, directed = TRUE)
  closeness_values <- closeness(graph_weighted, mode = "out")

  top_outdegree <- order(outdegrees, decreasing = TRUE)[1:n_initial]
  top_betweenness <- order(betweenness_values, decreasing = TRUE)[1:n_initial]
  top_closeness <- order(closeness_values, decreasing = TRUE)[1:n_initial]
  top_random <- sample(1:vcount(graph_weighted), n_initial)


  # Funkcja do obliczania średnich wyników dla danej strategii na podstawie n wykonań
  avg_results_for_strategy <- function(initial_nodes) {
    n_experiments <- 100
    all_histories <- list()
    for (exp in 1:n_experiments) {
      history <- independent_cascades(graph_weighted, initial_nodes)
      all_histories[[exp]] <- history
    }

    max_len <- max(sapply(all_histories, length))

    matrix_data <- matrix(NA, nrow = n_experiments, ncol = max_len)

    for (i in 1:n_experiments) {
      hist <- all_histories[[i]]
      if (length(hist) < max_len) {
        hist <- c(hist, rep(hist[length(hist)], max_len - length(hist)))
      }
      matrix_data[i, ] <- hist
    }
    avg_history <- colMeans(matrix_data, na.rm = TRUE)
    avg_history
  }

  output$ICPlot <- renderPlot({
    # Przeprowadzenie symulacji dla każdej strategii i rysowanie wyników
    outdegree_results <- avg_results_for_strategy(top_outdegree)
    betweenness_results <- avg_results_for_strategy(top_betweenness)
    closeness_results <- avg_results_for_strategy(top_closeness)
    random_results <- avg_results_for_strategy(top_random)


    max_len <- max(
      length(outdegree_results), length(betweenness_results),
      length(closeness_results), length(random_results)
    )
    # Maksymalna wartość na osi Y, aby dobrze widzieć wykres podzielona przez 3, bo i tak w żadnej strategii nie aktywujemy wszystkich węzłów
    max_val <- vcount(graph_weighted)

    plot(NULL,
      xlim = c(0, max_len), ylim = c(0, max_val),
      xlab = "Iteracja", ylab = "Liczba aktywowanych węzłów",
      main = "Rozprzestrzenianie informacji - Independent Cascades Model",
      las = 1
    )
    grid()

    lines(0:(length(outdegree_results) - 1), outdegree_results, col = "blue", lwd = 2)
    lines(0:(length(betweenness_results) - 1), betweenness_results, col = "red", lwd = 2)
    lines(0:(length(closeness_results) - 1), closeness_results, col = "green", lwd = 2)
    lines(0:(length(random_results) - 1), random_results, col = "orange", lwd = 2)

    legend("bottomright",
      legend = c("I. Outdegree", "II. Betweenness", "III. Closeness", "IV. Losowe"),
      col = c("blue", "red", "green", "orange"),
      lwd = 2, bty = "n"
    )
  })
}

# Konfiguracja Shiny do wizualizacji wyników


ui <- page_sidebar(
  title = "Rozprzestrzenianie informacji poprzez Email - Independent Cascades Model",
  sidebar = sidebar(
    sliderInput(
      inputId = "probability_multiplier",
      label = "Mnożnik prawdopodobieństwa aktywacji krawędzi:",
      min = 20,
      max = 200,
      value = 100
    ),
    sliderInput(
      inputId = "iterations",
      label = "Liczba wykonań jednej symulacji symulacji:",
      min = 1,
      max = 50,
      value = 10
    )
  ),
  plotOutput(outputId = "ICPlot")
)

shinyApp(ui = ui, server = server)
