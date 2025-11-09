library(igraph)
set.seed(666)

graph <- erdos.renyi.game(n = 100, p.or.m = 0.05, type = "gnp")

cat("\nPodsumowanie grafu #1\n")
summary(graph)
# Graf nie jest ważony


cat("\nWierzchołki\n")
V(graph)

cat("\nKrawędzie\n")
E(graph)

E(graph)$weight <- runif(ecount(graph), min = 0.01, max = 1)

cat("\nPodsumowanie grafu #2\n")
summary(graph)
# Graf jest ważony


degrees <- degree(graph)
cat("\nStopnie węzłów\n")
degrees

hist(degrees, main = "Histogram stopni węzłów", xlab = "Stopień węzła", ylab = "Częstość", breaks = 10)


components <- components(graph)
cat("Liczba klastrów:", components$no, "\n")


pagerank_values <- page_rank(graph)$vector
node_sizes <- scales::rescale(pagerank_values, to = c(5, 20))

plot(graph, vertex.size = node_sizes, vertex.label = NA, edge.arrow.size = 0.3, edge.width = 0.5)