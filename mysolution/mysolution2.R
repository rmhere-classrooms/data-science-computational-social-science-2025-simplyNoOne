library(igraph)
set.seed(666)

graph <- barabasi.game(n = 1000, m = 2, directed = FALSE)

cat("\nPodsumowanie grafu\n")
summary(graph)


layout_fr <- layout_with_fr(graph)
plot(graph, layout = layout_fr, vertex.size = 3, vertex.label = NA, edge.arrow.size = 0.1, edge.width = 0.3)


betweenness_values <- betweenness(graph)
most_central_node <- which.max(betweenness_values)
cat("\nNajbardziej centralny węzeł (betweenness):", most_central_node, "\n")
cat("Wartość betweenness:", betweenness_values[most_central_node], "\n")


graph_diameter <- diameter(graph)
cat("\nŚrednica grafu:", graph_diameter, "\n")


# Różnice między grafami Barabasi-Albert i Erdos-Renyi:
# 
# Graf Erdos-Renyi (model losowy):
# - Każda para węzłów ma takie samo prawdopodobieństwo połączenia
# - Rozkład stopni węzłów jest bliski rozkładowi Poissona
# - Nie występuje zjawisko Preferential Attachment
# - Struktura jest bardziej jednorodna
# - Brak wyraźnych węzłów centralnych
#
# Graf Barabasi-Albert (model sieci bezskalowej):
# - Wykorzystuje mechanizm Preferential Attachment - nowe węzły łączą się częściej z węzłami o wyższym stopniu
# - Rozkład stopni węzłów jest potęgowy - kilka węzłów ma bardzo wysoki stopień
# - Lepiej modeluje rzeczywiste sieci społecznościowe, internet, sieci cytowań
# - Struktura jest hierarchiczna z wyraźnymi węzłami centralnymi
