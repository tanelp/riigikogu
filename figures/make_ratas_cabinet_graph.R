library(riigikogu)
library(dplyr)
library(ggplot2)

data("factions")

start_date = "2016-11-23"
end_date = "2019-04-29"

votings = get_votings(start_date, end_date)
votings = filter(votings, type.code == "AVALIK")

votes_list = lapply(votings$uuid, get_votes)
has_no_votes = sapply(votes_list, is.null)
votes_list = votes_list[!has_no_votes]

nodes = get_nodes(votes_list)
edges = get_edges(nodes, votes_list)
nodes$in_coalition = is_in_coalition(nodes, start_date, end_date)

p = visualize_network(nodes, edges, align=TRUE) +
  labs(title=toupper("Political polarization"),
       subtitle="Jüri Ratas' I cabinet")

ggsave("figures/ratas_1st_cabinet.png", p, width=7, height=4)

