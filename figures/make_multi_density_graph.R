library(dplyr)
library(riigikogu)
library(ggplot2)

get_probs = function(ix1, ix2, edges, min_count=15){
  edges_sub1 = edges[ix1, ix2, ]
  edges_sub2 = edges[ix2, ix1, ]
  edges_sub2 = aperm(edges_sub2, c(2, 1, 3))
  edges_sub = pmax(edges_sub1, edges_sub2)
  probs = edges_sub[,,1]/(edges_sub[,,2] + 1)
  probs = as.vector(probs[edges_sub[,,2] > min_count])
  probs
}

get_df = function(nodes, edges){
  nodes$in_coalition = is_in_coalition(nodes, start_date, end_date)

  ix_coalition = which(nodes$in_coalition)
  ix_opposition = which(!nodes$in_coalition)

  probs_c = get_probs(ix_coalition, ix_coalition, edges)
  probs_o = get_probs(ix_opposition, ix_opposition, edges)
  probs_co = get_probs(ix_coalition, ix_opposition, edges)

  data.frame(type = c(rep("coalition", length(probs_c)),
                      rep("opposition", length(probs_o)),
                      rep("coalition_opposition", length(probs_co))),
             prob = c(probs_c, probs_o, probs_co))
}

# all cabinets
dfs = list()
for(i in 1:nrow(cabinets)){
  start_date = cabinets$start_date[i]
  end_date = cabinets$end_date[i]
  if(end_date == "") end_date = "2018-10-14"

  votings = get_votings(start_date, end_date)
  votings = filter(votings, type.code == "AVALIK")

  votes_list = lapply(votings$uuid, get_votes)
  has_no_votes = sapply(votes_list, is.null)
  votes_list = votes_list[!has_no_votes]

  nodes = get_nodes(votes_list)
  edges = get_edges(nodes, votes_list)

  ix = !is.na(nodes$faction_name)
  nodes = nodes[ix, ]
  edges = edges[ix, ix, ]

  df = get_df(nodes, edges)
  df$cabinet = cabinets$name[i]
  dfs[[i]] = df
}

df = bind_rows(dfs)
df$cabinet = factor(df$cabinet, levels=cabinets$name)
ggplot(df, aes(x=prob, fill=type)) +
  geom_density(alpha=0.5) +
  facet_wrap(~cabinet, scales = "free_y")
