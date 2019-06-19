data("cabinets")

get_plot = function(start_date, end_date){
  votings = get_votings(start_date, end_date)
  votings = filter(votings, type.code == "AVALIK")

  votes_list = lapply(votings$uuid, get_votes)
  has_no_votes = sapply(votes_list, is.null)
  votes_list = votes_list[!has_no_votes]

  nodes = get_nodes(votes_list)
  edges = get_edges(nodes, votes_list)

  ix = !is.na(nodes$faction_name)
  nodes = nodes[ix, ]
  nodes$in_coalition = is_in_coalition(nodes, start_date, end_date)

  visualize_network(nodes, edges, prob=0.6, align=TRUE, min_count = 100)
}

# all cabinets
plots = list()
for(i in 1:nrow(cabinets)){
  start_date = cabinets$start_date[i]
  end_date = cabinets$end_date[i]
  if(end_date == "") end_date = "2019-06-18"
  p = get_plot(start_date, end_date)
  p = p + labs(title = cabinets$name[i],
               subtitle =  paste0(start_date, " - ", end_date))
  plots[[i]] = p
}

#p = gridExtra::grid.arrange(grobs = plots, ncol = 3)
p = gridExtra::arrangeGrob(grobs = plots, ncol = 2)
ggsave("figures/cabinets.png", p, width=8, height = 20)
