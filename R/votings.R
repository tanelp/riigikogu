#' @import dplyr
#' @useDynLib riigikogu
#' @importFrom Rcpp sourceCpp
NULL

#' Get all votings within a given date range.
#'
#' @param start_date start of the date range (yyyy-MM-dd).
#' @param end_date end of the date range (yyyy-MM-dd).
#' @return A dataframe listing information about votings within the time period.
#' @examples
#' start_date = "2016-11-23"
#' end_date = "2019-04-29"
#' votings = get_votings(start_date, end_date)
#' @export
get_votings = function(start_date, end_date){
  url_pattern = "https://api.riigikogu.ee/api/votings?lang=et&startDate=%s&endDate=%s"
  url = sprintf(url_pattern, start_date, end_date)

  resp = httr::GET(url)
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  if (httr::http_error(resp)) {
    parsed = jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
    stop(
      sprintf(
        "riigikogu API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  out = jsonlite::fromJSON(httr::content(resp, "text"))

  num_votings = as.numeric(sapply(out$votings, nrow))
  votings = out$votings[num_votings > 0]
  dfs = sapply(votings, jsonlite::flatten)
  df = bind_rows(dfs)
  df
}

download_votes = function(uuid){
  base_url = "http://api.riigikogu.ee/api/votings/"
  url = paste0(base_url, uuid)
  x = tryCatch({
    message(paste0("Downloading: ", uuid, "...\n"))
    resp = httr::GET(url)
    out = jsonlite::fromJSON(httr::content(resp, "text"))
    x = jsonlite::flatten(out$voters)
  },
  error=function(e){
    message(e)
    return(NULL)
  })
  x
}

#' Get all members' votes for a specific voting.
#'
#' @param uuid id of the voting.
#' @param data_dir a cache directory for the downloaded data files.
#' @return A dataframe listing information about the votings within the time period.
#' @examples
#' uuid = "1a021b93-c629-4c2b-99ce-15f77596bfc3"
#' votes = get_votes(uuid)
#' @export
get_votes = function(uuid, data_dir="~/.riigikogu/votings/"){
  if(!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  data_path = paste0(data_dir, uuid, ".rds")
  if(file.exists(data_path)){
    dt = readRDS(data_path)
    dt$uuid_voting = uuid
  }
  else{
    dt = download_votes(uuid)
    if(!is.null(dt)) saveRDS(dt, file=data_path)
    dt = NULL
  }
  dt
}

#' Clears the cache directory.
#'
#' @param data_dir a cache directory that holds the downloaded data files.
#' @examples
#' clear_cache()
#' @export
clear_cache = function(data_dir="~/.riigikogu/votings/"){
  unlink(data_dir)
}

#' Get data about the parliament members for the political polarization visualization.
#'
#' @param dfs a list containing dataframes of members' votes.
#' @return A dataframe containing summary information about parliament members.
#' @examples
#' start_date = "2016-11-23"
#' end_date = "2019-04-29"
#'
#' votings = get_votings(start_date, end_date)
#' votings = filter(votings, type.code == "AVALIK")
#'
#' votes_list = lapply(votings$uuid, get_votes)
#' has_no_votes = sapply(votes_list, is.null)
#' votes_list = votes_list[!has_no_votes]
#'
#' nodes = get_nodes(votes_list)
#' @export
get_nodes = function(dfs){
  df_all = bind_rows(dfs)
  nodes = df_all %>%
    group_by(uuid) %>%
    summarise(full_name = most_frequent(fullName),
              faction_name = most_frequent(faction.name),
              faction_uuid = most_frequent(faction.uuid),
              count_votes = n()) %>%
    ungroup() %>%
    mutate(id = 1:n()) %>%
    left_join(factions, by="faction_name")

  nodes
}

#' Get adjacency matrix that describes the similarity between parliament members
#' for the political polarization visualization.
#'
#' @param nodes a dataframe containing summary information about parliament members.
#' @param dfs a list containing dataframes of members' votes.
#' @return Adjacency matrix representing co-occurrences between members' votes.
#' @examples
#' start_date = "2016-11-23"
#' end_date = "2019-04-29"
#'
#' votings = get_votings(start_date, end_date)
#' votings = filter(votings, type.code == "AVALIK")
#'
#' votes_list = lapply(votings$uuid, get_votes)
#' has_no_votes = sapply(votes_list, is.null)
#' votes_list = votes_list[!has_no_votes]
#'
#' nodes = get_nodes(votes_list)
#' edges = get_edges(nodes, votes_list)
#' @export
get_edges = function(nodes, dfs){
  num_nodes = nrow(nodes)
  adj_mat_agree = matrix(0L, num_nodes, num_nodes)
  adj_mat_total = matrix(0L, num_nodes, num_nodes)
  pb = txtProgressBar(min = 0, max = length(dfs), initial=0, style=3)
  start_time = Sys.time()
  for(n in 1:length(dfs)){
    setTxtProgressBar(pb, n)
    df = dfs[[n]]
    df = left_join(df, select(nodes, uuid, id), by="uuid")
    df = arrange(df, id)
    update_edge_counts(df$id, df$decision.code, adj_mat_agree, adj_mat_total)
  }
  abind::abind(adj_mat_agree, adj_mat_total, along=3)
}

#' Determines whether a parliament member was in coaliation between a given date range.
#'
#' @param nodes a dataframe containing summary information about parliament members.
#' @param start_date start of the date range (yyyy-MM-dd).
#' @param end_date end of the date range (yyyy-MM-dd).
#' @return A boolean vector indicating whether the member was in coaliation.
#' @examples
#' start_date = "2016-11-23"
#' end_date = "2019-04-29"
#'
#' votings = get_votings(start_date, end_date)
#' votings = filter(votings, type.code == "AVALIK")
#'
#' votes_list = lapply(votings$uuid, get_votes)
#' has_no_votes = sapply(votes_list, is.null)
#' votes_list = votes_list[!has_no_votes]
#'
#' nodes = get_nodes(votes_list)
#' nodes$in_coalition = is_in_coalition(nodes, start_date, end_date)
#' @export
is_in_coalition = function(nodes, start_date, end_date){
  data("cabinets")

  is_faction_in_coalition = function(start_date, end_date, faction_code){
    cabinets$end_date[nrow(cabinets)] = as.character(Sys.Date())
    ix = (start_date >= cabinets$start_date) &
      (end_date <= cabinets$end_date)
    if(sum(ix) == 1){
      res = grepl(paste0(" ", faction_code), cabinets$coalition[ix]) |
        grepl(paste0(faction_code, " "), cabinets$coalition[ix])
      return(res)
    }
  }

  df_coalition = data.frame(faction_name = unique(nodes$faction_name),
                            faction_code = unique(nodes$faction_code))
  df_coalition$in_coalition = sapply(1:nrow(df_coalition),
                                     function(i) is_faction_in_coalition(start_date,
                                                                 end_date,
                                                                 df_coalition$faction_code[i]))
  ix = match(nodes$faction_name, df_coalition$faction_name)
  return(df_coalition$in_coalition[ix])
}

align_xy = function(xy, group){
  # scale to [0, 1]
  max_value = max(xy)
  xy[, 1] = xy[, 1] - min(xy[, 1])
  #xy[, 1] = xy[, 1] / max(xy[, 1])
  xy[, 1] = xy[, 1] / max_value
  xy[, 2] = xy[, 2] - min(xy[, 2])
  #xy[, 2] = xy[, 2] / max(xy[, 2])
  xy[, 2] = xy[, 2] / max_value

  xy1 = xy[group==1, ]
  xy2 = xy[group==2, ]

  x1 = mean(xy1[, 1])
  x2 = mean(xy2[, 1])
  y1 = mean(xy1[, 2])
  y2 = mean(xy2[, 2])

  #plot(xy, col=c("red", "green")[group])
  #points(x1, y1, pch=16, cex=2, col="red")
  #points(x2, y2, pch=16, cex=2, col="green")

  a = abs(x2 - x1)
  b = abs(y2 - y1)
  theta_rad = atan(b/a)

  x = c(x1, x2)
  y = c(y1, y2)
  xmax_ix = which.max(x)
  # rotate such that left point is fixed
  if(y[xmax_ix] > y[-xmax_ix]) theta_rad = -theta_rad
  # group1 always on the left
  if(xmax_ix == 1) theta_rad = theta_rad + pi
  #theta_rad * 180/pi

  R = rbind(c(cos(theta_rad), -sin(theta_rad)),
            c(sin(theta_rad), cos(theta_rad)))
  out = xy %*% t(R)
  return(out)
}

#' Plots a political polarization graph.
#'
#' @param nodes a dataframe containing summary information about parliament members.
#' @param edges adjacency matrix representing co-occurrences between members' votes.
#' @param prob a probability threshold to binarize the adjacency matrix.
#' @param min_count remove nodes that have less than \code{min_count} votes.
#' @param align rotate the graph such that coaliation is on the right side.
#' @param seed a random seed.
#' @return A political polarization graph.
#' @examples
#' start_date = "2016-11-23"
#' end_date = "2019-04-29"
#'
#' votings = get_votings(start_date, end_date)
#' votings = filter(votings, type.code == "AVALIK")
#'
#' votes_list = lapply(votings$uuid, get_votes)
#' has_no_votes = sapply(votes_list, is.null)
#' votes_list = votes_list[!has_no_votes]
#'
#' nodes = get_nodes(votes_list)
#' edges = get_edges(nodes, votes_list)
#' nodes$in_coalition = is_in_coalition(nodes, start_date, end_date)
#'
#' p = visualize_network(nodes, edges, align=TRUE) +
#' labs(title=toupper("Political polarization"),
#'      subtitle="Jüri Ratas' I cabinet")
#'
#' @importFrom network %v%<-
#' @export
visualize_network = function(nodes, edges, prob=0.7, min_count=15, align=FALSE, seed=42){
  nodes = filter(nodes, count_votes >= min_count)
  edges = edges[nodes$id, nodes$id, ]

  is_connected = 1 * (edges[,,1]/(edges[,,2] + 1) > prob)
  net = network::network(is_connected, directed = FALSE)
  network::network.vertex.names(net) = nodes$full_name
  net %v% "faction" =  nodes$faction_code
  net %v% "count_votes" =  nodes$count_votes

  faction_names = unique(nodes$faction_code)
  num_factions = length(faction_names)
  col_pal = factions$faction_color[match(faction_names, factions$faction_code)]
  names(col_pal) = faction_names

  set.seed(seed)
  xy = sna::gplot.layout.fruchtermanreingold(net, list(cell.jitter=0.75, ncell=10))
  if(align){
    group = nodes$in_coalition + 1
    xy = align_xy(xy, group)
  }
  net %v% "x" = xy[, 1]
  net %v% "y" = xy[, 2]

  ggnet::ggnet2(net,
         size = "count_votes",
         color = "faction",
         palette = col_pal,
         edge.color = c("color", "grey50"),
         edge.alpha=0.2,
         node.alpha=0.8,
         mode=c("x", "y")) +
    guides(size=FALSE,
           color=guide_legend(nrow=1,
                              override.aes = list(size=5))) +
    theme(legend.position="bottom",
          legend.title = element_blank())
}
