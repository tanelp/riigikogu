% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/votings.R
\name{visualize_network}
\alias{visualize_network}
\title{Plots a political polarization graph.}
\usage{
visualize_network(nodes, edges, prob = 0.7, min_count = 15,
  align = FALSE, seed = 42)
}
\arguments{
\item{nodes}{a dataframe containing summary information about parliament members.}

\item{edges}{adjacency matrix representing co-occurrences between members' votes.}

\item{prob}{a probability threshold to binarize the adjacency matrix.}

\item{min_count}{remove nodes that have less than \code{min_count} votes.}

\item{align}{rotate the graph such that coaliation is on the right side.}

\item{seed}{a random seed.}
}
\value{
A political polarization graph.
}
\description{
Plots a political polarization graph.
}
\examples{
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

}
