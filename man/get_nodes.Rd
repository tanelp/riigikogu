% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/votings.R
\name{get_nodes}
\alias{get_nodes}
\title{Get data about the parliament members for the political polarization visualization.}
\usage{
get_nodes(dfs)
}
\arguments{
\item{dfs}{a list containing dataframes of members' votes.}
}
\value{
A dataframe containing summary information about parliament members.
}
\description{
Get data about the parliament members for the political polarization visualization.
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
}
