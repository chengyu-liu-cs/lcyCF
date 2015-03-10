\name{lcy.pathway.search}
\alias{lcy.pathway.search}
\title{
    Search gene set of pathways
}
\description{
    Used to search gene sets (or pathway interactions) of given pathway(s) from PID(pathway interactome database) and reactome. Currently only gene sets involved in the pathway are returned.
}
\usage{
lcy.pathway.search(columns = "", database = "REACTOME", condition = list(uniProt = NULL, pathwayDesc = NULL, pathwayName = NULL, pathwayId = NULL), type = "OR", strsql = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{columns}{
    comma separated string. Used to select which columns should be returned. Currrently "uniProt", "pathwayDesc", "pathwayName", "pathwayId" can be used. when it is empty character, all the columns are returned.
}
  \item{database}{
    There are two databases, PID and REACTOME, representing pid and reactome databases. used to choose which database is going to use.
}
  \item{condition}{
    a list of conditions. The list should be named and possible names are "uniProt", "pathwayDesc", "pathwayName", "pathwayId". Each component should be comma separated strings. (see example in Details)
}
  \item{type}{
    Two values can be selected, "OR" or "AND" to decide how condition are combined in sql statement. 
}
  \item{strsql}{
    instead of specifying each parameter, sql statement can be directly used.
}
}
\details{
lcy.pathway.search(columns="uniProt,pathwayDesc,pathwayName,pathwayId", database="REACTOME", condition=list(uniProt=NULL, pathwayDesc=NULL, pathwayName=NULL, pathwayId="200015,200003"), type = "OR", strsql = ""). it is equvilent to strsql="SELECT uniProt,pathwayDesc,pathwayName,pathwayId FROM PID WHERE pathwayId=200015 OR pathwayId=200003"
}
\value{
    a list with several components. columns=columns,table=table,condition=condition,res
}
\author{
    Chengyu Liu <Chengyu.liu@helsinki.fi>
}
\keyword{ pathway }
\keyword{ search }
