utils::globalVariables(c("id","name"))
#' Save a network graph object as Pajek files
#'
#'
#' The function \code{\link{net2Pajek}} save a bibliographic network previously created by \code{\link{networkPlot}} as pajek files.
#' 
#' @param net is a network graph object returned by the function \code{\link{networkPlot}}. 
#' @param filename is a character. It indicates the filename for Pajek export files.
#' @param path is a character. It indicates the path where the files will be saved. When path="NULL, the files will be saved in the current folder. Default is NULL. 
#' @return The function returns no object but will save three Pajek files in the folder given in the "path" argument with the name "filename.clu," "filename.vec," and "filename.net."
#' 
#' @examples
#' \dontrun{
#' data(management, package = "bibliometrixData")
#'
#' NetMatrix <- biblioNetwork(management, analysis = "co-occurrences", 
#' network = "keywords", sep = ";")
#' 
#' net <- networkPlot(NetMatrix, n = 30, type = "auto", Title = "Co-occurrence Network",labelsize=1) 
#' 
#' net2Pajek(net, filename="pajekfiles", path=NULL)
#' }
#' @seealso \code{\link{net2VOSviewer}} to export and plot the network with VOSviewer software.
#' 
#' @export
net2Pajek <- function(net, filename="my_pajek_network", path=NULL){
  
  graph <- net$graph
  
  nodes <- igraph::as_data_frame(graph, what = c("vertices")) %>% 
    mutate(id = row_number())
  
  edges <- igraph::as_data_frame(graph, what = c("edges"))
  edges <- edges %>% 
    left_join(nodes %>% select(id, name), by=c("from" = "name")) %>% 
    rename(id_from = id) %>% 
    left_join(nodes %>% select(id, name), by=c("to" = "name")) %>% 
    rename(id_to = id)
  
  ### Creation of NET file
  if (!is.null(path)){
    if (substr(path,nchar(path),nchar(path))!="/")
    path <- paste0(path,"/")
  }
  filename <- paste0(path,filename)
  
  file <- paste0(filename,".net")
  
  # Nodes
  write(paste0("*Vertices ",nrow(nodes)), file=file)
  write(paste0(nodes$id, ' "', nodes$name,'"'), file=file, append = T)
  
  # Edges
  write(paste0("*Edges ",nrow(nodes)), file=file, append = T)
  write(paste0(edges$id_from, " ",edges$id_to," ",edges$weight), file=file, append = T)
  
  ### Creation of VEC file
  file <- paste0(filename,".vec")
  
  # Nodes
  write(paste0("*Vertices ",nrow(nodes)), file=file)
  write(paste0(nodes$deg), file=file, append = T)
  
  ### Creation of CLU file
  file <- paste0(filename,".clu")
  
  # Nodes
  write(paste0("*Vertices ",nrow(nodes)), file=file)
  write(paste0(nodes$community), file=file, append = T)
  
}