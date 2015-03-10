lcy.graph.plot <- 
function(g, layout=layout.fruchterman.reingold,vertex.size=2,vertex.label=NA,vertex.label.degree=pi/4, vertex.label.font=1, vertex.label.cex=0.3,vertex.label.color='#0000FF',vertex.frame.color='black',vertex.label.dist=0.1, edge.arrow.size=0.2, edge.width=0.2,edge.label=NA,edge.label.font=1, edge.label.cex=0.3, main="",arg.legend=NULL,...){

    if(is.null(g)){
        plot.new()
        title(main=main,)
    }else{
        if(vcount(g)==0){
            plot.new()
            title(main=main,)
        }else{
            plot(g, layout=layout.fruchterman.reingold,vertex.label.degree=vertex.label.degree,vertex.size=vertex.size,vertex.label.font=vertex.label.font,vertex.label.cex=vertex.label.cex,vertex.label.color=vertex.label.color,vertex.frame.color=vertex.frame.color, vertex.label.dist=vertex.label.dist, edge.arrow.size=edge.arrow.size, edge.width=edge.width,edge.label=edge.label,edge.label.font=edge.label.font, edge.label.cex=edge.label.cex,main=main,...)
        }
        if(!is.null(arg.legend))
            legend(arg.legend$postion, fill=arg.legend$color,legend=arg.legend$text,title=arg.legend$title)
    }
}
