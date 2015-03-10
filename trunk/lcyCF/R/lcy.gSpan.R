lcy.gSpan <- function(graph, n.process=4, support=0.5, num.node=2, edge.label='', vertex.activity=TRUE, activity.key='color',  mine.tool.path=''){
    old.path <- getwd()
    setwd(output.dir)
    lcy.initiation('gSpan')
    # prepare input file for gSpan
    print('Preparing input file for gSpan')
    parameter <- lcy.gSpan.input.gen(graph.list=graph,
                            edge.label=edge.label,
                            vertex.activity=vertex.activity,
                            activity.key=activity.key,
                            file=paste('gSpanInputData_',support,'.csv',sep=''))
    # run gSpan to find frequent subgraphs
    print('Searching frequent subgraphs')
    system(sprintf('%s -f gSpanInputData_%s.csv -s %f -i -o',mine.tool.path,support, support))
    print('Reading frequent subgraphs')
    SUBGRAPH  <- lcy.gSpan.subgraph.import(file=paste('gSpanInputData_',support,'.csv.fp',sep=''),
                            n.process=n.process,
                            num.node=num.node,
                            group=parameter,
                            vertex.activity=vertex.activity,
                            activity.key=activity.key,
                            edge.label=edge.label)
    print(sprintf('Length of SUBGRAPH %d',length(SUBGRAPH)))
    save(parameter, graph, SUBGRAPH, file=paste('gSpan_subgraph_',support,'.csv',sep=''))
    setwd(old.path)
}
