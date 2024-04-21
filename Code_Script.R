#this is my changes

require(igraph)

Enedges <- read.csv("G:/FCDS/semester 3/Social Network/Final Project/twitch/ENGB/musae_ENGB_edges.csv")

# create a network graph (connections)
net <- graph.data.frame(Enedges,directed = F)

#display all  nodes
V(net)
#display all  Edges 
E(net)



#Network Diagram shows Connections
set.seed(222)
plot(net,
     vertex.color='blue',
     vertex.size= 0.3,
     edge.arrow.size=0.01,
     vertex.label.cex=0.4,
     edge.lty = 2,  # Use dashed lines for edges
     edge.lwd = 0.5,  # Thinner edges
     main = "Network Diagram")

################################################################################

                              #Centrality measuerments

#Degree centrality

net_deg<-degree(net)
V(net)$degree <- net_deg
print(V(net)$degree)

#know the highest degree value
print(max(V(net)$degree)) #720

#show index & vertex of the highest degree 
print(which.max(net_deg))  #  vertex "1773" in index 1750


#histogram of degrees
hist(V(net)$degree,
     col = 'cyan',
     main = "Histogram of Node Degree",
     ylab = "Number of Nodes",
     xlab = 'Degree of Verticies')



#AVG Shortest Path
average_shortest_path <- average.path.length(net)
print(average_shortest_path)




#Eigenvector centrality

net_eig <- evcent(net)$vector
V(net)$Eigen<-net_eig 
print(V(net)$Eigen)

#know the highest eignvector value
print(max(V(net)$Eigen))

#show index & vertex of the highest eignvector 
print(which.max(net_eig)) #vertex "4949" in index 4417


#histogram of eignvector
hist(V(net)$Eigen,
     col = 'red',
     main = "Histogram of Node eginvector",
     ylab = "Number of Nodes",
     xlab = 'eignvector of Verticies')




#Betweenness centrality

net_bw<-betweenness(net, directed = FALSE)
V(net)$betweenness<-net_bw
print(V(net)$betweenness)

#know the highest betweenness value
max(V(net)$betweenness) 

#show index & vertex of the highest betweenness 
which.max(net_bw) #vertex "1773" in index 1750


#histogram of Betweenness
hist(V(net)$betweenness,
     col = 'gray',
     main = "Histogram of Node Betweenness",
     ylab = "Number of Nodes",
     xlab = 'Betweenness of Verticies')


#closeness centrality

net_cl<-closeness(net)
V(net)$closeness<-net_cl
print(V(net)$closeness)

#know the highest closeness value

print(max(V(net)$closeness))

#show index & vertex of the highest closeness 
print(which.max(net_cl)) #vertex "4949" in index 4417


#histogram of closeness
hist(V(net)$closeness,
     col = 'red',
     main = "Histogram of Node closeness",
     ylab = "Number of Nodes",
     xlab = 'closeness of Verticies')



#adjacency matrix
adj_matrix <-get.adjacency(net)
print(adj_matrix)
