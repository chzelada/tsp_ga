generate_cities <- function(cities =5){
  pos_x <- sample(1:100, size = cities , replace=TRUE) 
  pos_y <- sample(1:100, size = cities , replace=TRUE)
  out <- data.frame(city = 1:cities, pos_x, pos_y )
  return(out)
}

initial_population <- function(N=500, cities=8){
  vec = 2:cities
  first <- c(1,sample(vec),1)
  population <- matrix(first,ncol = length(first)  )
  for(i in 1:(N-1)){
    population <- rbind( population, c(1,sample(vec),1) )
  }
  unique_population <- unique(population)
  return(unique_population)
}

distance_route <- function(route,distance){
  sum_distance<-0
  for(i in 1:length(route[-1])){
    out <- distance[ route[i], route[i+1]  ]
    sum_distance <- sum_distance + out
  }
  return(sum_distance)
}

selection <- function(population,K){
  players_index <- sample(1:nrow(population),size = K,replace = FALSE)
  players <- population[players_index,'fitness']
  winner_index <- players_index[which.min(players)[1]]
  player_1 <- as.numeric(population[winner_index,-ncol(population)])
  #removing winner from pool
  indx <- setdiff(1:nrow(population),players_index[winner_index])
  players_index <- sample(indx,size = K,replace = FALSE)
  players <- population[players_index,'fitness']
  winner_index <- players_index[which.min(players)[1]]
  player_2 <- as.numeric(population[winner_index,-ncol(population)])
  return(rbind(player_1,player_2))
}

crossover <- function(parents,n){
  parent_1 <- parents[1,2:(ncol(parents)-1)]
  parent_2 <- parents[2,2:(ncol(parents)-1)]
  child <- rep(0,length(parent_1))
  genes_parent_1 <- sample(1:length(parent_1),n,replace=FALSE)
  child[genes_parent_1] <- parent_1[genes_parent_1]
  child[-genes_parent_1] <- setdiff(parent_2,parent_1[genes_parent_1])
  child <- c(1,child,1)
  return(child)
}

mutate <- function(vec,i,j){
  vec <- vec[2:(length(vec)-1)]
  out <- vec
  idx <- sample(1:length(out),2,replace = FALSE)
  out[idx[1]] <- vec[idx[2]]
  out[idx[2]] <- vec[idx[1]]
  out<-c(1,out,1)
  return(out)
}


tsp_sol_ga <- function(ncities=15,pop_size=200,mutate_prob=0.01,generations=100){
  cities_space <- generate_cities(cities = ncities) 
  cities_dist <- as.matrix(dist(cities_space[,2:3]))
  population <- initial_population(N = pop_size, cities=ncities)
  plot(cities_space$pos_x, cities_space$pos_y, main='ciudades')
  for(generation in 0:generations){
    Sys.sleep(0.15)
    fitness <- apply(population,1,distance_route,distance=cities_dist)
    
    plot(cities_space$pos_x, cities_space$pos_y, main='Cities',xlab='X',ylab='Y')
    best_route_idx<-which.min(fitness)[1]
    x <- population[best_route_idx,]
    polygon(cities_space[x,2:3],border = "gray",lwd=2)
    print(c(generation,min(fitness)) )
    population <- cbind(population,fitness)
    new_population <- list()
    for(i in 1:pop_size){
      parents <- selection(population,5)
      child <- crossover(parents,4)
      if(runif(1) <= mutate_prob){
        child <- mutate(child)
      }
      new_population <- append(new_population,list(child))
    }
    population <- matrix(unlist(new_population),ncol = ncities+1,byrow = TRUE )
  }
}
set.seed(123)
tsp_sol_ga(ncities = 20,generations = 50,mutate_prob = 0.1)
