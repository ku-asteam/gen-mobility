

# Input parameters
cluster = as.integer(readline(prompt="Enter a number of cluster: "))
user = as.integer(readline(prompt="Enter a number of user: "))
TS = as.integer(readline(prompt="Enter a number of time slot: "))
intra_p = as.integer(readline(prompt="Enter a probability of outgoing: "))



cluster_x <- c(sample(x = 20:180, size = cluster))
cluster_y <- c(sample(x = 20:180, size = cluster))
cluster_location <- matrix(0, nrow = cluster, ncol = 2, byrow = TRUE)
for (z in 1:cluster) {
  cluster_location[z, 1] = cluster_x[z]
  cluster_location[z, 2] = cluster_y[z]
}

# other parameters
User_list = sort(sample(1:user))

side_length = 200
area = side_length * side_length
cluster_length = 20/2
user_step = 5
C_D = 5

# split whole user to each cluster
Cluster_matrix <- matrix(0, nrow = cluster, ncol = user/cluster)
user_id = 1
for (i in 1:cluster){
  for (j in 1:(user/cluster)){
    Cluster_matrix[i,j] = user_id
    user_id = user_id + 1
  }}

# movement of each user at each cluster  
location <- matrix(, nrow = 0, ncol = 4, byrow = TRUE) #user id + time slot + x-axis + y-axis
for (a in 1:cluster) {
  cluster_user = length(Cluster_matrix[a,]) - length(which(Cluster_matrix[a,] == 0)) # except 0, # of user
  add_user_x = cluster_location[a, 1] + floor(runif(cluster_user, -cluster_length, cluster_length)) # users initial placement
  add_user_y = cluster_location[a, 2] + floor(runif(cluster_user, -cluster_length, cluster_length)) # a is home cluster
  for (d in 1:length(add_user_x)) { # each user movement
    location = rbind(location, c(Cluster_matrix[a, d], 1, add_user_x[d], add_user_y[d])) # add initial placement (TS = 1)
    for (ts_index in 2:TS) {
      if (cluster_location[a, 1] - cluster_length <= tail(location, 1)[3] && 
          cluster_location[a, 1] + cluster_length >= tail(location, 1)[3] && 
          cluster_location[a, 2] - cluster_length <= tail(location, 1)[4] && 
          cluster_location[a, 2] + cluster_length >= tail(location, 1)[4]) { # if user in cluster
        if (floor(runif(1, 1, 101)) <= intra_p){
          # 80% stay home cluster
          repeat {
            c_x = tail(location, 1)[3]+ floor(runif(1, -user_step, user_step))
            c_y = tail(location, 1)[4]+ floor(runif(1, -user_step, user_step))
            if (cluster_location[a, 1] - cluster_length <= c_x && 
                cluster_location[a, 1] + cluster_length >= c_x &&  
                cluster_location[a, 2] - cluster_length <= c_y && 
                cluster_location[a, 2] + cluster_length >= c_y && 
                0 <= c_x && c_x <= side_length &&  0 <= c_y && c_y <= side_length)
              break
          }
          location = rbind(location, c(Cluster_matrix[a, d], ts_index, c_x, c_y))
        } else {
          # 20% stay another cluster
          repeat {
            z = floor(runif(1, 1, cluster + 1))
            if (a != (floor(runif(1, 1, cluster + 1))))
              break
          }
          # z is another cluster
          c_x = cluster_location[z, 1]+ floor(runif(1, -cluster_length, cluster_length))
          c_y = cluster_location[z, 2]+ floor(runif(1, -cluster_length, cluster_length))
          location = rbind(location, c(Cluster_matrix[a, d], ts_index, c_x, c_y))
        }} else { # if away cluster
          c_x = cluster_location[a, 1]+ floor(runif(1, -cluster_length, cluster_length))
          c_y = cluster_location[a, 2]+ floor(runif(1, -cluster_length, cluster_length))
          location = rbind(location, c(Cluster_matrix[a, d], ts_index, c_x, c_y))
        }}}}


# Draw user mobility coordination
matsplitter<-function(M, r, c) {
  rg <- (row(M)-1)%/%r+1
  cg <- (col(M)-1)%/%c+1
  rci <- (rg-1)*max(cg) + cg
  N <- prod(dim(M))/r/c
  cv <- unlist(lapply(1:N, function(x) M[rci==x]))
  dim(cv)<-c(r,c,N)
  cv
} 
user_location = matsplitter(location, TS, 4)

dist <- matrix(0, nrow=N*N*TS, ncol = 5, byrow = TRUE ) #user1 id+ user2 id  + time slot + dist+ contact
dc=1
for(uc1 in 1:N){
  temp_loc1 <- location[location[,1]==uc1,]
  for(uc2 in 1:N){
    temp_loc2 <- location[location[,1]==uc2,]
    for(tsc in 1:TS){
      xu1= temp_loc1[tsc,3]
      xu2= temp_loc2[tsc,3]
      yu1= temp_loc1[tsc,4]
      yu2= temp_loc2[tsc,4]
      
      dist[dc,1]= uc1    # user1 index
      dist[dc,2]= uc2    # user2 index
      dist[dc,3]= tsc   #timeslot index
      dist[dc,4]= ((xu2-xu1)^2 + (yu2-yu1)^2)^0.5
      if(dist[dc,4]<=C_D){
        dist[dc,5]=1
      }else {dist[dc,5]=0}
      dc=dc+1
    }}}


q = 1
for (k in 1:cluster) { # k is Cluster Id
  userset <- c()
  for (j in 1:(length(Cluster_matrix[k,]) - length(which(Cluster_matrix[k,] == 0)))) {
    userset <- c(userset, Cluster_matrix[k,j])
  }
  u_size = length(userset)
  
  jpeg(file = paste("cluster_",k,".jpeg", sep = ''))
  plot.new()
  plot(1, 
       type="n", 
       main="User Mobility",
       xlab="x_axis",
       ylab="y_axis",
       xlim=c(0,side_length),
       ylim=c(0,side_length),
       grid())
  abline(v=(seq(0,side_length,side_length/5)), col="lightgray", lty="dotted")
  abline(h=(seq(0,side_length,side_length/5)), col="lightgray", lty="dotted")
  # draw each users random walk history with line
  for (user in q:(q - 1 + u_size)) {
    points(x = user_location[,, user][,3], 
           y = user_location[,, user][,4], 
           type='o', 
           pch=20,
           col="1",
           xlim=range(0,side_length), 
           ylim=range(0,side_length))
    points(x = user_location[,, user][,3][TS], 
           y = user_location[,, user][,4][TS], 
           pch=19,
           cex=1.1,
           col="blue",
           xlim=range(0,side_length), 
           ylim=range(0,side_length))
  }
  points(cluster_location[, 1], cluster_location[, 2], type='p', cex=1.5 ,pch=19, col="red",xlim=range(0,side_length), ylim=range(0,side_length))
  dev.off()
  q = q + u_size
}
