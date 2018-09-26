#Borrows MM code from brms example [add OSF link here]

library(magrittr)
library(brms)
library(dplyr)
library(randomizr)
library(igraph)

# Define the simulation function
sim_multi_mem <- function(nschools, nstudents = nschools * 100, 
                          change = 0.1, ymean = 20,
                          var_overall = 20, icc = 0.3,
                          w1 = 0.5, w2 = 0.5) {
  # simulate data for a simple multi-membership model
  # Args:
  #   nschools: total number of schools
  #   nstudents: total number of students
  #   change: percentage of students changing school during the year
  #   ymean: mean performance of students across schools
  #   var_overall: overall variance between students
  #   icc: intra-class-correlation; percentage of overall variance 
  #        explained by schools
  #   w1, w2: used to weight schools
  #   seed: used by set.seed to make results reproducible
  # Returns:
  #   a data.frame with columns s1, s2, w1, w2, and y (the performance)
  stopifnot(icc >= 0, icc <= 1)
  stopifnot(change >= 0, change <= 1)
  var_schools <- var_overall * icc
  var_resid <- var_overall - var_schools
  eff_schools <- rnorm(nschools, ymean, sqrt(var_schools))
  nstudents_change <- round(nstudents * change)
  students_change <- sample(1:nstudents, nstudents_change)
  students_stay <- setdiff(1:nstudents, students_change)
  schools_change <- t(replicate(nstudents_change, sample(1:nschools, 2)))
  schools_stay <- sample(1:nschools, length(students_stay), replace = TRUE)
  schools_stay <- cbind(schools_stay, schools_stay)
  data <- as.data.frame(rbind(schools_change, schools_stay))
  names(data) <- c("s1", "s2")
  data$w1 <- w1
  data$w2 <- w2
  data$y <- data$w1 * eff_schools[data$s1] + 
    data$w2 * eff_schools[data$s2] + 
    rnorm(nstudents, 0, sqrt(var_resid))
  data
}

x <- 100
y1 <- 40
y2 <- 40

par(mfrow=c(1,3))

test_dat <- data.frame(
  cbind(student_id = gsub(" ","",do.call(paste,expand.grid(letters,LETTERS,letters,LETTERS))) %>% sample(x),
        sim_multi_mem(nschools = 2, nstudents = x, change =0.2)))

test_dat$s1 <- complete_ra(N = x,num_arms = y1) %>% as.character

test_dat$s2<- complete_ra(N = x, num_arms = y2, prob_each = rep((1/y2),y2)) %>% as.character

#test_dat$s2[sample(x = 1:length(test_dat$s2), size = x*0.25, replace = F)] <- NA

# table(test_dat$s1, test_dat$s2)

#par(mfrow=c(1,3))

mm_graph <- cbind(as.character(test_dat$student_id), unlist(test_dat[,c(2)])) %>% data.frame %>% unique %>% graph_from_data_frame(directed = F) 

V(mm_graph)$color[V(mm_graph)$name %in% c(test_dat$s1, test_dat$s2)] <- 'white'

V(mm_graph)$size <- c(rep(5,x),rep(15,max(y1,y2)))

V(mm_graph)$color[V(mm_graph)$name %in% subset(test_dat, s1 != s2)$student_id] <- 'lightblue'

V(mm_graph)$color[V(mm_graph)$name %in% subset(test_dat, s1 == s2)$student_id] <- 'indianred'

mm_graph %>% plot(main = "Group 1\nStructure",vertex.label=NA)

mm_graph <- cbind(as.character(test_dat$student_id), unlist(test_dat[,c(3)])) %>% data.frame %>% unique %>% graph_from_data_frame(directed = F) 

V(mm_graph)$color[V(mm_graph)$name %in% c(test_dat$s1, test_dat$s2)] <- 'white'

V(mm_graph)$size <- c(rep(5,x),rep(15,max(y1,y2)))

V(mm_graph)$color[V(mm_graph)$name %in% subset(test_dat, s1 != s2)$student_id] <- 'lightblue'

V(mm_graph)$color[V(mm_graph)$name %in% subset(test_dat, s1 == s2)$student_id] <- 'indianred'

mm_graph %>% plot(main = "Group 2\nStructure",vertex.label=NA)

mm_graph <- cbind(as.character(test_dat$student_id), unlist(test_dat[,c(2,3)])) %>% data.frame %>% unique %>% graph_from_data_frame(directed = F) 

V(mm_graph)$color[V(mm_graph)$name %in% c(test_dat$s1, test_dat$s2)] <- 'white'

V(mm_graph)$size <- c(rep(5,x),rep(15,max(y1,y2)))

V(mm_graph)$label <- c(rep('Emp',x),rep('Leader',max(y1,y2)))

V(mm_graph)$color[V(mm_graph)$name %in% subset(test_dat, s1 != s2)$student_id] <- 'lightblue'

V(mm_graph)$color[V(mm_graph)$name %in% subset(test_dat, s1 == s2)$student_id] <- 'indianred'

mm_graph %>% plot(layout = layout_components, main = 'Simultaneous\nMembership Plot',vertex.label=NA)

# #####
# 
# par(mfrow=c(2,5))
# 
# test_dat$s2<-test_dat$s1
# 
# cbind(as.character(test_dat$student_id), unname(unlist(test_dat[,c(2,3)]))) %>% data.frame %>% unique %>% graph_from_data_frame(directed = F) %>% plot(vertex.label=NA,vertex.size=8)
# 
# for(i in seq(0.1,0.9,0.1)){
# 
# test_dat <- data.frame(
#   cbind(student_id = gsub(" ","",do.call(paste,expand.grid(letters,LETTERS))) %>% sample(30),
#         sim_multi_mem(nschools = 2, nstudents = 30, change =i)))
# 
# cbind(as.character(test_dat$student_id), unname(unlist(test_dat[,c(2,3)]))) %>% data.frame %>% unique %>% graph_from_data_frame(directed = F) %>% plot(vertex.label=NA,vertex.size=8)
# }
# 
# 
# 
# 
# 
# 
# 










