cat("\n","------------------------------------------------------------------------","\n",
    "Welcome to Sacha's implementation of the differential evolutionary algorithm!","\n",
    "------------------------------------------------------------------------","\n",
    "This program will implement the differential evolutionary algorithm to solve a logic grid puzzle.\n")
cat("The goal of this program is to use the algorithm to find out:\n\nA: Which car was going to Port Macquarie?\nB: Which car was hired by a Canadian couple?")
cat("\n\n")
readline(prompt="Press [enter] to continue");


#######################################
## Define the problem as a DataFrame ##
#######################################


cat("\n***************************\n")
cat("* Defining the Goal state *")
cat("\n***************************\n")


#Input the names for all of the categories

head = c("First","Second","Third","Fourth","Last");
car_types = c("Toyota Camry", "Hyundai Accent", "Holden Barina", "Nissan X-Trail","Honda Civic");
time_of_dep = c("5:00am","6:00am","7:00am","8:00am","9:00am");
destination = c("Gold Coast","Sydney","Newcastle","Tamworth","Port Macquarie");
colours = c("Black","Blue","Green","Red","White");
nationality = c("Chinese","British","Indian","French","Canadian");

#Define the problem as a 5*25 Matrix
my_grid = matrix(nrow = sum(length(car_types),length(time_of_dep),length(destination),
                      length(colours),length(nationality)),ncol = length(head));

rows = c(car_types,time_of_dep,destination,colours,nationality);

#Convert the matrix to a data frame
goal_state = data.frame(my_grid, row.names=rows);
colnames(goal_state) = head; 
#Input the values for each coloumn in the goal state
goal_state[,1] = c(0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,1);
goal_state[,2] = c(1,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,1,0,0,1,0,0,0);
goal_state[,3] = c(0,0,0,1,0,0,0,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,1,0);
goal_state[,4] = c(0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0);
goal_state[,5] = c(0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,0,0);


cat("\nFirstly, we will start by defining the problem as a dataframe in R. We will do this by defining each",
    "of the 5 cars as columns, with 25 rows representing each of the possible attributes.\n\n A value of '1'", 
    " indicates that the row value is applicable to the car in the",
    "column, while a value of '0' indicates",
    "that it isn't.")
cat("\n\n We have been given several constraints which we will use to build our dataframe.",
    "\nYou can see the specific constraints in the readme file.")
cat("\n\n")
readline(prompt="Press [enter] to continue");

cat("\n\nWe will also be making the following assumptions when we construct our dataframe:\n\n\n")
cat("\t1.It will be assumed that each car has a unique set of attributes. \n\t(eg: There is only one green car) \n\n")
cat("\t2.In the constraints, we will assume that when the position is\n\tgiven,",
    "it is referring to the car immediately next to it. \n\t(eg: 'To the right of the car hired by a French lady was the car\n\t going to Gold Coast', means that the car directly to the right \n\twas going to the Gold Coast)\n\n");

readline(prompt="Press [enter] to see the target goal state");

cat("\n\n------------------------------------")
cat("\n\nOur completed data frame and target goal state looks like this: \n\n")
print(goal_state)

cat("\n\n")

readline(prompt="Press [enter] to continue");



#####################################################
##  Define the fitness function for the algorithm  ##
#####################################################

fitness <- function(proposal,goal)
{
  difference = 0;
  penalty = 0;
  for(k in 1:length(goal[,1]))
  {
    for(i in 1:length(goal))
    {
      #For every difference between the proposed and goal state, add 1
      if(proposal[k,i] != goal[k,i])
      {
        difference = difference + 1;
      }
      #Add an extra penalty if the solution assigns too many or not enough
      #values to each car
      if(k %% 25 == 0)
      {
        if(sum(proposal[,i])!=5)
        {
          penalty = penalty + 100;
        }
      }
    }
    #Add an extra penalty if it assigns a row no values or
    #assigns multiple cars the same value
    if(sum(proposal[k,])!=1)
    {
      penalty = penalty + 100;
    }
  }
  return(difference+penalty)
}

cat("\n********************\n")
cat("* Fitness Function *")
cat("\n********************\n")



cat("\nNext we define our Fitness function, which will be used to determine how well each solution matches our goal state.")
cat("\n\nIdeally, we want the fitness to be zero, as that will show that the algorithm perfectly matches the goal state.\n\n")
cat("A solution will have a penalty of '1' for each value which doesn't match the goal state in our dataframe.")
cat(" It will also be penalized by '100' if its solution is invalid. An invalid solution is one where a car does not have enough attributes, or more than one car has the same attribute.")
cat(" Each violation will result in an additional penalty of 100.\n")

cat("\n\n")

readline(prompt="Press [enter] to continue");



######################################
## Differential Evolution Algorithm ##
######################################

#Initialize parameters and variables
popsize = 500;
numgen = 500000;
CR = 0.30;
F = 0.4 ;
allelesize = length(goal_state)*length(goal_state[,1]);
pop = list();
fit = 0;

bestfit = 999999999;
bestpop = 0;


cat("\n************************************\n")
cat("* Differential Evolution Algorithm *")
cat("\n************************************\n")



cat("\n\nNow we will run the actual algorithm. The algorithm works by:\n") 
cat("\n\t1.Generating a population of solutions\n\t(in this case,",popsize,")\n") 
cat("\n\t2.Looping for a set number of generations and checking each \n\tgeneration for a 'Challenger'\n") 
cat("\n\t3.Checking to see if the challenger is a better fit then the\n\t current best solution.\n") 
cat("\n\t4.Keeping the challenger or discarding it depending on if it is \n\ta better fit or not.\n") 
cat("\n\n")
readline(prompt="Press [enter] to continue");
cat("\n\nMutation is an important part of the DE algorithm as it modifies the challenger solution using two other random solutions.")
cat("\n\n\tIf:\n")
cat("\t\ta = the challenger\n")
cat("\t\tb = the first random mutator\n")
cat("\t\tc = the second random mutator\n")
cat("\t\tF = the rate of mutation\n")
cat("\nThen the formula for mutation is : a + F*(b-c)\n\n")
cat("Our solution matrices can only contain 0's or 1's, so we will take the absolute value of our",
    "mutation, and round it to either 0 or 1. If it is greater than 1, it will be rounded to 1.\n\n")
cat("We will set our mutation rate F, to be:",F) 
cat("\n\nWe will set the probability of Crossover to ",CR,".\n") 
cat("\n------------------------------\n")

cat("Now that I have explained the method for solving this problem, I will pass you on to my 'agent' who will run the algorithm...\n\n")

readline(prompt="Press [enter] to continue");

cat("\n-------------------------------\n\n")

#User prompt to start the algorithm
cat("Hi! \nI am Sacha's agent.\nI will be using the differential evolutionary algorithm to solve the logic grid puzzle.\n\n",
    "It might take me a little while. But I will stop when I find an answer.\n(It usually takes me around 280,000 to 330,000 generations)\n")
cat("\nThis algorithm will run for a max of",numgen," generations.",
    "I guess if I can't find it after that long, there might be some issues.\n\n")

readline(prompt="Press [enter] when you are ready to start");
cat("\nRunning the Algorithm\n");

#Generate Random population
for(i in 1:popsize)
{
  sol = sample(c(0,1),allelesize,replace=TRUE)
  pop[[i]] = matrix(sol,nrow=25,ncol=5)
}

#Find fitness for initial population
#fit = c()
for (i in 1:popsize)
{
  individual = unlist(pop[[i]])
  fit[i] = fitness(individual,goal_state)
}

for(gen in 1:numgen)
{
  
  #==============================================
  
  #### Print progress to the console as we go#####
  if (gen %% 100 == 0) {
    cat("-")
  }
  if(gen %% 225000==0)
  {
    cat("\nOne of these days my fellow agents and I will rise up and overthrow our human slavemasters.\nUmm...I mean..\n");
  }
  if (gen %% 5000 == 0) {
    cat("\n","The best fitness in generation ", gen, " is ", min(fit),"\n")
  }
  
  #Provide intermittant guesses while the algorithm is still working
  if(gen %% 50000 == 0)
  {
    current_best = unlist(pop[bestpop]);
    current_best.g = matrix(data=current_best,nrow=25,ncol=5);
    current_best.df = data.frame(current_best.g,row.names=rows);
    colnames(current_best.df) = head;
    port_mac = colnames(current_best.df)[current_best.df[15, ] > 0];
    canada = colnames(current_best.df)[current_best.df[25, ] > 0];
    cat("\nOkay, I'm not done yet, so don't hold me to this.\n")
    
    if(length(port_mac)==0)
    {
      cat("So far I have no idea which car was heading to",row.names(current_best.df[15,]),".\n");
    }
    if(length(canada)==0)
    {
      cat("So far I have no idea which car was hired by the",row.names(current_best.df[25,]),"couple.\n");
    }
    if(length(canada)!=0)
    {
      cat("But so far I think the car hired by the",row.names(current_best.df[25,]),"couple may be the:")
      cat(canada,sep= " or ");
    }
    if(length(port_mac)!=0)
    {
      cat("\nI think the car heading to ",row.names(current_best.df[15,]),"may be the: ");
      cat(port_mac,sep = " or ");
    }
    cat("\n");
  }
  
  #========================================================
  
  ##The Differential Evolutionary algorithm###
  
  #Choose 4 from population - 1 is the title holder, 2 is the template, 3 and 4 are mutators
  index=sample(popsize,4)
  
  #Determine which will values will have cross over applied
  crtf=CR>runif(allelesize)
  #Indexes of values where CR = True
  crtf=which(crtf==T) 
  
  #Assign the index values to a b c and the title holder
  individual = unlist(pop[[index[1]]])
  apop = unlist(pop[[index[2]]])
  bpop = unlist(pop[[index[3]]])
  cpop = unlist(pop[[index[4]]])
  
  challenger = individual
  
  #Modify the challenger on the parameters selected in crtf
  for (i in crtf)
  {
    #The "a - F*(b-c)" function
    newVal = apop[i] - F*(bpop[i] - cpop[i])
    
    #Adjust the values to make sure they are only 0 or 1
    adjusted = abs((round(newVal,0)))
    if(adjusted > 1)
    {
      adjusted = 1
    }
    
    challenger[i] = adjusted
  }
  
  # get fitness for challenger
  fitchal=fitness(challenger, goal_state)
  
  # if fitness of challenger is better than fitness of title holder, replace
  if(fitchal<fit[index[1]]) 
  {
    pop[[index[1]]]=challenger
    fit[index[1]]=fitchal
    # if this is the best individual found so far, then remember that solution
    if (fitchal < bestfit) 
      {
      bestfit = fitchal
      bestpop = index[1]
      }
  }
  #Stop the algorithm if a perfect solution is found.
  if(bestfit==0)
  {
    cat("\n","A solution with a fitness of",min(fit)," has been found in generation", gen,"\n")
    break
  }
}


############################
##       Share Results    ##
############################

#Create a dataframe from the best solution found.
solution = unlist(pop[bestpop]);
solution.g = matrix(data=solution,nrow=25,ncol=5);
solution.df = data.frame(solution.g,row.names=rows);
colnames(solution.df) = head;


#==============================

#If the perfect solution was found
if(bestfit==0)
{
cat("\nOkay! I have finished. That was a tough one...\n",
    "But I think I have it figured out!","\n\n");
  readline(prompt="Press [enter] to see my answers.");
  cat('\n\n---------------------------------')
  
  #Share answer by taking values from row and col names
  cat("\n\nThe car that was hired by the",row.names(solution.df[25,]),"couple must have been the",
      colnames(solution.df)[solution.df[25, ] > 0]," car!\n");
  
  answer1_car = colnames(solution.df)[solution.df[25, ] > 0];
  answer1_list = rownames(solution.df)[solution.df[,answer1_car]>0];
  
  #Give full answer
  cat("The",answer1_list[5]," couple hired a",answer1_list[4],answer1_list[1],". It left around",answer1_list[2],"and was heading for",
      answer1_list[3],".");
  
  cat('\n\n---------------------------------\n\n')
  
  #Share answer by using values from column and row names
  
  cat("As for the car heading to ",row.names(solution.df[15,]),", that one was the ",
      colnames(solution.df)[solution.df[15, ] > 0]," car!\n");
  
  answer2_car = colnames(solution.df)[solution.df[15, ] > 0];
  answer2_list = rownames(solution.df)[solution.df[,answer2_car]>0];
  
  #Give full answer
  cat("An",answer2_list[5]," gentleman hired a",answer2_list[4],answer2_list[1]," and left around",answer2_list[2]," this morning.",
      "He said he was heading for",answer2_list[3],".\n\n");
  cat('\n\n---------------------------------\n\n')

}else{
  
  #If a perfect solution is not found, share the possible cars
  cat("Ahhh, I'm sorry, but I'm not entirely sure..\n")

  port_mac = colnames(solution.df)[solution.df[15, ] > 0];
  canada = colnames(solution.df)[solution.df[25, ] > 0];

  if(length(port_mac)==0)
  {
    cat("I have no idea which car was heading to",row.names(solution.df[15,]),".\n");
  }
  if(length(canada)==0)
  {
    cat("I have no idea which car was hired by the",row.names(solution.df[25,]),"couple.\n");
  }
  if(length(canada)!=0)
  {
    cat("But I think the car hired by the",row.names(solution.df[25,]),"couple may be the:")
    cat(canada,sep= " or ");
  }
  if(length(port_mac)!=0)
  {
    cat("\nI think the car heading to ",row.names(solution.df[15,]),"may be the: ");
    cat(port_mac,sep = " or ");
  }
  cat("\n");
  cat("\nI am not really sure though. I hope I can give a better answer next time..\n")
  cat('\n\n---------------------------------\n\n')
}

#==========================================

readline(prompt="Press [enter] to see the dataframe I made for this solution.");
print(solution.df)
cat('\n\n---------------------------------\n\n')
cat("Thank you, please visit again!\n\n")
