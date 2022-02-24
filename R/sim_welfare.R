#' Welfare simulator
#' 
#' @param project Name of project
#' @param X Independent variables from RUM
#' @param alts Number of alternative zones
#' @param beta_j Output from mvrgrnd
#' @param scenario Scenario list created in \code{\link{welfare_predict}}
#' @details Subfunction of \code{\link{welfare_predict}}
#' @export
#' @keywords internal

sim_welfare <- function(project, X, alts, beta_j, scenario){

 # X is matrix of indpendent variables from RUM
 # beta_k is the (sims x K) collection of trial parameter vectors
 # scenario is a list describing the various scenarios we'd like to investigate
 # alts is the number of alternatives in model.
# Returns welfare
 # #Revised version of sim_welfare by Rob Hicks.  Changes made 8/14 by C.
 # #Bassin to expand equation to muliple variables, vectorize for loop and
 # #report welfare in same way as other FishSET welfare code.
  
  temp <- unlist(scenario, recursive = FALSE)
  

  num_scenarios = length(scenario)
  scenario_names = rbind(temp[names(temp) == 'scenario.name'])
  occur <- nrow(X)
  alts <- ncol(scenario[[1]]$open)
  welf_store <- matrix(0, nrow(beta_j), num_scenarios) #zeros(size(beta_j,1),num_scenarios); #size beta j is modeloutput size

for(k in 1:nrow(beta_j)){ 
 # For each k, calculate the welfare for each scenario:
  for(s in 1:num_scenarios){

    open = scenario[[s]]$open
    quality= scenario[[s]]$quality
 #         quality1 = scenario.(scenario_names{s}).quality1;
 #         quality2 = scenario.(scenario_names{s}).quality2;
 #  #       

 # Initialize utility
 #       utility0=zeros(rows(X),alts); #NOTE: rows not a matlab func
 #       utility1=zeros(rows(X),alts); #NOTE: rows not a matlab func
 #       
    utility0 <- matrix(0, nrow(X), alts) #=zeros(size(X,1),alts);
    utility1 <- utility0
 # These two for loops set baseline and post-policy change utility levels
 # for all policies: these are for dealing with quality changes.

 # #MAIN CHNAGES TO EQUATIONS DONE HERE  #CJB 
    ###### -> I think X has to be the 0/1 matrix
    for(i in 1:alts){
      XSimp <- X[,seq(i, alts*alts, alts)] #X[,seq(i, ncol(X), alts)]#X(:,i:alts:end);
      utility0[,i] = rowSums(sweep(XSimp, 2, beta_j[k,], '*')) #sum(bsxfun(@times,XSimp,beta_j[k,]),2);
      utility1[,i] = rowSums(sweep(XSimp, 2, beta_j[k,], '*')*repmat(t(as.matrix(quality[1,])), nrow(X),1)) #Sum(bsxfun(@times,XSimp, beta_j[k,]).*repmat(quality[i,],occur,1),2);
    }

 # now deal with closures
 # first take exponents to use Hanemann's CV formula
      exp_util_0 = exp(utility0)
      exp_util_1 = exp(utility1)
      
       # next, zero out any columns that are closed
      exp_util_1 = exp_util_1 * kronecker(matrix(1,nrow(exp_util_1),1),open) #repmat(open,size(exp_util_1,1),1) #exp_util_1.*repmat(open,size(exp_util_1,1),1);
      
       # for this scenario and beta draw, calculate CV
      welf_store[k,s] =  mean(-1*(log(t(rowSums(exp_util_0))) - log(t(rowSums(exp_util_1)))) / beta_j(1))#;mean(-1*(log(sum(exp_util_0')') - log(sum(exp_util_1')')) ./ beta_j(1));
  }  #end s loop      
  } #end k loop
       
      
    

 #welfare = [prctile(welf_store,5);mean(welf_store);prctile(welf_store,95)];
welfare = quantile(welf_store, probs=c(0.025,.05,.50,.95,.975)) #NOTE: now Returns median not mean
return(welfare)

}

