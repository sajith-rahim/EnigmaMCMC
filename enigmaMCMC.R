library(ggplot2)
library(ggthemes)
library(plotly)
library(reshape2)

reference=readLines("war_peace_tolstoy.txt")
reference=toupper(reference)

# console-log
verbose.info <- function(label, ...) {
  vals <- unlist(... , use.names = FALSE)
  cat("[INFO]:",label,vals,"\n")
}


#Transition Matrix : 26letters + (Special Chars and Numbers)
trans.mat=matrix(0,27,27)
A_Z = c(toupper(letters),"")
rownames(trans.mat)=colnames(trans.mat)= A_Z
previous_letter=""
for (ln in 1:length(reference)) {
  verbose.info("Processing Line",list(ln,"/",length(reference)))
  for (pos in 1:nchar(reference[ln])) {
    if(pos%%10==0){
      verbose.info("\t Processing Character",list(pos,"/",nchar(reference[ln])))
    }
    current_letter=substring(reference[ln],pos,pos)
    #cat("Letter:",current_letter,"\n")
    if (current_letter %in% toupper(letters)) {
      trans.mat[rownames(trans.mat)==previous_letter,
                colnames(trans.mat)==current_letter]=
        trans.mat[rownames(trans.mat)==previous_letter,
                  colnames(trans.mat)==current_letter]+1
      previous_letter=current_letter
    } else {
      if (previous_letter!="") {
        trans.mat[rownames(trans.mat)==previous_letter,27]=
          trans.mat[rownames(trans.mat)==previous_letter,27]+1
        previous_letter=""
      }
    }
  }
  current_letter=""
  if (previous_letter!="") {
    
    trans.mat[rownames(trans.mat)==previous_letter,27]=
      trans.mat[rownames(trans.mat)==previous_letter,27]+1
  }
  previous_letter=""
}

#Adding a transition value of 1 to all fields
# Prior : Equally likely
# Normalizing : val/sum()
trans.prob.mat=sweep(trans.mat+1,1,rowSums(trans.mat+1),FUN="/") 


heat_map <- ggplot(melt(trans.prob.mat),aes(Var2,Var1))+geom_tile(aes(fill=value))+
  labs(x="Probability of Second Letter",y="Conditioning on First Letter",fill="Prob")+
  scale_y_discrete(limits = rev(levels(melt(trans.prob.mat)$Var1)))+
  coord_equal() + theme_light()

heat_map
ggplotly(heat_map)



decode <- function(mapping,coded) {
  coded=toupper(coded)
  decoded=coded
  for (i in 1:nchar(coded)) {
    if (substring(coded,i,i) %in% toupper(letters)) {
      substring(decoded,i,i)=toupper(letters[mapping==substring(coded,i,i)])
    }
  }
  decoded
}


log.prob <- function(decoded) {
  logprob=0
  
  lastletter=""
  for (i in 1:nchar(decoded)) {
    curletter=substring(decoded,i,i)
    if (curletter %in% toupper(letters)) {
      logprob=logprob+log(trans.prob.mat[rownames(trans.mat)==lastletter,
                                         colnames(trans.mat)==curletter])
      lastletter=curletter
    } else {
      if (lastletter!="") {
        logprob=logprob+log(trans.prob.mat[rownames(trans.mat)==lastletter,27])
        lastletter=""
      }
    }
  }
  
  if (lastletter!="") {
    logprob=logprob+log(trans.prob.mat[rownames(trans.mat)==lastletter,27])
    lastletter=""
  }
  logprob
}


correctTxt="ENTER HAMLET HAM TO BE OR NOT TO BE THAT IS THE QUESTION WHETHER TIS NOBLER IN THE MIND TO SUFFER THE SLINGS AND ARROWS OF OUTRAGEOUS FORTUNE OR TO TAKE ARMS AGAINST A SEA OF TROUBLES AND BY OPPOSING END"
coded=decode(sample(toupper(letters)),correctTxt) # randomly scramble the text

mapping=sample(toupper(letters)) # initialize a random mapping
i=1
iters=2000
cur.decode=decode(mapping,coded)
cur.loglike=log.prob(cur.decode)
#track values for decode and likelihood
max.loglike=cur.loglike
max.decode=cur.decode
while (i<=iters) {
  proposal=sample(1:26,2) # select 2 letters to switch
  prop.mapping=mapping
  # randomly switch 2 characters to generate a proposal
  prop.mapping[proposal[1]]=mapping[proposal[2]]
  prop.mapping[proposal[2]]=mapping[proposal[1]]
  
  #decode based on the new mapping and calculate prob of proposal
  prop.decode=decode(prop.mapping,coded)
  prop.loglike=log.prob(prop.decode)
  
  if (runif(1)<exp(prop.loglike-cur.loglike)) {
    mapping=prop.mapping
    cur.decode=prop.decode
    cur.loglike=prop.loglike
    
    if (cur.loglike>max.loglike) {
      max.loglike=cur.loglike
      max.decode=cur.decode
    }
    
    cat(i,cur.decode,"\n")
    i=i+1
  }
}
