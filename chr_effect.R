chr.effect = function(model,.gids){
  K = model$K
  ngid = length(.gids)
  size = length(K)
  comps = data.frame(matrix(NA,ncol=size,nrow=ngid))
  comps2 = data.frame(matrix(NA,ncol=size,nrow=ngid))
  
  for(i in 1:size) comps[,i] = K[[i]][[1]]
  names(comps) = names(K)
  rownames(comps) = .gids
  
  for(i in 1:size) comps2[,i] = K[[i]][[2]]
  names(comps2) = paste0('sd_',names(K))
  rownames(comps2) = .gids
  
  return(list(CHR = data.frame(comps),SD = data.frame(comps2)))
}



Vcomp.BGGE<-function(model,digits=4){
  K = model$K
  size = length(K)
  comps = data.frame(matrix(NA,ncol=3,nrow=size))
  VarE =  data.frame(matrix(NA,ncol=3,nrow=1))
  names(comps) = names(VarE) = c("K","Var","SD.var")
  for(k in 1:size){
    comps [k,1] = names(K)[k]
    comps [k,2] = round(K[[k]]$varu,digits   )
    comps [k,3] = round(K[[k]]$varu.sd,digits)
  }
  VarE  [1,1] = "Residual"
  VarE  [1,2] = round(model$varE, digits    )
  VarE  [1,3] = round(model$varE.sd,digits   )
  comps = rbind(comps,VarE)
  return(comps)
}

