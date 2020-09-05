ChromossomeKernel = function(.genotypes=NULL,.hapmap=NULL,.MatrixMarker = NULL,.wholegenome=TRUE,.chr=NULL,.markers=NULL){
  

  if(is.null(.genotypes)   ) stop('Please inform a list of genotypes/genotypes identification')
  if(is.null(.hapmap)      ) stop('Please inform a data.frame containing genotypes, chromossomes and markers')
  if(is.null(.MatrixMarker)) stop('Please inform a matrix of genotypes (row names) and markers (column names)')
  
  
 
  .hap = .hapmap[,c(1,2)]; names(.hap) = c('.snp','.chr')
  if(is.null(.markers)) .markers = colnames(.M)
  
  # Total of markers in the whole-genome
  .total = length(.markers[.markers %in% .hap$.snp]) 
  cat(paste0('-----------------------'),'\n')
  cat(paste0('Total of Markers = ',.total,'\n'))
  .M     = .MatrixMarker[row.names(.MatrixMarker) %in% .genotypes,]
  
  if(isFALSE(.wholegenome)) if(is.null(.chr)) .chr = 1
  if(isTRUE(.wholegenome)) .chr   = unique(.hap$.chr)
  
  # Preparing CHR
  gyM    = list()
  for(i in 1:length(.chr)){
    gyM[[i]] = .M[,.markers %in% .hap$.snp[.hap$.chr %in% .chr[i]]]
    cat(paste0('Chromossome: ',.chr[[i]],' size = ',nrow(gyM[[i]])),'x',ncol(gyM[[i]])),'\n')
  } 
  names(gyM) = .chr
  cat(paste0('-----------------------'),'\n')
  
  chrT    =   plyr::ldply(lapply(gyM,FUN = ncol))
  names(chrT) = c('chr','size')
  write.csv(x = chrT,file = 'markers_per_chromossome.csv')
  
  return(gyM)
  
}
