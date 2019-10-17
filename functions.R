#GEIC Alunos' functions
splitst=function(x)unlist(strsplit(x,"-"))
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
alunoinfo = function(dt){
  nome = paste0('Aluno: ',dt[,unique(NOME)])
  escola = unlist(dbGetQuery(sql,paste0('select NOME from ESCOLA where ID in (',dt[,unique(ESCOLA)],');')))
  Encoding(escola)='latin1'
  escola = paste0('Escola: ',escola)
  programas = paste0('Programas: ',paste0(dt[,unique(PROGRAMA)],collapse=', '))
  dias = paste0('Dias frequentados: ',dt[,uniqueN(Data)])
  sess = paste0('Sessões executadas: ',dt[,uniqueN(SESSAOEXEC_ID)])
  c(nome,escola,programas,dias,sess)
}
dateF = function(x){format.Date(x,"%d de %b de %Y")}

getTentOco = function(path,programa){
  prog=as.data.table(read_xlsx(path,1))
  prog[,NOME:=factor(NOME,levels=unique(NOME))]
  colnames(prog)[colnames(prog)=="Default"]="WHERE"
  tentOco=as.data.table(dbReadTable(sql,"TENTATIVAOCORRENCIA"))
  passos=prog[,PASSO]
  passoOco = as.data.table(dbGetQuery(sql,paste0('select * from PASSOOCORRENCIA where PASSO_ID in (',paste(passos,collapse=', '),
                                                 ') AND PROGRAMA_ID = ',programa,';')))
  prog[,PASSOOCO_ID:=""]
  for(i in prog$PASSO){
    set(prog,which(prog$PASSO==i),"PASSOOCO_ID",paste(passoOco[PASSO_ID==i,ID],collapse="-"))
  }
  
  prog[,TENTATIVAOCO_ID:=0]
  for (tt in which(!is.na(prog$TENTATIVA))){
    set(prog,tt,"TENTATIVAOCO_ID",tentOco[TENTATIVA_ID==prog$TENTATIVA[tt]&BLOCO_ID==prog$BLOCO[tt],ID])
  }
  prog[TENTATIVAOCO_ID==0,TENTATIVAOCO_ID:=NA]
  return(prog)
  
}

drawProg=function(path,al,Change="Default"){
  programa=unlist(strsplit(path,"-"))[[1]][1]%>%{gsub("[^0-9]","",.)}%>%as.numeric
  prog=getTentOco(path,programa)
  
  alt=200;larg=100
  par(mar=c(4,0,0,0))
  graphmax=prog[,uniqueN(NOME)]*22+25
  plot(c(-75,1275),c(-75,graphmax),xlab=sprintf("Número de sessões = %s",al[,uniqueN(SESSAOEXEC_ID)]),ylab="",xaxt='n',yaxt="n",bty='n',col='white')
  x=0;y=graphmax-25
  plts=0
  for(i in 1:prog[,uniqueN(NOME)]){
    cur.nome=prog[NOME%in%levels(NOME)[i],unique(NOME)]
    cur.oco=as.numeric(prog[NOME%in%levels(NOME)[i],unique(splitst(PASSOOCO_ID))])
    cur.oco.n=prog[NOME%in%levels(NOME)[i]&!is.na(TENTATIVAOCO_ID),unique(TENTATIVAOCO_ID)]
    
    if(grepl("Módulo1",path)){
      cur.passos=prog[NOME%in%levels(NOME)[i],as.numeric(splitst(unique(PASSOOCO_ID)))]
      cur.passos.n=prog[NOME%in%levels(NOME)[i]&!is.na(TENTATIVAOCO_ID),as.numeric(splitst(unique(PASSOOCO_ID)))]
      bgcol=ifelse(al[PASSOOCO_ID%in%cur.passos,any(PASSOOCO_ID%in%cur.oco)],'lightgreen','white')
      reps=al[OCORRENCIA_ID%in%cur.oco.n&PASSOOCO_ID%in%cur.passos,uniqueN(ID)]
    
      }else{
      cur.passos=passoOco[NOME%in%levels(NOME)[i],unique(PASSO_ID)]
      cur.passos.n=passoOco[NOME%in%levels(NOME)[i]&WHERE==1,unique(PASSO_ID)]
      bgcol=ifelse(al[,any(PASSOOCO_ID%in%cur.oco)],'lightgreen','white')
      reps=al[PASSOOCO_ID%in%cur.oco.n&OCORRENCIA_ID%in%passoOco[PASSO_ID%in%cur.passos.n,TentIni],uniqueN(ID),by=PASSOOCO_ID][,ifelse(.N>0,max(V1),0)]
    }
    
    symbols(x,y,rectangles = matrix(c(alt,larg),ncol=2),add = T,inches=F,bg=bgcol)
    text(x,y,labels=paste0(cur.nome,' (',reps,')'),cex=.7)
    plts=plts+1
    if(plts==7){plts=0;x=0;y=y-150}else{
      x=x+200
    }
  }
  
}
