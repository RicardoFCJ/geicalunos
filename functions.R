#GEIC Alunos' functions
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

drawProg=function(path,al,Change="Default"){
  programa=unlist(strsplit(path,"-"))[[1]][1]%>%{gsub("[^0-9]","",.)}%>%as.numeric
  prog=as.data.table(read_xlsx(path))
  passos=prog[,PASSO]
  passoOco = as.data.table(dbGetQuery(sql,paste0('select * from PASSOOCORRENCIA where PASSO_ID in (',paste(passos,collapse=', '),')')))
  passoOco = passoOco[PROGRAMA_ID==programa]
  passoOco[,NOME:=factor(prog[match(PASSO_ID,prog$PASSO),NOME],levels=prog[,unique(NOME)])]
  blocoOco=as.data.table(dbGetQuery(sql,paste0('select * from BLOCOOCORRENCIA where PASSO_ID in (',paste(passoOco[,unique(PASSO_ID)],collapse=', '),')')))
  bloco=as.data.table(dbGetQuery(sql,paste0('select * from BLOCO where ID in (',paste(blocoOco[,BLOCO_ID],collapse=', '),')')))[,.(ID,TENTATIVAOCORRENCIAINICIAL_ID)]
  bloco[,PASSO_ID:=blocoOco[match(bloco$ID,BLOCO_ID),PASSO_ID]]
  passoOco[,TentIni:=bloco[match(passoOco$PASSO_ID,bloco$PASSO_ID),TENTATIVAOCORRENCIAINICIAL_ID]]
  
  alt=200;larg=100
  par(mar=c(0,0,0,0))
  graphmax=passoOco[,uniqueN(NOME)]*22+25
  plot(c(-75,1275),c(-75,graphmax),ann=F,xaxt='n',yaxt='n',bty='n',col='white')
  x=0;y=graphmax-25
  plts=0
  for(i in 1:passoOco[,uniqueN(NOME)]){
    cur.nome=passoOco[NOME%in%levels(NOME)[i],unique(NOME)]
    cur.passosOco=passoOco[NOME%in%levels(NOME)[i],unique(ID)]
    cur.passos=passoOco[NOME%in%levels(NOME)[i],unique(PASSO_ID)]
    symbols(x,y,rectangles = matrix(c(alt,larg),ncol=2),add = T,inches=F,bg=ifelse(al[,any(PASSOOCO_ID%in%cur.passosOco)],'lightgreen','white'))
    if(Change=="Default"){
      al=al[Default==1]
    }
    reps=al[PASSOOCO_ID%in%cur.passosOco&OCORRENCIA_ID%in%passoOco[PASSO_ID%in%cur.passos,TentIni],uniqueN(ID),by=OCORRENCIA_ID][,ifelse(.N>0,max(V1),0)]
    text(x,y,labels=paste0(cur.nome,' (',reps,')'),cex=.7)
    plts=plts+1
    if(plts==7){plts=0;x=0;y=y-150}else{
      x=x+200
    }
  }
  
}
