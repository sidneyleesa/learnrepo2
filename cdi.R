#### download cdi

install.packages("writexl")
library(writexl)

write.xls(mydata, "c:/arquivo.xls")
ler.cdicetip = function(dt){
  stopifnot(is(dt, 'Date'),length(dt)==1)
  url= format(dt, 'ftp://ftp.cetip.com.br/MediaCDI/%Y%m%d.txt')
  txt=readLines(url)
  txt = gsub(' ','', txt)
  cdi = as.numeric(txt)/100 
  return(cdi)
  }

dt1 = as.Date("2020-05-11",'%Y-%m-%d')
vl = ler.cdicetip(dt1)
vl

library(bizdays)


cal=create.calendar('ANBIMA', holidaysANBIMA, weekdays = c('saturday','sunday'))
datas = bizdays::bizseq(as.Date('2012-08-20'),as.Date('2020-05-11'),'ANBIMA')
dtbse = data.frame(
  database = datas,
  txctip   = 1:length(datas)
)


for(i in 1:length(datas)){
  dt=datas[i]
  dtbse$txctip[i] = ler.cdicetip(dt)
  print(i)
}

write_xlsx(dtbse,"cdicetip.xlsx")


##### função ler bacen
ler.bacen.tpf= function(dt){
  stopifnot(is(dt, "Date"), length(dt)==1)
  url =format(dt,'https://www4.bcb.gov.br/pom/demab/negociacoes/download/NegT%Y%m.ZIP')
  filename=format(dt,'NegT%Y%m.ZIP')
  download.file(url = url, destfile = filename, mode = 'wb')
  files = unzip(zipfile = filename, exdir = "Downloads")
  dados =read.csv2(files, stringsAsFactors = FALSE)
  file.remove(files)
  file.remove(filename)
  return(dados)
}


dt1= as.date('2020-05-12')
neg = ler.bacen.tpf(dt1)
View(neg)


#url =format(dt,'https://www4.bcb.gov.br/pom/demab/negociacoes/download/NegT%Y%m.ZIP')
#filename=format(dt,'NegT%Y%m.ZIP')
#download.file(url = url, destfile = filename, mode = 'wb')
#files = unzip(zipfile = filename, exdir = "Downloads")
#dados =read.csv2(files, stringsAsFactors = FALSE)
#file.remove(files)
#file.remove(filename)

### conversão de dados
dados$DATA.MOV = as.Date(dados$DATA.MOV,'%d/%m/%Y')
dados$DATA.EMISSAO = as.Date(dados$EMISSAO,'%d/%m/%Y')
dados$VENCIMENTO = as.Date(dados$VENCIMENTO,'%d/%m/%Y')
dados= dados[dados$DATA.MOV == dt,]   #### pega todas as linhas cuja data de movimento seja a exscolhida
head(dados)
str(dados)

teste = read.fwf(files,widths = layout$tamanho, stringsAsFactors = FALSE, strip.white = FALSE)

######## taxas referencias bm&f

dt = as.Date('2020-05-12')
url = format(dt,'ftp://ftp.bmf.com.br/TaxasSwap/TS%y%m%d.ex_')
filename = format(dt,"Downloads/TS%y%m%d.exe")
download.file(url,filename, mode='wb')
files=unzip(filename, exdir='Downloads')

layout =read.csv2('C:/Users/sidao/Documents/BDM/Layout/layout_taxaswap.csv', stringsAsFactors = FALSE)


dados = read.fwf(
  files,widths = layout$tamanho,
  header = FALSE,
  col.names = layout$campo,
  strip.white = FALSE,
  stringsAsFactors = FALSE)
str(dados)
table(dados$SINAL_VL_TAXA)  ### gera tabela de ocorrencia de valores

###ifelse(dados$SINAL_VL_TAXA =="-", yes = -1, no = 1)  ### if aplicado sobre vetores
### ifelse(c(T,F,T,T), yes = c(1,2,3,4))
dados$VL_TAXA = dados$VL_TAXA/(10^7)
dados$VL_TAXA =ifelse(dados$SINAL_VL_TAXA =="-", yes = -dados$VL_TAXA, no = dados$VL_TAXA)
dados$SINAL_VL_TAXA = NULL
dados$DT_ARQUIVO = as.Date(as.character(dados$DT_ARQUIVO),'%Y%m%d')
