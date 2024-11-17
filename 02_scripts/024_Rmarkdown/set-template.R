# Instala template de arquivos .pdf do Professor Fernando Mayer (UFPR)
# Possibilita criar arquivos .Rmd no template desejado

require(remotes)
require(tinytex)
remotes::install_github("fernandomayer/tcctemplate")
tinytex::install_tinytex()
tinytex::tlmgr_install("babel-portuges")
