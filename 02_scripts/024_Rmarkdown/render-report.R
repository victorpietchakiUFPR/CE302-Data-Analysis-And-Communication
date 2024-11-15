# Sincroniza
renv::restore()

require(rmarkdown)
require(here)

# Define os caminhos do arquivo de entrada e do renderizado e atribuia a variaveis
input_path <- here("02_scripts/024_Rmarkdown/Data-Analysis-Report", "Data-Analysis-Report.Rmd")
output_path <- "03_results/031_report"

# Renderiza 'Data-Analysis-Report.Rmd' e salva em output_path
render(input_path, output_dir = output_path)