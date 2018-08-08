setlocal EnableDelayedExpansion
cd %~dp1
set port = 8080
if not "%2" == "" set port=%2
start e:\R\R-3.4.3\bin\Rscript.exe -e "shiny::runApp(host = '0.0.0.0',port = %port% )"
start firefox localhost:%port%
