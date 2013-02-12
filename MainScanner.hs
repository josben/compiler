
module MainScanner where

import Scanner

main = do 
	tokens <- scanner keyWord opsKey opsSimb file
	putStr ( show tokens)
	where
	keyWord = ["import","Portlet","Portal","type","new","Field","BackgroundAll","Background","Size","Title","this"]
	opsKey  = ["<%","%>","<$","$>","->","--"]
	opsSimb = "+=<>-{}[]();/$\",.%"
	--file    = "/home/benjamin/workspace/GenericPortlet/src/ejemploLenguaje.txt"
	file    = "ejemploLenguaje/ejemploLenguaje3.txt"