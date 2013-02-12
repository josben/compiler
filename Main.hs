
{-
	author Jose Benjamin Perez Soto
-}

module Main where

import Scanner
import Parser
import UU.Parsing

-- main :: IO ()

main = do 
	tokens <- scanner keyWord opsKey opsSimb file
	result <- parseIO pRoot tokens
	-- putStr ( show result)
	result
	where
	keyWord = ["import","type","new","Field","BackgroundAll","Background","Size"
			  ,"Title","module","where","addIcon","addPortlet","addTitle"]
	opsKey  = ["->","{","}","=","(",")",";","<%","%>","<$","$>","\"","/","[","]",","]
	-- opsSimb = ""
	-- file    = "ejemploLenguaje2.txt"
	opsSimb = "+=<>/$(){};%$-\".:\\,[]_"
	file    = "/home/benjamin/workspace/GenericPortlet/ejemploLenguaje/ejemploLenguaje3.txt"