
module ArbolBi where

data ArbolBinario = Rama ArbolBinario ArbolBinario
				  | Hoja Int
				  
cuentaHojas :: ArbolBinario -> Int
cuentaHojas (Hoja x) = 1
cuentaHojas (Rama izq der) = (cuentaHojas izq) + (cuentaHojas der) 

main = ejemplo

a = (Rama (Rama (Hoja 2) (Rama (Hoja 5) (Hoja 20))) (Hoja 10))

ejemplo = cuentaHojas a