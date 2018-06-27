# -*- coding: utf-8 -*-
"""
Created on Wed Jun 27 12:17:57 2018

@author: JCTM
"""
cadena1 = "cuando cuentes cuentos cuenta cuantos cuentos cuentas"

cadena2 = "cuando cuentes cuentos cuenta cuantos cuentos cuentas por que si no cuentas cuantos cuentos cuentas nunca sabras cuantos cuentos sabes contar"

#### #diccionario
dicc = {}

for x in cadena1:
    if x in dicc:
        dicc[x] +=1
    else:
        dicc[x] = 1
        
print(str(dicc)) 


###### Lista

lista = []
frec = 0

x=0
blanco = ''
largo = len(cadena2)
for x in range (largo):
    if cadena2[x] <> blanco
        frec+=1

print(str(frec))
