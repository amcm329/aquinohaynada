# -*- coding: utf-8 -*-
"""
Created on Wed Jun 27 18:14:10 2018

@author: JCTM
"""
lista = ["lata","lete","liti","loto","lutu"]
li = ["e","i","o"]

lc = [x for x in lista for y in x if y in li ]
print("nones" + str(lc))