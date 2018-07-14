
/*
*** EJERCICIO 10 ***

Implemente la técnica de Vigenère SÓLO para cifrar. Use 
los elementos que considere necesario, además revise la siguiente 
fuente:

https://pages.mtu.edu/~shene/NSF-4/Tutorial/VIG/Vig-Base.html

Véase los siguientes ejemplos:

ejercicio10("PARIS","LOUP") = "AOLXD"
ejercicio10("MICHIGAN","HOUGHTON") = "TWWNPZOA"


Consideraciones:
Tome en cuenta que en la cadena origen sólo hay espacios 
y letras mayúsculas y minúsculas del albafeto en inglés.

La clave puede tener letras repetidas.

Además:
ejercicio10("loquesea","") = "ERROR"
ejercicio10("","llave") = "ERROR"
ejercicio10("","") = "ERROR"
*/
def ejercicio10(entrada:String,clave:String): String={
if((entrada =="")||(clave==""))
return "ERROR"

val abc:String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
var vigenere:String = ""
var spaces:Int = 0

for(l <- 0 to entrada.length -1)
if(abc.contains(entrada(l)))
vigenere  = vigenere:+abc((abc.indexOf(entrada(l))+abc.indexOf(clave((l-spaces)%clave.length)))%26)
else{
vigenere = vigenere:+entrada(l)
spaces+=1
}
return vigenere
}









