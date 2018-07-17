/*
¡Bienvenidos a la Semana 3!
Para esta tarea se van a desarrollar ejercicios en Scala, por lo que el objetivo reside 
en implementar las funciones tomando como base el(los) parámetro(s) y retornando exactamente 
lo que se solicita ya que al final se van a llevar a cabo pruebas automatizadas (siguiente tema a ver).
Y se van a comparar tipos de datos y los resultados en sí, entonces el crédito por ejercicio se
obtiene por el número de pruebas que se hubieran aprobado.
Aprovechando la tarea, se trata además de visualizar de manera concreta la implementación de 
funciones en este lenguaje.
En lo que respecta a la entrega, se debe colocar en su directorio de Git un subdirectorio nombrado
"Tarea Semana 3" y dentro debe de estar una copia de este archivo (resuelto evidentemente) SIN MODIFICAR EL NOMBRE DEL ARCHIVO.
Todos los ejercicios son obligatorios sin excepciones. La fecha de entrega es para el próximo 
jueves 12 de Julio antes de las 11:59:59 hrs.
*/

/*
*** EJERCICIO 1 ***
A éste se le llama el ejercicio de la ola (imitando a la ola de un estadio).
Lo que se debe hacer es convertir letra por letra a mayúsculas (y dejando en minúscula las demás)
Para provocar el efecto de la "ola", por ejemplo
ejercicio1("mundo") = List("Mundo","mUndo","muNdo","munDo","mundO")
Entonces en este caso es justamente lo que se debe retornar, una lista con esas combinaciones.
Casos especiales:
ejercicio("") = List()
ejercicio(" ") = List()
ejercicio("muNdo") = List("Mundo","mUndo","munDo","mundO") //Es decir, si hay ya alguna letra con la "ola"
                                                           //no se considera.
ejercicio("MuNdo") = List("ERROR") //si la palabra tiene más de dos letras en mayúsculas, se considera ina-
                                   //propiada para la ola.
*/
//En términos estrictos se conserva la palabra reservada 
//return para regresar el resultado en una función

def ejercicio1(cadena:String):List[String] = {
import scala.collection.mutable.ListBuffer
var ola = new ListBuffer[String]()

var cadena2:String = cadena.replace(" ","")
var letra:String = ""
var numMay:Int = 0
var indice:Int = -1

for(k <- 0 to cadena2.length-1){ 
if (cadena2(k).isUpper){ 
numMay +=1 
indice = k
}
}

if (numMay > 1){
ola += "ERROR"
} else {

cadena2 = cadena2.toLowerCase

for(i <- 0 to cadena2.length-1){ 
var pal:String = ""
letra = cadena2(i).toUpper.toString
for(j <- 0 to cadena2.length-1){
if (i == j) {pal = pal + letra} 
else {pal = pal + cadena2(j).toString}
}
if (i != indice) {ola += pal}
}

}
val olalist = ola.toList
return olalist
}	

/*
*** EJERCICIO 2 ***
Este ejercicio se trata de, dada un array de CUALQUIER elemento como ésta:
Array(false,1,0,1,2,0,1,3,"a")
Se deben mover todos los 0 (se asume que sólo hay 0 de tipo entero) a la izquierda. 
En el ejemplo anterior se regresa:
Array(0,0,false,1,1,2,1,3,"a")
Consideraciones: 
Si el array de ingreso es vacía, se regresa la misma lista vacía.
*/
    //Esto es un poco de azúcar sintáctica de Scala: 
    //si por defecto no se le pone la palabra reservada return, Scala 
    //asume que el elemento a regresarse es el último que se declaró
    //dentro de la función y visto de arriba hacia abajo.

def ejercicio2(lista:Array[Any]): Array[Any] = {
import scala.collection.mutable.ListBuffer

var Arreglo:Array [Any] = Array("Error")
var ceros = new ListBuffer[Int]()
var algo = new ListBuffer[Any]()

if(lista.isEmpty== false){
for(x <- 0 to lista.length-1){
if(lista(x)== 0) ceros += 0
else algo = algo += lista(x)}
Arreglo = List.concat(ceros,algo).toArray
}

return Arreglo
}

/*
*** EJERCICIO 3 *** 
Implemente la fu
nción para calcular el enésimo término de Fibonacci 
SIN USAR NINGÚN TIPO DE RECURSIVIDAD.
Puede usar cualquier ciclo o iterador que desee y tantos como le funcionen.
Ejemplo:
ejercicio3(4) = [0,1,1,2] = 2
ejercicio3(7) = [0,1,1,2,3,5,8] = 8
Consideraciones:
Si termino es 0, el resultado es 0
Si termino es 1, el resultado es 1
Si termino es negativo, el resultado debe dar -1
*/

def ejercicio3(termino:Int):Int = {
import scala.collection.mutable.ListBuffer
var r = -1

var n = new ListBuffer[Int]()
n+=(0,1)

if(termino > 1){
for(x <- 2 to termino)
n += n(x-1)+n(x-2)}

if(termino > -1)
r = n(termino)

return r
}

/*
*** EJERCICIO 4 ***
Implemente un método para verificar el balanceo y adecuada colocación de paréntesis, ejemplo
ejercicio4("") = true
ejercicio4("()") = true
ejercicio4("(()())") = true
ejercicio4("(()") = false
ejercicio4("))((") = false
Es decir, el ejercicio debe verificar no sólo que exista el mismo número de paréntesis
que cierran y abren sino que deben estar colocados en la forma que se ocupan tradicionalmente.
Consideraciones:
Apóyese del siguiente recurso:
https://www.geeksforgeeks.org/check-for-balanced-parentheses-in-an-expression/
*/

def ejercicio4(parentesis:String):Boolean = {
def ejer4(prntss: List[Char], cuentaPar: Int): Int = {

if (cuentaPar < 0) {-1}
else {

if (prntss.isEmpty) {cuentaPar} 
else if (prntss.head == '(') {
ejer4(prntss.tail, cuentaPar + 1)
}
else if (prntss.head == ')'  ) { 
ejer4(prntss.tail, cuentaPar - 1)
}
else {
ejer4(prntss.tail, cuentaPar)
} //segundo if

}  //primer if

}  //funcion interna

if (ejer4(parentesis.toList, 0) == 0 ) {true}
else {false}
}

/*
*** EJERCICIO 5 ***
Este ejercicio se llama "Multiplicación como Se Enseñó en la Primaria"
ejercicio5("2","2") = "4"
ejercicio5("12","12") = "144"
Hasta ahí todo muy bien (y haste este punto resulta innecesario el uso de Strings), 
pero entonces, ¿en qué consiste el ejercicio? Muy fácil,las pruebas se van a 
llevar a cabo con número mucho más grandes que el máximo tamaño permitido para 
un tipo numérico, por lo que las pruebas pueden ser del estilo 
ejercicio5("55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555","55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555")
Lo que significa que, literalmente tiene que efectuar la multiplicación como se enseñó 
en la primaria, lo cual implica para este caso que únicamente se requieren de Strings, 
chars, ciclos y a lo sumo guardar valores en variables de tipo Int.
Consideraciones:
Por simplicidad considere que la multiplicación tiene sólo valores string que
representan números positivos o 0.
Este es uno de los pocos ejercicios, donde además de la solución automática,
se verifica personalmente la implementación del mismo, dicho lo anterior, cualquier
variable Double, Float o tipos de datos especiales que manejen números más grandes 
que resida en el código sin una justificación adecuada dará por anulada
la validez del ejercicio ya que como se mencionó, se tiene que hacer con Strings o Chars.
Además: 
ejercicio5("x","") = "ERROR"
ejercicio5("","x") = "ERROR"
ejercicio5("","") = "ERROR"
*/

def ejercicio5(multiplicando1:String,multiplicando2:String): String = 
{ 
import scala.collection.mutable.ListBuffer
var tem = new ListBuffer[String]()
var r = "Error"
var m1 = ""
var m2 = ""
var u = 0
var d = 0
var n = 0
var b = false

tem += "0"

if(multiplicando1.isEmpty == false & multiplicando2.isEmpty == false)
{
if(multiplicando1.length > multiplicando2.length)
{
m2 = multiplicando1
m1 = multiplicando2
}
else 
{
m1 = multiplicando1
m2 = multiplicando2
}

for(x <- 1 to m1.length)
for(y <- 1 to m2.length)
{
tem += "0"
n=m1(m1.length-x).asDigit * m2(m2.length-y).asDigit
d = n/10
u = n%10
if(tem(y+x-2).toInt + u > 9)
{
tem(y+x-2) = ((tem(y+x-2).toInt + u)%10).toString
tem(y+x-1) = (tem(y+x-1).toInt + 1).toString
}
else
tem(y+x-2) = (tem(y+x-2).toInt + u).toString

tem(y+x-1) = (tem(y+x-1).toInt + d).toString
}

r= ""
for (x<-1 to tem.length)
{
if((tem(tem.length-x)!="0") || (b==true))
{
b = true
r += tem(tem.length-x)
}
}

}
return r
}

/*
*** EJERCICIO 6 ***
Implemente la multiplicación de matrices empleando como matrices elementos de tipo 
List que a su vez contienen elementos también de tipo List.
Ejemplos:
ejercicio6(  List(List(2),    List(List(3,5)) ) = List(List(6,10),
                  List(1))  ,                          List(-3,-5))
 
ejercicio6(List(List(3,0,0),  ,  List(List(1,0,0)   )  =  List(List(3,0,0)
                List(0,2,0),          List(0,-1,0)             List(0,-2,0)
                List(0,0,1))          List(0,0,-7))            List(0,0,-7))
                                   
Consideraciones:
Tanto matriz1 como matriz2 pueden ser de CUALQUIER dimensión, incluso de un sólo elemento.
En caso de que el resultado (por la razón que sea) sea un escalar (un elemento) se debe 
regresar: 
List(List(elemento))
*/

def ejercicio6(matriz1: List[List[Double]], matriz2: List[List[Double]]): List[List[Double]] = {

import scala.collection.mutable.ListBuffer
var res = new ListBuffer[List[Double]]()
var tem = new ListBuffer[Double]()
var c = 0.0

for(k <- 0 to matriz1.length-1)
{
tem.clear
for(y <- 0 to matriz2(0).length-1)
{
c = 0.0
for(x <- 0 to matriz2.length-1)
c += matriz1(k)(x)* matriz2(x)(y)

tem += c
}
res += tem.toList
}

return res.toList
}

/*
*** EJERCICIO 7 ***
Implemente la Criba de Eratóstenes en Scala (sí, primero investigue de qué va).
Ejemplos:
ejercicio7(1) = List(2)
ejercicio7(2) = List(2,3)
ejercicio7(4) = List(2,3,5,7)
Consideraciones: 
El número 1 NO es primo.
Asuma que el limite (n) siempre es positivo y mayor que 0.
*/
def ejercicio7(n:Int): List[Int] = {

def eras(nums: Stream[Int]): Stream[Int] = {
nums.head #:: eras(nums.tail.filter(_ % nums.head != 0))
}

val listaf = eras(Stream.from(2)).take(n).toList
return listaf
}

/*
*** EJERCICIO 8 ***
A este ejercicio se le llama "Identificar el Pangrama".
Un pangrama es una oración (desechando signos de puntuación y espacios) donde se encuentra
al menos una vez cada letra del alfabeto.
Ejemplos:
ejercicio8("El veloz murcielago hindu comia feliz cardillo y kiwi. 
La ciguena tocaba el saxofon detras del palenque de paja") = true
ejercicio8("abcdefghijklmnopqrstuvwxy") = false 
ejercicio8("") = false
Consideraciones:
Asuma que se emplea el alfabeto en inglés (i.e. no hay ñ ni letras acentuadas).
No haga distinción entre mayúsculas y minúsculas, es decir, trátelas por igual.
No utilice ni la estructura Map ni la estructura Set en la resolución. 
De nueva cuenta, este es de los pocos ejercicios en los que se verificará el código.
*/

def ejercicio8(pangrama: String): Boolean = {
var alfabeto = List('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')
var ora = pangrama.toLowerCase.replace('á','a').replace('é','e').replace('í','i').replace('ó','o').replace('ú','u').replace(" ","").sorted
var ora2 = ora.toList

var i = 0
var bol = true

if (ora.length-1 == 25 & alfabeto == ora2){
bol = false
} 
else
{
while (bol && i <= 25)
{
var cuenta = ora.count(_ == alfabeto(i))
if (cuenta > 0) {
//print(alfabeto(i)+ ":" + cuenta)
bol = true
}
else {bol = false}
i += 1
} 

}
bol
}

/*
*** EJERCICIO 9 ***
A este ejercicio se le denomina "Promedio Especial" 
Consiste en que, obteniendo como parámetro una lista de números (del 0 al 9) representados
en letras, se tiene que obtener su promedio, truncarlo si se obtiene algún resultado 
con punto decimal y retornarlo a letras.
Ejemplos:
ejercicio9("uno uno uno") = "uno" ya que el promedio de 1,1,1 es 1
ejercicio9("cero") = "cero" 
ejercicio9("dos tres cinco cinco") = "tres" ya que el promedio de 2 3 5 5 es 3.75 pero al truncarlo es 3.
Consideraciones:
Siempre los números estarán separados por un espacio y nada más.
Es imperativo incluir al menos una estructura Map en este ejercicio, al igual que los demás 
en los que se ha mencionado, se revisará la resolución.
Además:
ejercicio9("uno uno cadenaQueNoEsNumero") = "ERROR"
ejercicio9("unodos uno uno uno") = "ERROR"
ejercicio9("") = "ERROR"
*/

def ejercicio9(numeros:String): String = { 
var map = Map("cero" -> 0, "uno" -> 1, "dos" -> 2, "tres" -> 3, "cuatro" -> 4, "cinco" -> 5, "seis" -> 6, "siete" -> 7, "ocho" -> 8, "nueve" -> 9)
var map1 = Map (0 -> "cero", 1 -> "uno", 2 -> "dos", 3 -> "tres", 4 -> "cuatro", 5 -> "cinco", 6 -> "seis", 7 -> "siete", 8 -> "ocho", 9 -> "nueve")
var res = ""
var sum = 0

var tem = numeros.split(" ")

for(x <- 0 to tem.length-1)
{
if(map.contains(tem(x))&res != "Error")
sum += map(tem(x))
else
res = "Error"
}

if(res != "Error")
res = map1(sum/tem.length)

return res
}

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

val cifrado = Array("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
var entradal = entrada.toLowerCase
var clavel = clave.toLowerCase
var resultado = ""
var i=0
var j = 0
if ((entrada.length > 0) && (clave.length > 0)) {
for (i<-0 to entradal.length()-1) {
if (i <= clavel.length-1){
resultado += cifrado((cifrado.indexOf(entradal(i).toString) + cifrado.indexOf(clavel(i).toString) ) % 26)
}
else {
resultado += cifrado((cifrado.indexOf(entradal(i).toString) + cifrado.indexOf(clavel(j).toString) ) % 26)
j +=1
}
}
} else {resultado = "ERROR"}
return resultado
}

/*
¡Éxito en su ejercicio!
*/
