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
def ejercicio1(cadena:String): List[String] = {
	var lista=List[String]()
	var len = cadena.trim.length-1
	var cadena2 = cadena.trim
	var cuenta:Int = 0

	for (i <- 0 to len) 
	{ 
		if (cadena2(i).isUpper)
		{
		cuenta += 1
		} 
	}
	
	if (len == -1 ) { return lista } 
	else 
	{
		if (cuenta > 1) { return List("ERROR") }
		else 
		{	
			if (cadena2(len).isUpper == false)
			{
			lista = List(cadena2.toLowerCase.updated(len,cadena2(len).toUpper)) 
			}
			else { cadena2 = cadena2.toLowerCase }  
		
			var x:Int = len-1
			while (x>=0) 
			{
				if (cadena2(x).isUpper == false)
				{
				lista =  cadena2.toLowerCase.updated(x,cadena2(x).toUpper) :: lista 
				}
				else { cadena2 = cadena2.toLowerCase }  
			x-=1
			}
		}
	}
    return lista
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
def ejercicio2(lista:Array[Any]): Array[Any] = {
	var arreglo:Array[Any] = Array()
	for (x <- lista)
		if (x == 0) 
			arreglo = arreglo :+ x
			
	for (y <- lista)
		if (y != 0) 
			arreglo = arreglo :+ y 
	arreglo
}


/*
*** EJERCICIO 3 *** 

Implemente la función para calcular el enésimo término de Fibonacci 
SIN USAR NINGÚN TIPO DE RECURSIVIDAD.
Puede usar cualquier ciclo o iterador que desee y tantos como le funcionen.

Ejemplo:

ejercicio3(4) = [0,1,1,2,3] = 3
ejercicio3(7) = [0,1,1,2,3,5,8,13] = 13

Consideraciones:
Si termino es 0, el resultado es 0
Si termino es 1, el resultado es 1
Si termino es negativo, el resultado debe dar -1
 
*/
def ejercicio3(termino:Int):Int = {
	var a=0
	var b=1
	var c=0

	if (termino < 0)
        return -1
    else if (termino == 0)
        return 0
    else if (termino == 1)
        return 1
    else
	{
		for (x <- 1 to termino-1) 
		{
		c = a + b
		a = b
		b = c
		}
	}
	return c
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
	var lista=List[String]()
	for (x <- parentesis) 
	{
		if (x ==  '('  ) 
			lista = "(" :: lista
		if (x ==  ')'   ) 
	        lista = lista.drop(1)
    } 
   return lista.isEmpty
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

ejercicio5("55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555",
          ("55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555")

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
def ejercicio5(multiplicando1:String,multiplicando2:String): String = {
	var n1 = multiplicando1
	var n2 = multiplicando2
	if (n1.length > n2.length) 
	for (x <- 1 to n1.length-n2.length) {  n2 = '0' + n2   }
	else
	for (x <- 1 to n2.length-n1.length) {  n1 = '0' + n1   }
	
var i=n1.length-1
var j=n2.length-1
var lista = List[String]()
var lista2 = List[String]()
var mul = 0
var muls = ""
var llevo = 0

	while (i >= 0) 
		{
		j=n2.length-1
		for (z <- 0 to lista2.length-1) { lista = "0" :: lista 	}
		llevo = 0
		
		while  (j >= 0) 
			{
			mul = (n1(i).asDigit * n2(j).asDigit) + llevo
			muls = mul.toString
			
			if (muls.length == 2) 
			{ 
			llevo = muls(muls.length-2).toString.toInt
			}
			else {
			llevo = 0 
			}
			lista = muls(muls.length-1).toString :: lista
			j-=1
			}
			lista = llevo.toString :: lista
				
		for ( y <- 1 to i) { lista = "0" :: lista }
		
		lista2 = lista.mkString :: lista2
		
		
		i-=1
		lista = List[String]()
		}
	
	var suma = 0
	var llevo2 = 0
	var k=lista2.length-1
	var l=0
	var sumas = "ACZUCU"
	var lista3 = List[String]()
	
	for (x <- 0 to lista2(0).length-1)
	{
				
		while (k >= 0) 
		{
		l=lista2(k).length-1
		suma += lista2(k)(l-x).toString.toInt 
		k-=1
		l+=1
		}
		suma = suma + llevo2
		sumas = suma.toString
		
		if (sumas.length == 2) 
			{ 
			llevo2 = sumas(sumas.length-2).toString.toInt
			}
		else{
			llevo2 = 0 
			}
	lista3 = sumas(sumas.length-1).toString :: lista3
	k=lista2.length-1
	suma = 0
		
	}
	lista3 = llevo2.toString :: lista3
	
	return lista3.mkString
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
// Ejemplo1: ejercicio6(  List( List(2),List(1) ) ,  List( List(3,5) )  )
// Ejemplo2: ejercicio6(  List( List(3,0,0), List(0,2,0), List(0,0,1) ) ,  List( List(1,0,0), List(0,-1,0), List(0,0,-7) ) )
//Se usa una lista mutable de puente, pero se respetan los tipos de la funcion.
import scala.collection.mutable.ListBuffer
def ejercicio6(matriz1: List[List[Double]], matriz2: List[List[Double]]): List[List[Double]]  = {
	
	var C = new ListBuffer[ListBuffer[Double]]()
	var D = List[List[Double]]()
		
	var fA = matriz1.length
	var cA = matriz1(0).length
	var fB = matriz2.length
	var cB = matriz2(0).length
	
	if (cA == fB ) 
	{
		val m=fA
		val p=cA
		val n=cB
         
		for (x <- 0 to n-1 ) C += ListBuffer.iterate(0.0,m)(0.+)

			for (i <- 0 to m-1 )
				for (j <- 0 to n-1 )
					C(i)(j) = 0

			for (i <- 0 to m-1 )
				for (j <- 0 to n-1 )
					for (k <- 0 to p-1 )
						C(i)(j) = C(i)(j) + matriz1(i)(k) * matriz2(k)(j)
	
	    //Convierte la lista Mutable en Inmutable para devolver el tipo correcto :)
		var y=n-1
		while (y >= 0) 
		{ 
			print(y)
			D = C(y).toList :: D  
			y-=1
		}
	
	//El numero de columnas de A debe ser igual a las filas de B en caso contrario no se pueden multiplicar.
	}
	else  
	{ 
	println("Matrices incompatibles") 
	}

    return D
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
//Por difinicion la criba de Eratóstenes es un algoritmo que permite hallar todos los números primos menores que un número natural dado.
//Por lo mismo los ejemplos anteriores no son correctos.
def ejercicio7(n:Int): List[Int] = 
{
    var dicc = collection.mutable.Map[Int, String]()
	var lista=List[Int]()
	var i = 2
	var j = 0
		
	for (x <- 2 to n+1) 
		dicc(x) = "X"
	
	while ( i <= math.sqrt(n) ) 
	{
		j=i
		while ( j <= n/i )
		{
			dicc( i*j ) = "_"
			j=j+1
		}	
		i=i+1
	}

	for (y <- 2 to n) 
		if ( dicc(y)=="X" ) 
			lista = y :: lista 
return lista.sorted
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
def ejercicio8(pangrama: String): Boolean = 
{
var arreglo = pangrama.toLowerCase.split("")
var abcdario = "abcdefghijklmnopqrstuvwxyz"
var bandera = true

for (x <- abcdario) 
	{
		if ( arreglo.exists(t => t == x.toString) ) 
		bandera=true
		else 
		return false 
	}
	return bandera
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
def ejercicio9(numeros:String): String = 
{ 
	val map1 = Map("cero"->0,"uno"->1,"dos"->2,"tres"->3,"cuatro"->4,"cinco"->5,"seis"->6,"siete"->7,"ocho"->8,"nueve"->9)
	val map2 = Map(0->"cero",1->"uno",2->"dos",3->"tres",4->"cuatro",5->"cinco",6->"seis",7->"siete",8->"ocho",9->"nueve")
	val arreglo = numeros.toLowerCase.split(" ")
	var suma = 0
	for (x <- arreglo)
        if (  map1.keys.exists(v => v == x.toString )	)
			 suma += map1(x.toString) 
		else return ("ERROR")
	return map2(suma/arreglo.length)
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
def ejercicio10(entrada:String,clave:String): String=
{
	val abcdario = ('A' to 'Z').toList
	val tabla = Array.ofDim[Char](26, 26)
	var cifrado = ""

	for (fila <- 0 to 25) 
			for (col <- 0 to 25) 
				tabla(fila)(col) = abcdario((fila + col) % 26)
				
	if ( (entrada.length > 0) & (clave.length > 0) )
	{
	val clave2 = Iterator.continually(clave).flatMap(x => x).take(entrada.length).toList
	
	for (i <- 0 to entrada.length-1) 
			cifrado = cifrado  + tabla(tabla(0).indexOf(entrada(i).toUpper))(tabla(0).indexOf(clave2(i).toUpper))  

	return cifrado
	}
	else { return ("ERROR") }
}


/*
¡Éxito en su ejercicio!
*/


