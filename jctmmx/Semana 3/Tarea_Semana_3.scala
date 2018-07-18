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

ejercicio1("") = List()
ejercicio1(" ") = List()
ejercicio1("muNdo") = List("Mundo","mUndo","munDo","mundO") //Es decir, si hay ya alguna letra con la "ola"
                                                           //no se considera.
ejercicio1("MuNdo") = List("ERROR") //si la palabra tiene más de dos letras en mayúsculas, se considera ina-
                                   //propiada para la ola.

*/
def ejercicio1(cadena:String):List[String] = {
    //En términos estrictos se conserva la palabra reservada
    //return para regresar el resultado en una función

    var cadenaNoSpace:String = cadena.replace(" ","")
    var cadenaLower:String = cadenaNoSpace.toLowerCase
    var uppers:Int = 0
    var returnList:List[String]=List()

    for (c <- 0 to cadenaNoSpace.length-1){
        if(cadenaNoSpace(c).isUpper){
            uppers +=1
            if(uppers>1)
                return List("ERROR")
        }
        else{
            returnList= returnList :+ (cadenaLower.substring(0,c)+cadenaLower(c).toUpper+cadenaLower.substring(c+1,cadenaLower.length))

        }
    }

    return returnList
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
    //Esto es un poco de azúcar sintáctica de Scala:
    //si por defecto no se le pone la palabra reservada return, Scala
    //asume que el elemento a regresarse es el último que se declaró
    //dentro de la función y visto de arriba hacia abajo.

    var returnArray:Array[Any] = Array()

    for (it <- 0 to lista.length-1){
        if(lista(it) == 0)
            returnArray= Array(0)++returnArray
        else
            returnArray = returnArray++Array(lista(it))
    }
    return returnArray
}


/*
*** EJERCICIO 3 ***

Implemente la función para calcular el enésimo término de Fibonacci
SIN USAR NINGÚN TIPO DE RECURSIVIDAD.
Puede usar cualquier ciclo o iterador que desee y tantos como le funcionen.

Ejemplo:

ejercicio3(3) = [0,1,1,2] = 2
ejercicio3(6) = [0,1,1,2,3,5,8] = 8

Consideraciones:
Si termino es 0, el resultado es 0
Si termino es 1, el resultado es 1
Si termino es negativo, el resultado debe dar -1

*/
def ejercicio3(termino:Int):Int = {
    val fib:Array[Int] = Array(0,0,1)
    if(termino<0)
        return -1
    else{
        for( I <- 0 to termino-1){
            fib(0) = fib(1)
            fib(1)=fib(2)
            fib(2)=fib(0)+fib(1)
        }
        return fib(1)
    }
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
    var balance:Int = 0
    for (p <- parentesis){
        if(p==')')
            balance-=1
        else if (p=='(')
            balance+=1
        if(balance < 0)
            return false
    }
    if(balance == 0)
        return true
    else
        return false
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
    var mm:Int = 0
    var nn:Int = 0
    var temp:Int = 0
    var resp:String = ""
    var carry:Int = 0
    var respSum:String = ""
    var respTot:String = ""

    if((multiplicando1 == "")||(multiplicando1 == "x")||(multiplicando2 == "")||(multiplicando2 == "x"))
        return "ERROR"
    else{
        for(n <- multiplicando1.length-1 to 0 by -1){
            nn = multiplicando1(n).toString.toInt
            carry=0
            resp=""
            for(m <- multiplicando2.length-1 to 0 by -1){
                mm = multiplicando2(m).toString.toInt
                temp = mm*nn
                resp = (temp%10+carry).toString +resp
                carry = (temp/10).toInt
                if(m==0)
                    resp=carry.toString+resp
            }

            if(n == multiplicando1.length-1)
                respSum = 0.toString+resp
            else{
                resp = resp+"0"*(multiplicando1.length-n-1)

                //Sum one by one plus carry resp + respSum
                carry=0
                temp=0
                respTot =""
                for(i <- resp.length-1 to 0 by -1){
                    temp = resp(i).toString.toInt + respSum(i).toString.toInt+carry
                    respTot = (temp%10).toString +respTot
                    carry=(temp/10).toInt
                    if(i == 0)
                        respTot = carry.toString+respTot
                }
                respSum=respTot
            }
        }
        if(respSum.substring(0,2)=="00")
            return respSum.substring(2,respSum.length)
        else
            return respSum.substring(1,respSum.length)
    }
}



/*
*** EJERCICIO 6 ***

Implemente la multiplicación de matrices empleando como matrices elementos de tipo
List que a su vez contienen elementos también de tipo List.

Ejemplos:

ejercicio6(  List(List(2),    List(List(3,5)) ) = List(List(6,10),
                  List(1))  ,                          List(3,5))

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
    var m:Int = matriz1.length
    var n:Int = matriz1(0).length
    var p:Int = matriz2(0).length
    var tempSum:Double = 0

    var matrizc:List[List[Double]]=List(List())
    var matrizTemp:List[Double]=List()

    for(cm <- 0 to m-1){
        matrizTemp = List()
        for(cp <- 0 to p-1){
            tempSum = 0
            for(cn <- 0 to n-1){
                tempSum+=matriz1(cm)(cn)*matriz2(cn)(cp)
            }
            matrizTemp = matrizTemp:+tempSum
        }
        if(cm ==0)
            matrizc=List(matrizTemp)
        else
            matrizc = matrizc:+matrizTemp
    }

    return matrizc
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
    var possibleValues:List[Int] = List.range(2,n*10)
    var primes:List[Int] = List()
    var tempPrime:Int = 0

    while(primes.length<n){

        tempPrime =possibleValues(0)
        primes = primes:+ tempPrime
        possibleValues = possibleValues.filter(_%tempPrime != 0)

    }
    return primes
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
    var uniqLetter:List[Char] = pangrama.toUpperCase.distinct.toList.sorted
    var aIndex:Int = uniqLetter.indexOf('A')
    var uniqLetterLength:Int = 0

    if(aIndex>=0)
        uniqLetterLength=uniqLetter.slice(aIndex,uniqLetter.length-1).length

    if(uniqLetterLength==25)
        return true
    else
        return false

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
    val strToNum:Map[String,Int] = Map("cero" -> 0,"uno" -> 1,"dos" -> 2,"tres" -> 3,"cuatro" -> 4,"cinco" ->5,"seis"->6,"siete"->7,"ocho" -> 8, "nueve" -> 9)

    val numToString:Map[Int,String] = Map(0->"cero",1->"uno",2->"dos",3->"tres",4->"cuatro",5->"cinco",6->"seis",7->"siete",8->"ocho", 9->"nueve")

    var textVal:List[String] = numeros.split(" ").toList

    var numList:List[Int] = List()

    for(i <- textVal){
        if(strToNum.keySet.contains(i))
            numList = numList:+strToNum(i)
        else
            return "ERROR"
    }
    return numToString((numList.sum/numList.length).toInt)
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

/*
¡Éxito en su ejercicio!
*/