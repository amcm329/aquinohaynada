//---------------------------------​----------------------------------------------------------------  E J E R C I C I O 01  ----
def ejercicio1(cadena:String):List[String] =
{
   import scala.collection.mutable.ListBuffer;

   var sCadena = cadena;
   var iNumeroCaracteres = sCadena.length();
   var iNumeroMayusculas = 0;
   var iPosicionMayuscula = 0;
   var sLista = List();
   var sListaSalida = new ListBuffer[String]()

   //CICLO DE VALIDACION   
   for (iContador1 <- 0 to iNumeroCaracteres-1)
   {
      // VALIDANDO SI ES MAYUSCULA
      if(sCadena(iContador1).toString() == sCadena(iContador1).toString().toUpperCase())
      {
         iNumeroMayusculas = iNumeroMayusculas +1;
         iPosicionMayuscula = iContador1
      }
      // VALIDANDO CARACTERES ESPECIALES
      if (iNumeroMayusculas > 1 || (sCadena(iContador1).toString() == "") || (sCadena(iContador1).toString() == " "))
      {
         return List("No cumple parámetros")
      }
   }

   //ACTUALIZANDO LA CADENA A MINUSCULAS
   var sNuevaCadena = sCadena.toString().toLowerCase();

   //CICLO DE GENERACION DE LISTA
   var iAuxiliar = 0;
   var sCadenaOla = "";
   for (iContador2 <- 0 to iNumeroCaracteres-1)
   {
      if (iContador2 != iPosicionMayuscula)
      {
         for (iContador3 <- 0 to iNumeroCaracteres-1)
         {
            if (iAuxiliar == iContador3)
            {
               sCadenaOla = sCadenaOla + sNuevaCadena(iContador3).toString().toUpperCase();
            }
            else
            {
               sCadenaOla = sCadenaOla + sNuevaCadena(iContador3);
            }
         }
      sListaSalida += sCadenaOla;
      }
      iAuxiliar = iAuxiliar + 1;
      sCadenaOla = "";
   }
   return sListaSalida.toList;
}
ejercicio1("Holamundo");
ejercicio1("HolaMundo");

//---------------------------------​----------------------------------------------------------------  E J E R C I C I O 02  ----
def ejercicio2(lista:Array[Any]): Array[Any] =
{
   var aValores = lista.filter(_ != 0);
   var aCeros = lista.filter(_ == 0);
   return aCeros ++ aValores;
}
ejercicio2(Array(false,1,0,1,2,0,1,3,"a"));
//---------------------------------​----------------------------------------------------------------  E J E R C I C I O 03  ----
def ejercicio3(iTermino:Int):Int =
{
   var iNumero1 = 0;
   var iNumero2 = 1;
   var iNumero3 = 0;
   if (iTermino < 0)
   {
      return -1
   }
   else
   {
      for (iContador <- 0 to iTermino-1)
      {
         if (iContador == 0){iNumero3 = 0} else if (iContador == 1){iNumero3 = 1}
         else
         {
            iNumero3 = iNumero1 + iNumero2;
            iNumero1 = iNumero2;
            iNumero2 = iNumero3;
         }
      }
   }
   return iNumero3;
}

ejercicio3(4)
ejercicio3(7)
//---------------------------------​----------------------------------------------------------------  E J E R C I C I O 04  ----
def fnCargaParentesis (cArray:Array[Char]):Array[Char] =
{
   //SI EL ARREGLO NO TIENE ELEMENTOS
   if(cArray.length == 0)
   {
      return Array('S')
   }
   else
   {
      // RECORREMOS EL ARREGLO
      for (iContador <-0 to cArray.length-2)
      {
         if(cArray(iContador) == '(' && cArray(iContador+1) == ')')
         {
            cArray.update(iContador,'S')
            cArray.update((iContador+1),'S')
         }
      }
      // VALIDANDO SI SE REDUJO EL ARREGLO
      if (cArray.length == cArray.filter(_ != 'S').length)
      {
         return Array('N')
      }
      // SI SI SE REDUJO SE HACE RECURSIVO EL PROCESO
      else
      {
         fnCargaParentesis (cArray.filter(_ != 'S'))
      }
   }
}


def ejercicio4(sParentesis:String):Boolean =
{
   var cParentesis = sParentesis.toCharArray();
   var iNumeroAbierto = 0;
   var iNumeroCerrado = 0;
   for (iContador <- 0 to cParentesis.length-1)
   {
      //BUSCANDO BALANCE
      if   (cParentesis(iContador).toString() == "(")
      {
         iNumeroAbierto = iNumeroAbierto + 1;
      }
      else if (cParentesis(iContador).toString() == ")")
      {
         iNumeroCerrado = iNumeroCerrado + 1;
      }
   }
   // SI LA CADENA ESTA BALANCEADA ANALIZAMOS SU SINTAXIS RECURSIVAMENTE
   if(iNumeroAbierto == iNumeroCerrado)
   {
      println("arreglo: " + fnCargaParentesis(cParentesis)(0))
      if(fnCargaParentesis(cParentesis)(0) == 'S')
      {
         return true
      }
      else
      {
         return false
      }
   }
   else
   {
      return false
   }
}

ejercicio4("");
ejercicio4("()");
ejercicio4("(())");
ejercicio4("(())()()");
ejercicio4("())()()");
ejercicio4(")(");

//---------------------------------​----------------------------------------------------------------  E J E R C I C I O 05  ----
def ejercicio5(sArriba:String,sAbajo:String): String =
{
   val sArregloArriba = sArriba.toCharArray();
   val sArreglosAbajo = sAbajo.toCharArray();

   if (sArriba.isEmpty == true || sAbajo.isEmpty == true)
   {
      return "E R R O R"
   }
   else
   {
      for (iContador <- 0 to sArregloArriba.length-1)
      {
         if(!(sArregloArriba(iContador).toInt >= 48 && sArregloArriba(iContador).toInt <= 57))
         {
            return "E R R O R"
         }
      }

      for (iContador <- 0 to sArreglosAbajo.length-1)
      {
         if(!(sArreglosAbajo(iContador).toInt >= 48 && sArreglosAbajo(iContador).toInt <= 57))
         {
            return "E R R O R"
         }
      }
   }
   var iProducto: Int = 0;
   var iAcumulado: Int = 0;
   var sMulti = "";
   var sSalida = "";
   for (iCont1 <- (0 to (sAbajo.length-1)).reverse)
   {
      for (iCont2 <- (0 to (sArriba.length-1)).reverse)
      { 
         iProducto = sArregloArriba(iCont2).toString.toInt * sArreglosAbajo(iCont1).toString.toInt
         if (iCont2 != 0)
         {
            sMulti = ((iProducto + iAcumulado)%10).toString + sMulti
            iAcumulado = (iProducto/10).toInt
         }
         else
         {
            if ((iProducto + iAcumulado)<10)
            {
               sMulti = "0" + (iProducto + iAcumulado).toString + sMulti            
            }
            else
            {
               sMulti = (iProducto + iAcumulado).toString + sMulti
            }
            iAcumulado = 0
         }
      }
      sSalida = sSalida + "0"*iCont1 + sMulti + "0"*((sAbajo.length-1) - iCont1) + " "
      sMulti = ""
   }
   val sLista = sSalida.split(" ")
   var iSuma = 0;
   sSalida = ""
   for (iCont1 <- (0 to sLista(0).length-1).reverse)
   {
      iSuma = 0;
      for (iCont2 <- (0 to sLista.length-1).reverse)
      {
         iSuma = iSuma + sLista(iCont2)(iCont1).toString.toInt;
      }
      if (iCont1 != 0)
      {
         if(iSuma!=0)
         {
            sSalida = (iSuma + iAcumulado)%10 + sSalida 
            iAcumulado = ((iSuma + iAcumulado)/10).toInt
         }
         else
         {
            sSalida = (iSuma + iAcumulado)%10 + sSalida 
            iAcumulado = 0
         }
      }
      else
      {
            sSalida = (iSuma + iAcumulado) + sSalida 
            iAcumulado = 0
      }
   }

   if (sSalida(0) == '0')
   {
      return sSalida.substring(1,sSalida.length)
   }
   else
   {
      return sSalida
   }
}

ejercicio5("520","500000") = "260000000"
ejercicio5("1","1")  = "1"
ejercicio5("x","") = "ERROR"
ejercicio5("","x") = "ERROR"
ejercicio5("","") = "ERROR"
ejercicio5("55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555","55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555")
= "3086419753086419753086419753086419753086419753086419753086419753086419753086419753086419753024691358024691358024691358024691358024691358024691358024691358024691358024691358024691358025"
//---------------------------------​----------------------------------------------------------------  E J E R C I C I O 06  ----
def ejercicio6(dMatriz1:List[List[Double]], dMatriz2 : List[List[Double]]):List[List[Double]]=
{
   import scala.collection.mutable.ListBuffer
   var dListaM1 = new ListBuffer[Double]()
   var dListaM2 = new ListBuffer[Double]()
   var dListaMS = new ListBuffer[Double]()
   var dMatrizS = new ListBuffer[List[Double]]()
   var dMatrizT =  dMatriz2.transpose
   val iFilM1 = dMatriz1.size;
   val iColM1 = dMatriz1(0).size;
   val iFilM2 = dMatrizT.size;
   val iColM2 = dMatrizT(0).size;
   var dSuma: Double = 0.0

   //EL NUMERO DE COLUMNAS DE A DEBE COINCIDIR CON EL NUMERO DE FILAS DE B
   if (iColM1 != iFilM2)
   {
      println("\n\n---------------------------------------------------\n|                   N O T A                       |\n---------------------------------------------------\n| No se puede realizar la multiplicación          |\n| El Número de Columnas de la Matriz A debe       |\n| coincidir con el número de Filas de la Matriz B |\n---------------------------------------------------")

      return dMatrizS.toList.toList
   }
   else
   {

      //LINEALIZANDO CADA MATRIZ

      for (iCont1 <- 0 to (iFilM1-1))
      {
         for (iCont2 <- 0 to (iColM1-1))
         {
              dListaM1 += dMatriz1(iCont1)(iCont2)
         }
      }
      for (iCont1 <- 0 to (iFilM2-1))
      {
         for (iCont2 <- 0 to (iColM2-1))
         {
            dListaM2 += dMatrizT(iCont1)(iCont2)
         }
      }

      //MULTIPLICANDO Y GENERANDO LISTA DE LISTAS

      for(iCont0 <- 0 to iColM1-1)
      {
         for(iCont1 <- 0 to iColM1-1)
         {
            for(iCont2 <- 0 to iColM1-1)
            {
               dSuma = dSuma + dListaM1(iColM1*iCont0+iCont2) * dListaM2(iColM1*iCont1+iCont2)
            }
            dListaMS += dSuma
            dSuma= 0.0
         }
         dMatrizS +=  dListaMS.toList
         dMatrizS(iCont0).toList
         dListaMS.clear
      }

      return dMatrizS.toList
   }
}

ejercicio6(List(List(1.0,2.0,3.0),List(4.0,5.0,6.0), List(7.0,8.0,9.0)),List(List(9.0,8.0,7.0),List(6.0,5.0,4.0), List(3.0,2.0,1.0)))
	R = List(List(30.0, 24.0, 18.0), List(84.0, 69.0, 54.0), List(138.0, 114.0, 90.0))
ejercicio6(List(List(1.0,2.0),List(4.0,5.0)),List(List(9.0,8.0,7.0),List(6.0,5.0,4.0)))
	R = E R R O R
ejercicio6(List(List(9.0)),List(List(9.0)))
	R = List( List(81.0 )
ejercicio6(List(List(1,2,3),List(4,5,6), List(7,8,9)),List(List(9,8,7),List(6,5,4), List(3,2,1)))
	R = List( List(30.0, 24.0, 18.0), List(84.0, 69.0, 54.0), List(138.0, 114.0, 90.0) )
ejercicio6(List(List(2),List(1)),List(List(3,5),List(3,5)))
	R = E R R O R
ejercicio6(List(List(3,0,0),List(0,-2,0),List(0,0,1)),List(List(1,0,0),List(0,1,0),List(0,0,-7)))
	R = List( List(3,0,0) , List(0,-2,0) , List(0,0,-7) )
//---------------------------------​----------------------------------------------------------------  E J E R C I C I O 07  ----
def ejercicio7(iLimite:Int):List[Int] =
{
   import scala.collection.mutable.ListBuffer;
   val listaNegra  = new ListBuffer[Int]();
   val listaSalida = new ListBuffer[Int]();
   //Generando multiplos
   for (iContador1 <- 2 to (iLimite*10))
   {
      for (iContador2 <- 2 to 10)
      {
         listaNegra += (iContador1 * iContador2);
      }
   }

   //Devuelve valores que no son multiplos
   for (iContador <- 2 to (iLimite*10))
   {
      if (!listaNegra.contains(iContador))
      {
         listaSalida += iContador;
      }
   }
   return listaSalida.toList.take(iLimite);
}

ejercicio7(1)	= List(2)
ejercicio7(2)	= List(2, 3)
ejercicio7(3)	= List(2, 3, 5)
ejercicio7(4)	= List(2, 3, 5, 7)
ejercicio7(5)	= List(2, 3, 5, 7, 11)
ejercicio7(10)	= List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
ejercicio7(100)	= List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 121, 127, 131, 137, 139, 143, 149, 151, 157, 163, 167, 169, 173, 179, 181, 187, 191, 193, 197, 199, 209, 211, 221, 223, 227, 229, 233, 239, 241, 247, 251, 253, 257, 263, 269, 271, 277, 281, 283, 289, 293, 299, 307, 311, 313, 317, 319, 323, 331, 337, 341, 347, 349, 353, 359, 361, 367, 373, 377, 379, 383, 389, 391, 397, 401, 403, 407, 409, 419, 421)
//---------------------------------​----------------------------------------------------------------  E J E R C I C I O 08  ----
def ejercicio8 (sCadena: String): Boolean =
{
   val sAbecedario: Set[String] = Set("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z");
   val sSet:        Set[String] = (sCadena.toLowerCase.replace(",","").replace(" ","").split("").map(_.trim).toList.sorted).distinct.toSet;
   if (sAbecedario.diff(sSet).size != 0)
   {
      println("Faltaron las letras : " + sAbecedario.diff(sSet))
      return false
   }
   return true
}

ejercicio8("El cadaver de Wamba, rey godo de Espana, fue exhumado y trasladado en una caja de zinc que peso un kilo");
ejercicio8("El veloz murcielago hindu comia feliz cardillo y kiwi");
ejercicio8("La ciguena tocaba el saxofon detras del palenque de paja");

//---------------------------------​----------------------------------------------------------------  E J E R C I C I O 09  ----
def ejercicio9 (sCadena: String): Any =
{
   import scala.collection.mutable.ListBuffer; 
   val sArreglo = sCadena.toLowerCase.split(" ");
   val iMapNumeros = Map("cero"->0,"uno"->1,"dos"->2,"tres"->3,"cuatro"->4,"cinco"->5,"seis"->6,"siete"->7,"ocho"->8,"nueve"->9);
   var iListaNum = new ListBuffer[Int]()
   for (iContador <- 0 to (sArreglo.length)-1)
   {
      if (iMapNumeros.contains(sArreglo(iContador)))
      {
         iListaNum += iMapNumeros.get(sArreglo(iContador)).get
      }
      else
      {
         return " E R R O R ";
      }
   }
   return (iListaNum.toList.sum/iListaNum.toList.size);
}
ejercicio9("uno dos tres cero");
ejercicio9("uno uno cadenaQueNoEsNumero") = "ERROR"
ejercicio9("unodos uno uno uno") = "ERROR"
ejercicio9("") = "ERROR"

//---------------------------------​----------------------------------------------------------------  E J E R C I C I O 10  ----
def ejercicio10(entrada:String,clave:String): String =
{
   val sCadenaTexto = entrada.replace(" ","").toUpperCase();
   val sCadenaClave = (clave*((sCadenaTexto.length/clave.length)+1)).substring(0,sCadenaTexto.length)
   var sCadenaSalida = "";
   for (iContador <- 0 to (sCadenaTexto.length-1))
   {
      if ((sCadenaTexto(iContador).toInt + (sCadenaClave(iContador).toInt)-65) >= 91)
      {
        sCadenaSalida = sCadenaSalida + (((sCadenaTexto(iContador).toInt + sCadenaClave(iContador).toInt)-91).toChar).toString
      }
      else
      {
         sCadenaSalida = sCadenaSalida + (((sCadenaTexto(iContador).toInt + sCadenaClave(iContador).toInt)-65).toChar).toString
      }
   }
   return sCadenaSalida
}
ejercicio10("HOLA","IVAN")			=	"PJLN"
ejercicio10("PARIS","LOUP")			=	"AOLXD"
ejercicio10("MICHIGAN","HOUGHTON")	=	"TWWNPZOA"
//---------------------------------​----------------------------------------------------------------  E J E R C I C I O 0N  ----
