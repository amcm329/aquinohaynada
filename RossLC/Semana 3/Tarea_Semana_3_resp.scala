def ejercicio1(cadena:String):List[String]={
import scala.collection.mutable.ListBuffer
 val x:Int=cadena.length()
 val a=cadena.toUpperCase
 val b=cadena.toLowerCase
 var lista = new ListBuffer[String]()
 var con=0
 var con1=0
 var con2=0
  for(i <- cadena)
   {if (i>=65 && i<= 90)
    {con=con+1
      if (con>1) {return List("Error") }
      }}
   if (cadena.take(1)==" "){return List()}
   while(con2<x)
    {lista=lista:+b.replace(b(con2),a(con2))
     con2=con2+1}
 var y=lista.filterNot(_==cadena).toList
 return y
}

def ejercicio2(lista:Array[Any]):Array[Any]={
var a=lista.filter(_ !=0)
var b=lista.filter(_ ==0)
var c=b++a
return c
}

def ejercicio3(termino:Int): Int = {
   var a = 0
   var b = 1
   var i = 1
   if (termino==1){return 1}
   else if(termino<0){return 0}
   else {
   while(i < termino){
      val suma = a+b
      a = b
      b = suma
      i=i+1 }
   return a}
}

def ejercicio4(parentesis:String):Boolean={
var con=0
var con1=0
val a=parentesis.count(_ == '(')
val b=parentesis.count(_ == ')')

if (a==b){
for(i<-parentesis)
{
  if (i==40)   {con=con+1}
  else if(i==41)  { con=con-1 }
if (con<0){return false}
}
{return true}
 }
else {return false}
}

def ejercicio5(m1:String,m2:String):String = {
for (i<-m1){ 
  if (!(i>=47 && i<=59)){return "Error"}}
for (i<-m2){
  if (!(i>=47 && i<=59)){return "Error"}}
if (m1.isEmpty==true || m2.isEmpty == true){return "Error"}

import scala.collection.mutable.ListBuffer
var ml1 = m1.toList.map(_.toInt-48)
var ml2 = m2.toList.map(_.toInt-48)
var listam1 = new ListBuffer[List[Int]]()
var listamf = new ListBuffer[List[Int]]()
var listamr = new ListBuffer[List[Int]]()

var con=0
var con1=0

while(con<ml2.size){
//suma de primera linea
  while (con1<ml1.size)
  {listam1=listam1:+(ml1(con1)*ml2(con)).toString.toList.map(_.toInt-48)
   con1=con1+1}
var a=listam1.toList
listamr=listamr:+List(a.flatten.mkString.takeRight(1).toInt)

//1er while para sumar entre elementos 
while (a.size>1){
//Para la suma entre los sumeros 
var b=(if((a(a.size-1).size)>1){(a(a.size-1)(0)+a(a.size-2)(a(a.size-2).size-1)).toString.toList.map(_.toInt-48)} else {a(a.size-2)})
//Para ver si se suma o si se agrega el elemento
if ((b.size)>1) {if(a(a.size-2).size>1){a=a.dropRight(1).dropRight(1):+List(a(a.size-2)(0),b(b.size-1))} else {a=a.dropRight(1).dropRight(1):+List(b(b.size-1))}
listamr=listamr:+List(b(1))
} else {if(a(a.size-2).size>1){a=a.dropRight(1).dropRight(1):+List(a(a.size-2)(0),b(b.size-1))} else {a=a.dropRight(1).dropRight(1):+List(b(b.size-1))}
//Se agrega a la lista de residuos
listamr=listamr:+List(a.flatten.mkString.takeRight(1).toInt)}
}
if(listam1(0).size>1){listamr=listamr:+List(a.flatten.mkString.take(1).toInt)} else {listamr}
 listamf=listamf:+listamr.flatten.reverse.toList
 listam1 = new ListBuffer[List[Int]]()
 listamr = new ListBuffer[List[Int]]()
  con=con+1
  con1=0}

//ciclo para colocarle los 0 faltantes a la derecha
var listams=listamf.toList
con=0
con1=1
listamf = new ListBuffer[List[Int]]()
while(con<listams.size){
     listamf=listamf:+List(listams(con),("0"*(listams.size-con1)).toList.map(_.toInt-48)).flatten
     con=con+1
     con1=con1+1}
listams=listamf.toList

//ciclo para colocarle los 0 faltantes a la izquierda
con1=1
con=0
listamf = new ListBuffer[List[Int]]()
while(con<listams.size){
     listamf=listamf:+List(("0"*(listams(0).size-listams(listams.size-con1).size)).toList.map(_.toInt-48),listams(listams.size-con1)).flatten
     con=con+1
     con1=con1+1}

//suma residuos
listams=listamf.toList.transpose
con=0
listamf = new ListBuffer[List[Int]]()
while(con<listams.size)
      {listamf=listamf:+listams(con).foldLeft(0)(_+_).toString.toList.map(_.toInt-48)
      con=con+1}

//sumafinal
listamr = new ListBuffer[List[Int]]()
var a=listamf.toList
listamr=listamr:+List(a.flatten.mkString.takeRight(1).toInt)

//1er while para sumar entre elementos
while (a.size>1){
//Para la suma entre los sumeros
var b=(if((a(a.size-1).size)>1){(a(a.size-1)(0)+a(a.size-2)(a(a.size-2).size-1)).toString.toList.map(_.toInt-48)} else {a(a.size-2)})
//Para ver si se suma o si se agrega el elemento
if ((b.size)>1) {if(a(a.size-2).size>1){a=a.dropRight(1).dropRight(1):+List(a(a.size-2)(0),b(b.size-1))} else {a=a.dropRight(1).dropRight(1):+List(b(b.size-1))}
listamr=listamr:+List(b(1))
} else {if(a(a.size-2).size>1){a=a.dropRight(1).dropRight(1):+List(a(a.size-2)(0),b(b.size-1))} else {a=a.dropRight(1).dropRight(1):+List(b(b.size-1))}
//Se agrega a la lista de residuos
listamr=listamr:+List(a.flatten.mkString.takeRight(1).toInt)}
}
if(listamf(0).size>1){listamr=listamr:+List(a.flatten.mkString.take(1).toInt)} else {listamr}
 
return listamr.flatten.reverse.toList.mkString
}

def ejercicio6(m1:List[List[Double]], m2:List[List[Double]]):List[List[Double]]={
import scala.collection.mutable.ListBuffer
var listaf = new ListBuffer[List[Double]]()
var con=0
var con1=0
var listas = new ListBuffer[Double]()
var mtr= new ListBuffer[List[Double]]()

var m21=m2
while (!m21.head.isEmpty) {
    mtr += (m21 map (_.head))
    m21 = (m21 map (_.tail))
  }
val mt=mtr.toList

while(con1<m1.size)
      {
     while (con<mt.size)
        {listaf=listaf:+(m1(con1)).zipAll(mt(con),0,0).map{case (x:Double,y:Double)=>(x*y)}
         con=con+1}
      con=0
      con1=con1+1}
con=0
while(con<listaf.size)
      {
      listas=listas:+listaf(con).foldLeft(0.0)(_+_)
      con=con+1}
return listas.toList.grouped(m2(0).size).toList
}

def ejercicio7(n:Int):List[Int]={
import scala.collection.mutable.ListBuffer
      var con1=0
      var y=2
      var con=y
      val a=0
      var lista = new ListBuffer[Int]()
      var z=0
  while (z<n){
        while (con>a){
            if (y%con==0)
            {con1=con1+1}
            con=con-1}
      var x: Int = if (con1>2){0} else {1}
      if (x==1) {lista=lista:+y} else {0l}
    z=lista.size
    y=y+1
con=y
con1=0
      }
var lista1:List[Int]=lista.toList
return lista1
}

def ejercicio8(pangrama: String):Boolean={
var ABC=("abcdefghijklmnopqrstuvwxyz")
val a=pangrama.toLowerCase

for (i<-a){
   ABC=ABC.filterNot(_==i)}

if (ABC=="") return true else return false
}

def ejercicio9(numeros:String): String = {
val b=numeros.split(" ")
var con=0
var con2=0
var con1=0
val c=b.size
val x=b.length
val a:Array[String]=Array("uno","dos","tres","cuatro","cinco","seis","siete","ocho","nueve")
while (con2<x) {
        if(Array(b(con2)).exists{a.contains} ==true) {
          con1= con1 +1 }
con2=con2+1
}
        if(con1!=x){return ("Error")} else {
for (i<-b) { 
if (i=="uno") con=con+1 
else if (i=="dos") con=con+2 
else if (i=="tres") con=con+3 
else if (i=="cuatro") con=con+4 
else if (i=="cinco") con=con+5 
else if (i=="seis") con=con+6 
else if (i=="siete") con=con+7 
else if (i=="ocho") con=con+8 
else if (i=="nueve") con=con+9 
}
val d = con/c
val e = Map(
1->"uno",
2->"dos",
3->"tres",
4->"cuatro",
5->"cinco",
6->"seis",
7->"siete",
8->"ocho",
9->"nueve"
)
return e(d).toString}}

def ejercicio10(entrada:String, llave:String):String = {
import scala.collection.mutable.ListBuffer

if(entrada=="" || llave==""){return "Error"}
val fentrada =entrada.toLowerCase.filter(!" ".contains(_))
val tamano=fentrada.length
val llave2=(llave*((tamano/llave.length)+1)).substring(0,tamano).toLowerCase
val cifrado = Array("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
var listae = new ListBuffer[Int]()
var listal = new ListBuffer[Int]()
var listaf = new ListBuffer[String]()
var con=0
while (con<tamano) {
      listae=listae:+(cifrado.indexOf(fentrada(con).toString))
      con=con+1}

con=0
while (con<tamano) {
      listal=listal:+(cifrado.indexOf(llave2(con).toString))
      con=con+1}

val cif=(listae.toList).zipAll(listal.toList,0,0).map{case (x,y)=>((x+y)%26)}

con=0
while (con<tamano){
listaf=listaf:+cifrado(cif(con))
con=con+1}
return listaf.toList.mkString	
}
