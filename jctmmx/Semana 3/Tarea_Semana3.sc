/*
*** EJERCICIO 1 ***
*/
def ejercicio1(cadena:String):List[String] = {

  var cadenaLimpia:String = cadena.replace(" ","")
  var cadenaChaparras:String = cadenaLimpia.toLowerCase
  var altas:Int = 0
  var listaCov:List[String]=List()

  for (jc <- 0.until(cadenaLimpia.length)){
    if(cadenaLimpia(jc).isUpper){
      altas +=1
      if(altas>1)
        return List("ERROR_??")
    }else{
      listaCov= listaCov :+ (cadenaChaparras.substring(0,jc)+
        cadenaChaparras(jc).toUpper+
        cadenaChaparras.substring(jc+1,cadenaChaparras.length))
    }
  }
  listaCov
}

println(ejercicio1("test de ejercicio"))

/*
*** EJERCICIO 2 ***
*/

def ejercicio2(lista:Array[Any]): Array[Any] = {
  var arrRes:Array[Any] = Array()

  for (i <- lista.indices){
    if(lista(i) == 0)
      arrRes= Array(0)++arrRes
    else
      arrRes = arrRes ++Array(lista(i))
  }
  arrRes
}

println(ejercicio2(Array("que",0,"o como")) mkString(","))


/*
*** EJERCICIO 3 ***
*/

def ejercicio3(termino:Int):Int = {
  val fib:Array[Int] = Array(0,0,1)
  if(termino<0)
    -1
  else{
    for( I <- 0 until termino){
      fib(0) = fib(1)
      fib(1)=fib(2)
      fib(2)=fib(0)+fib(1)
    }
    fib(1)
  }
}

println(ejercicio3(9))

/*
*** EJERCICIO 4 ***
*/

def ejercicio4(par:String):Boolean = {
  var balance:Int = 0
  for (p <- par){
    if(p==')')
      balance-=1
    else if (p=='(')
      balance+=1
    if(balance < 0)
      return false
  }
  if(balance == 0)
    true
  else
    false
}

println(ejercicio4("(())(())"))


/*
*** EJERCICIO 5 ***
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
    "ERROR"
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
      respSum.substring(2,respSum.length)
    else
      respSum.substring(1,respSum.length)
  }
}


println(ejercicio5("4","3"))