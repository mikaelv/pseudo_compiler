package pseudoc

case class VarMap(v: Map[String, (Class[_], Any)]) {
  // TODO one Map per type ?
  def apply(variable: String): Any = {
    v.apply(variable)._2
  }

  private def friendlyTypeName(clazz: Class[_]): String = clazz match {
    case c if c == classOf[Int] || c == classOf[java.lang.Integer] => "Integer"
    case c if c == classOf[Byte] || c == classOf[java.lang.Byte] => "Integer"  // In Scala.js, small numbers are optimized to Byte
    case c if c == classOf[Short] || c == classOf[java.lang.Short] => "Integer" // In Scala.js, medium numbers are optimized to Short
    case c if c == classOf[String] => "String"
    case c if c == classOf[Boolean] || c == classOf[java.lang.Boolean] => "Boolean"
    case c if c == classOf[Array[Int]] => "Array[Int]"
    case _ => clazz.getSimpleName
  }

  def store(variable: String, value: Any): VarMap = {

    val optTpe = v.get(variable).map(_._1)
    optTpe.foreach { tpe =>
      // Handle type compatibility using isInstanceOf for Scala.js
      // Check if the stored type is any numeric type (byte, short, int, Integer)
      val numericTypeNames = Set("byte", "short", "int", "Integer")
      val isNumericVariable = numericTypeNames.contains(tpe.getSimpleName.toLowerCase)
      val isBooleanVariable = tpe.getSimpleName.toLowerCase == "boolean"
      val isStringVariable = tpe.getSimpleName == "String"
      val isArrayVariable = tpe.getSimpleName.contains("Array")
      
      val isValueNumeric = value.isInstanceOf[Byte] || value.isInstanceOf[Short] || value.isInstanceOf[Int]
      val isCompatible = (isNumericVariable && isValueNumeric) ||
                        (isBooleanVariable && value.isInstanceOf[Boolean]) ||
                        (isStringVariable && value.isInstanceOf[String]) ||
                        (isArrayVariable && value.getClass.getSimpleName.contains("Array")) ||
                        (tpe == value.getClass)
      
      if (!isCompatible)
        throw new RuntimeException(
          s"Type error: Cannot assign ${friendlyTypeName(value.getClass)} value to variable '${variable}' of type ${friendlyTypeName(tpe)}"
        )
    }

    VarMap(v + (variable -> (value.getClass, value)))
  }

  def contains(variable: String): Boolean = v.contains(variable)
}

object VarMap:

  def apply(kvs: (String, Any)*) =
    new VarMap(Map(kvs.map((k, v) => k -> (v.getClass, v)): _*))

  def empty: VarMap = VarMap(Map.empty)
