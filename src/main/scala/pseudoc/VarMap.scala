package pseudoc

case class VarMap(v: Map[String, (Class[_], Any)]) {
  // TODO one Map per type ?
  def apply(variable: String): Any = {
    v.apply(variable)._2
  }

  private def friendlyTypeName(clazz: Class[_]): String = clazz match {
    case c if c == classOf[Int] || c == classOf[java.lang.Integer] => "Integer"
    case c if c == classOf[String] => "String"
    case c if c == classOf[Boolean] || c == classOf[java.lang.Boolean] => "Boolean"
    case c if c == classOf[Array[Int]] => "Array[Int]"
    case _ => clazz.getSimpleName
  }

  def store(variable: String, value: Any): VarMap = {

    val optTpe = v.get(variable).map(_._1)
    optTpe.foreach { tpe =>
      // Handle boxing compatibility for Int/Integer and Boolean/boolean
      val isCompatible = (tpe == classOf[Int] && value.getClass == classOf[java.lang.Integer]) ||
                        (tpe == classOf[java.lang.Integer] && value.getClass == classOf[Int]) ||
                        (tpe == classOf[Boolean] && value.getClass == classOf[java.lang.Boolean]) ||
                        (tpe == classOf[java.lang.Boolean] && value.getClass == classOf[Boolean]) ||
                        (tpe == classOf[Array[Int]] && value.getClass == classOf[Array[Int]]) ||
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
