package pseudoc

case class VarMap(v: Map[String, (Class[_], Any)]) {
  def apply(variable: String): Any = {
    v.apply(variable)._2
  }

  def store(variable: String, value: Any): VarMap = {

    val optTpe = v.get(variable).map(_._1)
    optTpe.foreach { tpe =>
      // Handle boxing compatibility for Int/Integer and Boolean/boolean
      val isCompatible = (tpe == classOf[Int] && value.getClass == classOf[java.lang.Integer]) ||
                        (tpe == classOf[java.lang.Integer] && value.getClass == classOf[Int]) ||
                        (tpe == classOf[Boolean] && value.getClass == classOf[java.lang.Boolean]) ||
                        (tpe == classOf[java.lang.Boolean] && value.getClass == classOf[Boolean]) ||
                        (tpe == value.getClass)
      
      if (!isCompatible)
        throw new RuntimeException(
          s"Type error: Cannot assign ${value.getClass} value to variable '${variable}' of type ${tpe}"
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
