package org.stambecco.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent

/**This class implements a plugin component using a tree
 *  traverser */
class TemplateTraverseComponent(val global: Global) extends PluginComponent {
  import global._
  import global.definitions._

  val runsAfter = List("refchecks")

  /**The phase name of the compiler plugin
   * @todo Adapt to specific plugin.
   */
  val phaseName = "plugintemplatetraverse"

  lazy val qBase = global.definitions.getClass("org.stambecco.QBase")



  import global._

  //val t: Type = definitions.getClass("scala.List").tpe
  //val tl: List[Type] = t.typeParams

  lazy val listType: Type = definitions.getClass("scala.List").tpeHK

  lazy val mapType: Type = definitions.getClass("scala.collection.immutable.Map").tpeHK

  lazy val scalaObj: Type = definitions.getClass("scala.ScalaObject").tpeHK

  lazy val prod: Type = definitions.getClass("scala.Product").tpeHK

  lazy val javaObj: Type = definitions.getClass("java.lang.Object").tpeHK

  lazy val legal: List[Type] = List(definitions.getClass("scala.Int"), qBase, definitions.getClass("java.lang.String"),
    definitions.getClass("scala.Boolean"),
    definitions.getClass("scala.Char"),
    definitions.getClass("scala.Byte"),
    definitions.getClass("scala.Float"),
    definitions.getClass("scala.Double"),
    definitions.getClass("scala.Short"),
    definitions.getClass("scala.Long")).map(_.tpeHK)

  /*
  lazy val legal: List[Type] = basicLegal.map(_.tpe) ::: {
    val list = definitions.getClass("scala.List")
    for {
      t <- basicLegal
    } yield appliedType(list.tpeHK, List(t.tpe))
  }
  */

  def newPhase(prev: Phase): Phase = new TraverserPhase(prev)
  class TraverserPhase(prev: Phase) extends StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      var s: Set[String] = Set()

      def newTraverser(): Traverser = new ForeachTreeTraverser(check)

      def isQ(in: List[TypeDef]): Boolean = {
        println("Types " + in.map(_.name))
        false
      }

      def check(tree: Tree): Unit = {
        s += tree.getClass.getName

        //println(qBase.getClass)

        def doWeCare(cd: ClassDef): Boolean = {

          cd.symbol.isClass && !cd.symbol.isModuleClass && !cd.symbol.isTrait && cd.symbol.tpe <:< qBase.tpe
        }

        def testIt(in: Type): Boolean = {

          if (in.typeSymbol.tpeHK == mapType) {
            in match {
              case TypeRef(_, _, kt :: vt :: Nil) => testIt(kt) && testIt(vt)
              case _ => false
            }
          } else if (in.typeSymbol.tpeHK == listType) {

            in match {
              case TypeRef(_, _, lt :: Nil) => testIt(lt)
              case _ => false
            }
          } else legal.exists(t => in <:< t)
        }



        tree match {
          case cd@ClassDef(mods, name, tp, impl) if doWeCare(cd) =>
            def validateClass(who: Type): Boolean =
            if (who <:< qBase.tpeHK || who == scalaObj || who == prod || who == javaObj) true
            else {
              unit.error(cd.pos, "Illegal inheritance "+name+" inherits from "+who+" which is not a subclass of QBase")
              false
            }

            def validateParent(who: List[Type]): Boolean =
              who.forall(validateClass)

            validateParent(cd.symbol.tpe.parents)
            // println("Parents of "+name+" "+cd.symbol.tpe.parents)

            cd.symbol.tpe.members.foreach {
              m =>
                      if (m.isVariable) {
                        unit.error(m.pos, "Illegal var '"+m.name+"' in "+name+", a subclass of QBase")
                      } else if (!m.isSourceMethod) {
                      m.tpe match {
                        case PolyType(_, r) if !testIt(r) =>
                          // println("Member " + m + " bad type "+r)
                        unit.error(m.pos, "Illegal val '"+m.name+"' in "+name+", a subclass of QBase: "+r)
                        case _ =>
                      }
                      }
            }


            /*
            impl.body.foreach {
              case ValDef(_, name, tpt: TypeTree, _) if testIt(tpt.tpe) =>

              case ValDef(_, name, tpt, _) =>
                println("Type " + tpt.tpe)
                error("Illegal variable type in subclass of QBase")
              case x =>
            }
            */


          case x =>
        }
      }


      newTraverser().traverse(unit.body)

    }
  }
}
