package org.stambecco.plugin

import scala.tools.nsc.Global

abstract class TemplateAnnotationChecker {
  val global: Global
  import global._

  object checker extends AnnotationChecker {
    def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {
     
      true
    }

    override def addAnnotations(tree: Tree, tpe: Type): Type = {
    
      tpe
    }
  }
}
