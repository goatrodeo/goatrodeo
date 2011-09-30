package org.stambecco
package internal
package util

/*
 Copyright (c) 2009, Jesper Nordenberg
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:
 * Redistributions of source code must retain the above copyright
 notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions and the following disclaimer in the
 documentation and/or other materials provided with the distribution.
 * Neither the name of the <organization> nor the
 names of its contributors may be used to endorse or promote products
 derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY Jesper Nordenberg ''AS IS'' AND ANY
 EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL <copyright holder> BE LIABLE FOR ANY
 DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

private[stambecco] object OO {
  import HLists._
  import Nats._
  import Tuples._
  
  trait MethodBase {
    type Object
  }
  
  trait MethodObj[Obj] extends MethodBase {
    type Object = Obj
  }
  
  trait Method0Base[Obj] extends MethodObj[Obj] {
    type Out
    def apply(obj : Obj) : Out
  }
  
  class Method0[Obj, O](fn : Obj => O) extends Method0Base[Obj] {
    type Out = O
    def apply(obj : Obj) = fn(obj)
  }
  
  trait Method1Base[Obj] extends MethodObj[Obj] {
    type In1
    type Out
    def apply(obj : Obj, arg : In1) : Out
  }
  
  class Method1[Obj, I, O](fn : (Obj, I) => O) extends Method1Base[Obj] {
    type In1 = I
    type Out = O
    def apply(obj : Obj, arg : In1) : Out = fn(obj, arg).asInstanceOf[Out]
  }
  
  def _override[M <: MethodBase, H, T <: HList](obj : HCons[H, T], m : M)(implicit fn : ReplaceByTypeFn[HCons[H, T], _0, M]) : HCons[H, T] =
    obj.replaceByType(_0, m)
  
    
  case class RichHCons[H, T <: HList](l : HCons[H, T]) {
    def get[M <: MethodBase](implicit fn : GetByTypeFn[HCons[H, T], _0, M]) : M = l.getByType[_0, M]
    def call[M <: Method0Base[HCons[H, T]]](implicit fn : GetByTypeFn[HCons[H, T], _0, M]) : M#Out = get[M].apply(l)
    def call[M <: Method1Base[HCons[H, T]]](arg : M#In1)(implicit fn : GetByTypeFn[HCons[H, T], _0, M]) : M#Out = applyArg(get[M], arg)
    def delegate[M <: Method0Base[HCons[H, T]]](l2 : HCons[H, T])(implicit fn : GetByTypeFn[HCons[H, T], _0, M]) : M#Out = get[M].apply(l2)
    def applyArg[M <: Method1Base[HCons[H, T]]](m : M, arg : M#In1) : M#Out = m(l, arg.asInstanceOf[m.In1])
    def |=[M <: MethodBase](m : M)(implicit fn : ReplaceByTypeFn[HCons[H, T], _0, M]) = _override(l, m)
  }
  
  implicit def hconsToRichHCons[H, T <: HList](l : HCons[H, T]) : RichHCons[H, T] = RichHCons(l)

  case class MethodProduct[P <: Product](p : P) {
    def call[M <: Method0Base[P]](implicit fn : Getter[P, M]) : M#Out = get[P, M](p).apply(p)
    def call[M <: Method1Base[P]](arg : M#In1)(implicit fn : Getter[P, M]) : M#Out = applyArg(get[P, M](p), arg)
    def delegate[M <: Method0Base[P]](l2 : P)(implicit fn : Getter[P, M]) : M#Out = get[P, M](p).apply(l2)
    def applyArg[M <: Method1Base[P]](m : M, arg : M#In1) : M#Out = m(p, arg.asInstanceOf[m.In1])
    def |=[M <: MethodBase](m : M)(implicit fn : Replacer[P, M]) = replace(p, m)
  }
  
  implicit def productToMethodProduct[P <: Product](p : P) = MethodProduct(p)
  
}
