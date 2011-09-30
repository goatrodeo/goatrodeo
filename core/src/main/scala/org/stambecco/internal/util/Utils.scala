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

private[stambecco] object Utils {
  trait Equal[T1 >: T2 <: T2, T2]

  class Fn1Wrapper[T1, R](fn : T1 => R) {
    def apply(a1 : T1) = fn(a1)
  }
  
  class Fn2Wrapper[T1, T2, R](fn : (T1, T2) => R) {
    def apply(a1 : T1, a2 : T2) = fn(a1, a2)
  }
  
  case class TypeToValue[T, VT](value : VT) {
    def apply() = value
  }
  
  def to[T, VT](implicit fn : TypeToValue[T, VT]) = fn()
  
  trait TypeVisitor {
    type ResultType
  }
  
  trait Visitable[V <: TypeVisitor] {
    type Accept[V2 <: V] <: V2#ResultType
  }
  
  trait Addable {
    type AddType <: Addable
    type Add[T <: AddType] <: AddType
  }
  
  type +[A1 <: Addable, A2 <: A1#AddType] = A1#Add[A2]
  
  trait Subtractable {
    type SubType <: Subtractable
    type Sub[T <: SubType] <: SubType
  }
  
  type -[S1 <: Subtractable, S2 <: S1#SubType] = S1#Sub[S2]
  
  def value[T] : T = null.asInstanceOf[T]
}
