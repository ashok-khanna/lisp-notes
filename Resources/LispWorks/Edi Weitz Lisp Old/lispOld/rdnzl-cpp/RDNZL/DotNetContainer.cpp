// $Header: /usr/local/cvsrep/rdnzl-cpp/RDNZL/DotNetContainer.cpp,v 1.26 2008/02/14 12:13:04 edi Exp $
//
// Copyright (c) 2004-2008, Dr. Edmund Weitz.  All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//
//   * Redistributions of source code must retain the above copyright
//     notice, this list of conditions and the following disclaimer.
//
//   * Redistributions in binary form must reproduce the above
//     copyright notice, this list of conditions and the following
//     disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
// OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
// GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include "stdafx.h"
#include "DotNetContainer.h"

// helper function for constructors - initialize both object and type
// slot, with NULL DotNetReference if necessary
void DotNetContainer::init(Object ^o, Type ^t) {
  object = (o != nullptr) ? new DotNetReference(o) : new DotNetReference();
  type = (t != nullptr) ? new DotNetReference(t) : new DotNetReference();
}

// another helper function - calls init above
void DotNetContainer::init(Object ^o) {
  if (o == nullptr) {
    init(nullptr, nullptr);
  } else {
    // if type isn't explicitely provided, derive it from the object
    init(o, o->GetType());
  }
}

// constructor if type is explicitely set
DotNetContainer::DotNetContainer(Object ^o, Type ^t) {
  init(o, t);
}

// standard constructor
DotNetContainer::DotNetContainer(Object ^o) {
  init(o);
}

// the following five constructors box values types
DotNetContainer::DotNetContainer(bool b) {
  init(b);
}

DotNetContainer::DotNetContainer(__int32 n) {
  init(n);
}

DotNetContainer::DotNetContainer(__int64 n) {
  init(n);
}

DotNetContainer::DotNetContainer(float f) {
  init(f);
}

DotNetContainer::DotNetContainer(double d) {
  init(d);
}

DotNetContainer::DotNetContainer(__wchar_t c) {
  init(c);
}

// this constructor converts a C string into a .NET string
DotNetContainer::DotNetContainer(const __wchar_t *s) {
  init(gcnew String(s));
}

// whether the stored object is NULL
bool DotNetContainer::isNull() {
  return (object->getObject() == nullptr);
}

Object ^DotNetContainer::getContainerObject() {
  return object->getObject();
}

Type ^DotNetContainer::getContainerType() {
  return safe_cast<Type ^>(type->getObject());
}

// change the value of the object slot
void DotNetContainer::setContainerObject(Object ^o) {
  object = new DotNetReference(o);
}

// change the value of the type slot (see CAST in Lisp code)
void DotNetContainer::setContainerType(Type ^t) {
  type = new DotNetReference(t);
}

DotNetContainer::~DotNetContainer() {
  if (object) {
    delete object;
  }
  if (type) {
    delete type;
  }
}

// make a passed-by-reference type out of the container's type (if it
// isn't one already)
void DotNetContainer::refContainerType() {
  Type ^t = safe_cast<Type ^>(type->getObject());
  if (!t->IsByRef)
    type = new DotNetReference(t->Assembly->GetType(String::Concat(t->FullName, "&")));
}

// set the container's type to be its underlying type if it was passed
// by reference
void DotNetContainer::unrefContainerType() {
  Type ^t = safe_cast<Type ^>(type->getObject());
  if (t->IsByRef) {
    type = new DotNetReference(t->GetElementType());
  }
}

// most of the functions below export the public interface of the
// DotNetContainer class to C

__declspec(dllexport) bool DotNetContainerIsNull(void *ptr) {
  return static_cast<DotNetContainer *>(ptr)->isNull();
}

// make a copy of an existing DotNetContainer
__declspec(dllexport) void *copyDotNetContainer(void *ptr) {
  DotNetContainer* original = static_cast<DotNetContainer *> (ptr);
  return new DotNetContainer(original->getContainerObject(), original->getContainerType());
}

// create a "placeholder" (NULL) container which only has a type
__declspec(dllexport) void *makeTypedNullDotNetContainer(const __wchar_t *type) {
  return new DotNetContainer(nullptr, Type::GetType(gcnew String(type)));
}

// create a container representing a type (obtained from its name)
__declspec(dllexport) void *makeTypeFromName(const __wchar_t *type) {
  return new DotNetContainer(Type::GetType(gcnew String(type)), Type::typeid);
}

__declspec(dllexport) void *makeDotNetContainerFromBoolean(bool b) {
  return new DotNetContainer(b);
}

__declspec(dllexport) void *makeDotNetContainerFromInt(int n) {
  return new DotNetContainer(n);
}

__declspec(dllexport) void *makeDotNetContainerFromLong(const __wchar_t *s) {
  return new DotNetContainer(Int64::Parse(gcnew String(s)));
}

__declspec(dllexport) void *makeDotNetContainerFromFloat(float d) {
  return new DotNetContainer(d);
}

__declspec(dllexport) void *makeDotNetContainerFromDouble(double d) {
  return new DotNetContainer(d);
}

__declspec(dllexport) void *makeDotNetContainerFromString(const __wchar_t *s) {
  return new DotNetContainer(s);
}

__declspec(dllexport) void *makeDotNetContainerFromChar(__wchar_t c) {
  return new DotNetContainer(c);
}

// we need to know the length to allocate storage on the Lisp side
__declspec(dllexport) int getDotNetContainerTypeStringLength(void *ptr) {
  return static_cast<DotNetContainer *>(ptr)->getContainerType()->FullName->Length;
}

// temporarily disable warnings here as we know that the destination is large enough
#pragma warning(disable:4996)
__declspec(dllexport) void getDotNetContainerTypeAsString(void *ptr, __wchar_t *s) {
  cli::pin_ptr<const __wchar_t> temp = PtrToStringChars(static_cast<DotNetContainer *>(ptr)->getContainerType()->FullName);
  wcscpy(s, temp);
  #pragma warning(default:4996)
}

// we need to know the length to allocate storage on the Lisp side
__declspec(dllexport) int getDotNetContainerObjectStringLength(void *ptr) {
  return static_cast<DotNetContainer *>(ptr)->getContainerObject()->ToString()->Length;
}

#pragma warning(disable:4996)
__declspec(dllexport) void getDotNetContainerObjectAsString(void *ptr, __wchar_t *s) {
  cli::pin_ptr<const __wchar_t> temp = PtrToStringChars(static_cast<DotNetContainer *>(ptr)->getContainerObject()->ToString());
  wcscpy(s, temp);
  #pragma warning(disable:4996)
}

__declspec(dllexport) void refDotNetContainerType(void *ptr) {
  static_cast<DotNetContainer *>(ptr)->refContainerType();
}

__declspec(dllexport) void unrefDotNetContainerType(void *ptr) {
  static_cast<DotNetContainer *>(ptr)->unrefContainerType();
}

// "unboxing"

__declspec(dllexport) __wchar_t getDotNetContainerCharValue(void *ptr) {
  return *safe_cast<System::Char ^>(static_cast<DotNetContainer *>(ptr)->getContainerObject());
}

__declspec(dllexport) int getDotNetContainerIntValue(void *ptr) {
  return *safe_cast<System::Int32 ^>(static_cast<DotNetContainer *>(ptr)->getContainerObject());
}

__declspec(dllexport) bool getDotNetContainerBooleanValue(void *ptr) {
  return *safe_cast<System::Boolean ^>(static_cast<DotNetContainer *>(ptr)->getContainerObject());
}

__declspec(dllexport) double getDotNetContainerDoubleValue(void *ptr) {
  return *safe_cast<System::Double ^>(static_cast<DotNetContainer *>(ptr)->getContainerObject());
}

__declspec(dllexport) float getDotNetContainerSingleValue(void *ptr) {
  return *safe_cast<System::Single ^>(static_cast<DotNetContainer *>(ptr)->getContainerObject());
}

__declspec(dllexport) void freeDotNetContainer(void *ptr) {
  delete static_cast<DotNetContainer *>(ptr);
}
