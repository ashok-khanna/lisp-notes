// $Header: /usr/local/cvsrep/rdnzl-cpp/RDNZL/InvocationResult.cpp,v 1.14 2008/02/14 07:43:41 edi Exp $
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
#include "InvocationResult.h"

// constructor for "void" results
InvocationResult::InvocationResult() : isVoid_(true), isException_(false), result(0) {}

// standard constructor
InvocationResult::InvocationResult(Object ^o, bool excp) :
  isVoid_(false), isException_(excp), result(new DotNetContainer(o)) {}

// constructor for results which are explicitely typed
InvocationResult::InvocationResult(Object ^o, Type ^t, bool excp) :
  isVoid_(false), isException_(excp), result(new DotNetContainer(o, t)) {}

bool InvocationResult::isVoid() {
  return isVoid_;
}

bool InvocationResult::isException() {
  return isException_;
}

DotNetContainer *InvocationResult::getResult() {
  return result;
}

__declspec(dllexport) bool InvocationResultIsVoid(void *ptr) {
  return static_cast<InvocationResult *>(ptr)->isVoid();
}

__declspec(dllexport) bool InvocationResultIsException(void *ptr) {
  return static_cast<InvocationResult *>(ptr)->isException();
}

__declspec(dllexport) void *getDotNetContainerFromInvocationResult(void *ptr) {
  // this returns a NULL pointer if the result is void
  return static_cast<InvocationResult *>(ptr)->getResult();
}

__declspec(dllexport) void freeInvocationResult(void *ptr) {
  delete static_cast<InvocationResult *>(ptr);
}

// helper function for setDotNetContainerTypeFromString and setDotNetContainerTypeFromContainer.
void *setDotNetContainerType(Type ^newType, void *ptr) {
  try {
    DotNetContainer *container = static_cast<DotNetContainer *>(ptr);
    Type ^oldType = container->getContainerType();
    
    if (oldType->IsAssignableFrom(newType)) {
      container->setContainerType(newType);
    } else {
      Object ^newObject = Convert::ChangeType(container->getContainerObject(), newType);
      container->setContainerObject(newObject);
      container->setContainerType(newType);
    }
    // return void result
    return new InvocationResult();
  } catch (Exception ^e) {
    return new InvocationResult(e, true);
  }
}

// this should actually be in DotNetContainer.cpp but couldn't be defined there

// if possible, change the type of the DotNetContainer given the string name of a type that will be found in the Load context.
__declspec(dllexport) void *setDotNetContainerTypeFromString(const __wchar_t *type, void *ptr) {
  try {
    // throw an exception if something happens
    Type ^newType = Type::GetType(gcnew String(type), true);
    return setDotNetContainerType(newType, ptr);
  } catch (Exception ^e) {
    return new InvocationResult(e, true);
  }
}
    
// change the type of the DotNetContainer, if possible, given a Type object.
__declspec(dllexport) void *setDotNetContainerTypeFromContainer(void *type, void *ptr) {
  try {
    // throw an exception if something happens
    Type ^newType = safe_cast<Type ^>(static_cast<DotNetContainer *>(type)->getContainerObject());
    return setDotNetContainerType(newType, ptr);
  } catch (Exception ^e) {
    return new InvocationResult(e, true);
  }
}