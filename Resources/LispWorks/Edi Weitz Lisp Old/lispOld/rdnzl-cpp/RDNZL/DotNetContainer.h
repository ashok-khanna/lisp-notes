// $Header: /usr/local/cvsrep/rdnzl-cpp/RDNZL/DotNetContainer.h,v 1.21 2008/02/14 07:34:31 edi Exp $
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

#pragma once

#include "DotNetReference.h"
#include "DelegateAdapter.h"

class DotNetContainer {
 public:
  DotNetContainer(Object ^o, Type ^t);
  DotNetContainer(Object ^o);
  DotNetContainer(bool b);
  DotNetContainer(__int32 n);
  DotNetContainer(__int64 n);
  DotNetContainer(float f);
  DotNetContainer(double d);
  DotNetContainer(__wchar_t c);
  DotNetContainer(const __wchar_t *s);
  ~DotNetContainer();

  bool isNull();
  Object ^getContainerObject();
  Type ^getContainerType();
  void setContainerObject(Object ^o);
  void setContainerType(Type ^t);
  void refContainerType();
  void unrefContainerType();
 private:
  // the actual object
  DotNetReference* object;
  // the type of this object as seen from RDNZL
  DotNetReference* type;

  void init(Object ^o, Type ^t);
  void init(Object ^o);
};

extern "C" {
  __declspec(dllexport) void *makeTypeFromName(const __wchar_t *type);
  __declspec(dllexport) void *makeTypedNullDotNetContainer(const __wchar_t *type);
  __declspec(dllexport) void *makeDotNetContainerFromBoolean(bool b);
  __declspec(dllexport) void *makeDotNetContainerFromInt(int n);
  __declspec(dllexport) void *makeDotNetContainerFromLong(const __wchar_t *s);
  __declspec(dllexport) void *makeDotNetContainerFromFloat(float d);
  __declspec(dllexport) void *makeDotNetContainerFromDouble(double d);
  __declspec(dllexport) void *makeDotNetContainerFromChar(__wchar_t c);
  __declspec(dllexport) void *makeDotNetContainerFromString(const __wchar_t *s);
  __declspec(dllexport) bool DotNetContainerIsNull(void *ptr);
  __declspec(dllexport) int getDotNetContainerTypeStringLength(void *ptr);
  __declspec(dllexport) void getDotNetContainerTypeAsString(void *ptr, __wchar_t *s);
  __declspec(dllexport) int getDotNetContainerObjectStringLength(void *ptr);
  __declspec(dllexport) void getDotNetContainerObjectAsString(void *ptr, __wchar_t *s);
  __declspec(dllexport) int getDotNetContainerIntValue(void *ptr);
  __declspec(dllexport) __wchar_t getDotNetContainerCharValue(void *ptr);
  __declspec(dllexport) bool getDotNetContainerBooleanValue(void *ptr);
  __declspec(dllexport) double getDotNetContainerDoubleValue(void *ptr);
  __declspec(dllexport) float getDotNetContainerSingleValue(void *ptr);
  __declspec(dllexport) void refDotNetContainerType(void *ptr);
  __declspec(dllexport) void unrefDotNetContainerType(void *ptr);
  __declspec(dllexport) void freeDotNetContainer(void *ptr);
  __declspec(dllexport) void *copyDotNetContainer(void *ptr);
 // function definition is in InvocationResult.cpp
  __declspec(dllexport) void *setDotNetContainerTypeFromString(const __wchar_t *type, void *ptr);
  __declspec(dllexport) void *setDotNetContainerTypeFromContainer(void *type, void *ptr);
}
