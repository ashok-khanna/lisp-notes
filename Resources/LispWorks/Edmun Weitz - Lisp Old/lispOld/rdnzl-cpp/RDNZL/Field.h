// $Header: /usr/local/cvsrep/rdnzl-cpp/RDNZL/Field.h,v 1.12 2008/02/14 07:34:31 edi Exp $
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

#include "InvocationResult.h"

public ref class Field {
 public:
  static void *getFieldValue(Object ^o, Type ^t, const __wchar_t *fieldName, BindingFlags bindingAttr);
  static void *getFieldValueDirectly(void *fieldInfo, Object ^o);
  static void *setFieldValue(Object ^o, Type ^t, const __wchar_t *fieldName, Object ^newValue, BindingFlags bindingAttr);
  static void *setFieldValueDirectly(void *fieldInfo, Object ^o, void *newValue);
 private:
  static void throwFieldNotFoundError(Type ^type, const __wchar_t *fieldName, BindingFlags bindingAttr);
};

extern "C" {
  __declspec(dllexport) void *getInstanceFieldValue(const __wchar_t *fieldName, void *target);
  __declspec(dllexport) void *getStaticFieldValue(const __wchar_t *fieldName, void *type);
  __declspec(dllexport) void *setInstanceFieldValue(const __wchar_t *fieldName, void *target, void *newValue);
  __declspec(dllexport) void *setStaticFieldValue(const __wchar_t *fieldName, void *type, void *newValue);
  __declspec(dllexport) void *getInstanceFieldValueDirectly(void *fieldInfo, void *target);
  __declspec(dllexport) void *getStaticFieldValueDirectly(void *fieldInfo);
  __declspec(dllexport) void *setInstanceFieldValueDirectly(void *fieldInfo, void *target, void *newValue);
  __declspec(dllexport) void *setStaticFieldValueDirectly(void *fieldInfo, void *newValue);
}
