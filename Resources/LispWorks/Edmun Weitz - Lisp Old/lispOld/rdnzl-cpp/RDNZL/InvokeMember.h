// $Header: /usr/local/cvsrep/rdnzl-cpp/RDNZL/InvokeMember.h,v 1.14 2008/02/14 07:34:32 edi Exp $
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

public ref class InvokeMember {
 public:
  static void* invokeMethod(Object ^o, Type ^t, const __wchar_t *methodName, int nargs, void *args [], BindingFlags bindingAttr);
  static void* invokeMethodDirectly(void *methodInfo, int nargs, void *args [], bool staticp);
 private:
  static MethodInfo^ findInterfaceMethod(cli::array<Type^> ^interfaces, const __wchar_t *methodName, cli::array<Type^> ^argTypes, BindingFlags bindingAttr);
  static MethodInfo^ findMethod(Type ^type, const __wchar_t *methodName, cli::array<Type^> ^argTypes, BindingFlags bindingAttr);
  static void throwMethodNotFoundError(Type ^type, const __wchar_t *methodName, cli::array<Type^> ^argTypes, BindingFlags bindingAttr);
};

extern "C" {
  __declspec(dllexport) void *invokeInstanceMember(const __wchar_t *methodName, void *target, int nargs, void *args[]);
  __declspec(dllexport) void* invokeInstanceMemberDirectly(void *methodInfo, int nargs, void *args[]);
  __declspec(dllexport) void *invokeStaticMember(const __wchar_t *methodName, void *type, int nargs, void *args[]);
  __declspec(dllexport) void* invokeStaticMemberDirectly(void *methodInfo, int nargs, void *args[]);
  __declspec(dllexport) void *getArrayElement(void *ptr, int index);
}
