// $Header: /usr/local/cvsrep/rdnzl-cpp/RDNZL/InvocationResult.h,v 1.10 2008/02/14 07:34:31 edi Exp $
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

#include "DotNetContainer.h"

class InvocationResult {
 public:
  InvocationResult();
  InvocationResult(Object ^o, bool excp = false);
  InvocationResult(Object ^o, Type ^t, bool excp = false);

  bool isVoid();
  bool isException();
  DotNetContainer *getResult();
 private:
  // whether the object represents the "result" of a method which
  // doesn't return anything
  bool isVoid_;
  // whether an exception occured
  bool isException_;
  // the actual result, if there was one
  DotNetContainer *result;
};

extern "C" {
  __declspec(dllexport) bool InvocationResultIsVoid(void *ptr);
  __declspec(dllexport) bool InvocationResultIsException(void *ptr);
  __declspec(dllexport) void *getDotNetContainerFromInvocationResult(void *ptr);
  __declspec(dllexport) void freeInvocationResult(void *ptr);
}
