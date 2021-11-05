// $Header: /usr/local/cvsrep/rdnzl-cpp/RDNZL/InvokeConstructor.cpp,v 1.11 2008/02/14 11:54:03 edi Exp $
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
#include "InvokeConstructor.h"

// invoke the constructor of the specified type and signature, the
// void pointers are pointers to DotNetContainer objects
__declspec(dllexport) void* invokeConstructor(void *type, int nargs, void *args[]) {
  cli::array<Object^> ^realArgs = gcnew cli::array<Object^>(nargs);
  for (int i = 0; i < nargs; i++) {
    realArgs[i] = static_cast<DotNetContainer *>(args[i])->getContainerObject();
  }
        
  try {
    Type ^t = safe_cast<Type ^>(static_cast<DotNetContainer *>(type)->getContainerObject());
    Object ^newInstance = Activator::CreateInstance(t, realArgs);
    return new InvocationResult(newInstance);
  } catch (TargetInvocationException ^e) {
    return new InvocationResult(e->InnerException, true);
  } catch (Exception ^e) {
    return new InvocationResult(e, true);
  }
}
