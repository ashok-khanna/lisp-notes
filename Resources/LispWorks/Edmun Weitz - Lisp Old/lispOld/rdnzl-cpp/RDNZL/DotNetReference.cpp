// $Header: /usr/local/cvsrep/rdnzl-cpp/RDNZL/DotNetReference.cpp,v 1.9 2008/02/14 07:34:31 edi Exp $
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
#include "DotNetReference.h"

// if constructor is called with no arguments act as NULL object
DotNetReference::DotNetReference() : ptr(0) {}

// normal constructor
DotNetReference::DotNetReference(Object ^o) {
  // acquire pointer to object so it can't be reclaimed by the .NET
  // garbage collector until explicitely freed
  ptr = ((IntPtr) GCHandle::Alloc(o)).ToPointer();
}

// destructor
DotNetReference::~DotNetReference() {
  if (ptr) {
    // give up pointer so garbage collector regains control
    static_cast<GCHandle>((IntPtr)ptr).Free();
  }
}

Object ^DotNetReference::getObject() {
  // return the object the instance was initialized with
  return ptr ? safe_cast<Object^>(static_cast<GCHandle>((IntPtr)ptr).Target) : nullptr;
}

