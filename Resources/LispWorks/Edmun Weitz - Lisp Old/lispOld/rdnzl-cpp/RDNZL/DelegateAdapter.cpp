// $Header: /usr/local/cvsrep/rdnzl-cpp/RDNZL/DelegateAdapter.cpp,v 1.20 2008/02/14 11:54:02 edi Exp $
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
#include "DelegateAdapter.h"

// the destructor notifies Lisp that this instance is no longer used -
// might be called from a "foreign" thread, see docs
DelegateAdapter::~DelegateAdapter() {
  if (release != NULL) {
    release(indexIntoLisp);
  }
}

// initialize the instance with an index that points back to the
// actual Lisp closure (via a Lisp hash table)
void DelegateAdapter::init(int index) {
  indexIntoLisp = index;
}

// this does all the work (by calling the Lisp closure) after the
// wrapper (build at runtime from Lisp via reflection) has marshalled
// the delegate's arguments into an array of objects
Object^ DelegateAdapter::invoke (cli::array<Object^> ^args) {
  if (callback != NULL) {
    void *ptr = callback(indexIntoLisp, new DotNetContainer(args));
    // the Lisp closure is supposed to return a pointer to a
    // DotNetContainer which we marshall back into the underlying object
    return static_cast<DotNetContainer *>(ptr)->getContainerObject();
  } else {
    // a dummy object...
    return gcnew Object();
  }
}

// this static function is called once in the beginning to initialize
// the function pointers pointing to two Lisp callbacks
void setFunctionPointers(void *(*callback_fp)(int, void *), void (*release_fp)(int)) {
  DelegateAdapter::callback = callback_fp;
  DelegateAdapter::release = release_fp;
}
