// $Header: /usr/local/cvsrep/rdnzl-cpp/RDNZL/InvokeMember.cpp,v 1.22 2008/02/19 18:40:54 edi Exp $
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
#include "InvokeMember.h"

// helper function for findMethod - recursively descends into
// all interfaces
MethodInfo^ InvokeMember::findInterfaceMethod(cli::array<Type^> ^interfaces, const __wchar_t *methodName, cli::array<Type^> ^argTypes, BindingFlags bindingAttr) {
  MethodInfo ^mi = nullptr;
   
  for (int i = 0; i < interfaces->Length && !mi; i++) {
    mi = interfaces[i]->GetMethod(gcnew String(methodName), bindingAttr, nullptr, argTypes, nullptr);    
    if (mi == nullptr)
      mi = findInterfaceMethod(interfaces[i]->GetInterfaces(), methodName, argTypes, bindingAttr);
  }
  return mi;
}

// find method named methodName with signature as described by
// argTypes and binding attributes bindingAttr
MethodInfo^ InvokeMember::findMethod(Type ^type, const __wchar_t *methodName, cli::array<Type^> ^argTypes, BindingFlags bindingAttr) {
  // first try it directly
  MethodInfo ^mi = type->GetMethod(gcnew String(methodName), bindingAttr, nullptr, argTypes, nullptr);
  
  // then try for all interfaces
  if (mi == nullptr && type->IsInterface) {
    mi = findInterfaceMethod(type->GetInterfaces(), methodName, argTypes, bindingAttr);
    
    // finally (still not found) check if its a method inherited from
    // System.Object
    if (mi == nullptr)
      mi = findMethod(Object::typeid, methodName, argTypes, bindingAttr);
  }
  return mi;
}

// provide informative message about which method wasn't found
void InvokeMember::throwMethodNotFoundError(Type ^type, const __wchar_t *methodName, cli::array<Type^> ^argTypes, BindingFlags bindingAttr) {
   String ^msg = String::Concat(
                                (int)(BindingFlags::Static & bindingAttr) ? "Static method not found: " : "Instance method not found: ",
                                type->FullName,
                                "::",
                                gcnew String(methodName)
                               );
   msg = String::Concat(msg, "(");
   for (int i = 0; i < argTypes->Length; i++) {
     if (i)
       msg = String::Concat(msg, ",");
     msg = String::Concat(msg, argTypes[i]->FullName);
   }
   msg = String::Concat(msg, ")");
   throw gcnew Exception (msg);
}

// helper function to invoke static as well as instance methods
void* InvokeMember::invokeMethod(Object ^o, Type ^t, const __wchar_t *methodName, int nargs, void *args[], BindingFlags bindingAttr) {
  try {
    // first convert index arguments - nargs is the number of
    // arguments, and args is an array of pointers to DotNetContainer
    // objects
    cli::array<Object^> ^realArgs = gcnew cli::array<Object^>(nargs);
    cli::array<Type^> ^realTypes = gcnew cli::array<Type^>(nargs);
    for (int i = 0; i < nargs; i++) {
      DotNetContainer *c = static_cast<DotNetContainer *>(args[i]);
      realArgs[i] = c->getContainerObject();
      realTypes[i] = c->getContainerType();
    }

    MethodInfo ^mi = findMethod(t, methodName, realTypes, bindingAttr);
    if (mi == nullptr)
      throwMethodNotFoundError(t, methodName, realTypes, bindingAttr);

    Object ^newInstance = mi->Invoke(o, realArgs);
    
    // for arguments that were pass-by-reference update the object
    // slots of the corresponding containers
    cli::array<ParameterInfo^> ^pi = mi->GetParameters();
    for (int i = 0; i < nargs; i++)
      if (pi[i]->ParameterType->IsByRef) {
        DotNetContainer *c = static_cast<DotNetContainer *>(args[i]);
        c->setContainerObject(realArgs[i]);
      }

    if (mi->ReturnType->Equals(Void::typeid)) {
      // return a "void" InvocationResult object if the method doesn't
      // return anything
      return new InvocationResult();
    } else {
      return new InvocationResult(newInstance, mi->ReturnType);
    }
  } catch (TargetInvocationException ^e) {
    return new InvocationResult(e->InnerException, true);
  } catch (Exception ^e) {
    return new InvocationResult(e, true);
  }
}

// helper function to invoke static as well as instance methods
void* InvokeMember::invokeMethodDirectly(void *methodInfo, int nargs, void *args[], bool staticp) {
  try {
    // first convert index arguments - nargs is the number of
    // arguments, and args is an array of pointers to DotNetContainer
    // objects
    int offset = staticp ? 0 : 1;
    cli::array<Object^> ^realArgs = gcnew cli::array<Object^>(nargs - offset);
    for (int i = 0; i + offset < nargs; i++)
      realArgs[i] = static_cast<DotNetContainer *>(args[i + offset])->getContainerObject();

    Object ^o = nullptr;
    if (!staticp)
      o = static_cast<DotNetContainer *>(args[0])->getContainerObject();

    MethodInfo ^mi = safe_cast<MethodInfo ^>(static_cast<DotNetContainer *>(methodInfo)->getContainerObject());

    Object ^newInstance = mi->Invoke(o, realArgs);
    
    // for arguments that were pass-by-reference update the object
    // slots of the corresponding containers
    cli::array<ParameterInfo^> ^pi = mi->GetParameters();
    for (int i = 0; i + offset < nargs; i++)
      if (pi[i]->ParameterType->IsByRef) {
        DotNetContainer *c = static_cast<DotNetContainer *>(args[i + offset]);
        c->setContainerObject(realArgs[i]);
      }

    if (mi->ReturnType->Equals(Void::typeid)) {
      // return a "void" InvocationResult object if the method doesn't
      // return anything
      return new InvocationResult();
    } else {
      return new InvocationResult(newInstance, mi->ReturnType);
    }
  } catch (TargetInvocationException ^e) {
    return new InvocationResult(e->InnerException, true);
  } catch (Exception ^e) {
    return new InvocationResult(e, true);
  }
}

// below void pointers always point to DotNetContainer objects

__declspec(dllexport) void* invokeInstanceMember(const __wchar_t *methodName, void *target, int nargs, void *args[]) {
  DotNetContainer *container = static_cast<DotNetContainer *>(target);
  Type ^t = container->getContainerType();
  Object ^o = container->getContainerObject();
  return InvokeMember::invokeMethod(o, t, methodName, nargs, args, static_cast<BindingFlags>(BindingFlags::Instance | BindingFlags::Public));
}

__declspec(dllexport) void* invokeInstanceMemberDirectly(void *methodInfo, int nargs, void *args[]) {
  return InvokeMember::invokeMethodDirectly(methodInfo, nargs, args, false);
}

__declspec(dllexport) void* invokeStaticMember(const __wchar_t *methodName, void *type, int nargs, void *args[]) {
  Type ^t = safe_cast<Type ^>(static_cast<DotNetContainer *>(type)->getContainerObject());
  return InvokeMember::invokeMethod(nullptr, t, methodName, nargs, args, static_cast<BindingFlags>(BindingFlags::Static | BindingFlags::Public));
}

__declspec(dllexport) void* invokeStaticMemberDirectly(void *methodInfo, int nargs, void *args[]) {
  return InvokeMember::invokeMethodDirectly(methodInfo, nargs, args, true);
}

// not directly related, but I didn't know where to put it... :)
// "shortcut" function used by DO-RDNZL-ARRAY (see Lisp code) -
// returns an array element for arrays of rank 1
__declspec(dllexport) void *getArrayElement(void *ptr, int index) {
  try {
    Array ^array = safe_cast<Array ^>(static_cast<DotNetContainer *>(ptr)->getContainerObject());
    Type ^elementType = array->GetType()->GetElementType();

    return new InvocationResult(array->GetValue(index), elementType);
  } catch (Exception ^e) {
    return new InvocationResult(e, true);
  }
}
