// $Header: /usr/local/cvsrep/rdnzl-cpp/RDNZL/DelegateAdapterBuilder.cpp,v 1.11 2008/02/14 11:54:02 edi Exp $
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
#include "DelegateAdapterBuilder.h"

// always returns the same module builder which is cached
ModuleBuilder ^DelegateAdapterBuilder::getModuleBuilder() {
  if (moduleBuilder != nullptr)
    return moduleBuilder;

  // if not cached already create it once
  AssemblyName ^assemblyName = gcnew AssemblyName();
  assemblyName->Name = privateAssemblyName;
  assemblyName->Version = gcnew Version("1.0.0.0");

  AppDomain ^appDomain = Thread::GetDomain();
  AssemblyBuilder ^assemblyBuilder = appDomain->DefineDynamicAssembly(
                                                                      assemblyName,
                                                                      AssemblyBuilderAccess::Run
                                                                     );

  moduleBuilder = assemblyBuilder->DefineDynamicModule(privateAssemblyName);

  return moduleBuilder;
}

// creates a constructor for our new type
void DelegateAdapterBuilder::generateConstructor(ILGenerator ^ilGenerator) {
  // the simplest one possible - no arguments, just call the
  // constructor of the base type
  ilGenerator->Emit(OpCodes::Ldarg_0);
  ilGenerator->Emit(OpCodes::Call, DelegateAdapter::typeid->GetConstructor(gcnew cli::array<Type^>(0)));
  ilGenerator->Emit(OpCodes::Ret);
}

// creates the "InvokeClosure" method which calls DelegateAdapter's
// "invoke"
void DelegateAdapterBuilder::generateInvokeMethod(ILGenerator ^ilGenerator, Type ^returnType, cli::array<Type^> ^argTypes) {
  int nargs = argTypes->Length;

  // create a System.Object array of the same length as argTypes
  ilGenerator->DeclareLocal(cli::array<Object^>::typeid /*__typeof(Object*[])*/);
  ilGenerator->Emit(OpCodes::Ldc_I4, nargs);  
  ilGenerator->Emit(OpCodes::Newarr, System::Object::typeid);
  ilGenerator->Emit(OpCodes::Stloc_0);

  // store method arguments in this array
  for (int i = 0; i < nargs; i++) {
    ilGenerator->Emit(OpCodes::Ldloc_0);
    ilGenerator->Emit(OpCodes::Ldc_I4, i);
    ilGenerator->Emit(OpCodes::Ldarg, i + 1);

    Type ^argType = argTypes[i];
    if (argType->IsValueType)
      ilGenerator->Emit(OpCodes::Box, argType);
    ilGenerator->Emit(OpCodes::Stelem_Ref);
  }

  // call "invoke" with this array
  ilGenerator->Emit(OpCodes::Ldarg_0);
  ilGenerator->Emit(OpCodes::Ldloc_0);
  ilGenerator->Emit(OpCodes::Call,DelegateAdapter::typeid->GetMethod("invoke"));

  // handle return value of "invoke"
  if (returnType->Equals(Void::typeid))
    ilGenerator->Emit(OpCodes::Pop);
  else if (returnType->IsValueType)
    ilGenerator->Emit(OpCodes::Unbox_Any, returnType);

  ilGenerator->Emit(OpCodes::Ret);
}

// build a new type which derives from DelegateAdapter and is
// responsible for Lisp callbacks with a signature as described by
// returnType and argTypes
Type ^DelegateAdapterBuilder::buildDelegateType (String ^typeName, Type ^returnType, cli::array<Type^> ^argTypes) {
  TypeBuilder ^typeBuilder = getModuleBuilder()->DefineType(
                                                            String::Concat(privateAssemblyName, ".", typeName),
                                                            TypeAttributes::Public,
                                                            DelegateAdapter::typeid
                                                           );

  generateConstructor(typeBuilder->DefineConstructor(
                                                     MethodAttributes::Public,
                                                     CallingConventions::Standard,
                                                     gcnew cli::array<Type^>(0)
                                                    )->GetILGenerator());
  generateInvokeMethod(
                       typeBuilder->DefineMethod(
                                                 "InvokeClosure",
                                                 MethodAttributes::Public,
                                                 returnType,
                                                 argTypes
                                                )->GetILGenerator(),
                       returnType,
                       argTypes
                      );

  return typeBuilder->CreateType();
}

// the C interface
__declspec(dllexport) void *buildDelegateType(const __wchar_t *typeName, void *returnType, void *argTypes) {
  Type ^newType = DelegateAdapterBuilder::buildDelegateType(
                                                            gcnew String(typeName),
                                                            safe_cast<Type ^>(static_cast<DotNetContainer *>(returnType)->getContainerObject()),
                                                            safe_cast<cli::array<Type^> ^>(static_cast<DotNetContainer *>(argTypes)->getContainerObject())
                                                           );

  return new DotNetContainer(newType);
}
