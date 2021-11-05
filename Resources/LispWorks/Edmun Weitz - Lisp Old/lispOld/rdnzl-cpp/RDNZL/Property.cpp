// $Header: /usr/local/cvsrep/rdnzl-cpp/RDNZL/Property.cpp,v 1.17 2008/02/14 11:54:03 edi Exp $
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
#include "Property.h"

// provide informative message about which property wasn't found
void Property::throwPropertyNotFoundError(Type ^type, const __wchar_t *propertyName, cli::array<Type^> ^argTypes, BindingFlags bindingAttr) {
   String ^msg = String::Concat(
                                (int)(BindingFlags::Static & bindingAttr) ? "Static property not found: " : "Instance property not found: ",
                                type->FullName,
                                "->",
                                gcnew String(propertyName)
                               );
   for (int i = 0; i < argTypes->Length; i++)
     msg = String::Concat(msg, "[", argTypes[i]->FullName, "]");
   throw gcnew Exception (msg);
}

// helper function to get values of static and instance properties
void *Property::getPropertyValue(Object ^o, Type  ^t, const __wchar_t *propertyName, BindingFlags bindingAttr, int nargs, void *args[]) {
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

    // find property by name, binding attributes and index signature
    PropertyInfo ^pi = t->GetProperty(gcnew String(propertyName), bindingAttr, nullptr, nullptr, realTypes, nullptr);

    if (pi == nullptr)
      throwPropertyNotFoundError(t, propertyName, realTypes, bindingAttr);

    return new InvocationResult(pi->GetValue(o, realArgs), pi->PropertyType);
  } catch (TargetInvocationException ^e) {
    return new InvocationResult(e->InnerException, true);
  } catch (Exception ^e) {
    return new InvocationResult(e, true);
  }
}

// helper function to get values of static and instance properties
void *Property::getPropertyValueDirectly(void *propertyInfo, int nargs, void *args[], bool staticp) {
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

    PropertyInfo ^pi = safe_cast<PropertyInfo ^>(static_cast<DotNetContainer *>(propertyInfo)->getContainerObject());

    return new InvocationResult(pi->GetValue(o, realArgs), pi->PropertyType);
  } catch (TargetInvocationException ^e) {
    return new InvocationResult(e->InnerException, true);
  } catch (Exception ^e) {
    return new InvocationResult(e, true);
  }
}

// helper function to set values of static and instance properties
void *Property::setPropertyValue(Object ^o, Type ^t, const __wchar_t *propertyName, BindingFlags bindingAttr, int nargs, void *args[]) {
  try {
    // first convert index arguments - nargs is the number of
    // arguments, and args is an array of pointers to DotNetContainer
    // objects
    cli::array<Object^> ^realArgs = gcnew cli::array<Object^>(nargs - 1);
    cli::array<Type^> ^realTypes = gcnew cli::array<Type^>(nargs - 1);
    for (int i = 1; i < nargs; i++) {
      DotNetContainer *c = static_cast<DotNetContainer *>(args[i]);
      realArgs[i - 1] = c->getContainerObject();
      realTypes[i - 1] = c->getContainerType();
    }

    // find property by name, binding attributes and index signature
    PropertyInfo ^pi = t->GetProperty(gcnew String(propertyName), bindingAttr, nullptr, nullptr, realTypes, nullptr);

    if (pi == nullptr)
      throwPropertyNotFoundError(t, propertyName, realTypes, bindingAttr);

    // note that the new value is the first element of args
    pi->SetValue(o, static_cast<DotNetContainer *>(args[0])->getContainerObject(), realArgs);
    return new InvocationResult();
  } catch (TargetInvocationException ^e) {
    return new InvocationResult(e->InnerException, true);
  } catch (Exception ^e) {
    return new InvocationResult(e, true);
  }
}

// helper function to set values of static and instance properties
void *Property::setPropertyValueDirectly(void *propertyInfo, int nargs, void *args[], bool staticp) {
  try {
    // first convert index arguments - nargs is the number of
    // arguments, and args is an array of pointers to DotNetContainer
    // objects
    int offset = staticp ? 0 : 1;
    cli::array<Object^> ^realArgs = gcnew cli::array<Object^>(nargs - 1 - offset);
    for (int i = 1; i + offset < nargs; i++)
      realArgs[i - 1] = static_cast<DotNetContainer *>(args[i + offset])->getContainerObject();

    Object ^o = nullptr;
    if (!staticp)
      o = static_cast<DotNetContainer *>(args[1])->getContainerObject();

    PropertyInfo ^pi = safe_cast<PropertyInfo ^>(static_cast<DotNetContainer *>(propertyInfo)->getContainerObject());

    // note that the new value is the first element of args
    pi->SetValue(o, static_cast<DotNetContainer *>(args[0])->getContainerObject(), realArgs);
    return new InvocationResult();
  } catch (TargetInvocationException ^e) {
    return new InvocationResult(e->InnerException, true);
  } catch (Exception ^e) {
    return new InvocationResult(e, true);
  }
}

// below void pointers always point to DotNetContainer objects

__declspec(dllexport) void *getInstancePropertyValue(const __wchar_t *propertyName, void *target, int nargs, void *args[]) {
  DotNetContainer *container = static_cast<DotNetContainer *>(target);
  Type ^t = container->getContainerType();
  Object ^o = container->getContainerObject();
  return Property::getPropertyValue(o, t, propertyName, static_cast<BindingFlags>(BindingFlags::Instance | BindingFlags::Public), nargs, args);
}

__declspec(dllexport) void *getStaticPropertyValue(const __wchar_t *propertyName, void *type, int nargs, void *args[]) {
  Type ^t = safe_cast<Type ^>(static_cast<DotNetContainer *>(type)->getContainerObject());
  return Property::getPropertyValue(nullptr, t, propertyName, static_cast<BindingFlags>(BindingFlags::Static | BindingFlags::Public), nargs, args);
}

__declspec(dllexport) void *setInstancePropertyValue(const __wchar_t *propertyName, void *target, int nargs, void *args[]) {
  DotNetContainer *container = static_cast<DotNetContainer *>(target);
  Type ^t = container->getContainerType();
  Object ^o = container->getContainerObject();
  return Property::setPropertyValue(o, t, propertyName, static_cast<BindingFlags>(BindingFlags::Instance | BindingFlags::Public), nargs, args);
}

__declspec(dllexport) void *setStaticPropertyValue(const __wchar_t *propertyName, void *type, int nargs, void *args[]) {
  Type ^t = safe_cast<Type ^>(static_cast<DotNetContainer *>(type)->getContainerObject());
  return Property::setPropertyValue(nullptr, t, propertyName, static_cast<BindingFlags>(BindingFlags::Static | BindingFlags::Public), nargs, args);
}

__declspec(dllexport) void *getInstancePropertyValueDirectly(void *propertyInfo, int nargs, void *args[]) {
  return Property::getPropertyValueDirectly(propertyInfo, nargs, args, false);
}

__declspec(dllexport) void *getStaticPropertyValueDirectly(void *propertyInfo, int nargs, void *args[]) {
  return Property::getPropertyValueDirectly(propertyInfo, nargs, args, true);
}

__declspec(dllexport) void *setInstancePropertyValueDirectly(void *propertyInfo, int nargs, void *args[]) {
  return Property::setPropertyValueDirectly(propertyInfo, nargs, args, false);
}

__declspec(dllexport) void *setStaticPropertyValueDirectly(void *propertyInfo, int nargs, void *args[]) {
  return Property::setPropertyValueDirectly(propertyInfo, nargs, args, true);
}

