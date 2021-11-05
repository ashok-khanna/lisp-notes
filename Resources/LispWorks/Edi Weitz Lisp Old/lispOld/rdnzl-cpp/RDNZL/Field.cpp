// $Header: /usr/local/cvsrep/rdnzl-cpp/RDNZL/Field.cpp,v 1.16 2008/02/14 11:54:02 edi Exp $
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
#include "Field.h"

// provide informative message about which field wasn't found
void Field::throwFieldNotFoundError(Type ^type, const __wchar_t *fieldName, BindingFlags bindingAttr) {
  throw gcnew Exception(String::Concat(
                                       (int)(BindingFlags::Static & bindingAttr) ? "Static field not found: " : "Instance field not found: ",
                                       type->FullName,
                                       "->",
                                       gcnew String(fieldName)
                                       ));
}

// helper function to get values of static and instance fields
void *Field::getFieldValue(Object ^o, Type ^t, const __wchar_t *fieldName, BindingFlags bindingAttr) {
  try {
    // find field by name and binding attributes
    FieldInfo ^fi = t->GetField(gcnew String(fieldName), bindingAttr);
    
    if (fi == nullptr)
      throwFieldNotFoundError(t, fieldName, bindingAttr);

    return new InvocationResult(fi->GetValue(o), fi->FieldType);
  } catch (TargetInvocationException ^e) {
    return new InvocationResult(e->InnerException, true);
  } catch (Exception ^e) {
    return new InvocationResult(e, true);
  }
}

// helper function to get values of static and instance fields
void *Field::getFieldValueDirectly(void *fieldInfo, Object ^o) {
  try {
    FieldInfo ^fi = safe_cast<FieldInfo ^>(static_cast<DotNetContainer *>(fieldInfo)->getContainerObject());
    return new InvocationResult(fi->GetValue(o), fi->FieldType);
  } catch (TargetInvocationException ^e) {
    return new InvocationResult(e->InnerException, true);
  } catch (Exception ^e) {
    return new InvocationResult(e, true);
  }
}

// helper function to set values of static and instance fields
void *Field::setFieldValue(Object ^o, Type ^t, const __wchar_t *fieldName, Object ^newValue, BindingFlags bindingAttr) {
  try {
    // find field by name and binding attributes
    FieldInfo ^fi = t->GetField(gcnew String(fieldName), bindingAttr);

    if (fi == nullptr)
      throwFieldNotFoundError(t, fieldName, bindingAttr);

    fi->SetValue(o, newValue);
    return new InvocationResult();
  } catch (TargetInvocationException ^e) {
    return new InvocationResult(e->InnerException, true);
  } catch (Exception ^e) {
    return new InvocationResult(e, true);
  }
}

// helper function to set values of static and instance fields
void *Field::setFieldValueDirectly(void *fieldInfo, Object ^o, void *newValue) {
  try {
    FieldInfo ^fi = safe_cast<FieldInfo ^>(static_cast<DotNetContainer *>(fieldInfo)->getContainerObject());

    fi->SetValue(o, static_cast<DotNetContainer *>(newValue)->getContainerObject());
    return new InvocationResult();
  } catch (TargetInvocationException ^e) {
    return new InvocationResult(e->InnerException, true);
  } catch (Exception ^e) {
    return new InvocationResult(e, true);
  }
}

// below void pointers always point to DotNetContainer objects

__declspec(dllexport) void *getInstanceFieldValue(const __wchar_t *fieldName, void *target) {
  DotNetContainer *container = static_cast<DotNetContainer *>(target);
  Type ^t = container->getContainerType();
  Object ^o = container->getContainerObject();
  return Field::getFieldValue(o, t, fieldName, static_cast<BindingFlags>(BindingFlags::Instance | BindingFlags::Public));
}

__declspec(dllexport) void *getStaticFieldValue(const __wchar_t *fieldName, void *type) {
  Type ^t = safe_cast<Type ^>(static_cast<DotNetContainer *>(type)->getContainerObject());
  return Field::getFieldValue(nullptr, t, fieldName, static_cast<BindingFlags>(BindingFlags::Static | BindingFlags::Public));
}

__declspec(dllexport) void *setInstanceFieldValue(const __wchar_t *fieldName, void *target, void *newValue) {
  DotNetContainer *container = static_cast<DotNetContainer *>(target);
  Type ^t = container->getContainerType();
  Object ^o = container->getContainerObject();
  return Field::setFieldValue(o, t, fieldName,
                              static_cast<DotNetContainer *>(newValue)->getContainerObject(),
                              safe_cast<BindingFlags>(BindingFlags::Instance | BindingFlags::Public));
}

__declspec(dllexport) void *setStaticFieldValue(const __wchar_t *fieldName, void *type, void *newValue) {
  Type ^t = safe_cast<Type ^>(static_cast<DotNetContainer *>(type)->getContainerObject());
  return Field::setFieldValue(nullptr, t, fieldName,
                              static_cast<DotNetContainer *>(newValue)->getContainerObject(),
                              safe_cast<BindingFlags>(BindingFlags::Static | BindingFlags::Public));
}

__declspec(dllexport) void *getInstanceFieldValueDirectly(void *fieldInfo, void *target) {
  Object ^o = static_cast<DotNetContainer *>(target)->getContainerObject();
  return Field::getFieldValueDirectly(fieldInfo, o);
}

__declspec(dllexport) void *getStaticFieldValueDirectly(void *fieldInfo) {
  return Field::getFieldValueDirectly(fieldInfo, nullptr);
}

__declspec(dllexport) void *setInstanceFieldValueDirectly(void *fieldInfo, void *target, void *newValue) {
  Object ^o = static_cast<DotNetContainer *>(target)->getContainerObject();
  return Field::setFieldValueDirectly(fieldInfo, o, newValue);
}

__declspec(dllexport) void *setStaticFieldValueDirectly(void *fieldInfo, void *newValue) {
  return Field::setFieldValueDirectly(fieldInfo, nullptr, newValue);
}
