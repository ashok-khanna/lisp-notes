;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-WBXML; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-wbxml/tokens.lisp,v 1.10 2007/01/01 23:44:09 edi Exp $

;;; Copyright (c) 2005-2007, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-wbxml)

(defconstant +syncml-namespace+
  ;; URI is "http://www.syncml.org/docs/syncml_represent_v10_20001207.dtd" (1.0)
  ;; or "http://www.syncml.org/docs/syncml_represent_v11_20020213.dtd" (1.1)
  "syncml:syncml")

(defconstant +syncml-metinf-namespace+
  ;; URI is "http://www.syncml.org/docs/syncml_metinf_v10_20001207.dtd" (1.0)
  ;; or "http://www.syncml.org/docs/syncml_metinf_v101_20010530.dtd" (1.0.1)
  ;; or "http://www.syncml.org/docs/syncml_metinf_v11_20020215.dtd" (1.1)
  "syncml:metinf")

(defconstant +devinf-namespace+
  ;; URI is "http://www.syncml.org/docs/devinf_v10_20001207.dtd" (1.0)
  ;; or "http://www.syncml.org/docs/devinf_v101_20010530.dtd" (1.0.1)
  ;; or "http://www.syncml.org/docs/devinf_v11_20020215.dtd" (1.1)
  "syncml:devinf")

(defconstant +syncml-tag-tokens+
  `((0 . ((#x05 . ("Add" . ,+syncml-namespace+))
          (#x06 . ("Alert" . ,+syncml-namespace+))
          (#x07 . ("Archive" . ,+syncml-namespace+))
          (#x08 . ("Atomic" . ,+syncml-namespace+))
          (#x09 . ("Chal" . ,+syncml-namespace+))
          (#x0A . ("Cmd" . ,+syncml-namespace+))
          (#x0B . ("CmdID" . ,+syncml-namespace+))
          (#x0C . ("CmdRef" . ,+syncml-namespace+))
          (#x0D . ("Copy" . ,+syncml-namespace+))
          (#x0E . ("Cred" . ,+syncml-namespace+))
          (#x0F . ("Data" . ,+syncml-namespace+))
          (#x10 . ("Delete" . ,+syncml-namespace+))
          (#x11 . ("Exec" . ,+syncml-namespace+))
          (#x12 . ("Final" . ,+syncml-namespace+))
          (#x13 . ("Get" . ,+syncml-namespace+))
          (#x14 . ("Item" . ,+syncml-namespace+))
          (#x15 . ("Lang" . ,+syncml-namespace+))
          (#x16 . ("LocName" . ,+syncml-namespace+))
          (#x17 . ("LocURI" . ,+syncml-namespace+))
          (#x18 . ("Map" . ,+syncml-namespace+))
          (#x19 . ("MapItem" . ,+syncml-namespace+))
          (#x1A . ("Meta" . ,+syncml-namespace+))
          (#x1B . ("MsgID" . ,+syncml-namespace+))
          (#x1C . ("MsgRef" . ,+syncml-namespace+))
          (#x1D . ("NoResp" . ,+syncml-namespace+))
          (#x1E . ("NoResults" . ,+syncml-namespace+))
          (#x1F . ("Put" . ,+syncml-namespace+))
          (#x20 . ("Replace" . ,+syncml-namespace+))
          (#x21 . ("RespURI" . ,+syncml-namespace+))
          (#x22 . ("Results" . ,+syncml-namespace+))
          (#x23 . ("Search" . ,+syncml-namespace+))
          (#x24 . ("Sequence" . ,+syncml-namespace+))
          (#x25 . ("SessionID" . ,+syncml-namespace+))
          (#x26 . ("SftDel" . ,+syncml-namespace+))
          (#x27 . ("Source" . ,+syncml-namespace+))
          (#x28 . ("SourceRef" . ,+syncml-namespace+))
          (#x29 . ("Status" . ,+syncml-namespace+))
          (#x2A . ("Sync" . ,+syncml-namespace+))
          (#x2B . ("SyncBody" . ,+syncml-namespace+))
          (#x2C . ("SyncHdr" . ,+syncml-namespace+))
          (#x2D . ("SyncML" . ,+syncml-namespace+))
          (#x2E . ("Target" . ,+syncml-namespace+))
          (#x2F . ("TargetRef" . ,+syncml-namespace+))
          (#x31 . ("VerDTD" . ,+syncml-namespace+))
          (#x32 . ("VerProto" . ,+syncml-namespace+))
          ;; only in 1.1
          (#x33 . ("NumberOfChanges" . ,+syncml-namespace+))
          ;; only in 1.1
          (#x34 . ("MoreData" . ,+syncml-namespace+))))
    (1 . ((#x05 . ("Anchor" . ,+syncml-metinf-namespace+))
          (#x06 . ("EMI" . ,+syncml-metinf-namespace+))
          (#x07 . ("Format" . ,+syncml-metinf-namespace+))
          (#x08 . ("FreeID" . ,+syncml-metinf-namespace+))
          (#x09 . ("FreeMem" . ,+syncml-metinf-namespace+))
          (#x0A . ("Last" . ,+syncml-metinf-namespace+))
          (#x0B . ("Mark" . ,+syncml-metinf-namespace+))
          (#x0C . ("MaxMsgSize" . ,+syncml-metinf-namespace+))
          (#x15 . ("MaxObjSize" . ,+syncml-metinf-namespace+))
          (#x0D . ("Mem" . ,+syncml-metinf-namespace+))
          (#x0E . ("Metinf" . ,+syncml-metinf-namespace+))
          (#x0F . ("Next" . ,+syncml-metinf-namespace+))
          (#x10 . ("NextNonce" . ,+syncml-metinf-namespace+))
          (#x11 . ("SharedMem" . ,+syncml-metinf-namespace+))
          (#x12 . ("Size" . ,+syncml-metinf-namespace+))
          (#x13 . ("Type" . ,+syncml-metinf-namespace+))
          (#x14 . ("Version" . ,+syncml-metinf-namespace+)))))
  "The token definitions for SyncML - we simply ignore the slight
differences \(see comments) between 1.0, 1.0.1, and 1.1.")

(defconstant +devinf-tag-tokens+
  `((0 . ((#x05 . ("CTCap" . ,+devinf-namespace+))
          (#x06 . ("CTType" . ,+devinf-namespace+))
          (#x07 . ("DataStore" . ,+devinf-namespace+))
          (#x08 . ("DataType" . ,+devinf-namespace+))
          (#x09 . ("DevID" . ,+devinf-namespace+))
          (#x0A . ("DevInf" . ,+devinf-namespace+))
          (#x0B . ("DevTyp" . ,+devinf-namespace+))
          (#x0C . ("DisplayName" . ,+devinf-namespace+))
          (#x0D . ("DSMem" . ,+devinf-namespace+))
          (#x0E . ("Ext" . ,+devinf-namespace+))
          (#x0F . ("FwV" . ,+devinf-namespace+))
          (#x10 . ("HwV" . ,+devinf-namespace+))
          (#x11 . ("Man" . ,+devinf-namespace+))
          (#x12 . ("MaxGUIDSize" . ,+devinf-namespace+))
          (#x13 . ("MaxID" . ,+devinf-namespace+))
          (#x14 . ("MaxMem" . ,+devinf-namespace+))
          (#x15 . ("Mod" . ,+devinf-namespace+))
          (#x16 . ("OEM" . ,+devinf-namespace+))
          (#x17 . ("ParamName" . ,+devinf-namespace+))
          (#x18 . ("PropName" . ,+devinf-namespace+))
          (#x19 . ("Rx" . ,+devinf-namespace+))
          (#x1A . ("Rx-Pref" . ,+devinf-namespace+))
          (#x1B . ("SharedMem" . ,+devinf-namespace+))
          (#x1C . ("Size" . ,+devinf-namespace+))
          (#x1D . ("SourceRef" . ,+devinf-namespace+))
          (#x1E . ("SwV" . ,+devinf-namespace+))
          (#x1F . ("SyncCap" . ,+devinf-namespace+))
          (#x20 . ("SyncType" . ,+devinf-namespace+))
          (#x21 . ("Tx" . ,+devinf-namespace+))
          (#x22 . ("Tx-Pref" . ,+devinf-namespace+))
          (#x23 . ("ValEnum" . ,+devinf-namespace+))
          (#x24 . ("VerCT" . ,+devinf-namespace+))
          (#x25 . ("VerDTD" . ,+devinf-namespace+))
          (#x26 . ("Xnam" . ,+devinf-namespace+))
          (#x27 . ("Xval" . ,+devinf-namespace+))
          ;; the next three are only in 1.1
          (#x28 . ("UTC" . ,+devinf-namespace+))
          (#x29 . ("SupportNumberOfChanges" . ,+devinf-namespace+))
          (#x2A . ("SupportLargeObjs" . ,+devinf-namespace+))
          ;; supposedly, the following are in 1.2...
          (#x2b . ("Property" . ,+devinf-namespace+))
          (#x2c . ("PropParam" . ,+devinf-namespace+))
          (#x2d . ("MaxOccur" . ,+devinf-namespace+))
          (#x2e . ("NoTruncate" . ,+devinf-namespace+))
          (#x30 . ("Filter-Rx" . ,+devinf-namespace+))
          (#x31 . ("FilterCap" . ,+devinf-namespace+))
          (#x32 . ("FilterKeyword" . ,+devinf-namespace+))
          (#x33 . ("FieldLevel" . ,+devinf-namespace+))
          (#x34 . ("SupportHierarchicalSync" . ,+devinf-namespace+))))))

(defconstant +known-token-tables+
  `(("-//SYNCML//DTD SyncML 1.0//EN" . (,+syncml-tag-tokens+ nil))
    ("-//SYNCML//DTD SyncML 1.1//EN" . (,+syncml-tag-tokens+ nil))
    ("-//SYNCML//DTD SyncML 1.2//EN" . (,+syncml-tag-tokens+ nil))
    ("-//SYNCML//DTD DevInf 1.0//EN" . (,+devinf-tag-tokens+ nil))
    ("-//SYNCML//DTD DevInf 1.1//EN" . (,+devinf-tag-tokens+ nil))
    ("-//SYNCML//DTD DevInf 1.2//EN" . (,+devinf-tag-tokens+ nil)))
  "An alist which maps public identifiers to known token
definitions.")
