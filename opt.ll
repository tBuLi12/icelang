; ModuleID = '.\stl.ll'
source_filename = "my cool jit"
target datalayout = "e-m:w-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-windows-msvc"

%null = type {}
%"std::String" = type { %"std::Vector<char,>" }
%"std::Vector<char,>" = type { { ptr, i32, i32 } }

@strLiteral = private unnamed_addr constant [4 x i8] c"bbb\00", align 1
@strLiteral.1 = private unnamed_addr constant [4 x i8] c"aaa\00", align 1
@strLiteral.2 = private unnamed_addr constant [4 x i8] c"ccc\00", align 1
@oobError = private unnamed_addr constant [139 x i8] c"   |\0A\1B[0m106|            if (\1B[31;1mthis.proto[i]\1B[0m != other.proto[i]) {\0D\0A\1B[0m   |                \1B[31;1m^^^^^^^^^^^^^\1B[0m\0A   @ :106:17\0A\00", align 1
@oobError.3 = private unnamed_addr constant [157 x i8] c"   |\0A\1B[0m106|            if (this.proto[i] != \1B[31;1mother.proto[i]\1B[0m) {\0D\0A\1B[0m   |                                 \1B[31;1m^^^^^^^^^^^^^^\1B[0m\0A   @ :106:34\0A\00", align 1

declare ptr @rtAlloc(i32) local_unnamed_addr

declare %null @rtMove(i32, ptr, ptr, i32) local_unnamed_addr

declare %null @rtFree(ptr) local_unnamed_addr

declare void @rtOobError(ptr, i32, i32) local_unnamed_addr

define i32 @main() local_unnamed_addr {
inbounds2.i:
  %0 = tail call ptr @rtAlloc(i32 3)
  %1 = tail call %null @rtMove(i32 0, ptr %0, ptr nonnull @strLiteral, i32 3)
  %2 = tail call ptr @rtAlloc(i32 3)
  %3 = tail call %null @rtMove(i32 0, ptr %2, ptr nonnull @strLiteral.1, i32 3)
  %.pre = load i8, ptr %0, align 1
  %4 = load i8, ptr %2, align 1
  %.not12.i = icmp eq i8 %4, %.pre
  br i1 %.not12.i, label %inbounds2.i.1, label %short

inbounds2.i.1:                                    ; preds = %inbounds2.i
  %.phi.trans.insert = getelementptr inbounds i8, ptr %0, i64 1
  %.pre19 = load i8, ptr %.phi.trans.insert, align 1
  %5 = getelementptr inbounds i8, ptr %2, i64 1
  %6 = load i8, ptr %5, align 1
  %.not12.i.1 = icmp eq i8 %6, %.pre19
  br i1 %.not12.i.1, label %inbounds2.i.2, label %short

inbounds2.i.2:                                    ; preds = %inbounds2.i.1
  %.phi.trans.insert20 = getelementptr inbounds i8, ptr %0, i64 2
  %.pre21 = load i8, ptr %.phi.trans.insert20, align 1
  %7 = getelementptr inbounds i8, ptr %2, i64 2
  %8 = load i8, ptr %7, align 1
  %.not12.i.2 = icmp eq i8 %8, %.pre21
  br i1 %.not12.i.2, label %loopcondition.i.2, label %short

loopcondition.i.2:                                ; preds = %inbounds2.i.2
  %9 = tail call %null @rtFree(ptr nonnull %2)
  %10 = tail call %null @rtFree(ptr nonnull %0)
  %11 = tail call ptr @rtAlloc(i32 3)
  %12 = tail call %null @rtMove(i32 0, ptr %11, ptr nonnull @strLiteral.2, i32 3)
  br label %short

short:                                            ; preds = %loopcondition.i.2, %inbounds2.i, %inbounds2.i.1, %inbounds2.i.2
  %.sink24 = phi ptr [ %2, %inbounds2.i.2 ], [ %2, %inbounds2.i.1 ], [ %2, %inbounds2.i ], [ %0, %loopcondition.i.2 ]
  %.sink = phi ptr [ %0, %inbounds2.i.2 ], [ %0, %inbounds2.i.1 ], [ %0, %inbounds2.i ], [ %11, %loopcondition.i.2 ]
  %13 = tail call %null @rtFree(ptr nonnull %.sink24)
  %14 = tail call %null @rtFree(ptr nonnull %.sink)
  ret i32 0
}

define i1 @"std::String$std::Equate<std::String,>$eq"(%"std::String" %this, %"std::String" %other) local_unnamed_addr {
entry:
  %0 = extractvalue %"std::String" %this, 0, 0, 2
  %1 = extractvalue %"std::String" %other, 0, 0, 2
  %.not = icmp eq i32 %0, %1
  br i1 %.not, label %loopcondition.preheader, label %common.ret

loopcondition.preheader:                          ; preds = %entry
  %.not913 = icmp eq i32 %0, 0
  br i1 %.not913, label %common.ret, label %loopbody.lr.ph

loopbody.lr.ph:                                   ; preds = %loopcondition.preheader
  %2 = extractvalue %"std::String" %this, 0
  %3 = extractvalue %"std::Vector<char,>" %2, 0, 2
  %4 = extractvalue %"std::Vector<char,>" %2, 0, 0
  %5 = extractvalue %"std::String" %other, 0
  %6 = extractvalue %"std::Vector<char,>" %5, 0, 2
  %7 = extractvalue %"std::Vector<char,>" %5, 0, 0
  br label %loopbody

common.ret:                                       ; preds = %inbounds2, %loopcondition.preheader, %entry
  %common.ret.op = phi i1 [ false, %entry ], [ true, %loopcondition.preheader ], [ %.not12, %inbounds2 ]
  ret i1 %common.ret.op

loopbody:                                         ; preds = %inbounds2, %loopbody.lr.ph
  %.014 = phi i32 [ 0, %loopbody.lr.ph ], [ %13, %inbounds2 ]
  %.not10 = icmp slt i32 %.014, %3
  br i1 %.not10, label %inbounds, label %outofbounds

outofbounds:                                      ; preds = %loopbody
  tail call void @rtOobError(ptr nonnull @oobError, i32 %.014, i32 %3)
  br label %inbounds

inbounds:                                         ; preds = %outofbounds, %loopbody
  %8 = sext i32 %.014 to i64
  %9 = getelementptr inbounds i8, ptr %4, i64 %8
  %10 = load i8, ptr %9, align 1
  %.not11 = icmp slt i32 %.014, %6
  br i1 %.not11, label %inbounds2, label %outofbounds1

outofbounds1:                                     ; preds = %inbounds
  tail call void @rtOobError(ptr nonnull @oobError.3, i32 %.014, i32 %6)
  br label %inbounds2

inbounds2:                                        ; preds = %outofbounds1, %inbounds
  %11 = getelementptr inbounds i8, ptr %7, i64 %8
  %12 = load i8, ptr %11, align 1
  %.not12 = icmp eq i8 %10, %12
  %13 = add nuw i32 %.014, 1
  %.not9 = icmp ne i32 %13, %0
  %or.cond.not = select i1 %.not12, i1 %.not9, i1 false
  br i1 %or.cond.not, label %loopbody, label %common.ret
}

define %null @"std::Vector<char,>$std::Drop$drop"(%"std::Vector<char,>" %this) local_unnamed_addr {
entry:
  %0 = extractvalue %"std::Vector<char,>" %this, 0, 0
  %1 = tail call %null @rtFree(ptr %0)
  ret %null undef
}
