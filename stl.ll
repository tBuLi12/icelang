; ModuleID = 'my cool jit'
source_filename = "my cool jit"
target datalayout = "e-m:w-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-windows-msvc"

%null = type {}
%"std::Vector<char,>" = type { { ptr, i32, i32 } }
%"std::String" = type { %"std::Vector<char,>" }

@strLiteral = private unnamed_addr constant [4 x i8] c"bbb\00", align 1
@strLiteral.1 = private unnamed_addr constant [4 x i8] c"aaa\00", align 1
@strLiteral.2 = private unnamed_addr constant [4 x i8] c"ccc\00", align 1
@oobError = private unnamed_addr constant [139 x i8] c"   |\0A\1B[0m106|            if (\1B[31;1mthis.proto[i]\1B[0m != other.proto[i]) {\0D\0A\1B[0m   |                \1B[31;1m^^^^^^^^^^^^^\1B[0m\0A   @ :106:17\0A\00", align 1
@oobError.3 = private unnamed_addr constant [157 x i8] c"   |\0A\1B[0m106|            if (this.proto[i] != \1B[31;1mother.proto[i]\1B[0m) {\0D\0A\1B[0m   |                                 \1B[31;1m^^^^^^^^^^^^^^\1B[0m\0A   @ :106:34\0A\00", align 1

declare ptr @rtAlloc(i32)

declare %null @rtMove(i32, ptr, ptr, i32)

declare %null @rtSlice(ptr, i32, i32)

declare %null @rtFree(ptr)

declare void @rtOobError(ptr, i32, i32)

define i32 @main() {
entry:
  %0 = call ptr @rtAlloc(i32 3)
  %1 = call %null @rtMove(i32 0, ptr %0, ptr @strLiteral, i32 3)
  %2 = insertvalue %"std::Vector<char,>" undef, ptr %0, 0, 0
  %3 = insertvalue %"std::Vector<char,>" %2, i32 3, 0, 1
  %4 = insertvalue %"std::Vector<char,>" %3, i32 3, 0, 2
  %5 = insertvalue %"std::String" undef, %"std::Vector<char,>" %4, 0
  %6 = call ptr @rtAlloc(i32 3)
  %7 = call %null @rtMove(i32 0, ptr %6, ptr @strLiteral.1, i32 3)
  %8 = insertvalue %"std::Vector<char,>" undef, ptr %6, 0, 0
  %9 = insertvalue %"std::Vector<char,>" %8, i32 3, 0, 1
  %10 = insertvalue %"std::Vector<char,>" %9, i32 3, 0, 2
  %11 = insertvalue %"std::String" undef, %"std::Vector<char,>" %10, 0
  %12 = call i1 @"std::String$std::Equate<std::String,>$eq"(%"std::String" %11, %"std::String" %5)
  %13 = extractvalue %"std::String" %11, 0
  %14 = call %null @"std::Vector<char,>$std::Drop$drop"(%"std::Vector<char,>" %13)
  %15 = extractvalue %"std::String" %5, 0
  %16 = call %null @"std::Vector<char,>$std::Drop$drop"(%"std::Vector<char,>" %15)
  br i1 %12, label %rhs, label %short

rhs:                                              ; preds = %entry
  %17 = call ptr @rtAlloc(i32 3)
  %18 = call %null @rtMove(i32 0, ptr %17, ptr @strLiteral.2, i32 3)
  %19 = insertvalue %"std::Vector<char,>" undef, ptr %17, 0, 0
  %20 = insertvalue %"std::Vector<char,>" %19, i32 3, 0, 1
  %21 = insertvalue %"std::Vector<char,>" %20, i32 3, 0, 2
  %22 = insertvalue %"std::String" undef, %"std::Vector<char,>" %21, 0
  %23 = call i1 @"std::String$std::Equate<std::String,>$eq"(%"std::String" %5, %"std::String" %22)
  %24 = extractvalue %"std::String" %5, 0
  %25 = call %null @"std::Vector<char,>$std::Drop$drop"(%"std::Vector<char,>" %24)
  %26 = extractvalue %"std::String" %22, 0
  %27 = call %null @"std::Vector<char,>$std::Drop$drop"(%"std::Vector<char,>" %26)
  br label %short

short:                                            ; preds = %rhs, %entry
  %28 = phi i1 [ false, %entry ], [ %23, %rhs ]
  ret i32 0
}

define i1 @"std::String$std::Equate<std::String,>$eq"(%"std::String" %this, %"std::String" %other) {
entry:
  %0 = alloca i32, align 4
  %1 = extractvalue %"std::String" %this, 0, 0, 2
  %2 = extractvalue %"std::String" %other, 0, 0, 2
  %3 = icmp ne i32 %1, %2
  br i1 %3, label %if, label %afterif

if:                                               ; preds = %entry
  ret i1 false

afterif:                                          ; preds = %entry
  store i32 0, ptr %0, align 4
  br label %loopcondition

loopcondition:                                    ; preds = %afterif4, %afterif
  %4 = load i32, ptr %0, align 4
  %5 = extractvalue %"std::String" %this, 0, 0, 2
  %6 = icmp ne i32 %4, %5
  br i1 %6, label %loopbody, label %afterwhile

loopbody:                                         ; preds = %loopcondition
  %7 = extractvalue %"std::String" %this, 0
  %8 = load i32, ptr %0, align 4
  %9 = extractvalue %"std::Vector<char,>" %7, 0, 2
  %10 = icmp sge i32 %8, %9
  br i1 %10, label %outofbounds, label %inbounds

outofbounds:                                      ; preds = %loopbody
  call void @rtOobError(ptr @oobError, i32 %8, i32 %9)
  br label %inbounds

inbounds:                                         ; preds = %outofbounds, %loopbody
  %11 = extractvalue %"std::Vector<char,>" %7, 0, 0
  %12 = getelementptr inbounds i8, ptr %11, i32 %8
  %13 = load i8, ptr %12, align 1
  %14 = extractvalue %"std::String" %other, 0
  %15 = load i32, ptr %0, align 4
  %16 = extractvalue %"std::Vector<char,>" %14, 0, 2
  %17 = icmp sge i32 %15, %16
  br i1 %17, label %outofbounds1, label %inbounds2

outofbounds1:                                     ; preds = %inbounds
  call void @rtOobError(ptr @oobError.3, i32 %15, i32 %16)
  br label %inbounds2

inbounds2:                                        ; preds = %outofbounds1, %inbounds
  %18 = extractvalue %"std::Vector<char,>" %14, 0, 0
  %19 = getelementptr inbounds i8, ptr %18, i32 %15
  %20 = load i8, ptr %19, align 1
  %21 = icmp ne i8 %13, %20
  br i1 %21, label %if3, label %afterif4

if3:                                              ; preds = %inbounds2
  ret i1 false

afterif4:                                         ; preds = %inbounds2
  %22 = load i32, ptr %0, align 4
  %23 = add i32 %22, 1
  %24 = load i32, ptr %0, align 4
  store i32 %23, ptr %0, align 4
  br label %loopcondition

afterwhile:                                       ; preds = %loopcondition
  %25 = load i32, ptr %0, align 4
  ret i1 true
}

define %null @"std::Vector<char,>$std::Drop$drop"(%"std::Vector<char,>" %this) {
entry:
  %0 = extractvalue %"std::Vector<char,>" %this, 0, 0
  %1 = call %null @rtFree(ptr %0)
  ret %null undef
}
