; ModuleID = 'main_opt.bc'
target triple = "mips-mipstechnologies"

@string_constant0 = internal constant [2 x i8] c"\0A\00"
@d = internal constant [3 x i8] c"%d\00"
@lf = internal constant [4 x i8] c"%lf\00"
@s = internal constant [3 x i8] c"%s\00"
@sn = internal constant [4 x i8] c"%s\0A\00"

; Function Attrs: nounwind readnone
define i32 @fact(i32 %__p__n) #0 {
  %1 = icmp eq i32 %__p__n, 1
  br i1 %1, label %6, label %2

; <label>:2                                       ; preds = %0
  %3 = add i32 %__p__n, -1
  %4 = call i32 @fact(i32 %3)
  %5 = mul i32 %4, %__p__n
  br label %6

; <label>:6                                       ; preds = %0, %2
  %result.0 = phi i32 [ %5, %2 ], [ 1, %0 ]
  ret i32 %result.0
}

; Function Attrs: nounwind
define i32 @main() #1 {
  %res.i = alloca i32, align 4
  %1 = bitcast i32* %res.i to i8*
  call void @llvm.lifetime.start(i64 4, i8* %1)
  %2 = call i32 (i8*, ...) @scanf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @d, i64 0, i64 0), i32* nonnull %res.i) #1
  %3 = load i32, i32* %res.i, align 4
  call void @llvm.lifetime.end(i64 4, i8* %1)
  %4 = call i32 @fact(i32 %3)
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @d, i64 0, i64 0), i32 %4) #1
  %6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @s, i64 0, i64 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @string_constant0, i64 0, i64 0)) #1
  ret i32 0
}

; Function Attrs: nounwind
define void @printInt(i32 %x) #1 {
  %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @d, i64 0, i64 0), i32 %x)
  ret void
}

; Function Attrs: nounwind
declare i32 @printf(i8* nocapture readonly, ...) #1

; Function Attrs: nounwind
define void @printDouble(double %x) #1 {
  %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @lf, i64 0, i64 0), double %x)
  ret void
}

; Function Attrs: nounwind
define void @printString(i8* %s) #1 {
  %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @s, i64 0, i64 0), i8* %s)
  ret void
}

; Function Attrs: nounwind
define i32 @readInt() #1 {
  %res = alloca i32, align 4
  %1 = call i32 (i8*, ...) @scanf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @d, i64 0, i64 0), i32* nonnull %res)
  %2 = load i32, i32* %res, align 4
  ret i32 %2
}

; Function Attrs: nounwind
declare i32 @scanf(i8* nocapture readonly, ...) #1

; Function Attrs: nounwind
define double @readDouble() #1 {
  %res = alloca double, align 8
  %1 = call i32 (i8*, ...) @scanf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @lf, i64 0, i64 0), double* nonnull %res)
  %2 = load double, double* %res, align 8
  ret double %2
}

; Function Attrs: nounwind
define i8* @readLine() #1 {
  %res = alloca i8*, align 8
  %1 = call i32 (i8*, ...) @scanf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @sn, i64 0, i64 0), i8** nonnull %res)
  %2 = load i8*, i8** %res, align 8
  ret i8* %2
}

; Function Attrs: argmemonly nounwind
declare void @llvm.lifetime.start(i64, i8* nocapture) #2

; Function Attrs: argmemonly nounwind
declare void @llvm.lifetime.end(i64, i8* nocapture) #2

attributes #0 = { nounwind readnone }
attributes #1 = { nounwind }
attributes #2 = { argmemonly nounwind }
