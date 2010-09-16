define i32 @main(i32 %argc, i8** nocapture %argv) nounwind {
  tail call i32 @puts(i8* getelementptr([13 x i8]* @.LC0, i64 0, i64 0)) nounwind
  ret i32 0
}

@.LC0 = internal constant[13 x i8] c"hello world\0A\00"
declare i32 @puts(i8*)
