%Pair = type { i64, i64 }

define i64 @include_value(%Pair* %p) {
entry:
  %flag_ptr = getelementptr %Pair, %Pair* %p, i32 0, i32 1
  %flag = load i64, i64* %flag_ptr
  %ok = icmp eq i64 %flag, 1
  br i1 %ok, label %yes, label %no

yes:
  %val_ptr = getelementptr %Pair, %Pair* %p, i32 0, i32 0
  %val = load i64, i64* %val_ptr
  ret i64 %val

no:
  ret i64 0
}

define i64 @main(i64 %argc, i8** %argv) {
entry:
  %arr = alloca [4 x %Pair]
  %sum = alloca i64
  %i = alloca i64

  store i64 0, i64* %sum
  store i64 0, i64* %i

  ; arr[0] = {10, 1}
  %p0 = getelementptr [4 x %Pair], [4 x %Pair]* %arr, i32 0, i32 0
  %p0v = getelementptr %Pair, %Pair* %p0, i32 0, i32 0
  %p0f = getelementptr %Pair, %Pair* %p0, i32 0, i32 1
  store i64 10, i64* %p0v
  store i64 1, i64* %p0f

  ; arr[1] = {20, 0}
  %p1 = getelementptr [4 x %Pair], [4 x %Pair]* %arr, i32 0, i32 1
  %p1v = getelementptr %Pair, %Pair* %p1, i32 0, i32 0
  %p1f = getelementptr %Pair, %Pair* %p1, i32 0, i32 1
  store i64 20, i64* %p1v
  store i64 0, i64* %p1f

  ; arr[2] = {30, 1}
  %p2 = getelementptr [4 x %Pair], [4 x %Pair]* %arr, i32 0, i32 2
  %p2v = getelementptr %Pair, %Pair* %p2, i32 0, i32 0
  %p2f = getelementptr %Pair, %Pair* %p2, i32 0, i32 1
  store i64 30, i64* %p2v
  store i64 1, i64* %p2f

  ; arr[3] = {40, 0}
  %p3 = getelementptr [4 x %Pair], [4 x %Pair]* %arr, i32 0, i32 3
  %p3v = getelementptr %Pair, %Pair* %p3, i32 0, i32 0
  %p3f = getelementptr %Pair, %Pair* %p3, i32 0, i32 1
  store i64 40, i64* %p3v
  store i64 0, i64* %p3f

  br label %loop

loop:
  %iv = load i64, i64* %i
  %cond = icmp slt i64 %iv, 4
  br i1 %cond, label %body, label %done

body:
  %elem = getelementptr [4 x %Pair], [4 x %Pair]* %arr, i32 0, i64 %iv
  %part = call i64 @include_value(%Pair* %elem)

  %oldsum = load i64, i64* %sum
  %newsum = add i64 %oldsum, %part
  store i64 %newsum, i64* %sum

  %nexti = add i64 %iv, 1
  store i64 %nexti, i64* %i
  br label %loop

done:
  %ans = load i64, i64* %sum
  ret i64 %ans
}