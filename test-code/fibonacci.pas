

program fibonacci;

  var n, i: integer;
      fib, prev, temp: integer;

begin
  { calculates the n'th fibonacci number }
  n := 8;

  i := 1; { counter }

  fib := 1;
  prev := 1;

  while i < n do
  begin
    temp := fib;

    fib := fib + prev;
    prev := temp;

    i := i + 1
  end

end.
