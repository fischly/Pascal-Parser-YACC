
program factorial;

  var a: integer;
      result: integer;
      fact: integer;
      k: integer;

begin
  { calculates a! and stores it into result }
  a := 9;

  fact := 1;
  k := 2;

  while k <= a do
  begin
    fact := fact * k;
    k := k + 1
  end;

  result := fact

end.
