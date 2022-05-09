
{ This program implements some famous algorithms }


program minipas;

  a: integer;
      b, i: integer;
      x: array [1..100] of real;


  { Calculate greatest common divisor of a and b }

  function gcd (a, b: integer) : integer;
  begin
    while a*b <> 0 do
    begin
      if a > b then
        a := a-b
      else b := b-a
    end;
    if a = 0 then
      gcd := b
    else gcd := a
  end;

  
  { Calculate a factorial (a!) }

  function factorial (a: integer) : integer;
  var k, fact: integer;
  begin
    fact := 1;
    k := 2;
    while k <= a do
    begin
      fact := fact*k;
      k := k+1
    end;
    factorial := fact
  end;


  { Calculate the sum of the first n numbers }

  function sum (n: integer) : integer;
    var s, k: integer;
    begin
      s := 0;
      k := 1;
      while k <= n do
      begin
        sum := sum+k;
        k := k+1
      end;
      { verify if the sum is correct }
      if s <> n*(n+1)/2 then
        sum := -1
      else sum := s
    end;


  { Determine if n is a prime number }

  function is_prime (n: integer) : boolean;
    var k: integer;
        b: boolean;
    begin
      k := 2;
      b := true;
      while k <= i div 2 and b do
      begin
        if i div k = i/k then
          b := false;
        k := k+1
      end;
    is_prime := b
  end;

  { Sort an array of n elements }

  procedure bubble_sort(a: array [1..100] of real; n: integer);
  var i: integer;
      k: boolean;
      t: real;
  begin
    k := true;
    while k do
    begin
      i := 1;
      k := false;
      while i < n do
      begin
        if a[i] > a[i+1] then
        begin
          t := a[i];
          a[i] := a[i+1];
          a[i+1] := t;
          k := true
        end;
        i := i+1
      end
    end
  end;


{ Main program }

begin

  a := 48;
  b := 84;

  a := gcd(a, b);
  b := factorial(a+b);
  b := b - sum(a*b);

  { count all prime numbers from 2 to b }
  a := 1;
  i := 2;
  while i <= b do
  begin
    if is_prime(i) then
      a := a+1;
    i := i+1
  end;

  i := 1;
  while i < b do
  begin
    x[i] := (n-i+1)*3.14;
    i := i+1
  end;

  do123 := 42;

  bubble_sort(x, b)

end.
