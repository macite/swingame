program RangeTests;
uses sgShared, sgTypes, strutils, sgUtils;

var
  data: LongintArray;
  i: Integer;
  testData: String;
begin
  
  testData := '[0-39],[1,2,3,0-39],40';
  WriteLn(ExtractDelimited(1, testData, [',']));
  WriteLn(ExtractDelimited(2, testData, [',']));
  WriteLn(ExtractDelimited(3, testData, [',']));
  WriteLn('---');
  WriteLn(ExtractDelimitedWithRanges(1, testData));
  WriteLn(ExtractDelimitedWithRanges(2, testData));
  WriteLn(ExtractDelimitedWithRanges(3, testData));
  
  data := ProcessRange('[0,1,2,3,-1,10-15,20-16,28,0--3,-5-0,-10--6,-11--15]');
  for i := 0 to High(data) do
  begin
    WriteLn(data[i]);
  end;
end.