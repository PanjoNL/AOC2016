unit uAOCTests;

interface

uses
  System.SysUtils, Winapi.Windows,
  uAocUtils, AocSolutions, AOCBase;

type AOCTest = record
  AOCClass: TAdventOfCodeRef;
  ExpectedSolutionA, ExpectedSolutionB, OverRidenTestInput: String;
end;

type AOCTests = class
public
  Class procedure RunTests;
end;

Const AOCTestData: array[0..4] of AOCTest =
(
 (AOCClass: TAdventOfCodeDay1; ExpectedSolutionA: '230'; ExpectedSolutionB: '154'),
 (AOCClass: TAdventOfCodeDay2; ExpectedSolutionA: '38961'; ExpectedSolutionB: '46C92'),
 (AOCClass: TAdventOfCodeDay3; ExpectedSolutionA: '983'; ExpectedSolutionB: '1836'),
// (AOCClass: TAdventOfCodeDay4; ExpectedSolutionA: '173787'; ExpectedSolutionB: '548'),
 (AOCClass: TAdventOfCodeDay5; ExpectedSolutionA: '2414BC77'; ExpectedSolutionB: '437e60fc'),
 (AOCClass: TAdventOfCodeDay6; ExpectedSolutionA: 'afwlyyyq'; ExpectedSolutionB: 'bhkzekao')
);

implementation

class procedure AOCTests.RunTests;

  procedure _Check(const DisplayName, Expected, Actual: String);
  begin
    if Expected <> '' then
      if Expected <> Actual then
      begin
        WriteLn(Format('FAIL, %s Expected: %s, Actual: %s', [DisplayName, Expected, Actual]));
        Assert(False);
      end
      else
        WriteLn(Format('PASS, %s', [DisplayName]))
  end;

Var Test: AOCTest;
    AdventOfCode: TAdventOfCode;
    SolutionA, SolutionB: string;
    StartTickTest, StartTick: Int64;
begin
  Writeln('');
  StartTick := GetTickCount;
  for Test in AOCTestData do
  begin
    Writeln(Format('Running tests for %s', [Test.AOCClass.ClassName]));

    StartTickTest := GetTickCount;
    AdventOfCode := Test.AOCClass.Create;
    AdventOfCode.Test(SolutionA, SolutionB, Test.OverRidenTestInput);
    AdventOfCode.Free;

    _Check('Part a', Test.ExpectedSolutionA, SolutionA);
    _Check('Part b', Test.ExpectedSolutionB, SolutionB);
    Writeln(FormAt('Total ticks %d', [GetTickCount - StartTickTest]));
    Writeln('');
  end;

  Writeln(Format('All tests done in %d ms', [GetTickCount - StartTick]));
end;

end.
