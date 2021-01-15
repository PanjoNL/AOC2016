unit AOCSolutions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Generics.Defaults, System.Generics.Collections,
  system.Diagnostics, AOCBase, RegularExpressions, System.DateUtils, system.StrUtils,
  system.Math, uAOCUtils, system.Types, IdHashMessageDigest;

type
  TAdventOfCodeDay1 = class(TAdventOfCode)
  private
    function TravelToHq(Const VisitTwice: Boolean): Integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay2 = class(TAdventOfCode)
  private
    function GetCode(Const StartKey: TPoint; Const Keys: TDictionary<TPoint, String>): String;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay3 = class(TAdventOfCode)
  private
    function IsValidTriangle(Const Sides: TList<Integer>): Boolean;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  type RCheckRec = record
    Char: Char;
    Count: integer
  end;

  TCheckRecComparer = class(TInterfacedObject, IComparer<RCheckRec>)
    function Compare(const Left, Right: RCheckRec): Integer;
  end;

  TAdventOfCodeDay4 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay5 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay6 = class(TAdventOfCode)
  private
    function DecodeMessage(Const aReverse: Boolean): String;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  {
  TAdventOfCodeDay = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  end;
  }

implementation

{$Region 'TAdventOfCodeDay1'}
function TAdventOfCodeDay1.SolveA: Variant;
begin
  Result := TravelToHq(False);
end;

function TAdventOfCodeDay1.SolveB: Variant;
begin
  Result := TravelToHq(True);
end;

Type TDirection = (North=0, East, South, West);
function TAdventOfCodeDay1.TravelToHq(Const VisitTwice: Boolean): Integer;
Var Direction: TDirection;
    Position: TPoint;
    Split: TStringDynArray;
    s: string;
    i: integer;
    Seen: TList<TPoint>;
    Stop: Boolean;
begin
  Direction := North;
  Position := TPoint.Zero;
  Seen := TList<TPoint>.Create;
  Split := SplitString(FInput[0], ',');
  Stop := False;

  for s in split do
  begin
    if Stop then
        Break;

    if s.Trim.StartsWith('L') then
    case Direction of
      North: Direction := West;
      East: Direction := North;
      South: Direction := East;
      West: Direction := South;
    end
    else if s.Trim.StartsWith('R') then
    case Direction of
      North: Direction := East;
      East: Direction := South;
      South: Direction := West;
      West: Direction := North;
    end
    else
      Assert(False, s);

    for i := 1 to s.Replace('L', '').Replace('R','').ToInteger do
    begin
      case Direction of
        North: Position.Offset(1, 0);
        East: Position.Offset(0, 1);
        South: Position.Offset(-1, 0);
        West: Position.Offset(0, -1);
      end;

      if VisitTwice then
      begin
        Stop := Seen.Contains(Position);
        if Stop then
          Break;
        Seen.Add(Position);
      end;
    end;
  end;

  Result := Abs(Position.X) + Abs(Position.Y);
  Seen.Free;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay2'}
function TAdventOfCodeDay2.SolveA: Variant;
var Keys: TDictionary<TPoint, String>;
begin
  Keys := TDictionary<TPoint, String>.Create;
  Keys.Add(TPoint.Create(0,0), '1');
  Keys.Add(TPoint.Create(1,0), '2');
  Keys.Add(TPoint.Create(2,0), '3');
  Keys.Add(TPoint.Create(0,1), '4');
  Keys.Add(TPoint.Create(1,1), '5');
  Keys.Add(TPoint.Create(2,1), '6');
  Keys.Add(TPoint.Create(0,2), '7');
  Keys.Add(TPoint.Create(1,2), '8');
  Keys.Add(TPoint.Create(2,2), '9');

  Result := GetCode(TPoint.Create(1,1), Keys);
  Keys.Free;
end;

function TAdventOfCodeDay2.SolveB: Variant;
var Keys: TDictionary<TPoint, String>;
begin
  Keys := TDictionary<TPoint, String>.Create;
  Keys.Add(TPoint.Create(2,0), '1');
  Keys.Add(TPoint.Create(1,1), '2');
  Keys.Add(TPoint.Create(2,1), '3');
  Keys.Add(TPoint.Create(3,1), '4');
  Keys.Add(TPoint.Create(0,2), '5');
  Keys.Add(TPoint.Create(1,2), '6');
  Keys.Add(TPoint.Create(2,2), '7');
  Keys.Add(TPoint.Create(3,2), '8');
  Keys.Add(TPoint.Create(4,2), '9');
  Keys.Add(TPoint.Create(1,3), 'A');
  Keys.Add(TPoint.Create(2,3), 'B');
  Keys.Add(TPoint.Create(3,3), 'C');
  Keys.Add(TPoint.Create(2,4), 'D');

  Result := GetCode(TPoint.Create(0,2), Keys);
  Keys.Free;
end;

function TAdventOfCodeDay2.GetCode(Const StartKey: TPoint; Const Keys: TDictionary<TPoint, String>): String;
var Point, OldPoint: TPoint;
    s: string;
    i: Integer;
begin
  Point := TPoint.Create(StartKey);
  Result := '';

  for s in FInput do
  begin
    for i := 1 to Length(s) do
    begin
      OldPoint := TPoint.Create(Point);
      case IndexStr(s[i], ['U', 'D', 'L', 'R']) of
        0: Point.Offset(0, -1);
        1: Point.Offset(0, 1);
        2: Point.Offset(-1, 0);
        3: Point.Offset(1, 0);
      else
        Assert(False, s[i]);
      end;

      if not Keys.ContainsKey(Point) then
        Point := TPoint.Create(OldPoint)
    end;

    Result := Result + Keys[Point];
  end;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay3'}
function TAdventOfCodeDay3.SolveA: Variant;
Var Sides: TList<Integer>;
    s: string;
begin
  Result := 0;
  Sides := TList<Integer>.Create;
  for s in FInput do
  begin
    Sides.Clear;
    Sides.Add(s.Substring(1,5).Trim.ToInteger);
    Sides.Add(s.Substring(6,5).Trim.ToInteger);
    Sides.Add(s.Substring(11,5).Trim.ToInteger);

    if IsValidTriangle(Sides) then
      Inc(Result);
  end;
  Sides.Free;
end;

function TAdventOfCodeDay3.SolveB: Variant;
Var Sides1, Sides2, Sides3: TList<Integer>;
    s: string;
    i: Integer;
begin
  Result := 0;
  Sides1 := TList<Integer>.Create;
  Sides2 := TList<Integer>.Create;
  Sides3 := TList<Integer>.Create;

  i := 0;
  for s in FInput do
  begin
    Sides1.Add(s.Substring(1,5).Trim.ToInteger);
    Sides2.Add(s.Substring(6,5).Trim.ToInteger);
    Sides3.Add(s.Substring(11,5).Trim.ToInteger);

    inc(i);
    if i = 3 then
    begin
      i := 0;

      if IsValidTriangle(Sides1) then
        Inc(Result);
      if IsValidTriangle(Sides2) then
        Inc(Result);
      if IsValidTriangle(Sides3) then
        Inc(Result);

      Sides1.Clear;
      Sides2.Clear;
      Sides3.Clear;
    end;
  end;

  Sides1.Free;
  Sides2.Free;
  Sides3.Free;
end;
function TAdventOfCodeDay3.IsValidTriangle(Const Sides: TList<Integer>): Boolean;
begin
  Assert(sides.count = 3, 'Invalid input');
  Sides.Sort;
  Result := Sides[0] + Sides[1] > Sides[2]
end;

{$ENDREGION}
{$Region 'TCheckRecComparer'}
function TCheckRecComparer.Compare(const Left, Right: RCheckRec): Integer;
begin
  if Left.Count = Right.Count then
    Result := Ord(Left.Char) - Ord(Right.Char)
  else
    Result := Right.Count - Left.Count;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay4'}
function TAdventOfCodeDay4.SolveA: Variant;
var CheckSum, s, s2: String;
    Split: TStringDynArray;
    Id, i: Integer;
    Rec: RCheckRec;
    Recs: TList<RCheckRec>;
begin
  Result := 0;
  Recs := TList<RCheckRec>.Create(TCheckRecComparer.Create);

  for s in FInput do
  begin
    s2 := '';
    Split := SplitString(s, '-[]');
    for i := 0 to Length(Split)-4 do
      s2 := s2 + Split[i];
    id := Split[Length(Split)-3].ToInteger;
    Checksum := Split[Length(Split)-2];

    Recs.Clear;
    for i := Ord('a') to Ord('z') do
    begin
      Rec.Char := chr(i);
      Rec.Count :=  OccurrencesOfChar(s2, Rec.Char);
      Recs.Add(Rec);
    end;

    Recs.Sort;
    s2 := '';
    for i := 0 to 4 do
      s2 := s2 + Recs[i].Char;
    if s2 = CheckSum then
      Inc(Result, Id);
  end;
  Recs.Free;
end;

function TAdventOfCodeDay4.SolveB: Variant;
var s, s2: String;
    Split: TStringDynArray;
    Id, i: Integer;
begin
  Result := 0;

  for s in FInput do
  begin
    s2 := '';
    Split := SplitString(s, '-[]');
    for i := 0 to Length(Split)-4 do
      s2 := s2 + ifthen(s2='','',' ') + Split[i];

    id := Split[Length(Split)-3].ToInteger;
    s2 := UpperCase(s2);
    for i := ord('a') to ord('z') do
      s2 := s2.Replace(UpperCase(Chr(i)), Chr(ord('a') + (i+id-ord('a')) mod 26));

    if s2.Contains('north') then
      Exit(id);
  end;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay5'}
function TAdventOfCodeDay5.SolveA: Variant;
var
  pMD5: TIdHashMessageDigest5;
  i: Integer;
  Hash: String;
begin
  Result := '';
  i := 0;
  pMD5 := TIdHashMessageDigest5.Create;
  try
    while Length(Result) < 8 do
    begin
      Hash := pMD5.HashStringAsHex(FInput[0]+IntToStr(i));
      if Hash.StartsWith('00000') then
      begin
        Result := Result + Hash[6];
        Writeln(Result);
      end;

      inc(i);
    end;
  finally
    pMD5.Free;
  end;

end;

function TAdventOfCodeDay5.SolveB: Variant;
var
  pMD5: TIdHashMessageDigest5;
  i, position: Integer;
  Code, Hash: String;
begin
  Code := '........';
  i := 0;
  pMD5 := TIdHashMessageDigest5.Create;
  try
    while Code.Contains('.') do
    begin
      Hash := pMD5.HashStringAsHex(FInput[0]+IntToStr(i));
      if Hash.StartsWith('00000') then
        if TryStrToInt(Hash[6], position) and (position <= 7) and (Code[position+1] = '.') then
        begin
          Code[position+1] := Hash[7];
          Writeln(Code);
        end;

      inc(i);
    end;
  finally
    pMD5.Free;
  end;

  Result := Code;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay6'}
function TAdventOfCodeDay6.SolveA: Variant;
begin
  Result := DecodeMessage(False);
end;

function TAdventOfCodeDay6.SolveB: Variant;
begin
  Result := DecodeMessage(True);
end;

function TAdventOfCodeDay6.DecodeMessage(Const aReverse: Boolean): String;
var i, j: integer;
    Recs: TList<RCheckRec>;
    Rec: RCheckRec;
    s: string;
begin
  Recs := TList<RCheckRec>.Create(TCheckRecComparer.Create);
  Result := '';

  for i := 1 to Length(FInput[0]) do
  begin
    s := '';

    for j := 0 to FInput.Count - 1 do
      s := s + FInput[j][i];

    Recs.Clear;
    for j := ord('a') to ord('z') do
    begin
      Rec.Char := Chr(j);
      Rec.Count := OccurrencesOfChar(s, Rec.Char);
      Recs.Add(Rec);
    end;

    Recs.Sort;
    if aReverse then
      Recs.Reverse;

    for Rec in Recs do
      if Rec.Count > 0 then
      begin
        Result := Result + Rec.Char;
        break;
      end;
  end;

  Recs.Free;
end;
{$ENDREGION}


(*
{$Region 'TAdventOfCodeDay'}
procedure TAdventOfCodeDay.BeforeSolve;
begin

end;

procedure TAdventOfCodeDay.AfterSolve;
begin

end;

function TAdventOfCodeDay.SolveA: Variant;
begin

end;

function TAdventOfCodeDay.SolveB: Variant;
begin

end;
{$ENDREGION}
*)

initialization
  RegisterClasses([TAdventOfCodeDay1, TAdventOfCodeDay2, TAdventOfCodeDay3, TAdventOfCodeDay4, TAdventOfCodeDay5,
                   TAdventOfCodeDay6]);

end.

