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

  TAdventOfCodeDay7 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TAdventOfCodeDay8 = class(TAdventOfCode)
  private
    DisPlay: TDictionary<TPoint,Boolean>;

    procedure DoRotateRow(NewValues: TDictionary<TPoint,Boolean>; aX, aY: Integer);
    procedure DoRotateColumn(NewValues: TDictionary<TPoint,Boolean>; aX, aY: Integer);

    const ScreenWidth: integer = 50;
          screenHeight: integer = 6;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  end;

  TAdventOfCodeDay9 = class(TAdventOfCode)
  private
    function Decompress(Const aLine: String; Const aRecursive: Boolean): Int64;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

  TRobot = Class
  private
    FId, FChip1, FChip2: Integer;
  public
    OutputLow, OutputHigh: Integer;
    OutputLowIsRobot, OutputHighIsRobot: boolean;

    constructor Create(Const aId: Integer);

    procedure AddChip(Const aChip: Integer);
    procedure RemoveChips;
    function LowChip: Integer;
    function HighChip: Integer;
    function HasTwoChips: Boolean;

    property Id: Integer Read FId;
  end;

  TAdventOfCodeDay10 = class(TAdventOfCode)
  private
    procedure FreeRobot(Sender: TObject; Const aValue: TRobot; Action: TCollectionNotification);
    function PrepareRobots: TDictionary<Integer, TRobot>;
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
{$Region 'TAdventOfCodeDay7'}
function TAdventOfCodeDay7.SolveA: Variant;

  function Abba(Const aString: String): Boolean;
  Var i: Integer;
  begin
    Result := False;
    for i := 0 to Length(aString) - 1 do
      if (aString[i] = aString[i+3]) and (aString[i+1] = aString[i+2]) and (aString[i] <> aString[i+2]) then
        Exit(True);
  end;

Var Split: TStringDynArray;
    s, Tls, HyperNet: string;
    i: Integer;
begin
  Result := 0;

  for s in FInput do
  begin
    i := 1;
    Split := SplitString(s, '[]');
    Tls := Split[0];
    HyperNet := '';
    while i < Length(Split) do
    begin
      HyperNet := HyperNet + '@@@@' + Split[i];
      Tls := Tls + '@@@@' + Split[i+1];
      Inc(i, 2);
    end;

    if Abba(Tls) and Not Abba(HyperNet) then
      Inc(Result, 1);
  end;
end;

function TAdventOfCodeDay7.SolveB: Variant;
Var Split: TStringDynArray;
    s, s2, Tls, HyperNet: string;
    i: Integer;
begin
  Result := 0;
  for s in FInput do
  begin
    i := 1;
    Split := SplitString(s, '[]');
    Tls := Split[0];
    HyperNet := '';
    while i < Length(Split) do
    begin
      HyperNet := HyperNet + '@@@@' + Split[i];
      Tls := Tls + '@@@@' + Split[i+1];
      Inc(i, 2);
    end;

    for i := 1 to Length(HyperNet) - 1 do
    begin
      s2 := HyperNet[i] + HyperNet[i+1] + HyperNet[i+2];
      if (HyperNet[i] = HyperNet[i+2]) and (HyperNet[i] <> HyperNet[i+1]) then
      begin
        s2 := HyperNet[i+1] + HyperNet[i] + HyperNet[i+1];
        if Pos(s2, Tls) > 0 then
        begin
          Inc(Result);
          Break;
        end;
      end;
    end;
  end;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay8'}
type TFillNewValues =  procedure(NewValues: TDictionary<TPoint,Boolean>; aX, aY: Integer) of object;

procedure TAdventOfCodeDay8.DoRotateColumn(NewValues: TDictionary<TPoint,Boolean>; aX, aY: Integer);
var y: Integer;
begin
  for y := 0 to screenHeight -1 do
    NewValues.Add(TPoint.Create(ax, (y+aY)Mod (screenHeight)), DisPlay[TPoint.Create(aX, y)]);
end;

procedure TAdventOfCodeDay8.DoRotateRow(NewValues: TDictionary<TPoint,Boolean>; aX, aY: Integer);
var x: Integer;
begin
  for x := 0 to ScreenWidth -1 do
    NewValues.Add(TPoint.Create((x+ax) Mod (ScreenWidth), aY), DisPlay[TPoint.Create(x, aY)]);
end;

procedure TAdventOfCodeDay8.BeforeSolve;

  procedure DoRect(aX, aY: Integer);
  Var x, y: Integer;
      Point: TPoint;
  begin
    for x := 0 to ax-1 do
      for y := 0 to ay-1 do
      begin
        Point := TPoint.Create(x,y);
        DisPlay[Point] := True;
      end;
  end;

  procedure Rotate(aFillNewValues: TFillNewValues; aX, aY: Integer);
  Var NewValues: TDictionary<TPoint,Boolean>;
      Point: TPoint;
  begin
    NewValues := TDictionary<TPoint,Boolean>.Create;
    aFillNewValues(NewValues, aX, aY);
    for Point in NewValues.Keys do
      DisPlay[Point] := NewValues[Point];
    NewValues.Free;;
  end;

var x,y: Integer;
    Point: TPoint;
    s: string;
    Split: TStringDynArray;
begin
  DisPlay := TDictionary<TPoint,Boolean>.Create;
  for x := 0 to ScreenWidth -1 do
    for y := 0 to screenHeight-1 do
    begin
      Point := TPoint.Create(x ,y);
      DisPlay.Add(Point, False);
    end;

  for s in FInput do
  begin
    if s.StartsWith('rect') then
    begin
      Split := SplitString(s, 'x ');
      DoRect(Split[1].ToInteger, Split[2].ToInteger);
    end
    else if s.StartsWith('rotate column') then
    begin
      Split := SplitString(s, ' =');
      Rotate(DoRotateColumn, Split[3].ToInteger, Split[5].ToInteger);
    end
    else if s.StartsWith('rotate row') then
    begin
      Split := SplitString(s, ' =');
      Rotate(DoRotateRow, Split[5].ToInteger, Split[3].ToInteger);
    end
    else
      Assert(False, s);
  end;
end;

procedure TAdventOfCodeDay8.AfterSolve;
begin
  DisPlay.Free;
end;

function TAdventOfCodeDay8.SolveA: Variant;
var b: boolean;
begin
  Result := 0;
  for b in DisPlay.Values do
    if b then
      Inc(Result);
end;

function TAdventOfCodeDay8.SolveB: Variant;
var x,y: integer;
    s: String;
    sl: TStringList;
begin
  WriteLn('');
  sl := TStringList.Create;
  for y := 0 to ScreenHeight -1 do
  begin
    s := '';
    for x := 0 to ScreenWidth -1 do
    begin
      if Display[TPoint.Create(x,y)] then
        s := s + '#'
      else
        s := s + ' ';
    end;
    sl.Add(s);
  end;

  Result := 'Solution saved at ' + SaveFilePath;
  sl.SaveToFile(SaveFilePath);
  sl.Free;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay9'}
function TAdventOfCodeDay9.Decompress(Const aLine: String; Const aRecursive: Boolean): Int64;
Var Marker, SubString: String;
    i, MarkerEnd: integer;
    split: TStringDynArray;
begin
  Result := 0;
  i := 1;

  while i <= Length(aLine) do
  begin
    if aLine[i] <> '(' then
      Inc(Result)
    else
    begin
      MarkerEnd := Pos(')', aLine, i);
      Marker := Copy(aLine, i+1, MarkerEnd-i-1);
      split := SplitString(Marker, 'x');
      SubString := Copy(aLine, MarkerEnd+1, Split[0].ToInteger);

      if aRecursive and (Pos('(', SubString) > 0) then
        Result := Result + split[1].ToInteger * Decompress(SubString, aRecursive)
      else
        Result := Result + split[1].ToInteger * Split[0].ToInteger;

      i := MarkerEnd + Split[0].ToInteger;
    end;

    Inc(i);
  end;
end;

function TAdventOfCodeDay9.SolveA: Variant;
begin
  Result := Decompress(FInput[0], False);
end;

function TAdventOfCodeDay9.SolveB: Variant;
begin
  Result := Decompress(FInput[0], True);
end;
{$ENDREGION}
{$Region 'TRobot'}
constructor TRobot.Create(Const aId: Integer);
begin
  FChip1 := 0;
  FChip2 := 0;
  OutputLow := 0;
  OutPutHigh := 0;
  FId := aId;
end;

procedure TRobot.AddChip(Const aChip: Integer);
begin
  if FChip1 = 0 then
    FChip1 := aChip
  else if FChip2 = 0 then
    FChip2 := aChip
  else
    Assert(False);
end;

procedure TRobot.RemoveChips;
begin
  FChip1 := 0;
  FChip2 := 0;
end;

function TRobot.LowChip: Integer;
begin
  Result := Min(FChip1, FChip2);
end;

function TRobot.HighChip: Integer;
begin
  Result := Max(FChip1, FChip2);
end;

function TRobot.HasTwoChips: Boolean;
begin
  Result := FChip2 <> 0;
end;

{$ENDREGION}
{$Region 'TAdventOfCodeDay10'}
procedure TAdventOfCodeDay10.FreeRobot(Sender: TObject; Const aValue: TRobot; Action: TCollectionNotification);
begin
  if Action = cnRemoved then
    aValue.Free;
end;

function TAdventOfCodeDay10.PrepareRobots: TDictionary<Integer, TRobot>;

  function _GetRobot(Const Robots: TDictionary<Integer, TRobot>; Const aId: Integer): TRobot;
  begin
    if not Robots.TryGetValue(aId, Result) then
    begin
      Result := TRobot.Create(aId);
      Robots.Add(Result.Id, Result);
    end;
  end;


var s: String;
    Split: TStringDynArray;
    Robot: TRobot;
begin
  Result := TDictionary<Integer, TRobot>.Create();
  Result.OnValueNotify := FreeRobot;

  for s in FInput do
  begin
    Split := SplitString(s, ' ');
    if SameText(Split[0], 'Value') then
    begin
      Robot := _GetRobot(Result, Split[5].ToInteger);
      Robot.AddChip(Split[1].ToInteger);
    end
    else if SameText(Split[0], 'bot') then // bot 0 gives low to output 2 and high to bot 0
    begin
      Robot := _GetRobot(Result, Split[1].ToInteger);

      Robot.OutputLow := Split[6].ToInteger;
      Robot.OutputLowIsRobot := SameText(Split[5], 'bot');
      Robot.OutPutHigh := Split[11].ToInteger;
      Robot.OutputHighIsRobot := SameText(Split[10], 'bot');
    end
    else
      Assert(False, s);
  end;
end;

function TAdventOfCodeDay10.SolveA: Variant;
var Robot: TRobot;
    Robots: TDictionary<Integer, TRobot>;
begin
  Robots := PrepareRobots;

  try
    while true do
    begin
      for Robot in Robots.Values do
      begin
        if not Robot.HasTwoChips then
          Continue;

        if (Robot.LowChip = 17) and (Robot.HighChip = 61) then
          Exit(Robot.Id);

        if Robot.OutputLowIsRobot then
          Robots[Robot.OutputLow].AddChip(Robot.LowChip);

        if Robot.OutputHighIsRobot then
          Robots[Robot.OutPutHigh].AddChip(Robot.HighChip);

        Robot.RemoveChips;
      end;
    end;
  finally
    Robots.Free;
  end;
end;

function TAdventOfCodeDay10.SolveB: Variant;
var Robot: TRobot;
    Robots: TDictionary<Integer, TRobot>;
    OutPutList: TList<Integer>;
    i: Integer;
begin
  Robots := PrepareRobots;
  OutPutList := TList<Integer>.Create;

  while OutPutList.Count < 3 do
  begin
    for Robot in Robots.Values do
    begin
      if not Robot.HasTwoChips then
        Continue;

      if Robot.OutputLowIsRobot then
        Robots[Robot.OutputLow].AddChip(Robot.LowChip)
      else if Robot.OutputLow <= 2 then
        OutPutList.Add(Robot.LowChip);

      if Robot.OutputHighIsRobot then
        Robots[Robot.OutPutHigh].AddChip(Robot.HighChip)
      else if Robot.OutputHigh <= 2 then
        OutPutList.Add(Robot.HighChip);

      Robot.RemoveChips;
    end;
  end;

  Result := 1;
  for i in OutPutList do
    Result := Result * i;

  OutPutList.Free;
  Robots.Free;
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
  RegisterClasses([TAdventOfCodeDay1,TAdventOfCodeDay2,TAdventOfCodeDay3,TAdventOfCodeDay4,TAdventOfCodeDay5,
                   TAdventOfCodeDay6,TAdventOfCodeDay7,TAdventOfCodeDay8,TAdventOfCodeDay9,TAdventOfCodeDay10]);

end.

