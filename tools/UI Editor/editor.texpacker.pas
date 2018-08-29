unit editor.texpacker;

interface

uses
  System.Classes, System.SysUtils, PXL.Types, se.game.types;

type
  TPackingMode = (ePackingMode_BestFitFromNWCorner,
    ePackingMode_BestFitFromNECorner, ePackingMode_BestFitFromSECorner,
    ePackingMode_BestFitFromSWCorner, ePackingMode_HorizontalStrips);

  TRect = Record
    Left, Right, Top, Bottom: Integer;
  End;

  TPartition = Class
  Private
    FStripsRect: TRect;
    FRowHeight: Integer;
    FX, FY, FX2, FY2, FWidth, FHeight: Integer;
    FPackingMode: TPackingMode;
    FUsed: Boolean;
    FA, FB: TPartition;
  Public
    Constructor Create(Const ax, ay, ax2, ay2: Integer;
      Const APackingMode: TPackingMode);
    Destructor Destroy; Override;

    Function Insert(Const AWidth, AHeight: Integer; Var ARect: TRect): Boolean;
  End;

  TPackerItem = record
    // The name of the image
    Name: String[32];
    // Width of the item
    Width: Integer;
    // Height of the item
    Height: Integer;
    // The image to pack
    Image: TEngineImage;
    // True if the image was added to the target
    Placed: Boolean;
    // The position of the image in the packed image
    Position: TPoint2i;
  end;

  PPackerItemList = ^TPackerItemList;
  TPackerItemList = Array[0.. $00FFFFFF] of TPackerItem;

  TCustomPackerItemList = class
	private
	  FCount: Integer;
		FCapacity: Integer;
		FList: PPackerItemList;

    Procedure Grow;

    function  GetItem(Index: Integer): TPackerItem;
    procedure SetItem(Index: Integer; const Value: TPackerItem);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
	public
    constructor Create;
    destructor Destroy; override;

    Procedure Clear;

	 	Procedure Sort;

     // Add a new pattern to the list
    Procedure Add(const Value: TPackerItem ); overload;

    Procedure Add(const Name: String; Image: TEngineImage ); overload;

    Procedure Delete(Index: Integer);

    // Returns the index of a pattern, -1 if not found.
    Function IndexOf(const Name: String): Integer;
  // Number of items in the pattern list
    Property Count: Integer read FCount write SetCount;
    // Capacity of the list
    Property Capacity: Integer read FCapacity write SetCapacity;
    // The internal list of patterns
    Property List: PPackerItemList read FList;
    // Gets and sets the patterns
    Property Items[Index: Integer]: TPackerItem read GetItem Write SetItem; default;
	end;

implementation

{ TPartition }

// APackingMode is to specify how to pack the rectangles.
// When APackingMode is
// ePackingMode_BestFitFromNWCorner, ePackingMode_BestFitFromNECorner,
// ePackingMode_BestFitFromSWCorner or ePackingMode_BestFitFromSECorner,
// the rectangles will be packed using best fit partitioning starting from that corner.
//
// When APackingMode is ePackingMode_HorizontalStrips, the rectangles will be packed
// in a single row till it can't fit anymore on the row.  When this happens it
// will got to the next row moving down by the largest rectangle height in
// the row just completed.
Constructor TPartition.Create(Const ax, ay, ax2, ay2: Integer;
  Const APackingMode: TPackingMode);
Begin
  Inherited Create;
  FStripsRect.Left := ax;
  FStripsRect.Top := ay;
  FRowHeight := 0;
  FX := ax;
  FY := ay;
  FX2 := ax2;
  FY2 := ay2;
  FWidth := FX2 - FX;
  FHeight := FY2 - FY;
  FPackingMode := APackingMode;
  FA := Nil;
  FB := Nil;
  FUsed := False;
End;

Destructor TPartition.Destroy;
Begin
  FA.Free;
  FB.Free;
  Inherited Destroy;
End;

// Inserts a rectangle into the package, returns a Rect if result is true
// If the rectangle could not fit, the result is False
Function TPartition.Insert(Const AWidth, AHeight: Integer;
  Var ARect: TRect): Boolean;
Var
  R: TRect;
Begin
  Result := False;
  If FPackingMode = ePackingMode_HorizontalStrips Then
  Begin
    FStripsRect.Right := FStripsRect.Left + AWidth;
    FStripsRect.Bottom := FStripsRect.Top + AHeight;
    If (FStripsRect.Right <= FX2) And (FStripsRect.Bottom <= FY2) Then
    Begin
      Result := True;
      ARect := FStripsRect;
      If AHeight > FRowHeight Then
        FRowHeight := AHeight;
    End;
    Inc(FStripsRect.Left, AWidth);
    FStripsRect.Right := FStripsRect.Left + AWidth;
    If FStripsRect.Right > FX2 Then
    // hit right side of partition so reset x and go to next row
    Begin
      FStripsRect.Left := FX;
      FStripsRect.Right := FStripsRect.Left + AWidth;
      Inc(FStripsRect.Top, FRowHeight);
      FStripsRect.Bottom := FStripsRect.Top + AHeight;
      FRowHeight := 0;
    End;
    Exit;
  End;
  If FUsed Then
  Begin
    If Assigned(FA) Then
      Result := FA.Insert(AWidth, AHeight, R);
    If (Not Result) And Assigned(FB) Then
      Result := FB.Insert(AWidth, AHeight, R);
    If Result Then
      ARect := R;
  End
  Else
  Begin
    If (AWidth <= FWidth) and (AHeight <= FHeight) Then
    Begin
      FUsed := True;
      Result := True;
      Case FPackingMode of
        ePackingMode_BestFitFromNWCorner:
          Begin
            ARect.Left := FX;
            ARect.Top := FY;
            ARect.Right := FX + AWidth;
            ARect.Bottom := FY + AHeight;

            If (FWidth - AWidth) >= (FHeight - AHeight) Then
            Begin
              FA := TPartition.Create(FX, (FY + AHeight), (FX + AWidth), FY2,
                FPackingMode);
              FB := TPartition.Create((FX + AWidth), FY, FX2, FY2,
                FPackingMode);
            End
            Else
            Begin
              FA := TPartition.Create((FX + AWidth), FY, FX2, (FY + AHeight),
                FPackingMode);
              FB := TPartition.Create(FX, (FY + AHeight), FX2, FY2,
                FPackingMode);
            End;
          End;
        ePackingMode_BestFitFromNECorner:
          Begin
            ARect.Left := FX2 - AWidth;
            ARect.Top := FY;
            ARect.Right := FX2;
            ARect.Bottom := FY + AHeight;

            If (FWidth - AWidth) >= (FHeight - AHeight) Then
            Begin
              FA := TPartition.Create(FX2 - AWidth, (FY + AHeight), FX2, FY2,
                FPackingMode);
              FB := TPartition.Create(FX, FY, FX2 - AWidth, FY2, FPackingMode);
            End
            Else
            Begin
              FA := TPartition.Create(FX, FY, FX2 - AWidth, (FY + AHeight),
                FPackingMode);
              FB := TPartition.Create(FX, (FY + AHeight), FX2, FY2,
                FPackingMode);
            End;
          End;
        ePackingMode_BestFitFromSWCorner:
          Begin
            ARect.Left := FX;
            ARect.Top := FY2 - AHeight;
            ARect.Right := FX + AWidth;
            ARect.Bottom := FY2;

            If (FWidth - AWidth) >= (FHeight - AHeight) Then
            Begin
              FA := TPartition.Create(FX, FY, (FX + AWidth), FY2 - AHeight,
                FPackingMode);
              FB := TPartition.Create((FX + AWidth), FY, FX2, FY2,
                FPackingMode);
            End
            Else
            Begin
              FA := TPartition.Create((FX + AWidth), FY2 - AHeight, FX2, FY2,
                FPackingMode);
              FB := TPartition.Create(FX, FY, FX2, FY2 - AHeight, FPackingMode);
            End;
          End;
        ePackingMode_BestFitFromSECorner:
          Begin
            ARect.Left := FX2 - AWidth;
            ARect.Top := FY2 - AHeight;
            ARect.Right := FX2;
            ARect.Bottom := FY2;

            If (FWidth - AWidth) >= (FHeight - AHeight) Then
            Begin
              FA := TPartition.Create(FX2 - AWidth, FY, FX2, FY2 - AHeight,
                FPackingMode);
              FB := TPartition.Create(FX, FY, FX2 - AWidth, FY2, FPackingMode);
            End
            Else
            Begin
              FA := TPartition.Create(FX, FY2 - AHeight, FX2 - AWidth, FY2,
                FPackingMode);
              FB := TPartition.Create(FX, FY, FX2, FY2 - AHeight, FPackingMode);
            End;
          End;
      End;
    End;
  End;
End;

procedure QuickSort(SortList: PPackerItemList; L, R: Integer);
var
  I, J: Integer;
  P, T: TPackerItem;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while (P.Width - SortList^[I].Width) < 0 do
        Inc(I);
      while (P.Width - SortList^[J].Width) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SortList, L, J);
    L := I;
  until I >= R;
end;

{ TCustomPackerItemList }

constructor TCustomPackerItemList.Create;
begin
  FCount   :=0;
  FCapacity:= 0;
end;

destructor TCustomPackerItemList.Destroy;
begin
  SetCount   (0);
  SetCapacity(0);
  inherited;
end;

function TCustomPackerItemList.GetItem(Index: Integer): TPackerItem;
begin
  Result:= FList^[Index];
end;

procedure TCustomPackerItemList.SetItem(Index: Integer;
  const Value: TPackerItem);
begin
  FList^[Index]:= Value;
end;

procedure TCustomPackerItemList.Clear;
begin
  SetCount   (0);
  SetCapacity(0);
end;

procedure TCustomPackerItemList.Add(const Value: TPackerItem);
begin
  SetCount(Count + 1);
  FList^[Count - 1]:= Value;
end;

procedure TCustomPackerItemList.Add(const Name: String; Image: TEngineImage);
begin
  SetCount(Count + 1);
  FList^[Count - 1].Name      := ShortString(Name);
  FList^[Count - 1].Width     := Image.Width;
  FList^[Count - 1].Height    := Image.Height;
  FList^[Count - 1].Image     := Image;
  FList^[Count - 1].Placed    := False;
  FList^[Count - 1].Position.X:= -1;
  FList^[Count - 1].Position.Y:= -1;
end;

procedure TCustomPackerItemList.Delete(Index: Integer);
begin
  If (Index < 0) or (Index >= FCount) then Exit;
  FCount := FCount-1;
  System.Move(FList^[Index+1], FList^[Index], (FCount - Index) * SizeOf(TPackerItem));
end;

procedure TCustomPackerItemList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
  if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;
  //
  SetCapacity(FCount + Delta);
end;

function TCustomPackerItemList.IndexOf(const Name: String): Integer;
var
  Index: Integer;
begin
  For Index:=0 to FCount-1 do
  begin
    if SameText( String(FList^[Index].Name), Name) then
    begin
      Result:= Index;
      Exit;
    end;
  end;
  Result:= -1;
end;

procedure TCustomPackerItemList.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then begin
    FCapacity := Value;
    ReallocMem(FList, FCapacity * SizeOf(TPackerItem));
  end;
end;

procedure TCustomPackerItemList.SetCount(const Value: Integer);
begin
  FCount := Value;
  if(FCount > FCapacity) then Grow;
end;

procedure TCustomPackerItemList.Sort;
begin
  if (FList <> nil) and (Count > 0) then
    QuickSort(FList, 0, Count - 1);
end;

end.
