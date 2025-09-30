unit UnitPrincipal;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, Types, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, Math;

type
  // EnumeraÃ§Ã£o para os modos de funcionamento
  TModoOperacao = (moCirculos, moLinhas, moLinhaEquacaoGeral, moLinhaParametrica,
                   moCohenSutherland, moBresenham, moEdgeFill, moFloodFill4, moFloodFill8,
                   moNenhum);

  { TFormPrincipal }

  TFormPrincipal = class(TForm)
    Image1: TImage;
    MainMenu1: TMainMenu;
    MenuFuncionalidades: TMenuItem;
    MenuCirculos: TMenuItem;
    MenuCirculoEquacaoPadrao: TMenuItem;
    MenuCirculoParametrica: TMenuItem;
    MenuCirculoRotacao: TMenuItem;
    MenuCirculoBresenham: TMenuItem;
    MenuLinhas: TMenuItem;
    MenuLinhaSimples: TMenuItem;
    MenuLinhaEquacaoGeral: TMenuItem;
    MenuLinhaParametrica: TMenuItem;
    MenuAlgoritmosRecorte: TMenuItem;
    MenuCohenSutherland: TMenuItem;
    MenuLiangBarsky: TMenuItem;
    MenuSeparadorAlg: TMenuItem;
    MenuResetarCohen: TMenuItem;
    MenuSair: TMenuItem;
    MenuLimpar: TMenuItem;
    { Itens de menu para as prÃ¡ticas de preenchimento }
    MenuPreenchimento: TMenuItem;
    MenuEdgeFill: TMenuItem;
    MenuFloodFill4: TMenuItem;
    MenuFloodFill8: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuCirculoEquacaoPadraoClick(Sender: TObject);
    procedure MenuCirculoParametricaClick(Sender: TObject);
    procedure MenuCirculoRotacaoClick(Sender: TObject);
    procedure MenuCirculoBresenhamClick(Sender: TObject);
    procedure MenuLinhaSimplesClick(Sender: TObject);
    procedure MenuLinhaEquacaoGeralClick(Sender: TObject);
    procedure MenuLinhaParametricaClick(Sender: TObject);
    procedure MenuCohenSutherlandClick(Sender: TObject);
    procedure MenuResetarCohenClick(Sender: TObject);
    procedure MenuSairClick(Sender: TObject);
    procedure MenuLimparClick(Sender: TObject);
    procedure MenuEdgeFillClick(Sender: TObject);
    procedure MenuFloodFill4Click(Sender: TObject);
    procedure MenuFloodFill8Click(Sender: TObject);
  private
    ModoAtual: TModoOperacao;
    
    // VariÃ¡veis para desenho de linhas
    StartPoint, EndPoint: TPoint;
    IsDrawingLine: Boolean;
    
    // VariÃ¡veis para Cohen-Sutherland
    JanelaMin, JanelaMax: TPoint;
    LinhaStart, LinhaEnd: TPoint;
    EstadoCohen: Integer; // 0: Definir Janela, 1: Definir Linha
    CohenAtivadoPorClick: Boolean; // Controle para garantir ativaÃ§Ã£o apenas por clique
    
    // VariÃ¡veis para Bresenham
    CentroCirculo: TPoint;
    IsDefiningCircle: Boolean;
    EstadoBresenham: Integer; // 0: Definir Centro, 1: Definir Raio
    
    // VariÃ¡veis para Flood Fill
    BorderColor, FillColor: TColor;
    FloodUse8: Boolean;
    FloodBounds: TRect;
    
    // MÃ©todos auxiliares de preenchimento
    procedure EdgeFillInvert(const Poly: array of TPoint);
    procedure FloodFill(x, y: Integer; Use8: Boolean);
    
    // MÃ©todos auxiliares
    procedure LimparTela;
    procedure DesenharLinhaSimples(x1, y1, x2, y2: Integer; Cor: TColor);
    procedure DesenharLinhaEquacaoGeral(x1, y1, x2, y2: Integer);
    procedure DesenharLinhaParametrica(x1, y1, x2, y2: Integer);
    procedure DesenharCirculoEquacaoPadrao;
    procedure DesenharCirculoParametrica;
    procedure DesenharCirculoRotacao;
    procedure DesenharCirculoBresenham(xc, yc, R: Integer);
    procedure PlotPixelCircle(xc, yc, x, y: Integer; Cor: TColor);
    procedure CohenSutherland(var x1, y1, x2, y2: Double; xmin, ymin, xmax, ymax: Double);
    procedure DesenhaCohenSutherland;
  public

  end;

var
  FormPrincipal: TFormPrincipal;

implementation

{$R *.lfm}

{ TFormPrincipal }

procedure TFormPrincipal.FormCreate(Sender: TObject);
begin
  // Configurar o formulÃ¡rio
  Caption := 'Projeto Integrado - ComputaÃ§Ã£o GrÃ¡fica';
  WindowState := wsNormal;
  
  // Configurar nomes dos menus\r\n  MenuFuncionalidades.Caption := 'Funcionalidades';\r\n  MenuCirculos.Caption := 'Círculos';\r\n  MenuCirculoEquacaoPadrao.Caption := 'Circunf. por Equação Padrão';\r\n  MenuCirculoParametrica.Caption := 'Circunf. por Eq. Paramétrica';\r\n  MenuCirculoRotacao.Caption := 'Circunf. por Rotação';\r\n  MenuCirculoBresenham.Caption := 'Circunf. por Bresenham';\r\n  MenuLinhas.Caption := 'Linhas';\r\n  MenuLinhaSimples.Caption := 'Desenhar Linhas (Simples)';\r\n  MenuLinhaEquacaoGeral.Caption := 'Linha por Equação Geral';\r\n  MenuLinhaParametrica.Caption := 'Linha por Eq. Paramétrica';\r\n  MenuAlgoritmosRecorte.Caption := 'Algoritmos de Recorte de Retas';\r\n  MenuCohenSutherland.Caption := 'Cohen-Sutherland';\r\n  MenuLiangBarsky.Caption := 'Liang-Barsky (Em breve)';\r\n  MenuResetarCohen.Caption := 'Resetar Algoritmo';\r\n  MenuLimpar.Caption := 'Limpar Tela';\r\n  MenuSair.Caption := 'Sair';

  MenuPreenchimento := TMenuItem.Create(MainMenu1);
  MenuPreenchimento.Caption := 'Preenchimento';
  MenuFuncionalidades.Add(MenuPreenchimento);

  MenuEdgeFill := TMenuItem.Create(MainMenu1);
  MenuEdgeFill.Caption := 'Inversão de Cores (Edge Fill)';
  MenuEdgeFill.OnClick := @MenuEdgeFillClick;
  MenuPreenchimento.Add(MenuEdgeFill);

  MenuFloodFill4 := TMenuItem.Create(MainMenu1);
  MenuFloodFill4.Caption := 'Flood Fill (Vizinhança 4)';
  MenuFloodFill4.OnClick := @MenuFloodFill4Click;
  MenuPreenchimento.Add(MenuFloodFill4);

  MenuFloodFill8 := TMenuItem.Create(MainMenu1);
  MenuFloodFill8.Caption := 'Flood Fill (Vizinhança 8)';
  MenuFloodFill8.OnClick := @MenuFloodFill8Click;
  MenuPreenchimento.Add(MenuFloodFill8);
  
  // Inicializar variÃ¡veis
  ModoAtual := moNenhum;
  IsDrawingLine := False;
  EstadoCohen := 0;
  CohenAtivadoPorClick := False;
  IsDefiningCircle := False;
  EstadoBresenham := 0;
  
  // Limpa e prepara a tela
  LimparTela;
  FloodBounds := Rect(0, 0, Image1.Width, Image1.Height);
end;

procedure TFormPrincipal.LimparTela;
begin
  Image1.Picture := nil;
  Image1.Canvas.Brush.Color := clWhite;
  Image1.Canvas.FillRect(Image1.Canvas.ClipRect);
  Image1.Canvas.Pen.Color := clBlack;
end;

procedure TFormPrincipal.MenuLimparClick(Sender: TObject);
begin
  LimparTela;
  ModoAtual := moNenhum;
  EstadoCohen := 0;
  CohenAtivadoPorClick := False;
  IsDefiningCircle := False;
  EstadoBresenham := 0;
  FloodUse8 := False;
  BorderColor := clBlack;
  FillColor := clWhite;

  FloodUse8 := False;
  BorderColor := clBlack;
  FillColor := clWhite;
  FloodBounds := Rect(0, 0, Image1.Width, Image1.Height);
end;
{ Manipuladores para as práticas de preenchimento }

procedure TFormPrincipal.MenuEdgeFillClick(Sender: TObject);
var
  Poly: array[0..4] of TPoint;
  w, h: Integer;
begin
  ModoAtual := moEdgeFill;
  LimparTela;

  w := Image1.Width;
  h := Image1.Height;
  Poly[0] := Point(w div 2, h div 8);
  Poly[1] := Point(7 * w div 8, h div 3);
  Poly[2] := Point(5 * w div 8, 7 * h div 8);
  Poly[3] := Point(w div 8, 7 * h div 8);
  Poly[4] := Point(w div 8, h div 3);

  Image1.Canvas.Pen.Color := clBlack;
  Image1.Canvas.Brush.Style := bsClear;
  Image1.Canvas.Polygon(Poly);
  Image1.Canvas.Brush.Style := bsSolid;

  EdgeFillInvert(Poly);

  ShowMessage('Algoritmo de inversão de cores concluído!');
end;

procedure TFormPrincipal.MenuFloodFill4Click(Sender: TObject);
const
  MarginX = 40;
  MarginY = 40;
var
  TestBitmap: TBitmap;
  destRect: TRect;
  destWidth, destHeight: Integer;
  offsetX, offsetY: Integer;
  availableW, availableH: Integer;
  borderRight, borderBottom: Integer;
  innerLeft, innerTop, innerRight, innerBottom: Integer;
  scale: Double;

  function ClampInt(Value, MinVal, MaxVal: Integer): Integer;
  begin
    if Value < MinVal then
      Result := MinVal
    else if Value > MaxVal then
      Result := MaxVal
    else
      Result := Value;
  end;

begin
  ModoAtual := moFloodFill4;
  FloodUse8 := False;
  BorderColor := clBlack;
  FillColor := clYellow;

  LimparTela;
  Image1.Canvas.Brush.Color := clWhite;
  Image1.Canvas.FillRect(Image1.ClientRect);

  availableW := Max(1, Image1.Width - (2 * MarginX));
  availableH := Max(1, Image1.Height - (2 * MarginY));

  TestBitmap := nil;
  if FileExists('Flood.bmp') then
  begin
    TestBitmap := TBitmap.Create;
    try
      TestBitmap.LoadFromFile('Flood.bmp');
      if (TestBitmap.Width > 0) and (TestBitmap.Height > 0) then
      begin
        scale := Min(availableW / TestBitmap.Width, availableH / TestBitmap.Height);
        destWidth := Max(1, Round(TestBitmap.Width * scale));
        destHeight := Max(1, Round(TestBitmap.Height * scale));
      end
      else
      begin
        destWidth := availableW;
        destHeight := availableH;
      end;
    except
      FreeAndNil(TestBitmap);
      destWidth := availableW;
      destHeight := availableH;
    end;
  end
  else
  begin
    destWidth := availableW;
    destHeight := availableH;
  end;

  offsetX := (Image1.Width - destWidth) div 2;
  offsetY := (Image1.Height - destHeight) div 2;
  destRect := Rect(offsetX, offsetY, offsetX + destWidth, offsetY + destHeight);

  if Assigned(TestBitmap) then
  begin
    try
      Image1.Canvas.StretchDraw(destRect, TestBitmap);
    finally
      TestBitmap.Free;
    end;
  end
  else
  begin
    Image1.Canvas.Brush.Style := bsClear;
    Image1.Canvas.Pen.Color := BorderColor;
    Image1.Canvas.Rectangle(destRect.Left, destRect.Top, destRect.Right, destRect.Bottom);
  end;

  Image1.Canvas.Brush.Style := bsClear;
  Image1.Canvas.Pen.Color := BorderColor;
  borderRight := ClampInt(destRect.Right - 1, 0, Image1.Width - 1);
  borderBottom := ClampInt(destRect.Bottom - 1, 0, Image1.Height - 1);
  Image1.Canvas.Rectangle(destRect.Left, destRect.Top, borderRight + 1, borderBottom + 1);
  Image1.Canvas.Brush.Style := bsSolid;

  innerLeft := ClampInt(destRect.Left + 1, 0, Image1.Width - 1);
  innerTop := ClampInt(destRect.Top + 1, 0, Image1.Height - 1);
  innerRight := ClampInt(borderRight, innerLeft + 1, Image1.Width);
  innerBottom := ClampInt(borderBottom, innerTop + 1, Image1.Height);
  FloodBounds := Rect(innerLeft, innerTop, innerRight, innerBottom);

  ShowMessage('Modo Flood Fill (vizinhança 4) ativado. Clique em um ponto dentro da região para preencher.');
end;

procedure TFormPrincipal.MenuFloodFill8Click(Sender: TObject);
const
  MarginX = 40;
  MarginY = 40;
var
  TestBitmap: TBitmap;
  destRect: TRect;
  destWidth, destHeight: Integer;
  offsetX, offsetY: Integer;
  availableW, availableH: Integer;
  borderRight, borderBottom: Integer;
  innerLeft, innerTop, innerRight, innerBottom: Integer;
  scale: Double;

  function ClampInt(Value, MinVal, MaxVal: Integer): Integer;
  begin
    if Value < MinVal then
      Result := MinVal
    else if Value > MaxVal then
      Result := MaxVal
    else
      Result := Value;
  end;

begin
  ModoAtual := moFloodFill8;
  FloodUse8 := True;
  BorderColor := clBlack;
  FillColor := clAqua;

  LimparTela;
  Image1.Canvas.Brush.Color := clWhite;
  Image1.Canvas.FillRect(Image1.ClientRect);

  availableW := Max(1, Image1.Width - (2 * MarginX));
  availableH := Max(1, Image1.Height - (2 * MarginY));

  TestBitmap := nil;
  if FileExists('Flood.bmp') then
  begin
    TestBitmap := TBitmap.Create;
    try
      TestBitmap.LoadFromFile('Flood.bmp');
      if (TestBitmap.Width > 0) and (TestBitmap.Height > 0) then
      begin
        scale := Min(availableW / TestBitmap.Width, availableH / TestBitmap.Height);
        destWidth := Max(1, Round(TestBitmap.Width * scale));
        destHeight := Max(1, Round(TestBitmap.Height * scale));
      end
      else
      begin
        destWidth := availableW;
        destHeight := availableH;
      end;
    except
      FreeAndNil(TestBitmap);
      destWidth := availableW;
      destHeight := availableH;
    end;
  end
  else
  begin
    destWidth := availableW;
    destHeight := availableH;
  end;

  offsetX := (Image1.Width - destWidth) div 2;
  offsetY := (Image1.Height - destHeight) div 2;
  destRect := Rect(offsetX, offsetY, offsetX + destWidth, offsetY + destHeight);

  if Assigned(TestBitmap) then
  begin
    try
      Image1.Canvas.StretchDraw(destRect, TestBitmap);
    finally
      TestBitmap.Free;
    end;
  end
  else
  begin
    Image1.Canvas.Brush.Style := bsClear;
    Image1.Canvas.Pen.Color := BorderColor;
    Image1.Canvas.Rectangle(destRect.Left, destRect.Top, destRect.Right, destRect.Bottom);
  end;

  Image1.Canvas.Brush.Style := bsClear;
  Image1.Canvas.Pen.Color := BorderColor;
  borderRight := ClampInt(destRect.Right - 1, 0, Image1.Width - 1);
  borderBottom := ClampInt(destRect.Bottom - 1, 0, Image1.Height - 1);
  Image1.Canvas.Rectangle(destRect.Left, destRect.Top, borderRight + 1, borderBottom + 1);
  Image1.Canvas.Brush.Style := bsSolid;

  innerLeft := ClampInt(destRect.Left + 1, 0, Image1.Width - 1);
  innerTop := ClampInt(destRect.Top + 1, 0, Image1.Height - 1);
  innerRight := ClampInt(borderRight, innerLeft + 1, Image1.Width);
  innerBottom := ClampInt(borderBottom, innerTop + 1, Image1.Height);
  FloodBounds := Rect(innerLeft, innerTop, innerRight, innerBottom);

  ShowMessage('Modo Flood Fill (vizinhança 8) ativado. Clique em um ponto dentro da região para preencher.');
end;

procedure TFormPrincipal.EdgeFillInvert(const Poly: array of TPoint);
var
  i, j, n: Integer;
  minX, maxX, minY, maxY, y: Integer;
  yTop, yBottom: Integer;
  x1, y1, x2, y2: Integer;
  t, interX: Double;
  intersections: array of Double;
  idx: Integer;
  xStart, xEnd, x: Integer;
  oldColor, r, g, b: LongInt;
  newColor: TColor;
  widthMax: Integer;

begin
  n := Length(Poly);
  if n < 3 then Exit;

  minX := Poly[0].X;
  maxX := Poly[0].X;
  minY := Poly[0].Y;
  maxY := Poly[0].Y;
  for i := 1 to n - 1 do
  begin
    if Poly[i].X < minX then minX := Poly[i].X;
    if Poly[i].X > maxX then maxX := Poly[i].X;
    if Poly[i].Y < minY then minY := Poly[i].Y;
    if Poly[i].Y > maxY then maxY := Poly[i].Y;
  end;

  if minX < 0 then minX := 0;
  if maxX > Image1.Width - 1 then maxX := Image1.Width - 1;
  if minY < 0 then minY := 0;
  if maxY > Image1.Height - 1 then maxY := Image1.Height - 1;

  widthMax := Image1.Width - 1;

  for y := minY to maxY do
  begin
    SetLength(intersections, 0);
    for i := 0 to n - 1 do
    begin
      j := (i + 1) mod n;
      x1 := Poly[i].X;
      y1 := Poly[i].Y;
      x2 := Poly[j].X;
      y2 := Poly[j].Y;

      if y1 = y2 then
        Continue;

      if y1 < y2 then
      begin
        yTop := y1;
        yBottom := y2;
      end
      else
      begin
        yTop := y2;
        yBottom := y1;
      end;

      if (y < yTop) or (y >= yBottom) then
        Continue;

      t := (y - y1) / (y2 - y1);
      interX := x1 + t * (x2 - x1);

      SetLength(intersections, Length(intersections) + 1);
      intersections[High(intersections)] := interX;
    end;

    if Length(intersections) > 1 then
      for i := 0 to High(intersections) - 1 do
        for j := i + 1 to High(intersections) do
          if intersections[i] > intersections[j] then
          begin
            t := intersections[i];
            intersections[i] := intersections[j];
            intersections[j] := t;
          end;

    idx := 0;
    while idx + 1 <= High(intersections) do
    begin
      xStart := Ceil(intersections[idx]);
      xEnd := Floor(intersections[idx + 1]);

      if xStart < minX then xStart := minX;
      if xEnd > maxX then xEnd := maxX;

      if xStart < 0 then xStart := 0;
      if xEnd > widthMax then xEnd := widthMax;

      if xStart <= xEnd then
      begin
        for x := xStart to xEnd do
        begin
          oldColor := ColorToRGB(Image1.Canvas.Pixels[x, y]);
          r := oldColor and $000000FF;
          g := (oldColor and $0000FF00) shr 8;
          b := (oldColor and $00FF0000) shr 16;
          newColor := RGBToColor(255 - r, 255 - g, 255 - b);
          Image1.Canvas.Pixels[x, y] := newColor;
        end;
      end;
      Inc(idx, 2);
    end;
  end;
end;

procedure TFormPrincipal.FloodFill(x, y: Integer; Use8: Boolean);
type
  TPointArray = array of TPoint;
var
  queue: array of TPoint;
  activeDiag: TPointArray;
  head, tail, capacity: Integer;
  pt, np: TPoint;
  w, h: Integer;
  dx4: array[0..3] of Integer = (1, -1, 0, 0);
  dy4: array[0..3] of Integer = (0, 0, 1, -1);
  dx8: array[0..7] of Integer = (1, -1, 0, 0, 1, 1, -1, -1);
  dy8: array[0..7] of Integer = (0, 0, 1, -1, 1, -1, 1, -1);
  i: Integer;
  currentColor: TColor;

  procedure EnqueuePoint(const P: TPoint; IsDiagonal: Boolean);
  begin
    if tail >= capacity then
    begin
      capacity := capacity * 2;
      SetLength(queue, capacity);
    end;
    queue[tail] := P;
    Inc(tail);

    if Use8 and IsDiagonal then
    begin
      SetLength(activeDiag, Length(activeDiag) + 1);
      activeDiag[High(activeDiag)] := P;
    end;
  end;

  function InsideBounds(const P: TPoint): Boolean; inline;
  begin
    Result :=
      (P.X >= FloodBounds.Left) and (P.X < FloodBounds.Right) and
      (P.Y >= FloodBounds.Top) and (P.Y < FloodBounds.Bottom);
  end;

begin
  w := Image1.Width;
  h := Image1.Height;

  if (FloodBounds.Right <= FloodBounds.Left) or (FloodBounds.Bottom <= FloodBounds.Top) then
    FloodBounds := Rect(0, 0, w, h);

  if not InsideBounds(Point(x, y)) then
    Exit;

  currentColor := Image1.Canvas.Pixels[x, y];
  if (ColorToRGB(currentColor) = ColorToRGB(BorderColor)) or
     (ColorToRGB(currentColor) = ColorToRGB(FillColor)) then
    Exit;

  capacity := 64;
  if capacity < 1 then
    capacity := 1;
  SetLength(queue, capacity);
  SetLength(activeDiag, 0);

  head := 0;
  tail := 0;

  Image1.Canvas.Pixels[x, y] := FillColor;
  queue[tail] := Point(x, y);
  Inc(tail);

  while head < tail do
  begin
    pt := queue[head];
    Inc(head);

    if Use8 then
    begin
      for i := 0 to 7 do
      begin
        np := Point(pt.X + dx8[i], pt.Y + dy8[i]);
        if InsideBounds(np) then
        begin
          currentColor := Image1.Canvas.Pixels[np.X, np.Y];
          if (ColorToRGB(currentColor) <> ColorToRGB(BorderColor)) and
             (ColorToRGB(currentColor) <> ColorToRGB(FillColor)) then
          begin
            Image1.Canvas.Pixels[np.X, np.Y] := FillColor;
            EnqueuePoint(np, (Abs(dx8[i]) = 1) and (Abs(dy8[i]) = 1));
          end;
        end;
      end;
    end
    else
    begin
      for i := 0 to 3 do
      begin
        np := Point(pt.X + dx4[i], pt.Y + dy4[i]);
        if InsideBounds(np) then
        begin
          currentColor := Image1.Canvas.Pixels[np.X, np.Y];
          if (ColorToRGB(currentColor) <> ColorToRGB(BorderColor)) and
             (ColorToRGB(currentColor) <> ColorToRGB(FillColor)) then
          begin
            Image1.Canvas.Pixels[np.X, np.Y] := FillColor;
            EnqueuePoint(np, False);
          end;
        end;
      end;
    end;
  end;

  if Use8 and (Length(activeDiag) > 0) then
    for i := 0 to High(activeDiag) do
      if InsideBounds(activeDiag[i]) and
         (ColorToRGB(Image1.Canvas.Pixels[activeDiag[i].X, activeDiag[i].Y]) = ColorToRGB(FillColor)) then
        Image1.Canvas.Pixels[activeDiag[i].X, activeDiag[i].Y] := clFuchsia;
end;

procedure TFormPrincipal.MenuSairClick(Sender: TObject);
begin
  Application.Terminate;
end;

{ Funcionalidades de CÃ­rculos }

procedure TFormPrincipal.MenuCirculoEquacaoPadraoClick(Sender: TObject);
begin
  ModoAtual := moCirculos;
  LimparTela;
  DesenharCirculoEquacaoPadrao;
  ShowMessage('CÃ­rculo desenhado usando a equaÃ§Ã£o padrÃ£o xÂ²+yÂ²=RÂ²');
end;

procedure TFormPrincipal.MenuCirculoParametricaClick(Sender: TObject);
begin
  ModoAtual := moCirculos;
  LimparTela;
  DesenharCirculoParametrica;
  ShowMessage('CÃ­rculo desenhado usando a equaÃ§Ã£o paramÃ©trica');
end;

procedure TFormPrincipal.MenuCirculoRotacaoClick(Sender: TObject);
begin
  ModoAtual := moCirculos;
  LimparTela;
  DesenharCirculoRotacao;
  ShowMessage('CÃ­rculo desenhado usando rotaÃ§Ãµes sucessivas');
end;

procedure TFormPrincipal.MenuCirculoBresenhamClick(Sender: TObject);
begin
  ModoAtual := moBresenham;
  EstadoBresenham := 0;
  IsDefiningCircle := False;
  LimparTela;
  ShowMessage('Algoritmo de Bresenham para CircunferÃªncias ativado.' + sLineBreak + 
              '1. Clique para definir o centro da circunferÃªncia' + sLineBreak +
              '2. Clique novamente para definir o raio');
end;

procedure TFormPrincipal.DesenharCirculoEquacaoPadrao;
var
  xc, yc, R: Integer;
  x, y: Integer;
begin
  // Define o centro (xc, yc) e o Raio (R)
  xc := Image1.Width div 2;
  yc := Image1.Height div 2;
  R := 150;

  // Loop de x=-R atÃ© R
  for x := -R to R do
  begin
    // Calcula y = sqrt(RÂ² - xÂ²)
    y := Round(sqrt(R*R - x*x));

    // Desenha os pixels, transladando pro centro (xc,yc)
    Image1.Canvas.Pixels[xc + x, yc + y] := clRed;
    Image1.Canvas.Pixels[xc + x, yc - y] := clRed;
  end;
end;

procedure TFormPrincipal.DesenharCirculoParametrica;
var
  xc, yc, R: Integer;
  x, y: Real;
  a_rad: Real;
  i: integer;
begin
  // Define o centro (xc, yc) e o Raio (R)
  xc := Image1.Width div 2;
  yc := Image1.Height div 2;
  R := 150;

  // O loop vai de 0 a 6.28 (2*Pi)
  // Usamos um passo pequeno para garantir que a circunferÃªncia nÃ£o fique falhada
  for i := 0 to round(2 * Pi * 100) do
  begin
    a_rad := i / 100; // Converte o contador do loop para o Ã¢ngulo em radianos

    // Calcula x = r*cos(a) e y = r*sin(a)
    x := R * cos(a_rad);
    y := R * sin(a_rad);

    // Desenha o pixel, transladando para o centro (xc, yc)
    Image1.Canvas.Pixels[xc + Round(x), yc + Round(y)] := clBlue;
  end;
end;

procedure TFormPrincipal.DesenharCirculoRotacao;
var
  xc, yc, R, i: Integer;
  x, y, xn: Real;
  cos1, sen1: Real;
  angulo_rad: Real;
begin
  // Define o centro (xc, yc) e o Raio (R)
  xc := Image1.Width div 2;
  yc := Image1.Height div 2;
  R := 150;

  // Ponto inicial
  x := R;
  y := 0;

  // As funÃ§Ãµes cos() e sin() em Pascal usam radianos.
  // Convertemos 1 grau para radianos.
  angulo_rad := 1 * Pi / 180.0;
  cos1 := cos(angulo_rad);
  sen1 := sin(angulo_rad);

  // Loop de 360 passos (um para cada grau)
  for i := 1 to 360 do
  begin
    // Desenha o pixel atual, transladando para o centro (xc, yc)
    Image1.Canvas.Pixels[xc + Round(x), yc + Round(y)] := clGreen;

    // Aplica a fÃ³rmula de rotaÃ§Ã£o para encontrar o prÃ³ximo ponto
    xn := x * cos1 - y * sen1;
    y := x * sen1 + y * cos1;
    x := xn;
  end;
end;

// MÃ©todo auxiliar para desenhar os 8 pontos simÃ©tricos da circunferÃªncia
procedure TFormPrincipal.PlotPixelCircle(xc, yc, x, y: Integer; Cor: TColor);
begin
  Image1.Canvas.Pixels[xc + x, yc + y] := Cor;  // Octante 1
  Image1.Canvas.Pixels[xc - x, yc + y] := Cor;  // Octante 2
  Image1.Canvas.Pixels[xc + x, yc - y] := Cor;  // Octante 8
  Image1.Canvas.Pixels[xc - x, yc - y] := Cor;  // Octante 7
  Image1.Canvas.Pixels[xc + y, yc + x] := Cor;  // Octante 3
  Image1.Canvas.Pixels[xc - y, yc + x] := Cor;  // Octante 4
  Image1.Canvas.Pixels[xc + y, yc - x] := Cor;  // Octante 6
  Image1.Canvas.Pixels[xc - y, yc - x] := Cor;  // Octante 5
end;

// ImplementaÃ§Ã£o do Algoritmo de Bresenham para CircunferÃªncias
procedure TFormPrincipal.DesenharCirculoBresenham(xc, yc, R: Integer);
var
  x, y: Integer;
  dE, dSE: Integer;
begin
  x := 0;
  y := R;
  dE := 3;
  dSE := -2 * R + 5;
  
  // Desenha o primeiro conjunto de pontos
  PlotPixelCircle(xc, yc, x, y, clPurple);
  
  while (x < y) do
  begin
    if (dE < 0) then
    begin
      // Seleciona E
      dE := dE + 2 * x + 3;
      dSE := dSE + 2 * x + 2;
    end
    else
    begin
      // Seleciona SE
      dE := dE + 2 * x - 2 * y + 5;
      dSE := dSE + 2 * x - 2 * y + 4;
      y := y - 1;
    end;
    x := x + 1;
    PlotPixelCircle(xc, yc, x, y, clPurple);
  end;
end;

{ Funcionalidades de Linhas }

procedure TFormPrincipal.MenuLinhaSimplesClick(Sender: TObject);
begin
  ModoAtual := moLinhas;
  LimparTela;
  ShowMessage('Modo de desenho de linhas simples ativado. Clique e arraste para desenhar uma linha.');
end;

procedure TFormPrincipal.MenuLinhaEquacaoGeralClick(Sender: TObject);
begin
  ModoAtual := moLinhaEquacaoGeral;
  LimparTela;
  ShowMessage('Desenho de linha por EquaÃ§Ã£o Geral ativado.' + sLineBreak + 
              'Clique e arraste para definir dois pontos da linha.');
end;

procedure TFormPrincipal.MenuLinhaParametricaClick(Sender: TObject);
begin
  ModoAtual := moLinhaParametrica;
  LimparTela;
  ShowMessage('Desenho de linha por EquaÃ§Ã£o ParamÃ©trica ativado.' + sLineBreak + 
              'Clique e arraste para definir dois pontos da linha.');
end;



procedure TFormPrincipal.DesenharLinhaSimples(x1, y1, x2, y2: Integer; Cor: TColor);
begin
  Image1.Canvas.Pen.Color := Cor;
  Image1.Canvas.MoveTo(x1, y1);
  Image1.Canvas.LineTo(x2, y2);
end;

// Desenho de linha usando EquaÃ§Ã£o Geral: Ax + By + C = 0
procedure TFormPrincipal.DesenharLinhaEquacaoGeral(x1, y1, x2, y2: Integer);
var
  A, B, C: Real;
  x, y: Integer;
  minX, maxX, minY, maxY: Integer;
begin
  // Calcula os coeficientes da equaÃ§Ã£o geral
  A := y2 - y1;
  B := x1 - x2;
  C := x2 * y1 - x1 * y2;
  
  // Define os limites para desenho
  minX := Min(x1, x2) - 50;
  maxX := Max(x1, x2) + 50;
  minY := Min(y1, y2) - 50;
  maxY := Max(y1, y2) + 50;
  
  // Garante que estÃ¡ dentro dos limites da tela
  minX := Max(0, minX);
  maxX := Min(Image1.Width - 1, maxX);
  minY := Max(0, minY);
  maxY := Min(Image1.Height - 1, maxY);
  
  // Se a linha Ã© mais horizontal que vertical
  if Abs(A) <= Abs(B) then
  begin
    // Percorre x e calcula y usando: y = -(Ax + C)/B
    if B <> 0 then
    begin
      for x := minX to maxX do
      begin
        y := Round(-(A * x + C) / B);
        if (y >= minY) and (y <= maxY) then
          Image1.Canvas.Pixels[x, y] := clBlue;
      end;
    end;
  end
  else
  begin
    // Percorre y e calcula x usando: x = -(By + C)/A
    if A <> 0 then
    begin
      for y := minY to maxY do
      begin
        x := Round(-(B * y + C) / A);
        if (x >= minX) and (x <= maxX) then
          Image1.Canvas.Pixels[x, y] := clBlue;
      end;
    end;
  end;
  
  // Marca os pontos de referÃªncia
  Image1.Canvas.Brush.Color := clRed;
  Image1.Canvas.Ellipse(x1-2, y1-2, x1+2, y1+2);
  Image1.Canvas.Ellipse(x2-2, y2-2, x2+2, y2+2);
  Image1.Canvas.Brush.Color := clWhite;
end;

// Desenho de linha usando EquaÃ§Ã£o ParamÃ©trica: P(t) = P1 + t(P2 - P1)
procedure TFormPrincipal.DesenharLinhaParametrica(x1, y1, x2, y2: Integer);
var
  t: Real;
  x, y: Integer;
  dx, dy: Integer;
  passos: Integer;
  i: Integer;
begin
  dx := x2 - x1;
  dy := y2 - y1;
  
  // Calcula o nÃºmero de passos baseado na maior distÃ¢ncia
  passos := Max(Abs(dx), Abs(dy));
  
  // Estende a linha para alÃ©m dos pontos definidos
  passos := passos * 3;
  
  // Desenha a linha usando equaÃ§Ã£o paramÃ©trica
  for i := -passos to passos * 2 do
  begin
    t := i / passos;
    
    // P(t) = P1 + t(P2 - P1)
    x := Round(x1 + t * dx);
    y := Round(y1 + t * dy);
    
    // Verifica se estÃ¡ dentro dos limites da tela
    if (x >= 0) and (x < Image1.Width) and (y >= 0) and (y < Image1.Height) then
      Image1.Canvas.Pixels[x, y] := clGreen;
  end;
  
  // Marca os pontos de referÃªncia
  Image1.Canvas.Brush.Color := clRed;
  Image1.Canvas.Ellipse(x1-2, y1-2, x1+2, y1+2);
  Image1.Canvas.Ellipse(x2-2, y2-2, x2+2, y2+2);
  Image1.Canvas.Brush.Color := clWhite;
end;

{ Funcionalidades Cohen-Sutherland }

procedure TFormPrincipal.MenuCohenSutherlandClick(Sender: TObject);
begin
  // SÃ³ ativa se foi um clique real (nÃ£o hover)
  CohenAtivadoPorClick := True;
  ModoAtual := moCohenSutherland;
  EstadoCohen := 0;
  LimparTela;
  ShowMessage('Algoritmo de Recorte de Retas ativado (Cohen-Sutherland).' + sLineBreak + 
              '1. Primeiro defina a janela de visualizaÃ§Ã£o (clique e arraste)' + sLineBreak +
              '2. Depois defina a linha para recorte (clique e arraste)');
end;

procedure TFormPrincipal.MenuResetarCohenClick(Sender: TObject);
begin
  if ModoAtual = moCohenSutherland then
  begin
    EstadoCohen := 0;
    CohenAtivadoPorClick := True; // MantÃ©m ativado apÃ³s reset
    LimparTela;
    ShowMessage('Algoritmo resetado. Defina novamente a janela de visualizaÃ§Ã£o.');
  end;
end;

procedure TFormPrincipal.DesenhaCohenSutherland;
begin
  Image1.Canvas.Brush.Color := clWhite;
  Image1.Canvas.FillRect(Image1.ClientRect);

  // Desenha a Janela de VisualizaÃ§Ã£o
  if EstadoCohen > 0 then
  begin
    Image1.Canvas.Pen.Color := clBlue;
    Image1.Canvas.Rectangle(JanelaMin.X, JanelaMin.Y, JanelaMax.X, JanelaMax.Y);
  end;
end;

procedure TFormPrincipal.CohenSutherland(var x1, y1, x2, y2: Double; xmin, ymin, xmax, ymax: Double);
  // FunÃ§Ã£o para calcular o cÃ³digo de 4 bits (outcode)
  function CalculaOutcode(x, y: Double): Byte;
  begin
    Result := 0;
    if y > ymax then Result := Result or 1; //Ponto estÃ¡ ACIMA da janela
    if y < ymin then Result := Result or 2; //ABAIXO
    if x > xmax then Result := Result or 4; //Ã  Direita
    if x < xmin then Result := Result or 8; //Ã  Esquerda
  end;

var
  Outcode1, Outcode2, OutcodeForClipping: Byte;
  aceito, feito: Boolean;
  x, y: Double;
begin
  aceito := False;
  feito := False;

  repeat
    Outcode1 := CalculaOutcode(x1, y1);
    Outcode2 := CalculaOutcode(x2, y2);

    if (Outcode1 = 0) and (Outcode2 = 0) then
    begin
      // Aceite trivial: a linha inteira estÃ¡ dentro da janela
      aceito := True;
      feito := True;
    end
    else if (Outcode1 and Outcode2) <> 0 then
    begin
      // Rejeite trivial: a linha estÃ¡ completamente fora
      feito := True;
    end
    else
    begin
      // Recorte
      OutcodeForClipping := Outcode1;
      if OutcodeForClipping = 0 then
        OutcodeForClipping := Outcode2;

      // Calcula as coordenadas de interseÃ§Ã£o
      if (OutcodeForClipping and 1) <> 0 then // Acima
      begin
        x := x1 + (x2 - x1) * (ymax - y1) / (y2 - y1);
        y := ymax;
      end
      else if (OutcodeForClipping and 2) <> 0 then // Abaixo
      begin
        x := x1 + (x2 - x1) * (ymin - y1) / (y2 - y1);
        y := ymin;
      end
      else if (OutcodeForClipping and 4) <> 0 then // Direita
      begin
        y := y1 + (y2 - y1) * (xmax - x1) / (x2 - x1);
        x := xmax;
      end
      else if (OutcodeForClipping and 8) <> 0 then // Esquerda
      begin
        y := y1 + (y2 - y1) * (xmin - x1) / (x2 - x1);
        x := xmin;
      end;

      // Atualiza o ponto que estava fora da janela
      if OutcodeForClipping = Outcode1 then
      begin
        x1 := x;
        y1 := y;
      end
      else
      begin
        x2 := x;
        y2 := y;
      end;
    end;
  until feito;

  if aceito then
  begin
    // Desenha a parte visÃ­vel da linha
    Image1.Canvas.Pen.Color := clRed;
    Image1.Canvas.MoveTo(Round(x1), Round(y1));
    Image1.Canvas.LineTo(Round(x2), Round(y2));
  end;
end;

{ Eventos de Mouse }

procedure TFormPrincipal.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case ModoAtual of
    moLinhas, moLinhaEquacaoGeral, moLinhaParametrica:
      if Button = mbLeft then
      begin
        StartPoint := Point(X, Y);
        EndPoint := StartPoint;
        IsDrawingLine := True;
      end;
      
    moCohenSutherland:
      if CohenAtivadoPorClick then
      begin
        if EstadoCohen = 0 then
        begin
          JanelaMin := Point(X, Y);
        end
        else if EstadoCohen = 1 then
        begin
          LinhaStart := Point(X, Y);
          LinhaEnd := LinhaStart;
        end;
      end;
      
    moBresenham:
      if Button = mbLeft then
      begin
        if EstadoBresenham = 0 then
        begin
          // Define o centro da circunferÃªncia
          CentroCirculo := Point(X, Y);
          EstadoBresenham := 1;
          IsDefiningCircle := True;
          
          // Desenha um pequeno marcador no centro
          Image1.Canvas.Brush.Color := clRed;
          Image1.Canvas.Ellipse(X-3, Y-3, X+3, Y+3);
          Image1.Canvas.Brush.Color := clWhite;
          
          ShowMessage('Centro definido! Agora clique em outro ponto para definir o raio.');
        end;
      end;

    moEdgeFill:
      begin
        // O desenho já foi realizado ao selecionar o menu.
      end;

    moFloodFill4, moFloodFill8:
      if Button = mbLeft then
        FloodFill(X, Y, FloodUse8);
  end;
end;

procedure TFormPrincipal.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Raio: Integer;
begin
  case ModoAtual of
    moLinhas:
      if IsDrawingLine and (ssLeft in Shift) then
      begin
        // Redesenhar tudo para mostrar linha temporÃ¡ria
        LimparTela;
        
        // Desenhar linha temporÃ¡ria em cinza
        Image1.Canvas.Pen.Color := clGray;
        Image1.Canvas.MoveTo(StartPoint.X, StartPoint.Y);
        Image1.Canvas.LineTo(X, Y);
        
        EndPoint := Point(X, Y);
      end;
      
    moLinhaEquacaoGeral, moLinhaParametrica:
      if IsDrawingLine and (ssLeft in Shift) then
      begin
        // Redesenhar tudo para mostrar linha temporÃ¡ria
        LimparTela;
        
        // Desenhar linha temporÃ¡ria em cinza
        Image1.Canvas.Pen.Color := clGray;
        Image1.Canvas.MoveTo(StartPoint.X, StartPoint.Y);
        Image1.Canvas.LineTo(X, Y);
        
        EndPoint := Point(X, Y);
      end;
      
    moCohenSutherland:
      if CohenAtivadoPorClick then
      begin
        if EstadoCohen = 0 then
        begin
          // Desenha a janela temporÃ¡ria enquanto o usuÃ¡rio arrasta
          if ssLeft in Shift then
          begin
            LimparTela;
            Image1.Canvas.Pen.Color := clBlue;
            Image1.Canvas.Rectangle(JanelaMin.X, JanelaMin.Y, X, Y);
          end;
        end
        else if EstadoCohen = 1 then
        begin
          // Desenha a linha temporÃ¡ria enquanto o usuÃ¡rio arrasta
          if ssLeft in Shift then
          begin
            DesenhaCohenSutherland;
            Image1.Canvas.Pen.Color := clGray;
            Image1.Canvas.MoveTo(LinhaStart.X, LinhaStart.Y);
            Image1.Canvas.LineTo(X, Y);
          end;
        end;
      end;
      
    moBresenham:
      if (EstadoBresenham = 1) and IsDefiningCircle then
      begin
        // Mostra prÃ©via da circunferÃªncia enquanto o usuÃ¡rio move o mouse
        LimparTela;
        
        // Redesenha o marcador do centro
        Image1.Canvas.Brush.Color := clRed;
        Image1.Canvas.Ellipse(CentroCirculo.X-3, CentroCirculo.Y-3, 
                             CentroCirculo.X+3, CentroCirculo.Y+3);
        Image1.Canvas.Brush.Color := clWhite;
        
        // Calcula o raio baseado na distÃ¢ncia do centro atÃ© a posiÃ§Ã£o atual do mouse
        Raio := Round(sqrt(sqr(X - CentroCirculo.X) + sqr(Y - CentroCirculo.Y)));
        
        if Raio > 0 then
        begin
          // Desenha prÃ©via da circunferÃªncia em cinza claro
          Image1.Canvas.Pen.Color := clSilver;
          Image1.Canvas.Brush.Style := bsClear;
          Image1.Canvas.Ellipse(CentroCirculo.X - Raio, CentroCirculo.Y - Raio,
                               CentroCirculo.X + Raio, CentroCirculo.Y + Raio);
          Image1.Canvas.Brush.Style := bsSolid;
        end;
      end;
  end;
end;

procedure TFormPrincipal.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Temp: Integer;
  x1, y1, x2, y2: Double;
  Raio: Integer;
begin
  case ModoAtual of
    moLinhas:
      if IsDrawingLine and (Button = mbLeft) then
      begin
        EndPoint := Point(X, Y);
        IsDrawingLine := False;
        
        // Limpar tela e desenhar linha final
        LimparTela;
        DesenharLinhaSimples(StartPoint.X, StartPoint.Y, EndPoint.X, EndPoint.Y, clBlack);
      end;
      
    moLinhaEquacaoGeral:
      if IsDrawingLine and (Button = mbLeft) then
      begin
        EndPoint := Point(X, Y);
        IsDrawingLine := False;
        
        // Limpar tela e desenhar linha usando equaÃ§Ã£o geral
        LimparTela;
        DesenharLinhaEquacaoGeral(StartPoint.X, StartPoint.Y, EndPoint.X, EndPoint.Y);
        ShowMessage('Linha desenhada usando EquaÃ§Ã£o Geral (Ax + By + C = 0)' + sLineBreak +
                   'Pontos: (' + IntToStr(StartPoint.X) + ', ' + IntToStr(StartPoint.Y) + ') a (' +
                   IntToStr(EndPoint.X) + ', ' + IntToStr(EndPoint.Y) + ')');
      end;
      
    moLinhaParametrica:
      if IsDrawingLine and (Button = mbLeft) then
      begin
        EndPoint := Point(X, Y);
        IsDrawingLine := False;
        
        // Limpar tela e desenhar linha usando equaÃ§Ã£o paramÃ©trica
        LimparTela;
        DesenharLinhaParametrica(StartPoint.X, StartPoint.Y, EndPoint.X, EndPoint.Y);
        ShowMessage('Linha desenhada usando EquaÃ§Ã£o ParamÃ©trica P(t) = P1 + t(P2 - P1)' + sLineBreak +
                   'Pontos: (' + IntToStr(StartPoint.X) + ', ' + IntToStr(StartPoint.Y) + ') a (' +
                   IntToStr(EndPoint.X) + ', ' + IntToStr(EndPoint.Y) + ')');
      end;
      
    moCohenSutherland:
      if CohenAtivadoPorClick then
      begin
        if EstadoCohen = 0 then
        begin
          JanelaMax := Point(X, Y);

          if JanelaMin.X > JanelaMax.X then
          begin
            Temp := JanelaMin.X;
            JanelaMin.X := JanelaMax.X;
            JanelaMax.X := Temp;
          end;

          if JanelaMin.Y > JanelaMax.Y then
          begin
            Temp := JanelaMin.Y;
            JanelaMin.Y := JanelaMax.Y;
            JanelaMax.Y := Temp;
          end;

          EstadoCohen := 1;
          DesenhaCohenSutherland;
          ShowMessage('Janela definida. Agora desenhe a linha para aplicar o algoritmo de recorte.');
        end
        else if EstadoCohen = 1 then
        begin
          LinhaEnd := Point(X, Y);

          x1 := LinhaStart.X;
          y1 := LinhaStart.Y;
          x2 := LinhaEnd.X;
          y2 := LinhaEnd.Y;

          DesenhaCohenSutherland;
          
          // Desenha linha original em cinza claro
          Image1.Canvas.Pen.Color := clSilver;
          Image1.Canvas.MoveTo(LinhaStart.X, LinhaStart.Y);
          Image1.Canvas.LineTo(LinhaEnd.X, LinhaEnd.Y);

          CohenSutherland(x1, y1, x2, y2, JanelaMin.X, JanelaMin.Y, JanelaMax.X, JanelaMax.Y);
        end;
      end;
      
    moBresenham:
      if (EstadoBresenham = 1) and IsDefiningCircle and (Button = mbLeft) then
      begin
        // Calcula o raio final
        Raio := Round(sqrt(sqr(X - CentroCirculo.X) + sqr(Y - CentroCirculo.Y)));
        
        if Raio > 0 then
        begin
          // Limpa a tela e desenha a circunferÃªncia final usando Bresenham
          LimparTela;
          
          // Redesenha o marcador do centro
          Image1.Canvas.Brush.Color := clRed;
          Image1.Canvas.Ellipse(CentroCirculo.X-3, CentroCirculo.Y-3, 
                               CentroCirculo.X+3, CentroCirculo.Y+3);
          Image1.Canvas.Brush.Color := clWhite;
          
          // Desenha a circunferÃªncia usando o algoritmo de Bresenham
          DesenharCirculoBresenham(CentroCirculo.X, CentroCirculo.Y, Raio);
          
          ShowMessage('CircunferÃªncia desenhada usando o Algoritmo de Bresenham!' + sLineBreak +
                     'Centro: (' + IntToStr(CentroCirculo.X) + ', ' + IntToStr(CentroCirculo.Y) + ')' + sLineBreak +
                     'Raio: ' + IntToStr(Raio) + ' pixels');
        end;
        
        // Reset para permitir desenhar outra circunferÃªncia
        EstadoBresenham := 0;
        IsDefiningCircle := False;
      end;
  end;
end;

end.






