unit UnitPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, Math;

type
  // Enumeração para os modos de funcionamento
  TModoOperacao = (moCirculos, moLinhas, moLinhaEquacaoGeral, moLinhaParametrica, moCohenSutherland, moBresenham, moNenhum);

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
  private
    ModoAtual: TModoOperacao;
    
    // Variáveis para desenho de linhas
    StartPoint, EndPoint: TPoint;
    IsDrawingLine: Boolean;
    
    // Variáveis para Cohen-Sutherland
    JanelaMin, JanelaMax: TPoint;
    LinhaStart, LinhaEnd: TPoint;
    EstadoCohen: Integer; // 0: Definir Janela, 1: Definir Linha
    CohenAtivadoPorClick: Boolean; // Controle para garantir ativação apenas por clique
    
    // Variáveis para Bresenham
    CentroCirculo: TPoint;
    IsDefiningCircle: Boolean;
    EstadoBresenham: Integer; // 0: Definir Centro, 1: Definir Raio
    
    // Métodos auxiliares
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
  // Configurar o formulário
  Caption := 'Projeto Integrado - Computação Gráfica';
  WindowState := wsNormal;
  
  // Configurar nomes dos menus
  MenuFuncionalidades.Caption := 'Funcionalidades';
  MenuCirculos.Caption := 'Círculos';
  MenuCirculoEquacaoPadrao.Caption := 'Circunf. por Equação Padrão';
  MenuCirculoParametrica.Caption := 'Circunf. por Eq. Paramétrica';
  MenuCirculoRotacao.Caption := 'Circunf. por Rotação';
  MenuCirculoBresenham.Caption := 'Circunf. por Bresenham';
  MenuLinhas.Caption := 'Linhas';
  MenuLinhaSimples.Caption := 'Desenhar Linhas (Simples)';
  MenuLinhaEquacaoGeral.Caption := 'Linha por Equação Geral';
  MenuLinhaParametrica.Caption := 'Linha por Eq. Paramétrica';
  MenuAlgoritmosRecorte.Caption := 'Algoritmos de Recorte de Retas';
  MenuCohenSutherland.Caption := 'Cohen-Sutherland';
  MenuLiangBarsky.Caption := 'Liang-Barsky (Em breve)';
  MenuResetarCohen.Caption := 'Resetar Algoritmo';
  MenuLimpar.Caption := 'Limpar Tela';
  MenuSair.Caption := 'Sair';
  
  // Inicializar variáveis
  ModoAtual := moNenhum;
  IsDrawingLine := False;
  EstadoCohen := 0;
  CohenAtivadoPorClick := False;
  IsDefiningCircle := False;
  EstadoBresenham := 0;
  
  // Limpa e prepara a tela
  LimparTela;
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
end;

procedure TFormPrincipal.MenuSairClick(Sender: TObject);
begin
  Application.Terminate;
end;

{ Funcionalidades de Círculos }

procedure TFormPrincipal.MenuCirculoEquacaoPadraoClick(Sender: TObject);
begin
  ModoAtual := moCirculos;
  LimparTela;
  DesenharCirculoEquacaoPadrao;
  ShowMessage('Círculo desenhado usando a equação padrão x²+y²=R²');
end;

procedure TFormPrincipal.MenuCirculoParametricaClick(Sender: TObject);
begin
  ModoAtual := moCirculos;
  LimparTela;
  DesenharCirculoParametrica;
  ShowMessage('Círculo desenhado usando a equação paramétrica');
end;

procedure TFormPrincipal.MenuCirculoRotacaoClick(Sender: TObject);
begin
  ModoAtual := moCirculos;
  LimparTela;
  DesenharCirculoRotacao;
  ShowMessage('Círculo desenhado usando rotações sucessivas');
end;

procedure TFormPrincipal.MenuCirculoBresenhamClick(Sender: TObject);
begin
  ModoAtual := moBresenham;
  EstadoBresenham := 0;
  IsDefiningCircle := False;
  LimparTela;
  ShowMessage('Algoritmo de Bresenham para Circunferências ativado.' + #13#10 + 
              '1. Clique para definir o centro da circunferência' + #13#10 +
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

  // Loop de x=-R até R
  for x := -R to R do
  begin
    // Calcula y = sqrt(R² - x²)
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
  // Usamos um passo pequeno para garantir que a circunferência não fique falhada
  for i := 0 to round(2 * Pi * 100) do
  begin
    a_rad := i / 100; // Converte o contador do loop para o ângulo em radianos

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

  // As funções cos() e sin() em Pascal usam radianos.
  // Convertemos 1 grau para radianos.
  angulo_rad := 1 * Pi / 180.0;
  cos1 := cos(angulo_rad);
  sen1 := sin(angulo_rad);

  // Loop de 360 passos (um para cada grau)
  for i := 1 to 360 do
  begin
    // Desenha o pixel atual, transladando para o centro (xc, yc)
    Image1.Canvas.Pixels[xc + Round(x), yc + Round(y)] := clGreen;

    // Aplica a fórmula de rotação para encontrar o próximo ponto
    xn := x * cos1 - y * sen1;
    y := x * sen1 + y * cos1;
    x := xn;
  end;
end;

// Método auxiliar para desenhar os 8 pontos simétricos da circunferência
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

// Implementação do Algoritmo de Bresenham para Circunferências
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
  ShowMessage('Desenho de linha por Equação Geral ativado.' + #13#10 + 
              'Clique e arraste para definir dois pontos da linha.');
end;

procedure TFormPrincipal.MenuLinhaParametricaClick(Sender: TObject);
begin
  ModoAtual := moLinhaParametrica;
  LimparTela;
  ShowMessage('Desenho de linha por Equação Paramétrica ativado.' + #13#10 + 
              'Clique e arraste para definir dois pontos da linha.');
end;



procedure TFormPrincipal.DesenharLinhaSimples(x1, y1, x2, y2: Integer; Cor: TColor);
begin
  Image1.Canvas.Pen.Color := Cor;
  Image1.Canvas.MoveTo(x1, y1);
  Image1.Canvas.LineTo(x2, y2);
end;

// Desenho de linha usando Equação Geral: Ax + By + C = 0
procedure TFormPrincipal.DesenharLinhaEquacaoGeral(x1, y1, x2, y2: Integer);
var
  A, B, C: Real;
  x, y: Integer;
  minX, maxX, minY, maxY: Integer;
begin
  // Calcula os coeficientes da equação geral
  A := y2 - y1;
  B := x1 - x2;
  C := x2 * y1 - x1 * y2;
  
  // Define os limites para desenho
  minX := Min(x1, x2) - 50;
  maxX := Max(x1, x2) + 50;
  minY := Min(y1, y2) - 50;
  maxY := Max(y1, y2) + 50;
  
  // Garante que está dentro dos limites da tela
  minX := Max(0, minX);
  maxX := Min(Image1.Width - 1, maxX);
  minY := Max(0, minY);
  maxY := Min(Image1.Height - 1, maxY);
  
  // Se a linha é mais horizontal que vertical
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
  
  // Marca os pontos de referência
  Image1.Canvas.Brush.Color := clRed;
  Image1.Canvas.Ellipse(x1-2, y1-2, x1+2, y1+2);
  Image1.Canvas.Ellipse(x2-2, y2-2, x2+2, y2+2);
  Image1.Canvas.Brush.Color := clWhite;
end;

// Desenho de linha usando Equação Paramétrica: P(t) = P1 + t(P2 - P1)
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
  
  // Calcula o número de passos baseado na maior distância
  passos := Max(Abs(dx), Abs(dy));
  
  // Estende a linha para além dos pontos definidos
  passos := passos * 3;
  
  // Desenha a linha usando equação paramétrica
  for i := -passos to passos * 2 do
  begin
    t := i / passos;
    
    // P(t) = P1 + t(P2 - P1)
    x := Round(x1 + t * dx);
    y := Round(y1 + t * dy);
    
    // Verifica se está dentro dos limites da tela
    if (x >= 0) and (x < Image1.Width) and (y >= 0) and (y < Image1.Height) then
      Image1.Canvas.Pixels[x, y] := clGreen;
  end;
  
  // Marca os pontos de referência
  Image1.Canvas.Brush.Color := clRed;
  Image1.Canvas.Ellipse(x1-2, y1-2, x1+2, y1+2);
  Image1.Canvas.Ellipse(x2-2, y2-2, x2+2, y2+2);
  Image1.Canvas.Brush.Color := clWhite;
end;

{ Funcionalidades Cohen-Sutherland }

procedure TFormPrincipal.MenuCohenSutherlandClick(Sender: TObject);
begin
  // Só ativa se foi um clique real (não hover)
  CohenAtivadoPorClick := True;
  ModoAtual := moCohenSutherland;
  EstadoCohen := 0;
  LimparTela;
  ShowMessage('Algoritmo de Recorte de Retas ativado (Cohen-Sutherland).' + #13#10 + 
              '1. Primeiro defina a janela de visualização (clique e arraste)' + #13#10 +
              '2. Depois defina a linha para recorte (clique e arraste)');
end;

procedure TFormPrincipal.MenuResetarCohenClick(Sender: TObject);
begin
  if ModoAtual = moCohenSutherland then
  begin
    EstadoCohen := 0;
    CohenAtivadoPorClick := True; // Mantém ativado após reset
    LimparTela;
    ShowMessage('Algoritmo resetado. Defina novamente a janela de visualização.');
  end;
end;

procedure TFormPrincipal.DesenhaCohenSutherland;
begin
  Image1.Canvas.Brush.Color := clWhite;
  Image1.Canvas.FillRect(Image1.ClientRect);

  // Desenha a Janela de Visualização
  if EstadoCohen > 0 then
  begin
    Image1.Canvas.Pen.Color := clBlue;
    Image1.Canvas.Rectangle(JanelaMin.X, JanelaMin.Y, JanelaMax.X, JanelaMax.Y);
  end;
end;

procedure TFormPrincipal.CohenSutherland(var x1, y1, x2, y2: Double; xmin, ymin, xmax, ymax: Double);
  // Função para calcular o código de 4 bits (outcode)
  function CalculaOutcode(x, y: Double): Byte;
  begin
    Result := 0;
    if y > ymax then Result := Result or 1; //Ponto está ACIMA da janela
    if y < ymin then Result := Result or 2; //ABAIXO
    if x > xmax then Result := Result or 4; //à Direita
    if x < xmin then Result := Result or 8; //à Esquerda
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
      // Aceite trivial: a linha inteira está dentro da janela
      aceito := True;
      feito := True;
    end
    else if (Outcode1 and Outcode2) <> 0 then
    begin
      // Rejeite trivial: a linha está completamente fora
      feito := True;
    end
    else
    begin
      // Recorte
      OutcodeForClipping := Outcode1;
      if OutcodeForClipping = 0 then
        OutcodeForClipping := Outcode2;

      // Calcula as coordenadas de interseção
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
    // Desenha a parte visível da linha
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
          // Define o centro da circunferência
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
        // Redesenhar tudo para mostrar linha temporária
        LimparTela;
        
        // Desenhar linha temporária em cinza
        Image1.Canvas.Pen.Color := clGray;
        Image1.Canvas.MoveTo(StartPoint.X, StartPoint.Y);
        Image1.Canvas.LineTo(X, Y);
        
        EndPoint := Point(X, Y);
      end;
      
    moLinhaEquacaoGeral, moLinhaParametrica:
      if IsDrawingLine and (ssLeft in Shift) then
      begin
        // Redesenhar tudo para mostrar linha temporária
        LimparTela;
        
        // Desenhar linha temporária em cinza
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
          // Desenha a janela temporária enquanto o usuário arrasta
          if ssLeft in Shift then
          begin
            LimparTela;
            Image1.Canvas.Pen.Color := clBlue;
            Image1.Canvas.Rectangle(JanelaMin.X, JanelaMin.Y, X, Y);
          end;
        end
        else if EstadoCohen = 1 then
        begin
          // Desenha a linha temporária enquanto o usuário arrasta
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
        // Mostra prévia da circunferência enquanto o usuário move o mouse
        LimparTela;
        
        // Redesenha o marcador do centro
        Image1.Canvas.Brush.Color := clRed;
        Image1.Canvas.Ellipse(CentroCirculo.X-3, CentroCirculo.Y-3, 
                             CentroCirculo.X+3, CentroCirculo.Y+3);
        Image1.Canvas.Brush.Color := clWhite;
        
        // Calcula o raio baseado na distância do centro até a posição atual do mouse
        Raio := Round(sqrt(sqr(X - CentroCirculo.X) + sqr(Y - CentroCirculo.Y)));
        
        if Raio > 0 then
        begin
          // Desenha prévia da circunferência em cinza claro
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
        
        // Limpar tela e desenhar linha usando equação geral
        LimparTela;
        DesenharLinhaEquacaoGeral(StartPoint.X, StartPoint.Y, EndPoint.X, EndPoint.Y);
        ShowMessage('Linha desenhada usando Equação Geral (Ax + By + C = 0)' + #13#10 +
                   'Pontos: (' + IntToStr(StartPoint.X) + ', ' + IntToStr(StartPoint.Y) + ') a (' +
                   IntToStr(EndPoint.X) + ', ' + IntToStr(EndPoint.Y) + ')');
      end;
      
    moLinhaParametrica:
      if IsDrawingLine and (Button = mbLeft) then
      begin
        EndPoint := Point(X, Y);
        IsDrawingLine := False;
        
        // Limpar tela e desenhar linha usando equação paramétrica
        LimparTela;
        DesenharLinhaParametrica(StartPoint.X, StartPoint.Y, EndPoint.X, EndPoint.Y);
        ShowMessage('Linha desenhada usando Equação Paramétrica P(t) = P1 + t(P2 - P1)' + #13#10 +
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
          // Limpa a tela e desenha a circunferência final usando Bresenham
          LimparTela;
          
          // Redesenha o marcador do centro
          Image1.Canvas.Brush.Color := clRed;
          Image1.Canvas.Ellipse(CentroCirculo.X-3, CentroCirculo.Y-3, 
                               CentroCirculo.X+3, CentroCirculo.Y+3);
          Image1.Canvas.Brush.Color := clWhite;
          
          // Desenha a circunferência usando o algoritmo de Bresenham
          DesenharCirculoBresenham(CentroCirculo.X, CentroCirculo.Y, Raio);
          
          ShowMessage('Circunferência desenhada usando o Algoritmo de Bresenham!' + #13#10 +
                     'Centro: (' + IntToStr(CentroCirculo.X) + ', ' + IntToStr(CentroCirculo.Y) + ')' + #13#10 +
                     'Raio: ' + IntToStr(Raio) + ' pixels');
        end;
        
        // Reset para permitir desenhar outra circunferência
        EstadoBresenham := 0;
        IsDefiningCircle := False;
      end;
  end;
end;

end.
