unit UnitPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, Math;

type
  // Enumeração para os modos de funcionamento
  TModoOperacao = (moCirculos, moLinhas, moCohenSutherland, moNenhum);

  { TFormPrincipal }

  TFormPrincipal = class(TForm)
    Image1: TImage;
    MainMenu1: TMainMenu;
    MenuFuncionalidades: TMenuItem;
    MenuCirculos: TMenuItem;
    MenuCirculoEquacaoPadrao: TMenuItem;
    MenuCirculoParametrica: TMenuItem;
    MenuCirculoRotacao: TMenuItem;
    MenuLinhas: TMenuItem;
    MenuLinhaSimples: TMenuItem;
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
    procedure MenuLinhaSimplesClick(Sender: TObject);
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
    
    // Métodos auxiliares
    procedure LimparTela;
    procedure DesenharLinhaSimples(x1, y1, x2, y2: Integer; Cor: TColor);
    procedure DesenharCirculoEquacaoPadrao;
    procedure DesenharCirculoParametrica;
    procedure DesenharCirculoRotacao;
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
  MenuLinhas.Caption := 'Linhas';
  MenuLinhaSimples.Caption := 'Desenhar Linhas';
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

{ Funcionalidades de Linhas }

procedure TFormPrincipal.MenuLinhaSimplesClick(Sender: TObject);
begin
  ModoAtual := moLinhas;
  LimparTela;
  ShowMessage('Modo de desenho de linhas ativado. Clique e arraste para desenhar uma linha.');
end;



procedure TFormPrincipal.DesenharLinhaSimples(x1, y1, x2, y2: Integer; Cor: TColor);
begin
  Image1.Canvas.Pen.Color := Cor;
  Image1.Canvas.MoveTo(x1, y1);
  Image1.Canvas.LineTo(x2, y2);
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
    moLinhas:
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
  end;
end;

procedure TFormPrincipal.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
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
  end;
end;

procedure TFormPrincipal.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Temp: Integer;
  x1, y1, x2, y2: Double;
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
  end;
end;

end.
