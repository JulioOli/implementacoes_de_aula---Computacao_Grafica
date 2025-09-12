# Projeto Integrado - Computação Gráfica

Este projeto integra três funcionalidades de computação gráfica em uma única aplicação Lazarus/Pascal:

## Funcionalidades

### 1. Desenho de Círculos
Eu implementei três métodos diferentes para desenhar circunferências:

- **Equação Padrão (x²+y²=R²)**: Desenha círculo em vermelho usando a equação matemática básica
- **Equação Paramétrica**: Desenha círculo em azul usando x=r*cos(θ), y=r*sin(θ)
- **Rotações Sucessivas**: Desenha círculo em verde aplicando rotações de 1° sucessivamente

### 2. Desenho de Linhas
- **Desenhar Linhas**: Modo interativo onde você clica e arrasta para desenhar linhas

### 3. Algoritmo Cohen-Sutherland
Aqui eu fiz a implementação do algoritmo de recorte de linhas:
- Primeiro defina a janela de visualização (retângulo azul) clicando e arrastando
- Depois desenhe uma linha para ser recortada
- O algoritmo mostra a linha original (cinza claro) e a parte visível (vermelha)

## Como usar

1. Abra o projeto `ProjetoIntegrado.lpi` no Lazarus
2. Compile e execute o projeto
3. Use o menu "Funcionalidades" para escolher a funcionalidade desejada
4. Para Cohen-Sutherland:
   - Primeiro clique em "Cohen-Sutherland" para ativar o modo
   - Defina a janela clicando e arrastando para criar um retângulo
   - Depois desenhe a linha que será recortada
   - Use "Resetar Cohen-Sutherland" para recomeçar

## Controles

- **Menu Funcionalidades**: Acesso a todas as funcionalidades
- **Limpar Tela**: Remove todos os desenhos e reseta modos
- **Mouse**: Interação com a tela para desenhos interativos
- **Sair**: Fecha a aplicação

## Estrutura do Código

- `ProjetoIntegrado.lpr`: Arquivo principal do programa
- `UnitPrincipal.pas`: Unit principal com toda a lógica
- `UnitPrincipal.lfm`: Formulário visual (interface)
- `ProjetoIntegrado.lpi`: Arquivo de projeto do Lazarus

## Requisitos

- Lazarus IDE
- Free Pascal Compiler
- Sistema operacional compatível com LCL (Linux, Windows, macOS)
