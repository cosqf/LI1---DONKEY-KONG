# Laboratórios de Informática I

## Descrição
Este repositório contém o código, a documentação e testes para um projeto desenvolvido no âmbito de uma disciplina da universidade. Trata-se da implementação de um jogo utilizando a linguagem Haskell e a biblioteca gráfica Gloss.
Nota: 18/20

## Executável

Pode compilar e executar o programa através dos comandos `build` e `run` do `cabal`.

```bash
cabal run primate-kong
```

## Interpretador

Pode abrir o interpretador do Haskell (GHCi) utilizando o cabal com o projecto automaticamente carregado.

```bash
cabal repl
```

## Testes

O projecto utiliza a biblioteca [HUnit](https://hackage.haskell.org/package/HUnit) para fazer testes unitários.

Pode correr os testes utilizando o seguinte comando.

```bash
cabal test
```

Se pretender executar os exemplos da documentação como testes unitários utiliza-se a biblioteca [Doctest](https://hackage.haskell.org/package/doctest).

```bash
cabal repl --build-depends=QuickCheck,doctest --with-ghc=doctest
```

## Documentação

Pode gerar a documentação com o [Haddock](https://haskell-haddock.readthedocs.io/).

```bash
cabal haddock
```
