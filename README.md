# phd-thesis

## Prerequisites

You need the Fira Sans and Fira Mono fonts to build the thesis.
For instance you can find them on https://fonts.google.com/specimen/Fira+Sans
and https://fonts.google.com/specimen/Fira+Mono.

## Compilation

I build using `xelatex` with the following series of commands

```bash
xelatex --shell-escape main
makeindex main.nlo -s nomencl.ist -o main.nls # Compile nomenclature
makeindex main # Compile index
biber main # Compile bibliography
makeglossaries main # Compile glossary
xelatex --shell-escape main
xelatex --shell-escape main
xelatex --shell-escape main
```

Once its done, just doing
```bash
xelatex --shell-escape main
```
is often enough.