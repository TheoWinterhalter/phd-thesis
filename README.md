# phd-thesis

## Prerequisites

You need the [Fira Sans] and [Fira Mono] fonts to build the thesis.
You also need Symbola because I use it as a fallback for some special unicode
characters.
I also now use DejaVu Sans and DejaVu Sans Mono as fallback fonts.
I will use [Fira Math] as fallback as well.

[Fira Sans]: https://fonts.google.com/specimen/Fira+Sans
[Fira Mono]: https://fonts.google.com/specimen/Fira+Mono
[Fira Math]: https://github.com/firamath/firamath

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

## Configure LaTeX Workshop for VSCode

If you want to build using VSCode like me, you can install the LaTeX Workshop
plugin (the one suggested by VSCode) and add some recipes in the
`settings.json`.

```json
"latex-workshop.latex.recipes":[
    {
        "name": "latexmk 🔃",
        "tools": [
            "latexmk"
        ]
    },
    {
        "name": "latexmk (latexmkrc)",
        "tools": [
            "latexmk_rconly"
        ]
    },
    {
        "name": "latexmk (lualatex)",
        "tools": [
            "lualatexmk"
        ]
    },
    {
        "name": "pdflatex ➞ bibtex ➞ pdflatex × 2",
        "tools": [
            "pdflatex",
            "bibtex",
            "pdflatex",
            "pdflatex"
        ]
    },
    {
        "name": "Compile Rnw files",
        "tools": [
            "rnw2pdf"
        ]
    },
    {
        "name": "kaobook",
        "tools": [
            "xelatex (shell escape)",
            "make nomenclature",
            "makeindex",
            "biber",
            "makeglossaries",
            "xelatex (shell escape)",
            "xelatex (shell escape)",
            "xelatex (shell escape)"
        ]
    }
],
"latex-workshop.latex.tools": [
    {
        "name": "latexmk",
        "command": "latexmk",
        "args": [
            "-synctex=1",
            "-interaction=nonstopmode",
            "-file-line-error",
            "-pdf",
            "-outdir=%OUTDIR%",
            "%DOC%"
        ],
        "env": {}
    },
    {
        "name": "lualatexmk",
        "command": "latexmk",
        "args": [
            "-synctex=1",
            "-interaction=nonstopmode",
            "-file-line-error",
            "-lualatex",
            "-outdir=%OUTDIR%",
            "%DOC%"
        ],
        "env": {}
    },
    {
        "name": "latexmk_rconly",
        "command": "latexmk",
        "args": [
            "%DOC%"
        ],
        "env": {}
    },
    {
        "name": "pdflatex",
        "command": "pdflatex",
        "args": [
            "-synctex=1",
            "-interaction=nonstopmode",
            "-file-line-error",
            "%DOC%"
        ],
        "env": {}
    },
    {
        "name": "bibtex",
        "command": "bibtex",
        "args": [
            "%DOCFILE%"
        ],
        "env": {}
    },
    {
        "name": "rnw2pdf",
        "command": "Rscript",
        "args": [
            "-e",
            "knitr::knit2pdf('%DOCFILE%')"
        ],
        "env": {}
    },
    {
        "name": "xelatex",
        "command": "xelatex",
        "args": [
            "-synctex=1",
            "-interaction=nonstopmode",
            "-file-line-error",
            "%DOC%"
        ]
    },
    {
        "name": "xelatex (shell escape)",
        "command": "xelatex",
        "args": [
            "--shell-escape",
            "-synctex=1",
            "-interaction=nonstopmode",
            "-file-line-error",
            "%DOC%"
        ]
    },
    {
        "name": "biber",
        "command": "biber",
        "args": [
            "%DOCFILE%"
        ],
        "env": {}
    },
    {
        "name": "make nomenclature",
        "command": "makeindex",
        "args": [
            "%DOCFILE%.nlo",
            "-s",
            "nomencl.ist",
            "-o",
            "%DOCFILE%.nls"
        ]
    },
    {
        "name": "makeindex",
        "command": "makeindex",
        "args": [
            "%DOCFILE%"
        ]
    },
    {
        "name": "makeglossaries",
        "command": "makeglossaries",
        "args": [
            "%DOCFILE%"
        ]
    }
]
```