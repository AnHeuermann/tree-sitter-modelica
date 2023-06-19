# tree-sitter-modelica

An [open-source](OSMC-License.txt) Modelica
([Modelica Language Specification v3.5](https://specification.modelica.org/maint/3.5/MLS.html))
grammar for [tree-sitter](https://github.com/tree-sitter/tree-sitter).

Currently used by [OMFrontend.js](https://github.com/OpenModelica/OMFrontend.js).

## Install

```bash
npm install --save nan
npm install --save-dev tree-sitter-cli
```

Add `node_modules/.bin` to your `PATH`:

Bash:
```bash
export PATH="$PATH":"$(pwd)/node_modules/.bin"
```

PowerShell:
```ps
$env:Path += ";$(pwd)\\node_modules\\.bin"
```

## Run example

```bash
tree-sitter generate
tree-sitter parse examples\\helloWorld.mo
```
