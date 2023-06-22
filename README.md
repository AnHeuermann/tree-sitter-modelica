# tree-sitter-modelica

An [open-source](OSMC-License.txt) Modelica
([Modelica Language Specification v3.5](https://specification.modelica.org/maint/3.5/MLS.html))
grammar for [tree-sitter](https://github.com/tree-sitter/tree-sitter).

Currently used by [OMFrontend.js](https://github.com/OpenModelica/OMFrontend.js).

## Install

You'll need:

  - Python >= v3.6,
  - Rust and Cargo
  - [emscripten](https://emscripten.org/index.html) or Docker
    - Calling emcc from Docker doesn't seem to work on Windows.

```bash
cargo build
npm install --save nan
npm install --save-dev tree-sitter-cli
npm run build
```

Add `node_modules/.bin` to your `PATH`.

Bash:
```bash
export PATH="$PATH":"$(pwd)/node_modules/.bin"
```

PowerShell:
```ps
$env:Path += ";$(pwd)\\node_modules\\.bin"
```

If you are on Windows forget about it:
  - tree-sitter-cli can't run emcc on Windows
    - https://github.com/tree-sitter/tree-sitter/issues/434
    - https://github.com/tree-sitter/tree-sitter/issues/532

## Parse and highlight example

```bash
tree-sitter parse examples/helloWorld.mo
```
