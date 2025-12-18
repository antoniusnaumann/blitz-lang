# Tree-sitter Blitz Grammar

The `tree-sitter-blitz/` directory is configured as a git submodule containing the tree-sitter grammar for the Blitz programming language.

## Working with the submodule

### Initial clone
When cloning this repository, initialize the submodule:
```bash
git clone <repo-url>
cd blitz-lang
git submodule init
git submodule update
```

Or clone with submodules in one step:
```bash
git clone --recurse-submodules <repo-url>
```

### Building the grammar
```bash
cd tree-sitter-blitz
npm install
npm run build
```

### Running tests
```bash
cd tree-sitter-blitz
npm test
```

### Updating the submodule
If you make changes in tree-sitter-blitz:
```bash
cd tree-sitter-blitz
# Make your changes
git add .
git commit -m "Your changes"
cd ..
git add tree-sitter-blitz
git commit -m "Update tree-sitter-blitz submodule"
```

## Current Status
✓ All 22 tests passing
✓ Complete syntax highlighting support
✓ Full Blitz language coverage
