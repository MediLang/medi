# Medi AST Crate

This crate provides the Abstract Syntax Tree (AST) definitions for the Medi programming language, along with utilities for traversing, transforming, and serializing the AST.

## Features

- **Comprehensive AST Nodes**: Defines all the node types needed to represent Medi programs.
- **Visitor Pattern**: Implements the visitor pattern for traversing and transforming the AST.
- **Serialization**: Supports serialization and deserialization of AST nodes using Serde.
- **Pretty Printing**: Includes utilities for pretty-printing AST nodes.

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
medic_ast = { path = "path/to/medic_ast" }
```

## Examples

### Traversing the AST with a Visitor

```rust
use medic_ast::ast::*;
use medic_ast::visit::*;

struct MyVisitor;

impl Visitor for MyVisitor {
    type Output = ();
    
    fn visit_identifier(&mut self, node: &IdentifierNode) -> VisitResult<()> {
        println!("Found identifier: {}", node.name);
        Ok(())
    }
    
    // Implement other visit methods as needed
}

// Create an AST node
let ast = ExpressionNode::Binary(Box::new(BinaryExpressionNode {
    left: ExpressionNode::Literal(LiteralNode::Int(1)),
    operator: BinaryOperator::Add,
    right: ExpressionNode::Literal(LiteralNode::Int(2)),
}));

// Visit the AST
let mut visitor = MyVisitor;
ast.accept(&mut visitor).unwrap();
```

### Serializing and Deserializing AST Nodes

```rust
use medic_ast::ast::*;
use medic_ast::to_json;

let expr = ExpressionNode::Binary(Box::new(BinaryExpressionNode {
    left: ExpressionNode::Literal(LiteralNode::Int(1)),
    operator: BinaryOperator::Add,
    right: ExpressionNode::Literal(LiteralNode::Int(2)),
}));

// Serialize to JSON
let json = to_json(&expr).unwrap();
println!("{}", json);

// Deserialize back to an AST node
let deserialized: ExpressionNode = medic_ast::from_json(&json).unwrap();
assert_eq!(expr, deserialized);
```

## Running Examples

This crate includes an example that demonstrates how to use the visitor pattern to traverse and analyze an AST. To run it:

```bash
cargo run --example ast_visitor --features serde
```

## License

Licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
