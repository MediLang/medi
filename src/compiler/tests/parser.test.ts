import { Lexer } from '../lexer/lexer';
import { Parser } from '../parser/parser';
import { Token } from '../lexer/token';
import { ASTNode } from '../ast/ast';

describe('Medi Parser', () => {
  function parseSource(source: string): ASTNode {
    const lexer = new Lexer(source);
    const tokens: Token[] = lexer.scanTokens();
    const parser = new Parser(tokens);
    return parser.parseProgram();
  }

  it('parses a variable declaration', () => {
    const ast = parseSource('let x = 42;');
    expect(ast).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'VariableDeclaration',
          identifier: 'x',
          value: { type: 'Literal', value: 42 }
        }
      ]
    });
  });

  it('parses a function definition', () => {
    const src = 'function foo(a, b) { let y = a + b; return y; }';
    const ast = parseSource(src);
    expect(ast).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'FunctionDeclaration',
          name: 'foo',
          params: ['a', 'b'],
          body: {
            type: 'BlockStatement',
            body: [
              { type: 'VariableDeclaration', identifier: 'y' },
              { type: 'ReturnStatement', argument: { type: 'Identifier', name: 'y' } }
            ]
          }
        }
      ]
    });
  });

  it('parses an if-else statement', () => {
    const src = 'if (x == 1) { let y = 2; } else { let y = 3; }';
    const ast = parseSource(src);
    expect(ast).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'IfStatement',
          test: { type: 'BinaryExpression', operator: '==', left: { type: 'Identifier', name: 'x' }, right: { type: 'Literal', value: 1 } },
          consequent: { type: 'BlockStatement' },
          alternate: { type: 'BlockStatement' }
        }
      ]
    });
  });

  it('parses a while statement', () => {
    const src = 'while (x < 10) { x = x + 1; }';
    const ast = parseSource(src);
    expect(ast).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'WhileStatement',
          test: { type: 'BinaryExpression', operator: '<' },
          body: { type: 'BlockStatement' }
        }
      ]
    });
  });

  it('parses a for statement', () => {
    const src = 'for (let i = 0; i < 10; i = i + 1) { x = x + i; }';
    const ast = parseSource(src);
    expect(ast).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'ForStatement',
          init: { type: 'VariableDeclaration', identifier: 'i' },
          test: { type: 'BinaryExpression', operator: '<' },
          update: { type: 'BinaryExpression', operator: '=' },
          body: { type: 'BlockStatement' }
        }
      ]
    });
  });

  it('parses a return statement', () => {
    const ast = parseSource('return 5;');
    expect(ast).toMatchObject({
      type: 'Program',
      body: [
        { type: 'ReturnStatement', argument: { type: 'Literal', value: 5 } }
      ]
    });
  });

  it('parses a FHIR query statement', () => {
    const ast = parseSource('fhir_query("Patient", 123);');
    expect(ast).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'ExpressionStatement',
          expression: {
            type: 'HealthcareQuery',
            queryType: 'fhir_query',
            arguments: [
              { type: 'Literal', value: '"Patient"' },
              { type: 'Literal', value: 123 }
            ]
          }
        }
      ]
    });
  });

  it('parses a basic binary expression', () => {
    const ast = parseSource('a + b * 2;');
    expect(ast).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'ExpressionStatement',
          expression: {
            type: 'BinaryExpression',
            operator: '+',
            left: { type: 'Identifier', name: 'a' },
            right: {
              type: 'BinaryExpression',
              operator: '*',
              left: { type: 'Identifier', name: 'b' },
              right: { type: 'Literal', value: 2 }
            }
          }
        }
      ]
    });
  });
});
