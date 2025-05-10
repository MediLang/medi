import { Token, TokenType } from '../lexer/token';
import { ASTNode, ProgramNode, StatementNode, ExpressionNode } from '../ast/ast';

export class Parser {
  private tokens: Token[];
  private current: number = 0;

  constructor(tokens: Token[]) {
    this.tokens = tokens;
  }

  public parseProgram(): ProgramNode {
    const statements: StatementNode[] = [];
    while (!this.isAtEnd()) {
      const stmt = this.parseStatement();
      if (stmt) statements.push(stmt);
    }
    return {
      type: 'Program',
      body: statements
    };
  }

  private parseStatement(): StatementNode | null {
    // Dispatch to specific statement parsers based on token lookahead
    if (this.match(TokenType.LET)) return this.parseVariableDeclaration();
    if (this.match(TokenType.FUNCTION)) return this.parseFunctionDefinition();
    if (this.match(TokenType.IF)) return this.parseIfStatement();
    if (this.match(TokenType.WHILE)) return this.parseWhileStatement();
    if (this.match(TokenType.FOR)) return this.parseForStatement();
    if (this.match(TokenType.RETURN)) return this.parseReturnStatement();
    if (this.match(TokenType.FHIR_QUERY)) return this.parseFhirQueryStatement();
    if (this.match(TokenType.FHIR_RESOURCE)) return this.parseFhirResourceStatement();
    if (this.match(TokenType.HL7)) return this.parseHl7Statement();
    if (this.match(TokenType.DICOM)) return this.parseDicomStatement();
    if (this.match(TokenType.LEFT_BRACE)) return this.parseBlockStatement();
    // Assignment or expression statement fallback
    if (this.check(TokenType.IDENTIFIER) && this.checkNext(TokenType.ASSIGN)) {
      return this.parseAssignmentStatement();
    }
    return this.parseExpressionStatement();
  }

  // Stub: Variable Declaration
  private parseVariableDeclaration(): StatementNode {
    // At this point, LET or CONST has already been consumed
    const identifierToken = this.consume(TokenType.IDENTIFIER, 'Expected variable name after let/const');
    this.consume(TokenType.ASSIGN, 'Expected = after variable name');
    const value = this.parseExpression();
    this.consume(TokenType.SEMICOLON, 'Expected ; after variable declaration');
    return {
      type: 'VariableDeclaration',
      identifier: identifierToken.lexeme,
      value
    };
  }

  // Helper: consume a token of a specific type, or throw
  private consume(type: TokenType, errorMessage: string): Token {
    if (this.check(type)) return this.advance();
    throw new Error(errorMessage + ` (line ${this.peek().line}, column ${this.peek().column})`);
  }

  // Expression parser entry point (supports precedence)
  private parseExpression(): ExpressionNode {
    return this.parseAssignmentExpr();
  }

  // Assignment expression (right-associative)
  private parseAssignmentExpr(): ExpressionNode {
    const left = this.parseLogicalOrExpr();
    if (this.match(TokenType.ASSIGN)) {
      const operator = '=';
      const right = this.parseAssignmentExpr();
      if (left.type !== 'Identifier') {
        throw new Error('Invalid assignment target');
      }
      return {
        type: 'BinaryExpression',
        operator,
        left,
        right
      };
    }
    return left;
  }

  // Logical OR
  private parseLogicalOrExpr(): ExpressionNode {
    let expr = this.parseLogicalAndExpr();
    while (this.match(TokenType.OR)) {
      const operator = 'or';
      const right = this.parseLogicalAndExpr();
      expr = {
        type: 'BinaryExpression',
        operator,
        left: expr,
        right
      };
    }
    return expr;
  }

  // Logical AND
  private parseLogicalAndExpr(): ExpressionNode {
    let expr = this.parseEqualityExpr();
    while (this.match(TokenType.AND)) {
      const operator = 'and';
      const right = this.parseEqualityExpr();
      expr = {
        type: 'BinaryExpression',
        operator,
        left: expr,
        right
      };
    }
    return expr;
  }

  // Equality (==, !=)
  private parseEqualityExpr(): ExpressionNode {
    let expr = this.parseComparisonExpr();
    while (this.match(TokenType.EQUALS, TokenType.NOT_EQUALS)) {
      const operator = this.previous().type === TokenType.EQUALS ? '==' : '!=';
      const right = this.parseComparisonExpr();
      expr = {
        type: 'BinaryExpression',
        operator,
        left: expr,
        right
      };
    }
    return expr;
  }

  // Comparison (<, >, <=, >=)
  private parseComparisonExpr(): ExpressionNode {
    let expr = this.parseTermExpr();
    while (this.match(TokenType.LESS_THAN, TokenType.GREATER_THAN, TokenType.LESS_EQUALS, TokenType.GREATER_EQUALS)) {
      let operator = '';
      switch (this.previous().type) {
        case TokenType.LESS_THAN: operator = '<'; break;
        case TokenType.GREATER_THAN: operator = '>'; break;
        case TokenType.LESS_EQUALS: operator = '<='; break;
        case TokenType.GREATER_EQUALS: operator = '>='; break;
      }
      const right = this.parseTermExpr();
      expr = {
        type: 'BinaryExpression',
        operator,
        left: expr,
        right
      };
    }
    return expr;
  }

  // Term (+, -)
  private parseTermExpr(): ExpressionNode {
    let expr = this.parseFactorExpr();
    while (this.match(TokenType.PLUS, TokenType.MINUS)) {
      const operator = this.previous().type === TokenType.PLUS ? '+' : '-';
      const right = this.parseFactorExpr();
      expr = {
        type: 'BinaryExpression',
        operator,
        left: expr,
        right
      };
    }
    return expr;
  }

  // Factor (*, /)
  private parseFactorExpr(): ExpressionNode {
    let expr = this.parseUnaryExpr();
    while (this.match(TokenType.MULTIPLY, TokenType.DIVIDE)) {
      const operator = this.previous().type === TokenType.MULTIPLY ? '*' : '/';
      const right = this.parseUnaryExpr();
      expr = {
        type: 'BinaryExpression',
        operator,
        left: expr,
        right
      };
    }
    return expr;
  }

  // Unary (!, -)
  private parseUnaryExpr(): ExpressionNode {
    if (this.match(TokenType.NOT, TokenType.MINUS)) {
      const operator = this.previous().type === TokenType.NOT ? '!' : '-';
      const right = this.parseUnaryExpr();
      return {
        type: 'BinaryExpression',
        operator,
        left: { type: 'Literal', value: null }, // unary left is null
        right
      };
    }
    return this.parseCallOrMemberExpr();
  }

  // Call, member, and primary
  private parseCallOrMemberExpr(): ExpressionNode {
    let expr = this.parsePrimaryExpr();
    // Member expression: foo.bar
    while (this.match(TokenType.DOT)) {
      const property = this.consume(TokenType.IDENTIFIER, 'Expected property name after .');
      expr = {
        type: 'MemberExpression',
        object: expr,
        property: { type: 'Identifier', name: property.lexeme }
      };
    }
    // Call expression: foo(...)
    while (this.match(TokenType.LEFT_PAREN)) {
      const args: ExpressionNode[] = [];
      if (!this.check(TokenType.RIGHT_PAREN)) {
        do {
          args.push(this.parseExpression());
        } while (this.match(TokenType.COMMA));
      }
      this.consume(TokenType.RIGHT_PAREN, 'Expected ) after function arguments');
      expr = {
        type: 'CallExpression',
        callee: expr,
        arguments: args
      };
    }
    return expr;
  }

  // Primary expressions: literals, identifiers, parenthesized
  private parsePrimaryExpr(): ExpressionNode {
    if (this.match(TokenType.NUMBER)) {
      return { type: 'Literal', value: Number(this.previous().lexeme) };
    }
    if (this.match(TokenType.STRING)) {
      return { type: 'Literal', value: this.previous().lexeme };
    }
    if (this.match(TokenType.IDENTIFIER)) {
      return { type: 'Identifier', name: this.previous().lexeme };
    }
    if (this.match(TokenType.LEFT_PAREN)) {
      const expr = this.parseExpression();
      this.consume(TokenType.RIGHT_PAREN, 'Expected ) after expression');
      return expr;
    }
    throw new Error(`Unexpected token: ${this.peek().lexeme}`);
  }

  // Function Definition
  private parseFunctionDefinition(): StatementNode {
    // At this point, FUNCTION has already been consumed
    const nameToken = this.consume(TokenType.IDENTIFIER, 'Expected function name after function keyword');
    this.consume(TokenType.LEFT_PAREN, 'Expected ( after function name');
    const params: string[] = [];
    if (!this.check(TokenType.RIGHT_PAREN)) {
      do {
        const paramToken = this.consume(TokenType.IDENTIFIER, 'Expected parameter name');
        params.push(paramToken.lexeme);
      } while (this.match(TokenType.COMMA));
    }
    this.consume(TokenType.RIGHT_PAREN, 'Expected ) after function parameters');
    const body = this.parseBlockStatement();
    return {
      type: 'FunctionDeclaration',
      name: nameToken.lexeme,
      params,
      body
    };
  }

  // If Statement
  private parseIfStatement(): StatementNode {
    this.consume(TokenType.LEFT_PAREN, 'Expected ( after if');
    const test = this.parseExpression();
    this.consume(TokenType.RIGHT_PAREN, 'Expected ) after if condition');
    const consequent = this.parseBlockStatement();
    let alternate: import('../ast/ast').BlockStatementNode | undefined = undefined;
    if (this.match(TokenType.ELSE)) {
      alternate = this.parseBlockStatement();
    }
    return {
      type: 'IfStatement',
      test,
      consequent,
      ...(alternate ? { alternate } : {})
    };
  }

  // While Statement
  private parseWhileStatement(): StatementNode {
    this.consume(TokenType.LEFT_PAREN, 'Expected ( after while');
    const test = this.parseExpression();
    this.consume(TokenType.RIGHT_PAREN, 'Expected ) after while condition');
    const body = this.parseBlockStatement() as any; // BlockStatementNode
    return {
      type: 'WhileStatement',
      test,
      body
    };
  }

  // For Statement
  private parseForStatement(): StatementNode {
    this.consume(TokenType.LEFT_PAREN, 'Expected ( after for');
    let init: StatementNode | null = null;
    if (!this.check(TokenType.SEMICOLON)) {
      if (this.match(TokenType.LET) || this.match(TokenType.CONST)) {
        init = this.parseVariableDeclaration();
      } else {
        init = this.parseExpressionStatement();
      }
    } else {
      this.advance(); // consume SEMICOLON
    }
    let test: ExpressionNode | null = null;
    if (!this.check(TokenType.SEMICOLON)) {
      test = this.parseExpression();
    }
    this.consume(TokenType.SEMICOLON, 'Expected ; after for loop test');
    let update: ExpressionNode | null = null;
    if (!this.check(TokenType.RIGHT_PAREN)) {
      update = this.parseExpression();
    }
    this.consume(TokenType.RIGHT_PAREN, 'Expected ) after for loop update');
    const body = this.parseBlockStatement() as any; // BlockStatementNode
    return {
      type: 'ForStatement',
      init,
      test,
      update,
      body
    };
  }

  // Return Statement
  private parseReturnStatement(): StatementNode {
    let argument: ExpressionNode | null = null;
    if (!this.check(TokenType.SEMICOLON)) {
      argument = this.parseExpression();
    }
    this.consume(TokenType.SEMICOLON, 'Expected ; after return statement');
    return {
      type: 'ReturnStatement',
      argument
    };
  }

  // Assignment Statement
  private parseAssignmentStatement(): StatementNode {
    const identifierToken = this.consume(TokenType.IDENTIFIER, 'Expected variable name on left side of assignment');
    this.consume(TokenType.ASSIGN, 'Expected = in assignment');
    const value = this.parseExpression();
    this.consume(TokenType.SEMICOLON, 'Expected ; after assignment');
    return {
      type: 'ExpressionStatement',
      expression: {
        type: 'BinaryExpression',
        operator: '=',
        left: {
          type: 'Identifier',
          name: identifierToken.lexeme
        },
        right: value
      }
    };
  }

  // Expression Statement
  private parseExpressionStatement(): StatementNode {
    const expr = this.parseExpression();
    this.consume(TokenType.SEMICOLON, 'Expected ; after expression');
    return {
      type: 'ExpressionStatement',
      expression: expr
    };
  }

  // Block Statement
  private parseBlockStatement(): import('../ast/ast').BlockStatementNode {
    this.consume(TokenType.LEFT_BRACE, 'Expected { at start of block');
    const body: StatementNode[] = [];
    while (!this.check(TokenType.RIGHT_BRACE) && !this.isAtEnd()) {
      const stmt = this.parseStatement();
      if (stmt) body.push(stmt);
    }
    this.consume(TokenType.RIGHT_BRACE, 'Expected } at end of block');
    return {
      type: 'BlockStatement',
      body
    };
  }

  // Healthcare-Specific Statements
  private parseFhirQueryStatement(): StatementNode {
    return this.parseHealthcareQueryStatement('fhir_query');
  }
  private parseFhirResourceStatement(): StatementNode {
    return this.parseHealthcareQueryStatement('fhir_resource');
  }
  private parseHl7Statement(): StatementNode {
    return this.parseHealthcareQueryStatement('hl7');
  }
  private parseDicomStatement(): StatementNode {
    return this.parseHealthcareQueryStatement('dicom');
  }

  // Generic healthcare query statement parser
  private parseHealthcareQueryStatement(queryType: string): StatementNode {
    this.consume(TokenType.LEFT_PAREN, `Expected ( after ${queryType}`);
    const args: ExpressionNode[] = [];
    if (!this.check(TokenType.RIGHT_PAREN)) {
      do {
        args.push(this.parseExpression());
      } while (this.match(TokenType.COMMA));
    }
    this.consume(TokenType.RIGHT_PAREN, `Expected ) after ${queryType} arguments`);
    this.consume(TokenType.SEMICOLON, `Expected ; after ${queryType} statement`);
    return {
      type: 'ExpressionStatement',
      expression: {
        type: 'HealthcareQuery',
        queryType,
        arguments: args
      }
    };
  }

  // Util: Lookahead for assignment
  private checkNext(type: TokenType): boolean {
    if (this.current + 1 >= this.tokens.length) return false;
    return this.tokens[this.current + 1].type === type;
  }

  // Utility methods
  private match(...types: TokenType[]): boolean {
    for (const type of types) {
      if (this.check(type)) {
        this.advance();
        return true;
      }
    }
    return false;
  }

  private check(type: TokenType): boolean {
    if (this.isAtEnd()) return false;
    return this.peek().type === type;
  }

  private advance(): Token {
    if (!this.isAtEnd()) this.current++;
    return this.previous();
  }

  private isAtEnd(): boolean {
    return this.peek().type === TokenType.EOF;
  }

  private peek(): Token {
    return this.tokens[this.current];
  }

  private previous(): Token {
    return this.tokens[this.current - 1];
  }
}
