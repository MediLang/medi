/**
 * AST Node Definitions for Medi Language
 *
 * This file defines the core Abstract Syntax Tree (AST) node types used by the parser.
 * The AST represents the syntactic structure of Medi programs, including healthcare-specific constructs.
 */

export type ASTNode =
  | ProgramNode
  | StatementNode
  | ExpressionNode;

export interface ProgramNode {
  type: 'Program';
  body: StatementNode[];
}

export type StatementNode =
  | VariableDeclarationNode
  | FunctionDeclarationNode
  | ExpressionStatementNode
  | IfStatementNode
  | ForStatementNode
  | WhileStatementNode
  | ReturnStatementNode
  | BlockStatementNode;

export interface VariableDeclarationNode {
  type: 'VariableDeclaration';
  identifier: string;
  value: ExpressionNode;
}

export interface FunctionDeclarationNode {
  type: 'FunctionDeclaration';
  name: string;
  params: string[];
  body: BlockStatementNode;
}

export interface ExpressionStatementNode {
  type: 'ExpressionStatement';
  expression: ExpressionNode;
}

export interface IfStatementNode {
  type: 'IfStatement';
  test: ExpressionNode;
  consequent: BlockStatementNode;
  alternate?: BlockStatementNode;
}

export interface ForStatementNode {
  type: 'ForStatement';
  init: StatementNode | null;
  test: ExpressionNode | null;
  update: ExpressionNode | null;
  body: BlockStatementNode;
}

export interface WhileStatementNode {
  type: 'WhileStatement';
  test: ExpressionNode;
  body: BlockStatementNode;
}

export interface ReturnStatementNode {
  type: 'ReturnStatement';
  argument: ExpressionNode | null;
}

export interface BlockStatementNode {
  type: 'BlockStatement';
  body: StatementNode[];
}

// Expressions
export type ExpressionNode =
  | IdentifierNode
  | LiteralNode
  | BinaryExpressionNode
  | CallExpressionNode
  | PipelineExpressionNode
  | MemberExpressionNode
  | HealthcareQueryNode;

export interface IdentifierNode {
  type: 'Identifier';
  name: string;
}

export interface LiteralNode {
  type: 'Literal';
  value: string | number | boolean | null;
}

export interface BinaryExpressionNode {
  type: 'BinaryExpression';
  operator: string;
  left: ExpressionNode;
  right: ExpressionNode;
}

export interface CallExpressionNode {
  type: 'CallExpression';
  callee: ExpressionNode;
  arguments: ExpressionNode[];
}

export interface PipelineExpressionNode {
  type: 'PipelineExpression';
  left: ExpressionNode;
  right: ExpressionNode;
}

export interface MemberExpressionNode {
  type: 'MemberExpression';
  object: ExpressionNode;
  property: ExpressionNode;
}

// Healthcare-specific query (e.g., fhir_query(...))
export interface HealthcareQueryNode {
  type: 'HealthcareQuery';
  queryType: string; // e.g., 'fhir_query', 'kaplan_meier'
  arguments: ExpressionNode[];
}

// =====================
// Visitor Pattern Types
// =====================

export interface Visitor<R = any> {
  visitProgram(node: ProgramNode): R;
  visitVariableDeclaration(node: VariableDeclarationNode): R;
  visitFunctionDeclaration(node: FunctionDeclarationNode): R;
  visitExpressionStatement(node: ExpressionStatementNode): R;
  visitIfStatement(node: IfStatementNode): R;
  visitForStatement(node: ForStatementNode): R;
  visitWhileStatement(node: WhileStatementNode): R;
  visitReturnStatement(node: ReturnStatementNode): R;
  visitBlockStatement(node: BlockStatementNode): R;
  visitIdentifier(node: IdentifierNode): R;
  visitLiteral(node: LiteralNode): R;
  visitBinaryExpression(node: BinaryExpressionNode): R;
  visitCallExpression(node: CallExpressionNode): R;
  visitPipelineExpression(node: PipelineExpressionNode): R;
  visitMemberExpression(node: MemberExpressionNode): R;
  visitHealthcareQuery(node: HealthcareQueryNode): R;
}

// Utility function for AST traversal
export function visitAST<R>(node: ASTNode, visitor: Visitor<R>): R {
  switch (node.type) {
    case 'Program': return visitor.visitProgram(node);
    case 'VariableDeclaration': return visitor.visitVariableDeclaration(node);
    case 'FunctionDeclaration': return visitor.visitFunctionDeclaration(node);
    case 'ExpressionStatement': return visitor.visitExpressionStatement(node);
    case 'IfStatement': return visitor.visitIfStatement(node);
    case 'ForStatement': return visitor.visitForStatement(node);
    case 'WhileStatement': return visitor.visitWhileStatement(node);
    case 'ReturnStatement': return visitor.visitReturnStatement(node);
    case 'BlockStatement': return visitor.visitBlockStatement(node);
    case 'Identifier': return visitor.visitIdentifier(node);
    case 'Literal': return visitor.visitLiteral(node);
    case 'BinaryExpression': return visitor.visitBinaryExpression(node);
    case 'CallExpression': return visitor.visitCallExpression(node);
    case 'PipelineExpression': return visitor.visitPipelineExpression(node);
    case 'MemberExpression': return visitor.visitMemberExpression(node);
    case 'HealthcareQuery': return visitor.visitHealthcareQuery(node);
    default:
      throw new Error(`Unknown AST node type: ${(node as any).type}`);
  }
}
