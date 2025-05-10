/**
 * Token definitions for the Medi programming language
 * 
 * This file defines all token types used in the Medi lexer,
 * including standard programming language tokens and
 * healthcare-specific tokens.
 */

export enum TokenType {
  // Special tokens
  EOF = 'EOF',
  ERROR = 'ERROR',
  
  // Literals
  IDENTIFIER = 'IDENTIFIER',
  STRING = 'STRING',
  NUMBER = 'NUMBER',
  
  // Keywords
  LET = 'LET',
  CONST = 'CONST',
  FUNCTION = 'FUNCTION',
  IF = 'IF',
  ELSE = 'ELSE',
  FOR = 'FOR',
  WHILE = 'WHILE',
  RETURN = 'RETURN',
  
  // Healthcare-specific keywords
  PATIENT = 'PATIENT',
  FHIR_QUERY = 'FHIR_QUERY',
  FHIR_RESOURCE = 'FHIR_RESOURCE',
  HL7 = 'HL7',
  DICOM = 'DICOM',
  KAPLAN_MEIER = 'KAPLAN_MEIER',
  SIR_MODEL = 'SIR_MODEL',
  FEDERATED = 'FEDERATED',
  REGULATE = 'REGULATE',
  STREAM = 'STREAM',
  VISUALIZE = 'VISUALIZE',
  
  // Operators
  PLUS = 'PLUS',
  MINUS = 'MINUS',
  MULTIPLY = 'MULTIPLY',
  DIVIDE = 'DIVIDE',
  ASSIGN = 'ASSIGN',
  EQUALS = 'EQUALS',
  NOT_EQUALS = 'NOT_EQUALS',
  LESS_THAN = 'LESS_THAN',
  GREATER_THAN = 'GREATER_THAN',
  LESS_EQUALS = 'LESS_EQUALS',
  GREATER_EQUALS = 'GREATER_EQUALS',
  AND = 'AND',
  OR = 'OR',
  NOT = 'NOT',
  
  // Healthcare-specific operators
  PIPELINE = 'PIPELINE', // |>
  
  // Delimiters
  LEFT_PAREN = 'LEFT_PAREN',
  RIGHT_PAREN = 'RIGHT_PAREN',
  LEFT_BRACE = 'LEFT_BRACE',
  RIGHT_BRACE = 'RIGHT_BRACE',
  LEFT_BRACKET = 'LEFT_BRACKET',
  RIGHT_BRACKET = 'RIGHT_BRACKET',
  COMMA = 'COMMA',
  DOT = 'DOT',
  SEMICOLON = 'SEMICOLON',
  COLON = 'COLON',
}

export interface Token {
  type: TokenType;
  lexeme: string;
  literal?: any;
  line: number;
  column: number;
}

// Map of keywords to their corresponding token types
export const KEYWORDS: Record<string, TokenType> = {
  'let': TokenType.LET,
  'const': TokenType.CONST,
  'function': TokenType.FUNCTION,
  'if': TokenType.IF,
  'else': TokenType.ELSE,
  'for': TokenType.FOR,
  'while': TokenType.WHILE,
  'return': TokenType.RETURN,
  
  // Healthcare-specific keywords
  'patient': TokenType.PATIENT,
  'fhir_query': TokenType.FHIR_QUERY,
  'fhir_resource': TokenType.FHIR_RESOURCE,
  'hl7': TokenType.HL7,
  'dicom': TokenType.DICOM,
  'kaplan_meier': TokenType.KAPLAN_MEIER,
  'sir_model': TokenType.SIR_MODEL,
  'federated': TokenType.FEDERATED,
  'regulate': TokenType.REGULATE,
  'stream': TokenType.STREAM,
  'visualize': TokenType.VISUALIZE,
};
