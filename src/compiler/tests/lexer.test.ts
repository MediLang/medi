/**
 * Tests for the Medi lexical analyzer
 * 
 * This file contains tests to verify that the lexer correctly tokenizes
 * Medi code, including healthcare-specific syntax.
 */

import { Lexer } from '../lexer/lexer';
import { TokenType } from '../lexer/token';

// Helper function to test lexer output
function testLexer(input: string, expectedTokenTypes: TokenType[]): void {
  const lexer = new Lexer(input);
  const tokens = lexer.scanTokens();
  
  // Check that we have the expected number of tokens (including EOF)
  if (tokens.length !== expectedTokenTypes.length + 1) {
    console.error(`Expected ${expectedTokenTypes.length + 1} tokens, got ${tokens.length}`);
    console.error('Tokens:', tokens.map(t => t.type));
    throw new Error('Token count mismatch');
  }
  
  // Check each token type
  for (let i = 0; i < expectedTokenTypes.length; i++) {
    if (tokens[i].type !== expectedTokenTypes[i]) {
      console.error(`Token ${i} mismatch: expected ${expectedTokenTypes[i]}, got ${tokens[i].type}`);
      console.error('Token:', tokens[i]);
      throw new Error('Token type mismatch');
    }
  }
  
  // Check that the last token is EOF
  if (tokens[tokens.length - 1].type !== TokenType.EOF) {
    console.error('Last token is not EOF');
    throw new Error('Missing EOF token');
  }
  
  console.log(`✓ Test passed for input: ${input}`);
}

// Test basic tokens
console.log('Testing basic tokens...');
testLexer('+ - * /', [
  TokenType.PLUS,
  TokenType.MINUS,
  TokenType.MULTIPLY,
  TokenType.DIVIDE
]);

// Test identifiers and keywords
console.log('Testing identifiers and keywords...');
testLexer('let x = 5;', [
  TokenType.LET,
  TokenType.IDENTIFIER,
  TokenType.ASSIGN,
  TokenType.NUMBER,
  TokenType.SEMICOLON
]);

// Test healthcare-specific keywords
console.log('Testing healthcare-specific keywords...');
testLexer('patient fhir_query kaplan_meier', [
  TokenType.PATIENT,
  TokenType.FHIR_QUERY,
  TokenType.KAPLAN_MEIER
]);

// Test healthcare-specific syntax
console.log('Testing healthcare-specific syntax...');
testLexer('patients |> filter(condition: "diabetes")', [
  TokenType.IDENTIFIER,
  TokenType.PIPELINE,
  TokenType.IDENTIFIER,
  TokenType.LEFT_PAREN,
  TokenType.IDENTIFIER,
  TokenType.COLON,
  TokenType.STRING,
  TokenType.RIGHT_PAREN
]);

// Test FHIR query example
console.log('Testing FHIR query example...');
testLexer('patients = fhir_query("Patient").where(age > 65);', [
  TokenType.IDENTIFIER,
  TokenType.ASSIGN,
  TokenType.FHIR_QUERY,
  TokenType.LEFT_PAREN,
  TokenType.STRING,
  TokenType.RIGHT_PAREN,
  TokenType.DOT,
  TokenType.IDENTIFIER,
  TokenType.LEFT_PAREN,
  TokenType.IDENTIFIER,
  TokenType.GREATER_THAN,
  TokenType.NUMBER,
  TokenType.RIGHT_PAREN,
  TokenType.SEMICOLON
]);

console.log('All lexer tests passed!');
