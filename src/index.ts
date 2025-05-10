/**
 * Medi Programming Language
 * 
 * A language purpose-built for healthcare, designed to transform medical analytics
 * with unparalleled ease, speed, and security.
 */

import { Lexer } from './compiler/lexer/lexer';
import { TokenType } from './compiler/lexer/token';

// Example usage of the lexer
function main() {
  const sampleCode = `
    // FHIR query example
    patients = fhir_query("Patient").where(age > 65 && condition == "diabetes");
    
    // Statistical analysis
    survival = kaplan_meier(patients, start_date, end_date, event);
    
    // Healthcare data pipeline
    result = patients |> filter(has_condition("J44")) |> group_by(gender) |> count();
  `;
  
  console.log("Medi Programming Language - Lexer Demo");
  console.log("---------------------------------------");
  console.log("Sample code:");
  console.log(sampleCode);
  console.log("---------------------------------------");
  console.log("Tokenizing...");
  
  const lexer = new Lexer(sampleCode);
  const tokens = lexer.scanTokens();
  
  console.log("Tokens:");
  tokens.forEach((token, index) => {
    // Skip EOF token
    if (token.type === TokenType.EOF) return;
    
    console.log(`${index}: [${token.line}:${token.column}] ${token.type} '${token.lexeme}'${token.literal !== undefined ? ` (${token.literal})` : ''}`);
  });
}

// Run the demo if this file is executed directly
if (require.main === module) {
  main();
}

// Export components for use in other files
export { Lexer } from './compiler/lexer/lexer';
export { Token, TokenType } from './compiler/lexer/token';
