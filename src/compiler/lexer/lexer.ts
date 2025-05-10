/**
 * Lexical Analyzer (Lexer) for the Medi programming language
 * 
 * This lexer tokenizes Medi source code, recognizing both standard programming
 * language tokens and healthcare-specific tokens.
 */

import { Token, TokenType, KEYWORDS } from './token';

export class Lexer {
  private source: string;
  private tokens: Token[] = [];
  
  // State tracking
  private start: number = 0;
  private current: number = 0;
  private line: number = 1;
  private column: number = 1;
  
  constructor(source: string) {
    this.source = source;
  }
  
  /**
   * Scans the source code and returns all tokens
   */
  scanTokens(): Token[] {
    while (!this.isAtEnd()) {
      // Beginning of the next lexeme
      this.start = this.current;
      this.scanToken();
    }
    
    // Add EOF token
    this.tokens.push({
      type: TokenType.EOF,
      lexeme: '',
      line: this.line,
      column: this.column
    });
    
    return this.tokens;
  }
  
  /**
   * Scans a single token
   */
  private scanToken(): void {
    const c = this.advance();
    
    switch (c) {
      // Single-character tokens
      case '(': this.addToken(TokenType.LEFT_PAREN); break;
      case ')': this.addToken(TokenType.RIGHT_PAREN); break;
      case '{': this.addToken(TokenType.LEFT_BRACE); break;
      case '}': this.addToken(TokenType.RIGHT_BRACE); break;
      case '[': this.addToken(TokenType.LEFT_BRACKET); break;
      case ']': this.addToken(TokenType.RIGHT_BRACKET); break;
      case ',': this.addToken(TokenType.COMMA); break;
      case '.': this.addToken(TokenType.DOT); break;
      case ';': this.addToken(TokenType.SEMICOLON); break;
      case ':': this.addToken(TokenType.COLON); break;
      
      // Operators
      case '+': this.addToken(TokenType.PLUS); break;
      case '-': this.addToken(TokenType.MINUS); break;
      case '*': this.addToken(TokenType.MULTIPLY); break;
      case '/': 
        // Check for comments
        if (this.match('/')) {
          // Single-line comment, consume until end of line
          while (this.peek() !== '\n' && !this.isAtEnd()) {
            this.advance();
          }
        } else {
          this.addToken(TokenType.DIVIDE);
        }
        break;
      
      // Comparison operators
      case '=': 
        this.addToken(this.match('=') ? TokenType.EQUALS : TokenType.ASSIGN);
        break;
      case '!': 
        this.addToken(this.match('=') ? TokenType.NOT_EQUALS : TokenType.NOT);
        break;
      case '<': 
        this.addToken(this.match('=') ? TokenType.LESS_EQUALS : TokenType.LESS_THAN);
        break;
      case '>': 
        this.addToken(this.match('=') ? TokenType.GREATER_EQUALS : TokenType.GREATER_THAN);
        break;
      
      // Healthcare-specific operators
      case '|':
        if (this.match('>')) {
          this.addToken(TokenType.PIPELINE); // Pipeline operator |>
        } else {
          this.addToken(TokenType.OR);
        }
        break;
      
      // Logical operators
      case '&':
        if (this.match('&')) {
          this.addToken(TokenType.AND);
        } else {
          this.error(`Unexpected character: ${c}`);
        }
        break;
      
      // Whitespace
      case ' ':
      case '\r':
      case '\t':
        // Ignore whitespace
        break;
      
      case '\n':
        this.line++;
        this.column = 1;
        break;
      
      // String literals
      case '"': this.string(); break;
      
      default:
        if (this.isDigit(c)) {
          this.number();
        } else if (this.isAlpha(c)) {
          this.identifier();
        } else {
          this.error(`Unexpected character: ${c}`);
        }
        break;
    }
  }
  
  /**
   * Process a string literal
   */
  private string(): void {
    while (this.peek() !== '"' && !this.isAtEnd()) {
      if (this.peek() === '\n') {
        this.line++;
        this.column = 1;
      }
      this.advance();
    }
    
    if (this.isAtEnd()) {
      this.error('Unterminated string.');
      return;
    }
    
    // Consume the closing "
    this.advance();
    
    // Extract the string value (without the quotes)
    const value = this.source.substring(this.start + 1, this.current - 1);
    this.addToken(TokenType.STRING, value);
  }
  
  /**
   * Process a number literal
   */
  private number(): void {
    while (this.isDigit(this.peek())) {
      this.advance();
    }
    
    // Look for a decimal part
    if (this.peek() === '.' && this.isDigit(this.peekNext())) {
      // Consume the "."
      this.advance();
      
      while (this.isDigit(this.peek())) {
        this.advance();
      }
    }
    
    const value = parseFloat(this.source.substring(this.start, this.current));
    this.addToken(TokenType.NUMBER, value);
  }
  
  /**
   * Process an identifier or keyword
   */
  private identifier(): void {
    while (this.isAlphaNumeric(this.peek())) {
      this.advance();
    }
    
    // Check if the identifier is a keyword
    const text = this.source.substring(this.start, this.current);
    const type = KEYWORDS[text] || TokenType.IDENTIFIER;
    
    this.addToken(type);
  }
  
  /**
   * Utility methods
   */
  private isAtEnd(): boolean {
    return this.current >= this.source.length;
  }
  
  private advance(): string {
    const char = this.source.charAt(this.current);
    this.current++;
    this.column++;
    return char;
  }
  
  private peek(): string {
    if (this.isAtEnd()) return '\0';
    return this.source.charAt(this.current);
  }
  
  private peekNext(): string {
    if (this.current + 1 >= this.source.length) return '\0';
    return this.source.charAt(this.current + 1);
  }
  
  private match(expected: string): boolean {
    if (this.isAtEnd()) return false;
    if (this.source.charAt(this.current) !== expected) return false;
    
    this.current++;
    this.column++;
    return true;
  }
  
  private isDigit(c: string): boolean {
    return c >= '0' && c <= '9';
  }
  
  private isAlpha(c: string): boolean {
    return (c >= 'a' && c <= 'z') ||
           (c >= 'A' && c <= 'Z') ||
           c === '_';
  }
  
  private isAlphaNumeric(c: string): boolean {
    return this.isAlpha(c) || this.isDigit(c);
  }
  
  private addToken(type: TokenType, literal?: any): void {
    const text = this.source.substring(this.start, this.current);
    this.tokens.push({
      type,
      lexeme: text,
      literal,
      line: this.line,
      column: this.column - text.length
    });
  }
  
  private error(message: string): void {
    this.tokens.push({
      type: TokenType.ERROR,
      lexeme: message,
      line: this.line,
      column: this.column
    });
  }
}
