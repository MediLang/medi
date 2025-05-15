//! Parser implementation for Medi language using nom and TokenSlice
//! Handles healthcare-specific constructs, expressions, and statements

use medic_ast::ast::{
    BinaryExpressionNode, BinaryOperator, BlockNode, ExpressionNode, IdentifierNode,
    LetStatementNode, LiteralNode, LiteralValueNode, PatternNode, ProgramNode, ReturnNode,
    StatementNode,
};
use medic_lexer::token::{Token, TokenType};
use nom::{
    branch::alt,
    combinator::{map, opt},
    error::{Error as NomError, ErrorKind, ParseError},
    multi::many0,
    sequence::terminated, // Added for parse_statement
    Compare,
    IResult,
    InputIter,
    InputLength,
    InputTake,
    InputTakeAtPosition,
    Slice,
};
use std::iter::Enumerate;
use std::ops::{Range, RangeFrom, RangeTo}; // Range, RangeTo might be needed for Slice impl

// --- TokenSlice Definition ---
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct TokenSlice<'a>(pub &'a [Token]);

impl<'a> TokenSlice<'a> {
    /// Creates a new `TokenSlice` from a slice of tokens.
    ///
    /// # Examples
    ///
    /// ```
    /// let tokens = vec![Token::new(TokenType::Identifier, "foo", 0)];
    /// let slice = TokenSlice::new(&tokens);
    /// assert_eq!(slice.len(), 1);
    /// ```
    pub fn new(data: &'a [Token]) -> Self {
        TokenSlice(data)
    }

    /// Returns `true` if the token slice contains no tokens.
    ///
    /// # Examples
    ///
    /// ```
    /// let tokens: Vec<Token> = vec![];
    /// let slice = TokenSlice(&tokens);
    /// assert!(slice.is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Returns the number of tokens in the slice.
    ///
    /// # Examples
    ///
    /// ```
    /// let tokens = vec![token1, token2, token3];
    /// let slice = TokenSlice(&tokens);
    /// assert_eq!(slice.len(), 3);
    /// ```
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns a reference to the first token in the slice without consuming it.
    ///
    /// # Examples
    ///
    /// ```
    /// let tokens = vec![Token::new(TokenType::Identifier, "foo", 0)];
    /// let slice = TokenSlice(&tokens);
    /// assert_eq!(slice.peek().unwrap().token_type, TokenType::Identifier);
    /// ```
    pub fn peek(&self) -> Option<&Token> {
        self.0.first()
    }
}

// --- nom Trait Implementations for TokenSlice ---

impl InputLength for TokenSlice<'_> {
    #[inline]
    /// Returns the number of tokens in the slice.
    ///
    /// # Examples
    ///
    /// ```
    /// let tokens = vec![token1, token2, token3];
    /// let slice = TokenSlice(&tokens);
    /// assert_eq!(slice.input_len(), 3);
    /// ```
    fn input_len(&self) -> usize {
        self.0.len()
    }
}

impl InputTake for TokenSlice<'_> {
    #[inline]
    /// Returns a new `TokenSlice` containing the first `count` tokens of the current slice.
    ///
    /// # Examples
    ///
    /// ```
    /// let tokens = vec![token1, token2, token3];
    /// let slice = TokenSlice(&tokens);
    /// let prefix = slice.take(2);
    /// assert_eq!(prefix.len(), 2);
    /// ```
    fn take(&self, count: usize) -> Self {
        TokenSlice(&self.0[0..count])
    }

    #[inline]
    /// Splits the token slice at the given index, returning the remaining and taken parts as expected by nom.
    ///
    /// The returned tuple contains the suffix (remaining input) and the prefix (taken input), matching nom's convention.
    ///
    /// # Examples
    ///
    /// ```
    /// let tokens = vec![token_a, token_b, token_c];
    /// let slice = TokenSlice(&tokens);
    /// let (remaining, taken) = slice.take_split(2);
    /// assert_eq!(taken.len(), 2);
    /// assert_eq!(remaining.len(), 1);
    /// ```
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.0.split_at(count);
        (TokenSlice(suffix), TokenSlice(prefix)) // nom expects (remaining, taken)
    }
}

// InputTakeSplit implementation removed as TokenSlice already has take_split in InputTake impl
impl<'a> InputIter for TokenSlice<'a> {
    type Item = &'a Token;
    type Iter = Enumerate<std::slice::Iter<'a, Token>>;
    type IterElem = std::slice::Iter<'a, Token>;

    #[inline]
    /// Returns an iterator over the indices and tokens in the slice.
    ///
    /// # Examples
    ///
    /// ```
    /// let tokens = vec![Token::new(TokenType::Identifier, "a"), Token::new(TokenType::Integer, "1")];
    /// let slice = TokenSlice(&tokens);
    /// let mut iter = slice.iter_indices();
    /// assert_eq!(iter.next().unwrap().0, 0);
    /// assert_eq!(iter.next().unwrap().0, 1);
    /// ```
    fn iter_indices(&self) -> Self::Iter {
        self.0.iter().enumerate()
    }
    #[inline]
    /// Returns an iterator over the tokens in the slice.
    ///
    /// # Examples
    ///
    /// ```
    /// let tokens = vec![Token::new(TokenType::Identifier, "foo"), Token::new(TokenType::Integer, "42")];
    /// let slice = TokenSlice(&tokens);
    /// let mut iter = slice.iter_elements();
    /// assert_eq!(iter.next().unwrap().token_type, TokenType::Identifier);
    /// assert_eq!(iter.next().unwrap().token_type, TokenType::Integer);
    /// assert!(iter.next().is_none());
    /// ```
    fn iter_elements(&self) -> Self::IterElem {
        self.0.iter()
    }
    #[inline]
    /// Returns the index of the first token for which the predicate returns true, or `None` if no such token exists.
    ///
    /// # Examples
    ///
    /// ```
    /// let tokens = vec![Token::new(TokenType::Integer, "1"), Token::new(TokenType::Plus, "+")];
    /// let slice = TokenSlice(&tokens);
    /// let idx = slice.position(|t| t.token_type == TokenType::Plus);
    /// assert_eq!(idx, Some(1));
    /// ```
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.0.iter().position(predicate)
    }
    #[inline]
    /// Returns the index for slicing if the token slice contains at least `count` elements.
    ///
    /// Returns `Ok(count)` if the slice is long enough, or an error indicating how many additional elements are needed.
    ///
    /// # Examples
    ///
    /// ```
    /// let tokens = vec![Token::dummy(); 3];
    /// let slice = TokenSlice(&tokens);
    /// assert_eq!(slice.slice_index(2), Ok(2));
    /// assert!(slice.slice_index(5).is_err());
    /// ```
    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        if self.0.len() >= count {
            Ok(count)
        } else {
            Err(nom::Needed::new(count - self.0.len()))
        }
    }
}

impl<T> Compare<T> for TokenSlice<'_>
where
    T: AsRef<[Token]>, // Allows comparison with &[Token], Vec<Token>, etc.
{
    #[inline]
    /// Compares the token types of this `TokenSlice` with another slice, ignoring token locations.
    ///
    /// Returns `Ok` if the token types match exactly or if this slice is longer but matches the prefix of the other.
    /// Returns `Incomplete` if this slice is shorter but matches the prefix of the other.
    /// Returns `Error` if any token type does not match.
    ///
    /// # Examples
    ///
    /// ```
    /// use medi_parser::{Token, TokenType, TokenSlice};
    /// use nom::Compare;
    ///
    /// let tokens1 = [Token { token_type: TokenType::Identifier, ..Default::default() }];
    /// let tokens2 = [Token { token_type: TokenType::Identifier, ..Default::default() }];
    /// let slice1 = TokenSlice(&tokens1);
    /// let slice2 = TokenSlice(&tokens2);
    /// assert_eq!(slice1.compare(&tokens2), nom::CompareResult::Ok);
    /// ```
    fn compare(&self, t: T) -> nom::CompareResult {
        let t_slice = t.as_ref();
        let l = self.0.len();
        let r = t_slice.len();
        let min_len = std::cmp::min(l, r);

        for (i, token) in self.0.iter().enumerate().take(min_len) {
            // Compare TokenType for equality. Location/span is ignored for parsing logic.
            if token.token_type != t_slice[i].token_type {
                return nom::CompareResult::Error;
            }
        }

        match l.cmp(&r) {
            std::cmp::Ordering::Equal => nom::CompareResult::Ok,
            std::cmp::Ordering::Less => nom::CompareResult::Incomplete,
            std::cmp::Ordering::Greater => nom::CompareResult::Ok, // self is longer and a prefix match
        }
    }

    /// For token-based parsing, this is equivalent to a standard comparison since case-insensitivity does not apply to token types.
    fn compare_no_case(&self, t: T) -> nom::CompareResult {
        self.compare(t)
    }
}

impl<'a> InputTakeAtPosition for TokenSlice<'a> {
    type Item = &'a Token;

    /// Splits the token slice at the first position where the predicate returns true.
    ///
    /// Returns a tuple containing the prefix up to the matching token and the remainder starting at that token.
    /// If no token satisfies the predicate, returns an `Incomplete` error indicating more input is needed.
    ///
    /// # Examples
    ///
    /// ```
    /// use medi_parser::TokenSlice;
    /// let tokens = vec![Token::new_int(1), Token::new_int(2), Token::new_int(3)];
    /// let slice = TokenSlice(&tokens);
    /// let (rest, prefix) = slice.split_at_position(|t| t.is_value(2)).unwrap();
    /// assert_eq!(prefix.len(), 1);
    /// assert_eq!(rest.len(), 2);
    /// ```
    fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.0.iter().position(predicate) {
            Some(n) => Ok(self.take_split(n)),
            None => Err(nom::Err::Incomplete(nom::Needed::new(1))),
        }
    }

    /// Splits the token slice at the first position where the predicate returns true, requiring at least one token before the match.
    ///
    /// Returns an error if the predicate matches at the first position or if no match is found.
    ///
    /// # Examples
    ///
    /// ```
    /// use nom::error::ErrorKind;
    /// let tokens = [Token::new(TokenType::Identifier, "foo"), Token::new(TokenType::Equal, "=")];
    /// let slice = TokenSlice(&tokens);
    /// let result = slice.split_at_position1(|t| matches!(t.token_type, TokenType::Equal), ErrorKind::Tag);
    /// assert!(result.is_ok());
    /// ```
    fn split_at_position1<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.0.iter().position(predicate) {
            Some(0) => Err(nom::Err::Error(E::from_error_kind(*self, e))),
            Some(n) => Ok(self.take_split(n)),
            None => Err(nom::Err::Incomplete(nom::Needed::new(1))),
        }
    }

    /// Splits the token slice at the first position where the predicate returns true, or at the end if no such position exists.
    ///
    /// Returns a tuple containing the prefix up to the matching token and the remainder starting at the matching token. If no token satisfies the predicate, the entire slice is returned as the prefix and the remainder is empty.
    ///
    /// # Examples
    ///
    /// ```
    /// let tokens = vec![Token::new(TokenType::IntLiteral, "1"), Token::new(TokenType::Plus, "+"), Token::new(TokenType::IntLiteral, "2")];
    /// let slice = TokenSlice(&tokens);
    /// let (prefix, remainder) = slice.split_at_position_complete(|t| t.token_type == TokenType::Plus).unwrap().1;
    /// assert_eq!(prefix.len(), 1);
    /// assert_eq!(remainder.len(), 2);
    /// ```
    fn split_at_position_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.0.iter().position(predicate) {
            Some(n) => Ok(self.take_split(n)),
            None => Ok(self.take_split(self.input_len())),
        }
    }

    /// Splits the token slice at the first position where the predicate returns true, requiring at least one token before the match.
    ///
    /// Returns an error if the predicate matches at the first position or if the input is empty. If the predicate does not match any token, splits at the end of the slice.
    ///
    /// # Examples
    ///
    /// ```
    /// use nom::error::ErrorKind;
    /// let tokens = [Token::new(TokenType::Identifier, "foo"), Token::new(TokenType::Equal, "=")];
    /// let slice = TokenSlice(&tokens);
    /// let result = slice.split_at_position1_complete(|t| matches!(t.token_type, TokenType::Equal), ErrorKind::Tag);
    /// assert!(result.is_ok());
    /// let (after, before) = result.unwrap();
    /// assert_eq!(before.len(), 1);
    /// assert_eq!(after.len(), 1);
    /// ```
    fn split_at_position1_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.0.iter().position(predicate) {
            Some(0) => Err(nom::Err::Error(E::from_error_kind(*self, e))),
            Some(n) => Ok(self.take_split(n)),
            None => {
                if self.input_len() == 0 {
                    Err(nom::Err::Error(E::from_error_kind(*self, e)))
                } else {
                    Ok(self.take_split(self.input_len()))
                }
            }
        }
    }
}

// Slice implementations to allow TokenSlice to be sliced.
impl Slice<Range<usize>> for TokenSlice<'_> {
    #[inline]
    /// Returns a new `TokenSlice` containing the tokens within the specified range.
    ///
    /// # Examples
    ///
    /// ```
    /// let tokens = vec![token1, token2, token3];
    /// let slice = TokenSlice(&tokens);
    /// let sub = slice.slice(1..3);
    /// assert_eq!(sub.len(), 2);
    /// ```
    fn slice(&self, range: Range<usize>) -> Self {
        TokenSlice(&self.0[range])
    }
}

impl Slice<RangeTo<usize>> for TokenSlice<'_> {
    #[inline]
    /// Returns a new `TokenSlice` containing all tokens from the start up to the specified exclusive end index.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::ops::RangeTo;
    /// let tokens = vec![token_a, token_b, token_c];
    /// let slice = TokenSlice(&tokens);
    /// let sub = slice.slice(..2);
    /// assert_eq!(sub.len(), 2);
    /// ```
    fn slice(&self, range: RangeTo<usize>) -> Self {
        TokenSlice(&self.0[range])
    }
}

impl Slice<RangeFrom<usize>> for TokenSlice<'_> {
    #[inline]
    /// Returns a new `TokenSlice` containing tokens from the specified starting index to the end.
    ///
    /// # Examples
    ///
    /// ```
    /// let tokens = vec![token_a, token_b, token_c];
    /// let slice = TokenSlice(&tokens);
    /// let subslice = slice.slice(1..);
    /// assert_eq!(subslice.len(), 2);
    /// ```
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        TokenSlice(&self.0[range])
    }
}

// --- Helper Parser Functions ---

/// Returns a parser that consumes and returns the next token if its type satisfies the given predicate.
///
/// The parser returns an error of the specified kind if the input is empty or the predicate does not match.
///
/// # Examples
///
/// ```
/// use medi_parser::{take_token_if, Token, TokenType, TokenSlice};
/// use nom::error::ErrorKind;
///
/// let tokens = vec![Token { token_type: TokenType::Identifier, location: 0 }];
/// let input = TokenSlice(&tokens);
/// let mut parser = take_token_if(|t| matches!(t, TokenType::Identifier), ErrorKind::Tag);
/// let (remaining, token) = parser(input).unwrap();
/// assert_eq!(token.token_type, TokenType::Identifier);
/// assert!(remaining.is_empty());
/// ```pub fn take_token_if<'a, F, E: ParseError<TokenSlice<'a>>>(
    mut pred: F,
    err_kind: ErrorKind,
) -> impl FnMut(TokenSlice<'a>) -> IResult<TokenSlice<'a>, Token, E>
where
    F: FnMut(&TokenType) -> bool, // Changed to take a reference to TokenType
{
    move |input: TokenSlice<'a>| {
        if input.is_empty() {
            return Err(nom::Err::Error(E::from_error_kind(input, err_kind)));
        }
        let first_token = &input.0[0];
        if pred(&first_token.token_type) {
            // Pass reference to TokenType
            let (remaining_input, _) = input.take_split(1);
            Ok((remaining_input, first_token.clone())) // Clone Token for output
        } else {
            Err(nom::Err::Error(E::from_error_kind(input, err_kind)))
        }
    }
}

// ---- Core Parser Functions ----

/// Parses an identifier token from the input and returns an `IdentifierNode`.
///
/// Returns an error if the input is empty or the next token is not an identifier.
///
/// # Examples
///
/// ```
/// use medi_parser::{parse_identifier, Token, TokenType, TokenSlice, IdentifierNode};
/// let tokens = vec![Token { token_type: TokenType::Identifier("foo".to_string()), location: 0 }];
/// let input = TokenSlice(&tokens);
/// let (remaining, node) = parse_identifier(input).unwrap();
/// assert_eq!(node.name, "foo");
/// assert!(remaining.is_empty());
/// ```pub fn parse_identifier(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, IdentifierNode> {
    if input.is_empty() {
        // Using NomError::from_error_kind directly as per nom 7.x preferred style
        return Err(nom::Err::Error(NomError::from_error_kind(
            input,
            ErrorKind::Eof,
        )));
    }
    // Peek at the first token without consuming, using the slice directly.
    let token = &input.0[0];
    match &token.token_type {
        TokenType::Identifier(name) => {
            // take_split(1) consumes the token and returns (remaining_input, consumed_token_slice)
            let (remaining_input, _) = input.take_split(1);
            Ok((remaining_input, IdentifierNode { name: name.clone() }))
        }
        _ => Err(nom::Err::Error(NomError::from_error_kind(
            input,
            ErrorKind::Tag,
        ))),
    }
}

/// Parses an expression, handling literals, identifiers, and potentially more complex forms.
/// Parses an expression, handling operator precedence and associativity.
///
/// This function serves as the entry point for parsing expressions in the Medi language,
/// supporting binary operators with correct precedence and associativity rules.
///
/// # Examples
///
/// ```
/// # use medi_parser::{parse_expression, Token, TokenType, TokenSlice, ExpressionNode};
/// let tokens = vec![
///     Token::new(TokenType::IntegerLiteral(2), 0..1),
///     Token::new(TokenType::Plus, 1..2),
///     Token::new(TokenType::IntegerLiteral(3), 2..3),
///     Token::new(TokenType::Star, 3..4),
///     Token::new(TokenType::IntegerLiteral(4), 4..5),
/// ];
/// let input = TokenSlice(&tokens);
/// let (rest, expr) = parse_expression(input).unwrap();
/// assert!(matches!(expr, ExpressionNode::Binary(_)));
/// ```
pub fn parse_expression(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode>pub fn parse_expression(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode> {
    // For now, calls the recursive helper starting with the lowest precedence level (0).
    // The actual operator precedence logic will be built into parse_binary_expression_recursive.
    parse_binary_expression_recursive(input, 0)
}

/// Parses primary expressions (literals, identifiers, parenthesized expressions).
/// Parses a primary expression, such as a literal or identifier.
///
/// This function serves as the base case for the recursive descent precedence climbing parser,
/// handling literals and identifiers as primary expressions. Placeholders exist for future support
/// of parenthesized expressions, function calls, member access, and healthcare queries.
///
/// # Examples
///
/// ```
/// # use medi_parser::{parse_primary_expression, Token, TokenType, TokenSlice, ExpressionNode};
/// let tokens = [Token::new(TokenType::IntegerLiteral(42), 0..2)];
/// let input = TokenSlice(&tokens);
/// let (rest, expr) = parse_primary_expression(input).unwrap();
/// assert!(matches!(expr, ExpressionNode::Literal(_)));
/// ```fn parse_primary_expression(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode> {
    alt((
        parse_literal_expression,
        map(parse_identifier, ExpressionNode::Identifier),
        // Placeholder for parenthesized expressions: delimited(tag(TokenType::LeftParen), parse_expression, tag(TokenType::RightParen))
        // Placeholder for function calls: parse_call_expression
        // Placeholder for member access: parse_member_expression
        // Placeholder for healthcare queries: parse_healthcare_query_expression
    ))(input)
}

// Placeholder for parse_call_expression, parse_member_expression, parse_healthcare_query_expression
// These will be implemented later when their AST nodes and token types are fully defined.
// fn parse_call_expression<'a>(input: TokenSlice<'a>) -> IResult<TokenSlice<'a>, ExpressionNode> { todo!() }
// fn parse_member_expression<'a>(input: TokenSlice<'a>) -> IResult<TokenSlice<'a>, ExpressionNode> { todo!() }
// fn parse_healthcare_query_expression<'a>(input: TokenSlice<'a>) -> IResult<TokenSlice<'a>, ExpressionNode> { todo!() }

// ---- Binary Expression Parsing with Precedence ----

/// Returns the precedence level for a given binary operator TokenType.
/// Returns the precedence level for a given binary operator token type.
///
/// Higher numbers indicate higher precedence. Non-operator tokens return -2.
///
/// # Examples
///
/// ```
/// let prec = get_operator_precedence(&TokenType::Plus);
/// assert_eq!(prec, 2);
/// let prec = get_operator_precedence(&TokenType::Or);
/// assert_eq!(prec, -1);
/// let prec = get_operator_precedence(&TokenType::Identifier);
/// assert_eq!(prec, -2);
/// ```fn get_operator_precedence(token_type: &TokenType) -> i32 {
    match token_type {
        TokenType::Star | TokenType::Slash | TokenType::Percent => 3,
        TokenType::Plus | TokenType::Minus => 2,
        TokenType::Less
        | TokenType::LessEqual
        | TokenType::Greater
        | TokenType::GreaterEqual
        | TokenType::EqualEqual
        | TokenType::NotEqual => 1,
        TokenType::And => 0,
        TokenType::Or => -1,
        _ => -2, // Not a binary operator or lowest precedence
    }
}

/// Parses a binary operator token from the input and returns the corresponding `BinaryOperator`.
///
/// Returns an error if the next token is not a recognized binary operator.
///
/// # Examples
///
/// ```
/// use medi_parser::{parse_binary_operator, Token, TokenType, TokenSlice, BinaryOperator};
/// let tokens = [Token { token_type: TokenType::Plus, location: 0 }];
/// let input = TokenSlice(&tokens);
/// let (rest, op) = parse_binary_operator(input).unwrap();
/// assert_eq!(op, BinaryOperator::Add);
/// assert!(rest.is_empty());
/// ```pub fn parse_binary_operator(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, BinaryOperator> {
    if input.is_empty() {
        return Err(nom::Err::Error(NomError::from_error_kind(
            input,
            ErrorKind::Eof,
        )));
    }
    let token = &input.0[0];
    let op = match &token.token_type {
        TokenType::Plus => Some(BinaryOperator::Add),
        TokenType::Minus => Some(BinaryOperator::Sub),
        TokenType::Star => Some(BinaryOperator::Mul),
        TokenType::Slash => Some(BinaryOperator::Div),
        TokenType::Percent => Some(BinaryOperator::Mod),
        TokenType::EqualEqual => Some(BinaryOperator::Eq),
        TokenType::NotEqual => Some(BinaryOperator::Ne),
        TokenType::Less => Some(BinaryOperator::Lt),
        TokenType::LessEqual => Some(BinaryOperator::Le),
        TokenType::Greater => Some(BinaryOperator::Gt),
        TokenType::GreaterEqual => Some(BinaryOperator::Ge),
        TokenType::And => Some(BinaryOperator::And),
        TokenType::Or => Some(BinaryOperator::Or),
        _ => None,
    };

    if let Some(operator) = op {
        let (remaining_input, _) = input.take_split(1);
        Ok((remaining_input, operator))
    } else {
        Err(nom::Err::Error(NomError::from_error_kind(
            input,
            ErrorKind::Tag,
        )))
    }
}

/// Recursively parses binary expressions using precedence climbing.
/// Recursively parses a binary expression using operator precedence climbing.
///
/// This function parses binary expressions by repeatedly parsing primary expressions and combining them with binary operators according to their precedence. It ensures correct grouping of operators by comparing each operator's precedence to the provided `min_precedence`, supporting left-associative operators.
///
/// # Parameters
/// - `min_precedence`: The minimum precedence level an operator must have to be parsed in this call. Operators with lower precedence terminate the current expression.
///
/// # Returns
/// Returns a tuple containing the remaining input and the parsed `ExpressionNode` representing the binary expression.
///
/// # Examples
///
/// ```
/// // Suppose tokens represent: 1 + 2 * 3
/// let tokens = vec![
///     Token::int_literal(1),
///     Token::plus(),
///     Token::int_literal(2),
///     Token::star(),
///     Token::int_literal(3),
/// ];
/// let input = TokenSlice(&tokens);
/// let (_, expr) = parse_binary_expression_recursive(input, 0).unwrap();
/// // expr now represents the AST for (1 + (2 * 3))
/// ```fn parse_binary_expression_recursive(
    input: TokenSlice<'_>,
    min_precedence: i32,
) -> IResult<TokenSlice<'_>, ExpressionNode> {
    // Parse the left-hand side expression (could be a primary expression or another binary expression).
    let (mut remaining_input, mut lhs) = parse_primary_expression(input)?;

    loop {
        // Peek at the next token to see if it's an operator.
        if remaining_input.is_empty() {
            break; // No more tokens, end of expression.
        }
        let potential_op_token = &remaining_input.0[0];
        let op_precedence = get_operator_precedence(&potential_op_token.token_type);

        // If the operator's precedence is less than the minimum for this recursive call, stop.
        if op_precedence < min_precedence {
            break;
        }

        // It's an operator we should handle. Parse it.
        let (after_op_input, operator) = parse_binary_operator(remaining_input)?;

        // Parse the right-hand side expression.
        // For right-associative operators, pass op_precedence.
        // For left-associative, pass op_precedence + 1 to bind tighter.
        // Most arithmetic operators are left-associative.
        let (after_rhs_input, rhs) =
            parse_binary_expression_recursive(after_op_input, op_precedence + 1)?;

        // Combine LHS, operator, and RHS into a new LHS.
        lhs = ExpressionNode::Binary(Box::new(BinaryExpressionNode {
            left: lhs,
            operator,
            right: rhs,
        }));
        remaining_input = after_rhs_input; // Continue parsing from after the RHS.
    }

    Ok((remaining_input, lhs))
}

// ---- Literal Parsing ----

/// Parses a literal token and returns it as an expression node.
///
/// # Examples
///
/// ```
/// # use medi_parser::{parse_literal_expression, Token, TokenType, TokenSlice, ExpressionNode, LiteralNode};
/// let tokens = [Token { token_type: TokenType::Integer(42), location: 0 }];
/// let input = TokenSlice(&tokens);
/// let (rest, expr) = parse_literal_expression(input).unwrap();
/// match expr {
///     ExpressionNode::Literal(LiteralNode::Integer(42)) => {},
///     _ => panic!("Expected integer literal expression"),
/// }
/// assert!(rest.is_empty());
/// ```pub fn parse_literal_expression(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode> {
    map(parse_literal, ExpressionNode::Literal)(input)
}

/// Parses a single literal token (integer, float, string, or boolean) from the input and returns a `LiteralNode`.
///
/// Returns an error if the next token is not a supported literal type or if the input is empty.
///
/// # Examples
///
/// ```
/// use medi_parser::{parse_literal, Token, TokenType, TokenSlice, LiteralNode, LiteralValueNode};
///
/// let tokens = [Token { token_type: TokenType::Integer(42), location: 0 }];
/// let input = TokenSlice(&tokens);
/// let (remaining, node) = parse_literal(input).unwrap();
/// assert!(matches!(node, LiteralNode::Int(LiteralValueNode::Int(42))));
/// assert!(remaining.is_empty());
/// ```pub fn parse_literal(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, LiteralNode> {
    if input.is_empty() {
        return Err(nom::Err::Error(NomError::from_error_kind(
            input,
            ErrorKind::Eof,
        )));
    }
    let token = &input.0[0];
    match &token.token_type {
        TokenType::Integer(val) => {
            let (remaining, _) = input.take_split(1);
            Ok((remaining, LiteralNode::Int(LiteralValueNode::Int(*val))))
        }
        TokenType::Float(val) => {
            let (remaining, _) = input.take_split(1);
            Ok((remaining, LiteralNode::Float(LiteralValueNode::Float(*val))))
        }
        TokenType::String(val) => {
            let (remaining, _) = input.take_split(1);
            Ok((
                remaining,
                LiteralNode::String(LiteralValueNode::String(val.clone())),
            ))
        }
        TokenType::Boolean(val) => {
            let (remaining, _) = input.take_split(1);
            Ok((remaining, LiteralNode::Bool(LiteralValueNode::Bool(*val))))
        }
        // Consider adding TokenType::Null if your language supports it
        // TokenType::Null => {
        //     let (remaining, _) = input.take_split(1);
        //     Ok((remaining, LiteralNode::Null(LiteralValueNode::Null)))
        // }
        _ => Err(nom::Err::Error(NomError::from_error_kind(
            input,
            ErrorKind::Tag,
        ))),
    }
}

// ---- Statement Parsing ----

/// Parses a block of statements enclosed in curly braces.
/// Parses a block of statements enclosed in braces, optionally ending with a trailing expression.
///
/// A block consists of zero or more statements, followed by an optional trailing expression without a semicolon. The trailing expression, if present, is wrapped as an expression statement and included in the block's statements.
///
/// # Examples
///
/// ```
/// // Given tokens for: { let x = 5; x }
/// let tokens = vec![
///     Token::new(TokenType::LeftBrace, ...),
///     Token::new(TokenType::Let, ...),
///     Token::new(TokenType::Identifier("x".into()), ...),
///     Token::new(TokenType::Equals, ...),
///     Token::new(TokenType::IntegerLiteral(5), ...),
///     Token::new(TokenType::Semicolon, ...),
///     Token::new(TokenType::Identifier("x".into()), ...),
///     Token::new(TokenType::RightBrace, ...),
/// ];
/// let input = TokenSlice(&tokens);
/// let (_, block) = parse_block(input).unwrap();
/// assert_eq!(block.statements.len(), 2);
/// ```
pub fn parse_block(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, BlockNode> {pub fn parse_block(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, BlockNode> {
    let (i, _) = take_token_if(|tt| matches!(tt, TokenType::LeftBrace), ErrorKind::Tag)(input)?;

    // Use many0 to parse zero or more statements within the block.
    let (i, mut statements) = many0(parse_statement)(i)?;

    // Check if there's a trailing expression (without semicolon)
    let (i, maybe_expr) = opt(parse_expression)(i)?;

    // If we found a trailing expression, add it as an expression statement
    if let Some(expr) = maybe_expr {
        statements.push(StatementNode::Expr(expr));
    }

    let (i, _) = take_token_if(|tt| matches!(tt, TokenType::RightBrace), ErrorKind::Tag)(i)?;
    Ok((i, BlockNode { statements }))
}

/// Parses a let statement.
/// Parses a `let` statement, producing a `StatementNode::Let`.
///
/// Expects the sequence: `let <identifier> = <expression>;`.
///
/// # Examples
///
/// ```
/// # use medi_parser::{parse_let_statement, Token, TokenType, TokenSlice, StatementNode};
/// let tokens = vec![
///     Token::new(TokenType::Let, "let"),
///     Token::new(TokenType::Identifier, "x"),
///     Token::new(TokenType::Equal, "="),
///     Token::new(TokenType::IntegerLiteral, "10"),
///     Token::new(TokenType::Semicolon, ";"),
/// ];
/// let input = TokenSlice(&tokens);
/// let (_, stmt) = parse_let_statement(input).unwrap();
/// match stmt {
///     StatementNode::Let(_) => {},
///     _ => panic!("Expected let statement"),
/// }
/// ```pub fn parse_let_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    let (i, _) = take_token_if(|tt| matches!(tt, TokenType::Let), ErrorKind::Tag)(input)?;
    let (i, identifier_node) = parse_identifier(i)?;
    let (i, _) = take_token_if(|tt| matches!(tt, TokenType::Equal), ErrorKind::Tag)(i)?;
    let (i, value_node) = parse_expression(i)?;
    let (i, _) = take_token_if(|tt| matches!(tt, TokenType::Semicolon), ErrorKind::Tag)(i)?;
    Ok((
        i,
        StatementNode::Let(Box::new(LetStatementNode {
            name: identifier_node, // Corrected field name from 'identifier' to 'name'
            value: value_node,
        })),
    ))
}

/// Parses a return statement.
/// Parses a `return` statement, optionally followed by an expression and a semicolon.
///
/// Returns a `StatementNode::Return` containing a `ReturnNode` with an optional expression value.
///
/// # Examples
///
/// ```
/// use medi_parser::{parse_return_statement, Token, TokenType, TokenSlice, StatementNode, ReturnNode, ExpressionNode};
///
/// let tokens = vec![
///     Token::new(TokenType::Return, 0..6),
///     Token::new(TokenType::Identifier("x".into()), 7..8),
///     Token::new(TokenType::Semicolon, 8..9),
/// ];
/// let input = TokenSlice(&tokens);
/// let (_, stmt) = parse_return_statement(input).unwrap();
/// if let StatementNode::Return(ret) = stmt {
///     assert!(ret.value.is_some());
/// }
/// ```
pub fn parse_return_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode>pub fn parse_return_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    let (i, _) = take_token_if(|tt| matches!(tt, TokenType::Return), ErrorKind::Tag)(input)?;
    // Use nom's `opt` combinator to parse an optional expression.
    let (i, value_opt) = opt(parse_expression)(i)?;
    let (i, _) = take_token_if(|tt| matches!(tt, TokenType::Semicolon), ErrorKind::Tag)(i)?;
    Ok((
        i,
        StatementNode::Return(Box::new(ReturnNode {
            value: value_opt.map(Box::new),
        })),
    ))
}

/// Parses any valid statement.
/// Parses a single statement from the input token stream.
///
/// Attempts to parse the input as a let statement, return statement, block, assignment, or expression statement (terminated by a semicolon), in that order. Returns the corresponding `StatementNode` on success.
///
/// # Examples
///
/// ```
/// # use medi_parser::{parse_statement, TokenSlice, Token, TokenType, StatementNode};
/// let tokens = vec![
///     Token { token_type: TokenType::Let, ..Default::default() },
///     Token { token_type: TokenType::Identifier("x".into()), ..Default::default() },
///     Token { token_type: TokenType::Equals, ..Default::default() },
///     Token { token_type: TokenType::Integer(42), ..Default::default() },
///     Token { token_type: TokenType::Semicolon, ..Default::default() },
/// ];
/// let input = TokenSlice(&tokens);
/// let result = parse_statement(input);
/// assert!(matches!(result, Ok((_, StatementNode::Let(_)))));
/// ```pub fn parse_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    alt((
        parse_let_statement,    // Assumes this is the TokenSlice version
        parse_return_statement, // Assumes this is the TokenSlice version
        // Block statements
        map(parse_block, StatementNode::Block), // Assumes parse_block is TokenSlice version
        parse_assignment_statement,             // Assignment statement
        // Expression statements (e.g., function_call(); or assignment;)
        // Must be followed by a semicolon.
        map(
            terminated(
                parse_expression,
                take_token_if(|tt| matches!(tt, TokenType::Semicolon), ErrorKind::Tag),
            ),
            StatementNode::Expr,
        ), // TODO: Add other statement parsers here, e.g.:
           // parse_if_statement,
           // parse_while_statement,
           // parse_for_statement,
           // parse_match_statement,
    ))(input)
}
/// Parses an entire Medi program as a sequence of statements.
///
/// Returns a `ProgramNode` containing all parsed statements.
///
/// # Examples
///
/// ```
/// let tokens = lex("let x = 1; return x;").unwrap();
/// let input = TokenSlice(&tokens);
/// let (_, program) = parse_program(input).unwrap();
/// assert_eq!(program.statements.len(), 2);
/// ```pub fn parse_program(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ProgramNode> {
    // A program is zero or more statements.
    let (i, statements) = many0(parse_statement)(input)?;
    // Ensure all input is consumed by the program parser, or it's an error if there are trailing tokens.
    // For now, we allow trailing tokens, as `many0` will stop on first error / non-match.
    // To enforce full consumption, one might add: `if !i.is_empty() { return Err(...) }`
    Ok((i, ProgramNode { statements }))
}

// ---- Placeholder functions from before - to be implemented or refined ----

/// Returns an error indicating that parsing of `if` statements is not yet implemented.
///
/// # Examples
///
/// ```
/// let tokens = vec![]; // Replace with tokens representing an if statement
/// let input = TokenSlice(&tokens);
/// let result = parse_if_statement(input);
/// assert!(result.is_err());
/// ```pub fn parse_if_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    // Placeholder implementation
    Err(nom::Err::Error(NomError::new(
        input,
        ErrorKind::Permutation,
    )))
}

/// Parses an assignment statement of the form `<identifier> = <expression>;`.
///
/// Returns a `StatementNode::Assignment` containing the parsed assignment.
///
/// # Examples
///
/// ```
/// # use medic_parser::{parse_assignment_statement, Token, TokenType, TokenSlice, StatementNode};
/// # use medic_ast::ast::{AssignmentNode, ExpressionNode, IdentifierNode};
/// let tokens = vec![
///     Token::new(TokenType::Identifier("x".into())),
///     Token::new(TokenType::Equal),
///     Token::new(TokenType::IntegerLiteral(42)),
///     Token::new(TokenType::Semicolon),
/// ];
/// let input = TokenSlice(&tokens);
/// let (_, stmt) = parse_assignment_statement(input).unwrap();
/// if let StatementNode::Assignment(assign) = stmt {
///     if let ExpressionNode::Identifier(IdentifierNode { name, .. }) = &assign.target {
///         assert_eq!(name, "x");
///     }
/// }
/// ```pub fn parse_assignment_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    // Parse identifier
    let (i, id) = parse_identifier(input)?;

    // Parse equals sign
    let (i, _) = take_token_if(|tt| matches!(tt, TokenType::Equal), ErrorKind::Tag)(i)?;

    // Parse expression
    let (i, value) = parse_expression(i)?;

    // Parse semicolon
    let (i, _) = take_token_if(|tt| matches!(tt, TokenType::Semicolon), ErrorKind::Tag)(i)?;

    // Create assignment node
    let target = ExpressionNode::Identifier(id);
    let assignment = medic_ast::ast::AssignmentNode { target, value };

    Ok((i, StatementNode::Assignment(Box::new(assignment))))
}

/// Returns an error indicating that parsing of `while` statements is not yet implemented.
///
/// # Examples
///
/// ```
/// let tokens = vec![]; // No tokens for demonstration
/// let input = TokenSlice(&tokens);
/// let result = parse_while_statement(input);
/// assert!(result.is_err());
/// ```pub fn parse_while_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    // Placeholder implementation - to be refactored with TokenSlice
    Err(nom::Err::Error(NomError::from_error_kind(
        input,
        ErrorKind::Permutation,
    )))
}

/// Returns an error indicating that parsing of `for` statements is not yet implemented.
///
/// # Examples
///
/// ```
/// let tokens = vec![]; // No tokens, as parsing is unimplemented
/// let input = TokenSlice(&tokens);
/// let result = parse_for_statement(input);
/// assert!(result.is_err());
/// ```pub fn parse_for_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    // Placeholder implementation - to be refactored with TokenSlice
    Err(nom::Err::Error(NomError::from_error_kind(
        input,
        ErrorKind::Permutation,
    )))
}

/// Returns an error indicating that parsing of match statements is not yet implemented.
///
/// # Examples
///
/// ```
/// let tokens = vec![]; // No tokens, as parsing is unimplemented
/// let input = TokenSlice(&tokens);
/// let result = parse_match_statement(input);
/// assert!(result.is_err());
/// ```pub fn parse_match_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    // Placeholder implementation - to be refactored with TokenSlice
    Err(nom::Err::Error(NomError::from_error_kind(
        input,
        ErrorKind::Permutation,
    )))
}

/// Returns an error indicating that pattern parsing is not yet implemented.
///
/// # Examples
///
/// ```
/// let tokens = TokenSlice(&[]);
/// let result = parse_pattern(tokens);
/// assert!(result.is_err());
/// ```pub fn parse_pattern(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, PatternNode> {
    // Placeholder implementation - to be refactored with TokenSlice for actual pattern parsing
    Err(nom::Err::Error(NomError::from_error_kind(
        input,
        ErrorKind::Permutation,
    )))
}

#[cfg(test)]
mod tests;
