use crate::string_interner::InternedString;
use crate::token::{Location, Token, TokenType};
use crate::LogosToken;

/// Controls how certain token categories are mapped during conversion.
#[derive(Debug, Clone, Copy, Default)]
pub struct ConversionConfig {
    /// If true, map healthcare keywords (e.g., `patient`, `fhir_query`) to Identifier tokens
    /// instead of dedicated healthcare TokenType variants.
    pub healthcare_keywords_as_identifiers: bool,
    /// If true, map medical operators like `of` and `per` to Identifier tokens
    /// instead of dedicated operator TokenType variants.
    pub medical_operators_as_identifiers: bool,
}

/// Convert a `LogosToken` plus its `lexeme` into a semantic `Token` with the given `location`.
///
/// This centralizes all mapping from lexer-specific tokens to compiler semantic tokens.
/// Callers remain responsible for position tracking and any filtering decisions (e.g.,
/// whether to include whitespace or comments).
pub fn convert_logos_to_token(
    logos_token: LogosToken,
    lexeme: &str,
    location: Location,
    config: ConversionConfig,
) -> Token {
    let token_type = match logos_token {
        // Keywords
        LogosToken::Module => TokenType::Module,
        LogosToken::Import => TokenType::Import,
        LogosToken::Fn => TokenType::Fn,
        LogosToken::Let => TokenType::Let,
        LogosToken::Const => TokenType::Const,
        LogosToken::Type => TokenType::Type,
        LogosToken::Struct => TokenType::Struct,
        LogosToken::Enum => TokenType::Enum,
        LogosToken::Trait => TokenType::Trait,
        LogosToken::Impl => TokenType::Impl,
        LogosToken::Pub => TokenType::Pub,
        LogosToken::Priv => TokenType::Priv,
        LogosToken::Return => TokenType::Return,
        LogosToken::While => TokenType::While,
        LogosToken::For => TokenType::For,
        LogosToken::In => TokenType::In,
        LogosToken::Match => TokenType::Match,
        LogosToken::If => TokenType::If,
        LogosToken::Else => TokenType::Else,

        // Literals and identifiers
        LogosToken::Integer(i) => TokenType::Integer(i),
        LogosToken::IntegerWithTrailingDot(i) => TokenType::Integer(i),
        LogosToken::NegativeInteger(i) => TokenType::NegativeInteger(i),
        LogosToken::Float(f) => TokenType::Float(f),
        LogosToken::String(s) => TokenType::String(InternedString::from(&s[..])),
        LogosToken::Bool(b) => TokenType::Boolean(b),
        LogosToken::Identifier(ident) => TokenType::Identifier(InternedString::from(&ident[..])),

        // Medical operators
        LogosToken::Of => {
            if config.medical_operators_as_identifiers {
                TokenType::Identifier(InternedString::from("of"))
            } else {
                TokenType::Of
            }
        }
        LogosToken::Per => {
            if config.medical_operators_as_identifiers {
                TokenType::Identifier(InternedString::from("per"))
            } else {
                TokenType::Per
            }
        }

        // Medical codes
        LogosToken::ICD10(code) => TokenType::ICD10(InternedString::from(&code[..])),
        LogosToken::LOINC(code) => TokenType::LOINC(InternedString::from(&code[..])),
        LogosToken::SNOMED(code) => TokenType::SNOMED(InternedString::from(&code[..])),
        LogosToken::CPT(code) => TokenType::CPT(InternedString::from(&code[..])),
        // Function-like medical literals
        LogosToken::PatientIdFunc(pid) => TokenType::PatientId(InternedString::from(&pid[..])),
        LogosToken::ICD10Func(code) => TokenType::ICD10(InternedString::from(&code[..])),

        // Operators (ensure logical vs bitwise are mapped correctly)
        LogosToken::Plus => TokenType::Plus,
        LogosToken::Minus => TokenType::Minus,
        LogosToken::Star => TokenType::Star,
        LogosToken::Slash => TokenType::Slash,
        LogosToken::Percent => TokenType::Percent,
        LogosToken::EqualEqual => TokenType::EqualEqual,
        LogosToken::NotEqual => TokenType::NotEqual,
        LogosToken::Less => TokenType::Less,
        LogosToken::LessEqual => TokenType::LessEqual,
        LogosToken::Greater => TokenType::Greater,
        LogosToken::GreaterEqual => TokenType::GreaterEqual,
        LogosToken::And => TokenType::AndAnd,
        LogosToken::Or => TokenType::OrOr,
        LogosToken::Not => TokenType::Not,
        LogosToken::Equal => TokenType::Equal,
        LogosToken::PlusEqual => TokenType::PlusEqual,
        LogosToken::MinusEqual => TokenType::MinusEqual,
        LogosToken::StarEqual => TokenType::StarEqual,
        LogosToken::SlashEqual => TokenType::SlashEqual,
        LogosToken::PercentEqual => TokenType::PercentEqual,
        LogosToken::DoubleStar => TokenType::DoubleStar,
        LogosToken::DoubleStarAssign => TokenType::DoubleStarAssign,
        LogosToken::BitAnd => TokenType::BitAnd,
        LogosToken::BitAndAssign => TokenType::BitAndAssign,
        LogosToken::BitOr => TokenType::BitOr,
        LogosToken::BitOrAssign => TokenType::BitOrAssign,
        LogosToken::BitXor => TokenType::BitXor,
        LogosToken::BitXorAssign => TokenType::BitXorAssign,
        LogosToken::Shl => TokenType::Shl,
        LogosToken::ShlAssign => TokenType::ShlAssign,
        LogosToken::Shr => TokenType::Shr,
        LogosToken::ShrAssign => TokenType::ShrAssign,
        LogosToken::QuestionQuestion => TokenType::QuestionQuestion,
        LogosToken::QuestionColon => TokenType::QuestionColon,
        LogosToken::Range => TokenType::Range,
        LogosToken::RangeInclusive => TokenType::RangeInclusive,

        // Delimiters and punctuation
        LogosToken::LeftParen => TokenType::LeftParen,
        LogosToken::RightParen => TokenType::RightParen,
        LogosToken::LeftBrace => TokenType::LeftBrace,
        LogosToken::RightBrace => TokenType::RightBrace,
        LogosToken::LeftBracket => TokenType::LeftBracket,
        LogosToken::RightBracket => TokenType::RightBracket,
        LogosToken::Comma => TokenType::Comma,
        LogosToken::Dot => TokenType::Dot,
        LogosToken::Colon => TokenType::Colon,
        LogosToken::Semicolon => TokenType::Semicolon,
        LogosToken::Arrow => TokenType::Arrow,
        LogosToken::FatArrow => TokenType::FatArrow,
        LogosToken::Underscore => TokenType::Underscore,

        // Healthcare keywords
        LogosToken::Patient => {
            if config.healthcare_keywords_as_identifiers {
                TokenType::Identifier(InternedString::from("patient"))
            } else {
                TokenType::Patient
            }
        }
        LogosToken::Observation => {
            if config.healthcare_keywords_as_identifiers {
                TokenType::Identifier(InternedString::from("observation"))
            } else {
                TokenType::Observation
            }
        }
        LogosToken::Medication => {
            if config.healthcare_keywords_as_identifiers {
                TokenType::Identifier(InternedString::from("medication"))
            } else {
                TokenType::Medication
            }
        }
        LogosToken::FhirQuery => {
            if config.healthcare_keywords_as_identifiers {
                TokenType::Identifier(InternedString::from("fhir_query"))
            } else {
                TokenType::FhirQuery
            }
        }
        LogosToken::Query => {
            if config.healthcare_keywords_as_identifiers {
                TokenType::Identifier(InternedString::from("query"))
            } else {
                TokenType::Query
            }
        }
        LogosToken::Regulate => {
            if config.healthcare_keywords_as_identifiers {
                TokenType::Identifier(InternedString::from("regulate"))
            } else {
                TokenType::Regulate
            }
        }
        LogosToken::Scope => {
            if config.healthcare_keywords_as_identifiers {
                TokenType::Identifier(InternedString::from("scope"))
            } else {
                TokenType::Scope
            }
        }
        LogosToken::Federated => {
            if config.healthcare_keywords_as_identifiers {
                TokenType::Identifier(InternedString::from("federated"))
            } else {
                TokenType::Federated
            }
        }
        LogosToken::Safe => {
            if config.healthcare_keywords_as_identifiers {
                TokenType::Identifier(InternedString::from("safe"))
            } else {
                TokenType::Safe
            }
        }
        LogosToken::RealTime => {
            if config.healthcare_keywords_as_identifiers {
                TokenType::Identifier(InternedString::from("real_time"))
            } else {
                TokenType::RealTime
            }
        }

        // Error and skipped categories
        LogosToken::Error => {
            // Preserve legacy behavior where lexers emit an Error token containing a message
            // that includes the offending lexeme. This is relied upon by tests.
            let msg = format!("Invalid token '{lexeme}'");
            TokenType::Error(InternedString::from(msg.as_str()))
        }
        // Note: Logos is configured to skip whitespace/comments, so these are unlikely to be produced.
        LogosToken::Whitespace => TokenType::Whitespace,
        LogosToken::LineComment | LogosToken::BlockComment => {
            TokenType::Comment(InternedString::from(lexeme))
        }
    };

    Token::new(token_type, lexeme, location)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn loc() -> Location {
        Location {
            line: 1,
            column: 1,
            offset: 0,
        }
    }

    #[test]
    fn healthcare_keywords_default_map_to_dedicated_tokens() {
        let cfg = ConversionConfig::default();

        let t = convert_logos_to_token(LogosToken::Patient, "patient", loc(), cfg);
        assert!(matches!(t.token_type, TokenType::Patient));

        let t = convert_logos_to_token(LogosToken::Observation, "observation", loc(), cfg);
        assert!(matches!(t.token_type, TokenType::Observation));

        let t = convert_logos_to_token(LogosToken::Medication, "medication", loc(), cfg);
        assert!(matches!(t.token_type, TokenType::Medication));

        let t = convert_logos_to_token(LogosToken::FhirQuery, "fhir_query", loc(), cfg);
        assert!(matches!(t.token_type, TokenType::FhirQuery));

        let t = convert_logos_to_token(LogosToken::Query, "query", loc(), cfg);
        assert!(matches!(t.token_type, TokenType::Query));

        let t = convert_logos_to_token(LogosToken::Regulate, "regulate", loc(), cfg);
        assert!(matches!(t.token_type, TokenType::Regulate));

        let t = convert_logos_to_token(LogosToken::Scope, "scope", loc(), cfg);
        assert!(matches!(t.token_type, TokenType::Scope));

        let t = convert_logos_to_token(LogosToken::Federated, "federated", loc(), cfg);
        assert!(matches!(t.token_type, TokenType::Federated));

        let t = convert_logos_to_token(LogosToken::Safe, "safe", loc(), cfg);
        assert!(matches!(t.token_type, TokenType::Safe));

        let t = convert_logos_to_token(LogosToken::RealTime, "real_time", loc(), cfg);
        assert!(matches!(t.token_type, TokenType::RealTime));
    }

    #[test]
    fn healthcare_keywords_as_identifiers_when_configured() {
        let cfg = ConversionConfig {
            healthcare_keywords_as_identifiers: true,
            ..Default::default()
        };

        let t = convert_logos_to_token(LogosToken::Patient, "patient", loc(), cfg);
        assert!(matches!(t.token_type, TokenType::Identifier(ref s) if s.as_str() == "patient"));

        let t = convert_logos_to_token(LogosToken::Observation, "observation", loc(), cfg);
        assert!(
            matches!(t.token_type, TokenType::Identifier(ref s) if s.as_str() == "observation")
        );

        let t = convert_logos_to_token(LogosToken::Medication, "medication", loc(), cfg);
        assert!(matches!(t.token_type, TokenType::Identifier(ref s) if s.as_str() == "medication"));

        let t = convert_logos_to_token(LogosToken::FhirQuery, "fhir_query", loc(), cfg);
        assert!(matches!(t.token_type, TokenType::Identifier(ref s) if s.as_str() == "fhir_query"));

        let t = convert_logos_to_token(LogosToken::Query, "query", loc(), cfg);
        assert!(matches!(t.token_type, TokenType::Identifier(ref s) if s.as_str() == "query"));

        let t = convert_logos_to_token(LogosToken::Regulate, "regulate", loc(), cfg);
        assert!(matches!(t.token_type, TokenType::Identifier(ref s) if s.as_str() == "regulate"));

        let t = convert_logos_to_token(LogosToken::Scope, "scope", loc(), cfg);
        assert!(matches!(t.token_type, TokenType::Identifier(ref s) if s.as_str() == "scope"));

        let t = convert_logos_to_token(LogosToken::Federated, "federated", loc(), cfg);
        assert!(matches!(t.token_type, TokenType::Identifier(ref s) if s.as_str() == "federated"));

        let t = convert_logos_to_token(LogosToken::Safe, "safe", loc(), cfg);
        assert!(matches!(t.token_type, TokenType::Identifier(ref s) if s.as_str() == "safe"));

        let t = convert_logos_to_token(LogosToken::RealTime, "real_time", loc(), cfg);
        assert!(matches!(t.token_type, TokenType::Identifier(ref s) if s.as_str() == "real_time"));
    }

    #[test]
    fn medical_operators_default_and_identifier_config() {
        let cfg_default = ConversionConfig::default();
        let t = convert_logos_to_token(LogosToken::Of, "of", loc(), cfg_default);
        assert!(matches!(t.token_type, TokenType::Of));
        let t = convert_logos_to_token(LogosToken::Per, "per", loc(), cfg_default);
        assert!(matches!(t.token_type, TokenType::Per));

        let cfg_id = ConversionConfig {
            medical_operators_as_identifiers: true,
            ..Default::default()
        };
        let t = convert_logos_to_token(LogosToken::Of, "of", loc(), cfg_id);
        assert!(matches!(t.token_type, TokenType::Identifier(ref s) if s.as_str() == "of"));
        let t = convert_logos_to_token(LogosToken::Per, "per", loc(), cfg_id);
        assert!(matches!(t.token_type, TokenType::Identifier(ref s) if s.as_str() == "per"));
    }

    #[test]
    fn medical_codes_preserve_lexeme_and_value() {
        let cfg = ConversionConfig::default();
        let t = convert_logos_to_token(
            LogosToken::ICD10("ICD10:A01.1".into()),
            "ICD10:A01.1",
            loc(),
            cfg,
        );
        match t.token_type {
            TokenType::ICD10(ref s) => assert_eq!(s.as_str(), "ICD10:A01.1"),
            _ => panic!("expected ICD10"),
        }
        assert_eq!(t.lexeme.as_str(), "ICD10:A01.1");

        let t = convert_logos_to_token(
            LogosToken::LOINC("LOINC:12345-6".into()),
            "LOINC:12345-6",
            loc(),
            cfg,
        );
        match t.token_type {
            TokenType::LOINC(ref s) => assert_eq!(s.as_str(), "LOINC:12345-6"),
            _ => panic!("expected LOINC"),
        }

        let t = convert_logos_to_token(
            LogosToken::SNOMED("SNOMED:1234567".into()),
            "SNOMED:1234567",
            loc(),
            cfg,
        );
        match t.token_type {
            TokenType::SNOMED(ref s) => assert_eq!(s.as_str(), "SNOMED:1234567"),
            _ => panic!("expected SNOMED"),
        }

        let t = convert_logos_to_token(
            LogosToken::CPT("CPT:12345A-7".into()),
            "CPT:12345A-7",
            loc(),
            cfg,
        );
        match t.token_type {
            TokenType::CPT(ref s) => assert_eq!(s.as_str(), "CPT:12345A-7"),
            _ => panic!("expected CPT"),
        }
    }
}
