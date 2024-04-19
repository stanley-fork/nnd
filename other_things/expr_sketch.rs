struct EvalScope {
    vars: HashMap<String, Value>,
}

// Persists across frames to allow caching results of slow operations like full-text search for partially qualified type/function name.
// The outermost scope is preserved across expressions to allow variables assigned in one watch to be used in later watches. Not preserved across re-evaluations.
struct EvalState {
    binaries: Pool<BinaryInfo>,
    types: Types, // cleared on each frame
    scopes: Vec<EvalScope>,
}


enum LiteralValue {
    Unsigned(u64),
    Signed(i64),
    Float(f64),
    String(String),
}
impl LiteralValue {
    fn is_unsigned(&self) -> bool { match self { Self::Unsigned => true, _ => false } }
}

enum UnaryOperator {
    Neg, // unary -
    Not, // unary !

    Borrow, // &
    Dereference, // *
}

enum BinaryOperator {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Rem, // %
    BitAnd, // &
    BitOr, // |
    BitXor, // ^
    Shl, // <<
    Shr, // >>

    Eq, // ==
    Ne, // !=
    Gt, // >
    Lt, // <
    Ge, // >=
    Le, // <=

    LazyAnd, // &&
    LazyOr, // ||

    Assign, // =
    AddAssign, // +=
    SubAssign, // -=
    MulAssign, // *=
    DivAssign, // /=
    RemAssign, // %=
    AndAssign, // &=
    OrAssign, // |=
    XorAssign, // ^=
    ShlAssign, // <<=
    ShrAssign, // >>=

    Range, // ..
}

struct ASTIdx(usize);

// Syntax and names are partially stolen from https://doc.rust-lang.org/reference/expressions.html
enum AST {
    Literal(LiteralValue), // 42.69, "foo"
    Variable(String), // foo::bar::variable
    UnaryOperator(UnaryOperator), // -x, !f
    BinaryOperator(BinaryOperator), // a + b, x %= 2
    Array, // [a, 42, b]
    Index, // a[b]
    Tuple, // (a, 42, "foo", b)
    TupleIndexing(usize), // t.0
    StructExpression(Vec<String>), // struct { x: 42, y: 69 }
    Call(String), // f(1, "foo")
    Field(String), // a.b
    Continue, // continue
    Break, // break
    Return, // return

    Block { // {a += 1; f(x); 42}
        discard_value: bool, // if final semicolon or empty block
    },

    While, // while i < 10 { i += 1; }
    For(String), // for i in 10..20 { i += 1 }
    If, // if a < b { a = b; } else { b = a; }

    Let { // let v = 42
        name: String,
        type_name: Option<String>,
    },

    TypeCast(String), // a as i64
    TypeByName(String), // #type(i64)

    FunctionDefinition {
        name: String,
        args: Vec<(/*name*/ String, /*type*/ String)>,
        return_type: Option<String>,
    },
    StructDefinition {
        name: String,
        fields: Vec<(/*name*/ String, /*type*/ String)>,
    },
}

struct ASTNode {
    range: Range<usize>,
    type_: Option<*const TypeInfo>,
    a: AST,
    children: Vec<ASTIdx>,
}

struct InputStream<'a> {
    input: &'a str,
    pos: usize,
}
impl<'a> InputStream<'a> {
    fn peek(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }
    fn eat(&mut self) -> Option<char> {
        let mut it = self.input[self.pos..].chars();
        let r = it.next();
        self.pos = it.as_str().as_ptr() - self.input.as_ptr();
        r
    }
    fn eat_if_eq(&mut self, c: char) -> bool {
        match self.peek() {
            Some(x) if x == c => {
                self.eat();
                return true;
            }
            _ => false,
        }
    }
    fn eat_digits(&mut self) -> &'a str {
        let start = self.pos;
        let mut end = start;
        let mut it = self.input[self.pos..].chars();
        loop {
            self.pos = it.as_str().as_ptr() - self.input.as_ptr();
            let c = match it.next() {
                None => break,
                Some(c) => c };
            match c {
                '0'..='9' | 'a'..='f' | 'A'..='F' => (),
                _ => break,
            }
        }
        &self.input[start..self.pos]
    }
}

struct Lexer<'a> {
    input: InputStream<'a>,
    next_tokens: Vec<(Range<usize>, Token<'a>)>,
}
impl<'a> Lexer<'a> {
    // peek(1) to get next token without consuming it, peek(2) for second-next, etc
    fn peek(&mut self, n: usize) -> Result<(Range<usize>, &Token<'a>)> {
        assert!(n > 0);
        loop {
            if let Some((r, t)) = self.next_tokens.get(n-1) {
                return Ok((r.clone(), t));
            }

            let c = match self.input.eat() {
                None => return Ok((self.input.pos..self.input.pos, Token::Eof)),
                Some(c) => c };
            let start = self.input.pos;
            let token = match c {
                ' ' | '\t' | '\n' | '\r' => continue,
                '+' => Token::BinaryOperator(if self.input.eat_if_eq('=') { BinaryOperator::AddAssign } else { BinaryOperator::Add }),
                '-' => if input.eat_if_eq('>') { Token::Arrow } else { Token::BinaryOperator(if self.input.eat_if_eq('=') { BinaryOperator::SubAssign } else { BinaryOperator::Sub }) },
                '*' => Token::BinaryOperator(if self.input.eat_if_eq('=') { BinaryOperator::MulAssign } else { BinaryOperator::Mul }),
                '/' => Token::BinaryOperator(if self.input.eat_if_eq('=') { BinaryOperator::DivAssign } else { BinaryOperator::Div }),
                '%' => Token::BinaryOperator(if self.input.eat_if_eq('=') { BinaryOperator::RemAssign } else { BinaryOperator::Rem }),
                '&' => Token::BinaryOperator(if self.input.eat_if_eq('=') { BinaryOperator::AndAssign } else if self.input.eat_if_eq('&') { BinaryOperator::LazyAnd } else { BinaryOperator::BitAnd }),
                '|' => Token::BinaryOperator(if self.input.eat_if_eq('=') { BinaryOperator::OrAssign } else if self.input.eat_if_eq('|') { BinaryOperator::LazyOr } else { BinaryOperator::BitOr }),
                '^' => Token::BinaryOperator(if self.input.eat_if_eq('=') { BinaryOperator::XorAssign } else { BinaryOperator::BitXor }),
                '<' => Token::BinaryOperator(if self.input.eat_if_eq('=') { BinaryOperator::Le } else if self.input.eat_if_eq('<') { if self.input.eat_if_eq('=') { BinaryOperator::ShlAssign } else { BinaryOperator::Shl } } else { BinaryOperator::Lt }),
                '>' => Token::BinaryOperator(if self.input.eat_if_eq('=') { BinaryOperator::Ge } else if self.input.eat_if_eq('>') { if self.input.eat_if_eq('=') { BinaryOperator::ShrAssign } else { BinaryOperator::Shr } } else { BinaryOperator::Gt }),
                '=' => Token::BinaryOperator(if self.input.eat_if_eq('=') { BinaryOperator::Eq } else { BinaryOperator::Assign }),
                '.' => if self.input.eat_if_eq('.') { Token::BinaryOperator(BinaryOperator::Range) } else { Token::Char('.') },
                '!' => if self.input.eat_if_eq('=') { Token::BinaryOperator(BinaryOperator::Ne) } else { Token::UnaryOperator(UnaryOperator::Not) },
                ',' | '(' | ')' | '[' | ']' | '{' | '}' | ';' => Token::Char(c),
                '"' => {
                    let mut s = String::new();
                    loop {
                        match self.input.eat() {
                            None => return err!(Syntax, "unterminated string literal at {}", start),
                            Some('"') => break,
                            Some('\\') => s.push(self.parse_char_escape_sequence("string", start)?),
                            Some(c) => s.push(c),
                        }
                    }
                    Token::Literal(LiteralValue::String(s))
                }
                '\'' => {
                    let c = match self.input.eat() {
                        None => return err!(Syntax, "unterminated char literal at {}", start),
                        Some('\'') => return err!(Syntax, "empty char literal at {}", start),
                        Some('\\') => self.parse_char_escape_sequence("char", start),
                        Some(c) => c,
                    };
                    match self.input.eat() {
                        None => return err!(Syntax, "unterminated char literal at {}", start),
                        Some('\'') => (),
                        Some(c) => return err!(Syntax, "char literal is too long at {}", start),
                    }
                    Token::Literal(LiteralValue::Unsigned(c as u32 as usize))
                }
                '0'..='9' => {
                    let int_s = self.input.eat_digits();
                    let mut radix = 10u32;
                    if int_s == "0" {
                        if self.input.eat_if_eq('x') {
                            radix = 16;
                        } else if self.input.eat_if_eq('b') {
                            radix = 2;
                        }
                    }
                    let (has_dot, frac_s) = if self.input.eat_if_eq('.') {
                        (true, self.input.eat_digits())
                    } else {
                        (false, "")
                    };
                    let exp_radix = if self.input.eat_if_eq('e') {
                        10u32
                    } else if self.input.eat_if_eq('p') {
                        2
                    } else {
                        0
                    };
                    let exp_negative = exp_radix != 0 && self.input.eat_if_eq('-');
                    let exp_s = if exp_radix == 0 { "" } else { self.input.eat_digits() };
                    for c in int_s.chars() { if !c.is_digit(radix) { return err!(Syntax, "invalid digit '{}' in number at {}", c, start); } }
                    for c in frac_s.chars() { if !c.is_digit(radix) { return err!(Syntax, "invalid digit '{}' in fraction at {}", c, start); } }
                    for c in exp_s.chars() { if !c.is_digit(exp_radix) { return err!(Syntax, "invalid digit '{}' in exponent at {}", c, start); } }
                    if has_dot || exp_radix != 0 {
                        let mut val = 0.0f64;
                        for c in int_s.chars() {
                            val = val * radix as f64 + c.to_digit(radix).unwrap() as f64;
                        }
                        let mut frac = 0.0f64;
                        for c in frac_s.chars().rev() {
                            // This accumulates error, there's probably a better way.
                            frac = (frac + c.as_digit(radix).unwrap() as f64) / radix as f64;
                        }
                        val += frac;
                        if exp_radix != 0 {
                            let mut exp = 0i32;
                            for c in exp_s.chars() {
                                exp = exp.saturating_mul(exp_radix as isize).saturating_add(c.as_digit(exp_radix).unwrap() as isize);
                            }
                            if exp_negative {
                                exp = -exp;
                            }
                            val *= (exp_radix as f64).powi(exp);
                        }
                        Token::Literal(LiteralValue::Float(val))
                    } else {
                        let mut val = 0usize;
                        for c in int_s.chars() {
                            val = match val.checked_mul(radix as usize).map_or(None, |x| x.checked_add(c.as_digit(radix).unwrap() as usize)) {
                                None => return err!(Syntax, "integer literal is too big at {}", start),
                                Some(v) => v,
                            };
                        }
                        Token::Literal(LiteralValue::Unsigned(val))
                    }
                }
                '`' => {
                    let mut s = String::new();
                    loop {
                        match self.input.eat() {
                            None => return err!(Syntax, "unterminated quoted identifier at {}", start),
                            Some('`') => break,
                            Some('\\') => s.push(self.parse_char_escape_sequence("quoted identifier", start)?),
                            Some(c) => s.push(c),
                        }
                    }
                    Token::Identifier {quoted: true, s}
                }
                'a'..='z' | 'A'..='Z' | '_' | '#' | '@' | '$' => {
                    let mut s = String::new();
                    loop {
                        let c = match self.input.peek() {
                            None => break,
                            Some(c) => c };
                        match c {
                            'a'..='z' | 'A'..='Z' | '_' | '#' | '@' | '$' | '0'..='9' => (),
                            _ => break,
                        }
                        s.push(c);
                        self.input.eat();
                    }
                    Token::Identifier {quoted: false, s}
                }
            };
            self.next_tokens.push((start..self.input.pos, token));
        }
    }

    fn parse_char_escape_sequence(&mut self, literal_type: &str, literal_start: usize) -> Result<char> {
        Ok(
            match self.input.eat() {
                None => return err!(Syntax, "unterminated {} literal at {}", literal_type, literal_start),
                Some('0') => '\0',
                Some('n') => '\n',
                Some('r') => '\r',
                Some('t') => '\t',
                Some('x') => {
                    let (a, b) = (self.input.eat(), self.input.eat());
                    if b.is_none() { return err!(Syntax, "unterminated {} literal at {}", literal_type, literal_start); }
                    let (a, b) = (a.unwrap(), b.unwrap());
                    match (v[0].to_digit(16), v[1].to_digit(16)) {
                        (Some(a), Some(b)) => s.push((a*16 + b) as char),
                        _ => return err!(Syntax, "invalid escape sequence in {} literal: \"\\x{}\" at {}", literal_type, v, self.input.pos - 4),
                    }
                }
            }
        )
    }

    fn eat(&mut self, n: usize) -> Result<(Range<usize>, Token<'a>)> {
        assert!(n > 0);
        self.peek(n)?;
        Ok(self.next_tokens.drain(..n).last().unwrap_or_else(|| (self.pos..self.pos, Token::Eof)))
    }

    fn eat_if<F: FnOnce(&Token<'a>) -> bool>(&mut self, f: F) -> Result<bool> {
        Ok(if f(self.peek(1)?.1) {
            self.next_tokens.remove(0);
            true
        } else {
            false
        })
    }

    fn peek_expect<F: FnOnce(&Token<'a>) -> bool>(&mut self, what: &str, f: F) -> Result<(Range<usize>, &Token<'a>)> {
        let (r, t) = self.peek(1)?;
        if !f(t) {
            return err!(Syntax, "expected {}, found {:?} at {}", what, t, r.start);
        }
        Ok((r, t))
    }

    fn expect<F: FnOnce(&Token<'a>) -> bool>(&mut self, what: &str, f: F) -> Result<(Range<usize>, Token<'a>)> {
        self.peek_expect(what, f)?;
        Ok(self.eat(1)?)
    }
}

enum Token<'a> {
    BinaryOperator(BinaryOperator), // abusing the struct to mean specific characters (e.g. '*') rather than their semantics (e.g. multiplication or dereference)
    UnaryOperator(UniaryOperator),
    Arrow, // ->
    Char(char), // , [ ] ( ) { } etc
    Literal(LiteralValue), // 42 "foo" etc
    Identifier {
        s: &'a str,
        quoted: bool,
    },
    Eof,
}
impl<'a> Token<'a> {
    fn is_identifier(&self) -> bool { match self { Self::Identifier {..} => true, _ => false } }
    fn as_identifier(&self) -> &'a str { match self { Self::Identifier {s, ..} => s, _ => panic!("identifier expected") } }
}

struct Expression {
    ast: Vec<ASTNode>,
    root: ASTIdx,
}

fn parse_block(lex: &mut Lexer, expr: &mut Expression) -> Result<ASTIdx> {
    lex.expect('{', |t| t == Token::Char('{'))?;
    let r = parse_expression(lex, expr, Precedence::Weakest)?;
    lex.expect('}', |t| t == Token::Char('}'))?;
    Ok(r)
}

fn parse_type_name(lex: &mut Lexer) -> Result<String> {
    Ok(lex.expect("type name", |t| t.is_identifier())?.1.as_identifier().to_string())
}

fn parse_expression(lex: &mut Lexer, expr: &mut Expression, outer_precedence: Precedence) -> Result<ASTIdx> {
    let (range, token) = lex.eat(1)?;
    let mut node = ASTNode {range, type_: None, children: Vec::new(), a: AST::Tuple};
    let mut replace: Option<ASTIdx> = None; // ignore `node` and use this node as the initial expression
    node.a = match token {
        Token::Literal(value) => AST::Literal(value),
        Token::Identifier{s, quoted} => {
            let mut ast = AST::Variable(s.clone());
            if !quoted {
                match &s {
                    "struct" => {
                        let (range, token) = lex.eat(1)?;
                        match token {
                            Token::Char('{') => {
                                let mut field_names: Vec<String> = Vec::new();
                                loop {
                                    let (r, t) = lex.eat(1)?;
                                    let name = match t {
                                        Token::Identifier {s, ..} => s,
                                        Token::Char('}') => break,
                                        _ => return err!(Syntax, "expected field name, got {:?} at {}", t, r.start),
                                    };
                                    lex.expect("':'", |t| t == Token::Char(':'))?;
                                    let ex = parse_expression(lex, expr, Precedence::Weakest)?;
                                    field_names.push(name);
                                    node.children.push(ex);

                                    if !lex.eat_if(|t| t == Token::Char(','))? {
                                        lex.peek_expect("'}'", |t| t == Token::Char('}'))?;
                                    }
                                }
                                ast = AST::StructExpression(field_names)
                            }
                            Token::Identifier {quoted, s: name} => {
                                lex.expect("struct definition body", |t| t == Token::Char('{'))?;
                                let mut fields: Vec<(String, String)> = Vec::new();
                                loop {
                                    let (r, t) = lex.eat(1)?;
                                    let name = match t {
                                        Token::Identifier {s, ..} => s,
                                        Token::Char('}') => break,
                                        _ => return err!(Syntax, "expected field name, got {:?} at {}", t, r.start),
                                    };
                                    lex.expect("':'", |t| t == Token::Char(':'))?;
                                    let type_ = parse_type_name(lex)?;
                                    fields.push((name, type_));

                                    if !lex.eat_if(|t| t == Token::Char(','))? {
                                        lex.peek_expect("'}'", |t| t == Token::Char('}'))?;
                                    }
                                }
                                ast = AST::StructDefinition {name, fields};
                            }
                            _ => return err!(Syntax, "expected struct name or body, got {:?} at {}", token, range.start),
                        }
                    }
                    "continue" => ast = AST::Continue,
                    "break" => ast = AST::Break,
                    "return" => {
                        match lex.peek(1)?.1 {
                            Token::Eof | Token::Char(';') | Token::Char(',') | Token::Char('}') | Token::Char(')') => (),
                            _ => node.children.push(parse_expression(lex, expr, Precedence::Return)?),
                        }
                        ast = AST::Return;
                    }
                    "while" => {
                        node.children.push(parse_expression(lex, expr, Precedence::Weakest)?);
                        node.children.push(parse_block(lex, expr)?);
                        ast = AST::While;
                    }
                    "for" => {
                        let name = lex.expect("loop variable name", |t| t.is_identifier())?.1.as_identifier().to_string();
                        lex.expect("'in'", |t| t == Token::Identifier {quoted: false, s: "in"})?;
                        node.children.push(parse_expression(lex, expr, Precedence::Weakest)?);
                        node.children.push(parse_block(lex, expr)?);
                        ast = AST::For(name);
                    }
                    "loop" => {
                        node.children.push(parse_block(lex, expr)?);
                        ast = AST::While;
                    }
                    "if" => {
                        node.children.push(parse_expression(lex, expr, Precedence::Weakest)?);
                        node.children.push(parse_block(lex, expr)?);

                        if lex.eat_if(|t| t == Token::Identifier {quoted: false, s: "else"}) {
                            if lex.peek(1)?.1 == Token::Identifier {quoted: false, s: "if"} {
                                node.children.push(parse_expression(lex, expr, Precedence::Strongest)?);
                            } else {
                                node.children.push(parse_block(lex, expr)?);
                            }
                        }

                        ast = AST::If;
                    }
                    "let" => {
                        let name = lex.expect("variable name", |t| t.is_identifier())?.1.as_identifier().to_string();
                        let type_name = if lex.eat_if(|t| t == Token::Char(':')) {
                            Some(parse_type_name(lex)?)
                        } else {
                            None
                        };
                        if lex.eat_if(|t| t == Token::BinaryOperator(BinaryOperator::Assign))? {
                            node.children.push(parse_expression(lex, expr, Precedence::Weakest)?);
                        }
                        ast = AST::Let {name, type_name};
                    }
                    "fn" => {
                        let name = lex.expect("function name", |t| t.is_identifier())?.1.as_identifier().to_string();
                        let mut args: Vec<(String, String)> = Vec::new();
                        lex.expect("'('", |t| t == Token::Char('('))?;
                        while !lex.eat_if(|t| t == Token::Char(')'))? {
                            let arg_name = lex.expect("arg name", |t| t.is_identifier())?.1.as_identifier().to_string();
                            lex.expect("':'", |t| t == Token::Char(':'))?;
                            let arg_type = parse_type_name(lex)?;
                            args.push((arg_name, arg_type));
                        }
                        let return_type = if lex.eat_if(|t| t == Token::Arrow)? {
                            Some(parse_type_name(lex)?)
                        } else {
                            None
                        };
                        node.children.push(parse_block(lex, expr)?);
                        ast = AST::FunctionDefinition {name, args, return_type};
                    }
                    _ => (),
                }
            }
            ast
        }
        Token::BinaryOperator(op) => {
            let unary_op = match op {
                BinaryOperator::Sub => UnaryOperator::Neg, // we could easily fuse sign into numeric literal here, but there's no need
                BinaryOperator::Mul => UnaryOperator::Dereference,
                BinaryOperator::BitAnd => UnaryOperator::Borrow,
                op => return err!(Syntax, "expected expression, got {:?} at {}", op, node.range.start),
            };
            let precedence = unary_operator_precedence(unary_op);
            let e = parse_expression(lex, expr, precedence)?;
            node.children.push(e);
            AST::UnaryOperator(unary_op)
        }
        Token::UnaryOperator(unary_op) => match unary_op {
            UnaryOperator::Neg => {
                let precedence = unary_operator_precedence(unary_op);
                let e = parse_expression(lex, expr, precedence)?;
                node.children.push(e);
                AST::UnaryOperator(unary_op)
            }
            x => panic!("unexpected lexer output: {:?}", x),
        }
        Token::Char('{') => {
            lex.eat(1)?;
            let mut discard_value = false;
            loop {
                if lex.eat_if(|t| t == Token::Char('}'))? {
                    discard_value = true;
                    break;
                }
                node.children.push(parse_expression(lex, expr, Precedence::Weakest)?);
                let (r, t) = lex.eat(1)?;
                match t {
                    Token::Char(';') => continue,
                    Token::Char('}') => break,
                }
            }
            AST::Block {discard_value}
        }
        Token::Char('(') => {
            let e = parse_expression(lex, expr, Precedence::Weakest)?;
            let (r, t) = lex.eat(1)?;
            match t {
                Token::Char(')') => {
                    replace = Some(e);
                    AST::Tuple
                }
                Token::Char(',') => {
                    lex.eat(1)?;
                    node.children.push(e);
                    while !lex.eat_if(|t| t == Token::Char(')'))? {
                        node.children.push(parse_expression(lex, expr, Precedence::Weakest)?);
                        if !lex.eat_if(|t| t== Token::Char(','))? {
                            lex.expect("')'", |t| t == Token::Char(')'))?;
                            break;
                        }
                    }
                    AST::Tuple
                }
                _ => return err!(Syntax, "expected ',' or ')', got {:?} at {}", t, r.start),
            }
        }
        Token::Char('[') => {
            while !lex.eat_if(|t| t == Token::Char(']'))? {
                node.children.push(parse_expression(lex, expr, Precedence::Weakest)?);
                if !lex.eat_if(|t| t == Token::Char(','))? {
                    lex.peek_expect("']'", |t| t == Token::Char(']'))?;
                }
            }
            AST::Array
        }
        Token::Eof => return err!(Syntax, "expected expression, got eof at {}", node.range.start),
    };
    let mut node_idx = if let Some(idx) = replace {
        idx
    } else {
        node.range.end = lex.previous_token_end;
        expr.ast.push(node);
        ASTIdx(expr.ast.len()-1)
    };

    loop {
        let (range, token) = lex.peek(1)?;
        match token {
            &Token::BinaryOperator(op) => {
                let precedence = binary_operator_precedence(op);
                if outer_precedence >= precedence { // left to right associativity (for right-to-left use '>'; for requiring parens, keep track of previous precedences in the loop)
                    break;
                }
                lex.eat(1)?;
                let rhs = parse_expression(lex, expr, precedence)?;
                let node = ASTNode {range: expr.ast[node_idx.0].range.start..expr.ast[rhs.0].range.end, type_: None, children: vec![node_idx, rhs], a: AST::BinaryOperator(op)};
                expr.ast.push(node);
                node_idx = ASTIdx(expr.ast.len()-1);
            }
            Token::Char('(') => {
                if outer_precedence >= Precedence::CallOrIndex {
                    break;
                }
                lex.eat(1)?;
                let node = &mut expr.ast[node_idx.0];
                match &node.a {
                    AST::Variable(name) => {
                        let name = name.clone();
                        if name == "type" {
                            node.a = AST::TypeByName(parse_type_name(lex)?);
                        } else {
                            while lex.peek(1)? != Token::Char(')') {
                                node.children.push(parse_expression(lex, expr, Precedence::Weakest)?);
                                if !lex.eat_if(|t| t == Token::Char(','))? {
                                    lex.peek_expect("')'", |t| t == Token::Char(')'))?;
                                }
                            }
                            node.a = AST::Call(name);
                        }
                        let (r, _) = lex.expect(')', |t| t == Token::Char(')'))?;
                        node.range.end = r.end;
                    }
                    _ => return err!(Syntax, "expression can't be called at {}", range.start),
                }
            }
            Token::Char('[') => {
                if outer_precedence >= Precedence::CallOrIndex {
                    break;
                }
                lex.eat(1)?;
                let rhs = parse_expression(lex, expr, Precedence::Weakest)?;
                lex.expect("']'", |t| t == Token::Char(']'))?;
                let node = ASTNode {range: expr.ast[node_idx.0].range.start..expr.ast[rhs.0].range.end, type_: None, children: vec![node_idx, rhs], a: AST::Index};
                expr.ast.push(node);
                node_idx = ASTIdx(expr.ast.len()-1);
            }
            Token::Char('.') => {
                if outer_precedence >= Precedence::Field {
                    break;
                }
                lex.eat(1)?;
                let (r, t) = lex.eat(1)?;
                let a = match t {
                    Token::Identifier(name) => {
                        AST::Field(name)
                    }
                    Token::Literal(value) if value.is_unsigned() => {
                        AST::TupleIndexing(value.as_unsigned())
                    }
                    _ => return err!(Syntax, "expected field name, got {:?} at {}", t, r.start),
                }
                let node = ASTNode {range: expr.ast[node_idx.0].range.start..r.end, type_: None, children: vec![node_idx], a};
                expr.ast.push(node);
                node_idx = ASTIdx(expr.ast.len()-1);
            }
            Token::Identifier{s, quoted} if !quoted && s == "as" => {
                if outer_precedence >= Precedence::TypeCast {
                    break;
                }
                lex.eat(1)?;
                let name = parse_type_name(lex)?;
                let node = ASTNode {range: expr.ast[node_idx.0].range.start..r.end, type_: None, children: vec![node_idx], AST::TypeCast(name)};
                expr.ast.push(node);
                node_idx = ASTIdx(expr.ast.len()-1);
            }
            _ => break,
        }
    }
    
    Ok(node_idx)
}

// Operator precedence.
#[derive(Eq, PartialEq, Ord, PartialOrd, Debug)]
enum Precedence {
    Weakest,

    Return, // return
    Assign, // = += -= *= /= %= &= |= ^= <<= >>=
    Range, // .. ..=
    LazyOr, // ||
    LazyAnd, // &&
    Cmp, // == != < > <= >=
    Or, // |
    Xor, // ^
    And, // &
    Shift, // << >>
    Add, // + -
    Mul, // * / %
    TypeCast, // x as i64
    Unary, // -x, !b, &v, *p
    CallOrIndex, // f(x), a[i]
    Field, // a.b

    Strongest,
}

fn binary_operator_precedence(op: BinaryOperator) -> Precedence {
    match op {
        BinaryOperator::Add | BinaryOperator::Sub => Precedence::Add,
        BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Rem => Precedence::Mul,
        BinaryOperator::BitAnd => Precedence::And,
        BinaryOperator::BitOr => Precedence::Or,
        BinaryOperator::BitXor => Precedence::Xor,
        BinaryOperator::Shl | BinaryOperator::Shr => Precedence::Shift,
        BinaryOperator::Eq | BinaryOperator::Ne | BinaryOperator::Gt | BinaryOperator::Lt | BinaryOperator::Ge | BinaryOperator::Le => Precedence::Cmp,
        BinaryOperator::LazyAnd => Precedence::LazyAnd,
        BinaryOperator::LazyOr => Precedence::LazyOr,
        BinaryOperator::Assign | BinaryOperator::AddAssign | BinaryOperator::SubAssign | BinaryOperator::MulAssign | BinaryOperator::DivAssign | BinaryOperator::RemAssign
            | BinaryOperator::AndAssign | BinaryOperator::OrAssign | BinaryOperator::XorAssign | BinaryOperator::ShlAssign | BinaryOperator::ShrAssign => Precedence::Assign,
        BinaryOperator::Range => Precedence::Range,
    }
}

fn unary_operator_precedence(_op: UnaryOperator) -> Precedence {
    Precedence::Unary
}





identifier;
number;
operator :: '[\.+-*/%^&|!<>]' | '==|!=|+=|-=|*=|/=|&=|\|=|%=|min=|max=|<=|>=' ;
expression :: identifier | number | '(' expression ')' | '(' expression_list ')' | expression operator expression | expression '.' identifier | expression '.' number | identifier '(' expression_list ')' | expression '[' expression ']' | expression ';' expression | '{' expression '}' ;
expression :: 'if' expression '{' expression '}' ('else' '{' expression '}')? | 'while' expression '{' expression '}' | loop '{' expression '}' | 'break' | 'continue' ;     ) ;
expression :: expression '=' expression | let identifier (':' type)? ('=' expression)? ';' ;




%print_string_pool :: (%_x) { %i:=0; while %i<%_x.chunks.len { defer %i+=1; %c:=%_x.chunks.ptr[%i]; %j:=0; while %j<%c.meta.len { defer %j+=1; %m := %c.meta.ptr[%j]; yield m.offset, (%c.data[%m.offset .. if %j+1<%c.meta.len {%c.meta.ptr[%j+1].offset} else {%c.size}], %m.id); } } }

%i:=0; %j:=0;
if %i==%_x.chunks.len {eof} else if %j>=%_x.chunks.ptr[%i].meta.len {%i+=1; skip} else {%j+=1; %c:=%_x.chunks.ptr[%i]; %m:=%c.meta.ptr[%j]; (c.data[%m.offset .. if %j+1<%c.meta.len {%c.meta.ptr[%j+1].offset} else {c.size}], m.id)}


maybe use value.b for binary like remedy
