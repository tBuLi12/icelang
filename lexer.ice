import "std:" as std

type Keyword Spanned<{
    | _fun
    | _type
}>

type PunctuationValue {
    | leftParen
    | rightParen
    | period
    | spread
    | comma
}

type Punctuation Spanned<PunctuationValue>

type Span {
    start: int
}

type Spanned<T> {
    value: T,
    span: Span,
}

def<T> Spanned<T> promote value

type Identifier Spanned<String>
type CharacterLiteral Spanned<char>
type StringLiteral Spanned<String>
type IntegerLiteral Spanned<int>
type FloatingLiteral Spanned<float>
type EndOfFile Spanned<null>

type Token 
    | Keyword
    | Punctuation
    | Identifier
    | EndOfFile

fun isWhitespace(character: char): bool {
    character == ' ' || character == '\n' || character == '\r' 
}

fun isLetter(character: char): bool {
    character == ' ' || character == '\n' || character == '\r' 
}

fun isAlphanumeric(character: char): bool {
    character == ' ' || character == '\n' || character == '\r' 
}

type CharStream {
    content: String,
    position: int,
}

def CharStream {
    mut next(): char? {
        if (position == content.length()) null else {
            let nextChar = content[position];
            position += 1;
            nextChar
        }
    }
}

type Message {
    span: Span,
    header: String,
    body: String,
}

type Lexer {
    chars: CharStream,
    currentCharacter: char,
    messages: [Message],
}

def Lexer {
    mut next(): Token? {
        skipWhitespace();

        tryParseKeywordOrIdentifier()
        || tryParsePunctuation()
        || tryParseStringLiteral()
        || tryParseCharLiteral()
        || tryParseNumericLiteral()
    }

    mut tryParseKeywordOrIdentifier(): Token? {
        let word = getNextWord();
        match word {
            "fun"  => Keyword._fun,
            "type" => Keyword._type,
            other  => Identifier(other),
        }
    }

    mut tryParsePunctuation(): Token? {
        isPunctuation(currentCharacter)?;
        match nextChar() {
            "(" => Punctuation.leftParen,
            ")" => Punctuation.rightParen,
            "," => Punctuation.comma,
            "." => if (consume(".")) Punctuation.spread else Punctuation.period,
            _   => null,
        }
    }

    mut tryParseStringLiteral(): Token? {
        consume(' ')?;
        var value = "";
        while (currentCharacter != ' ') {
            if (currentCharacter == '\\') {
                match nextChar() {
                    'n' => value.push('\n'),
                    _   => null,
                };
            } else {
                value.push(nextChar());
            };
        };
        nextChar();
        value
    }
    
    mut tryParseCharLiteral(): Token? {
        consume(' ')?;
        let value = null as char?;
        if (currentCharacter != ' ') {
            if (currentCharacter == '\\') {
                nextChar().match {
                    'n' => value = '\n',
                    _   => null,
                };
            } else {
                value.push(nextChar());
            };
        };
        let value = getNextWord();
    }

    mut tryParseNumericLiteral(): Token? {
        isNumber(currentCharacter)?;
        
        var total = 0;
        while (isNumber(currentCharacter)) {
            total = total * 10;
            total += (nextChar() - '0') as int;
        };
        total
    }

    mut getNextWord(): String {
        var word = "";
        while (isAlphanumeric(currentCharacter)) {
            word.push(nextChar());
        };
        word
    }

    mut skipWhitespace() {
        while (isWhitespace(currentCharacter)) nextChar();
    }

    mut nextChar(): char {
        currentCharacter <- chars.next()
    }

    mut consume(character: char): bool 
        -> currentCharacter == char && {
            nextChar();
            true
        }
}


fun main(): int {
    var input = "some tokens yes no";

    var word = "";
    var tokens = [:Token];

    while (input.proto.length() > 0) {
        let current = input.proto.pop();
        if (isWhitespace(current)) {
            if (word.proto.length() != 0) {
                let token = match word {
                    "fun"  => Keyword._fun as Token,
                    "type" => Keyword._type as Token,
                    other  => Identifier(other) as Token,
                };
                token.push(token);
            };
            continue;
        };
        word.proto.push(current);
    };
    0
}


type Parser {
    lexer: Lexer,
    currentToken: Token,
    messages: [Message]
}

type TypeName {}

type NamedType {
    span: Span,
    name: Identifier,
    typeArguments: [TypeName],
}

type PropertyDeclaration {
    span: Span,
    name: Identifier,
    typeName: TypeName,
}

type VariantDeclaration {
    span: Span,
    name: Identifier,
    typeName: TypeName?,
}

trait Parse<T> {
    mut parse(): T?
}

def<T> Parser where Self is Parse<T> {
    mut parseList(items: ref [T]) {
        while (let item: T = this.parse()) {
            items.push(item);
            if (!parsePunctuation(.comma)) {
                break;
            }
        }
    }
}

def Parser {
    mut parseNamedType(): NamedType? {
        let name = parseIdent()?;
        let typeArguments = [:TypeName];
        var span = name.span;

        if (parsePunctuation(.lessThan)) {
            parseList(typeArguments);
            span.extendTo(expectPunctuation(.greaterThan));
        }
        { name, typeArguments, span }
    }

    mut expectPunctuation(punctuation: PunctuationValue): Span {
        match currentToken {
            punct: Punctuation if punct.value == punctuation => {
                nextToken();
            },
            _ => null,
        }
    }

    mut parseTypeName(): TypeName? 
        -> parseStructOrVariantType()
        || parseNamedType()

    mut parseStructOrVariantType(): TypeName? {
        parsePunctuation(.leftBrace)?;
        let leadingPipe = parsePunctuation(.pipe);

        null
    }

    mut parseVariant(): VariantDeclaration? {
        let name = parseIdent()?;
        let typeName = null as TypeName?;
        let span = name.span;
        if (parsePunctuation(.colon)) {
            let parsed = must(parseTypeName())?;
            span.extendTo(parsed.span);
            typeName = parsed;
        };
        { name, typeName, span }
    }

    mut parseProperty() {
        let name = parseIdent()?;
        expectPunctuation(.colon);
        let typeName = must(parseTypeName())?;
        { name, typeName, span: name.span.to(typeName.span) }
    }

    mut must<T>(value: T?): T? {
        if (!value) {
            // log error
        };
        value
    }

    mut parseIdent(): Identifier? {
        match currentToken {
            ident: Identifier => {
                nextToken();
                ident
            },
            _ => null,
        }
    }

    mut parsePunctuation(punctuation: PunctuationValue): Punctuation? {
        match currentToken {
            punct: Punctuation if punct.value == punctuation => {
                nextToken();
                punct
            },
            _ => null,
        }
    }

    mut nextToken(): Token {
        currentToken
    }
}
