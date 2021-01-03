const std = @import("std");
const print = std.debug.print;

usingnamespace @import("value.zig");
usingnamespace @import("chunk.zig");
usingnamespace @import("compiler.zig");

pub var sacnner: Scanner = undefined;

pub const TokenType = enum(u8) {
// Single-character tokens.
    TOKEN_LEFT_PAREN, TOKEN_RIGHT_PAREN, TOKEN_LEFT_BRACE, TOKEN_RIGHT_BRACE, TOKEN_COMMA, TOKEN_DOT, TOKEN_MINUS, TOKEN_PLUS, TOKEN_SEMICOLON, TOKEN_SLASH, TOKEN_STAR,

    // One or two character tokens.
    TOKEN_BANG, TOKEN_BANG_EQUAL, TOKEN_EQUAL, TOKEN_EQUAL_EQUAL, TOKEN_GREATER, TOKEN_GREATER_EQUAL, TOKEN_LESS, TOKEN_LESS_EQUAL,

    // Literals.
    TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_NUMBER,

    // Keywords.
    TOKEN_AND, TOKEN_CLASS, TOKEN_ELSE, TOKEN_FALSE, TOKEN_FOR, TOKEN_FUN, TOKEN_IF, TOKEN_NIL, TOKEN_OR, TOKEN_PRINT, TOKEN_RETURN, TOKEN_SUPER, TOKEN_THIS, TOKEN_TRUE, TOKEN_VAR, TOKEN_WHILE, TOKEN_ERROR, TOKEN_EOF
};

pub const Token = struct {
    tokenType: TokenType,
    line: usize,
    literal: []const u8,

    pub fn equal(a: Token, b: Token) bool {
        if (a.literal.len != b.literal.len) return false;
        return std.mem.eql(u8, a.literal, b.literal);
    }
};

pub const Scanner = struct {
    start: usize,
    current: usize,
    line: usize,
    source: []const u8,

    pub fn init(source: []const u8) void {
        sacnner.source = source;
        sacnner.start = 0;
        sacnner.current = 0;
        sacnner.line = 1;
    }

    pub fn scanToken(self: *Scanner) Token {
        self.skipWhitespace();

        self.start = self.current;
        if (self.isAtEnd()) {
            return self.makeToken(TokenType.TOKEN_EOF);
        }

        const c = self.advance();

        if (isDigit(c)) {
            return self.number();
        } else if (isAlpha(c)) {
            return self.identifier();
        }

        return switch (c) {
            '(' => self.makeToken(TokenType.TOKEN_LEFT_PAREN),
            ')' => self.makeToken(TokenType.TOKEN_RIGHT_PAREN),
            '{' => self.makeToken(TokenType.TOKEN_LEFT_BRACE),
            '}' => self.makeToken(TokenType.TOKEN_RIGHT_BRACE),
            ';' => self.makeToken(TokenType.TOKEN_SEMICOLON),
            ',' => self.makeToken(TokenType.TOKEN_COMMA),
            '.' => self.makeToken(TokenType.TOKEN_DOT),
            '-' => self.makeToken(TokenType.TOKEN_MINUS),
            '+' => self.makeToken(TokenType.TOKEN_PLUS),
            '/' => self.makeToken(TokenType.TOKEN_SLASH),
            '*' => self.makeToken(TokenType.TOKEN_STAR),
            '!' => self.makeToken(if (self.match('=')) TokenType.TOKEN_BANG_EQUAL else TokenType.TOKEN_BANG),
            '=' => self.makeToken(if (self.match('=')) TokenType.TOKEN_EQUAL_EQUAL else TokenType.TOKEN_EQUAL),
            '<' => self.makeToken(if (self.match('=')) TokenType.TOKEN_LESS_EQUAL else TokenType.TOKEN_LESS),
            '>' => self.makeToken(if (self.match('=')) TokenType.TOKEN_GREATER_EQUAL else TokenType.TOKEN_GREATER),
            '"' => self.string(),
            else => self.errorToekn("Unexpected character."),
        };
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn isAlpha(c: u8) bool {
        return (c >= 'a' and c <= 'z') or
            (c >= 'A' and c <= 'Z') or
            c == '_';
    }

    pub fn advance(self: *Scanner) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    fn match(self: *Scanner, char: u8) bool {
        if (self.isAtEnd()) {
            return false;
        }
        if (self.source[self.current] != char) {
            return false;
        }

        self.current += 1;
        return true;
    }

    fn skipWhitespace(self: *Scanner) void {
        while (true) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => _ = self.advance(),
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        while (self.peek() != '\n' and !self.isAtEnd()) {
                            _ = self.advance();
                        }
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    fn identifierType(self: *Scanner) Token {
        return switch (self.source[self.start]) {
            'a' => self.checkKeyword(1, 2, "nd", TokenType.TOKEN_AND),
            'c' => self.checkKeyword(1, 4, "lass", TokenType.TOKEN_CLASS),
            'e' => self.checkKeyword(1, 3, "lse", TokenType.TOKEN_ELSE),
            'f' => self.checkKeywordF(),
            'i' => self.checkKeyword(1, 1, "f", TokenType.TOKEN_IF),
            'n' => self.checkKeyword(1, 2, "il", TokenType.TOKEN_NIL),
            'o' => self.checkKeyword(1, 1, "r", TokenType.TOKEN_OR),
            'p' => self.checkKeyword(1, 4, "rint", TokenType.TOKEN_PRINT),
            'r' => self.checkKeyword(1, 5, "eturn", TokenType.TOKEN_RETURN),
            's' => self.checkKeyword(1, 4, "uper", TokenType.TOKEN_SUPER),
            't' => self.checkKeywordT(),
            'v' => self.checkKeyword(1, 2, "ar", TokenType.TOKEN_VAR),
            'w' => self.checkKeyword(1, 4, "hile", TokenType.TOKEN_WHILE),
            else => self.getIdentifierType(),
        };
    }

    fn checkKeywordF(self: *Scanner) Token {
        return switch (self.source[self.start + 1]) {
            'a' => self.checkKeyword(2, 3, "lse", TokenType.TOKEN_FALSE),
            'o' => self.checkKeyword(2, 1, "r", TokenType.TOKEN_FOR),
            'u' => self.checkKeyword(2, 1, "n", TokenType.TOKEN_FUN),
            else => self.getIdentifierType(),
        };
    }

    fn checkKeywordT(self: *Scanner) Token {
        return switch (self.source[self.start + 1]) {
            'h' => self.checkKeyword(2, 2, "is", TokenType.TOKEN_THIS),
            'r' => self.checkKeyword(2, 2, "ue", TokenType.TOKEN_TRUE),
            else => self.getIdentifierType(),
        };
    }

    fn getIdentifierType(self: *Scanner) Token {
        return self.makeToken(TokenType.TOKEN_IDENTIFIER);
    }

    fn identifier(self: *Scanner) Token {
        while (isAlpha(self.peek()) or isDigit(self.peek())) {
            _ = self.advance();
        }
        return if (self.current - self.start > 1) self.identifierType() else self.getIdentifierType();
    }

    fn checkKeyword(self: *Scanner, start: usize, length: usize, rest: []const u8, tokenType: TokenType) Token {
        if (self.current - self.start == start + length and
            std.mem.eql(u8, self.source[self.start + start .. self.current], rest))
        {
            return Token{
                .tokenType = tokenType,
                .line = self.line,
                .literal = self.source[self.start..self.current],
            };
        }

        return self.getIdentifierType();
    }

    fn number(self: *Scanner) Token {
        while (isDigit(self.peek())) {
            _ = self.advance();
        }

        // Look for a fraction part.
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            _ = self.advance();

            while (isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        return self.makeToken(TokenType.TOKEN_NUMBER);
    }

    fn string(self: *Scanner) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            return self.errorToekn("Unterminated string.");
        } else {
            _ = self.advance();
            return self.makeToken(TokenType.TOKEN_STRING);
        }
    }

    fn peek(self: *Scanner) u8 {
        return self.source[self.current];
    }

    fn peekNext(self: *Scanner) u8 {
        if (self.isAtEnd()) {
            return 0;
        }
        return self.source[self.current + 1];
    }

    fn isAtEnd(self: *Scanner) bool {
        return self.source[self.current] == 0;
    }

    fn makeToken(self: *Scanner, tokenType: TokenType) Token {
        return Token{
            .tokenType = tokenType,
            .line = sacnner.line,
            .literal = self.source[self.start..self.current],
        };
    }

    fn errorToekn(self: *Scanner, message: []const u8) Token {
        return Token{
            .tokenType = TokenType.TOKEN_ERROR,
            .line = sacnner.line,
            .literal = message,
        };
    }
};
