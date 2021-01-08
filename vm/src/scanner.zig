const std = @import("std");
const print = std.debug.print;

usingnamespace @import("common.zig");
usingnamespace @import("value.zig");
usingnamespace @import("chunk.zig");

pub const Token = struct {
    tokenType: TokenType,
    literal: []const u8,
    line: usize,
};

pub const TokenType = enum {
// Single-character tokens.
    TOKEN_LEFT_PAREN, TOKEN_RIGHT_PAREN, TOKEN_LEFT_BRACE, TOKEN_RIGHT_BRACE, TOKEN_COMMA, TOKEN_DOT, TOKEN_MINUS, TOKEN_PLUS, TOKEN_SEMICOLON, TOKEN_SLASH, TOKEN_STAR,

    // One or two character tokens.
    TOKEN_BANG, TOKEN_BANG_EQUAL, TOKEN_EQUAL, TOKEN_EQUAL_EQUAL, TOKEN_GREATER, TOKEN_GREATER_EQUAL, TOKEN_LESS, TOKEN_LESS_EQUAL,

    // Literals.
    TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_NUMBER,

    // Keywords.
    TOKEN_AND, TOKEN_CLASS, TOKEN_ELSE, TOKEN_FALSE, TOKEN_FOR, TOKEN_FUN, TOKEN_IF, TOKEN_NIL, TOKEN_OR, TOKEN_PRINT, TOKEN_RETURN, TOKEN_SUPER, TOKEN_THIS, TOKEN_TRUE, TOKEN_VAR, TOKEN_WHILE, TOKEN_ERROR, TOKEN_EOF
};

pub const Scanner = struct {
    start: [*]const u8,
    current: [*]const u8,
    line: usize,

    pub fn init(source: []const u8) Scanner {
        return Scanner{
            .start = source.ptr,
            .current = source.ptr,
            .line = 1,
        };
    }

    pub fn scanToken(self: *Scanner) Token {
        self.skipWhitespace();

        self.start = self.current;
        if (self.isAtEnd()) return self.makeToken(.TOKEN_EOF);

        const char = self.advance();

        if (isAlpha(char)) return self.identifier();
        if (isDigit(char)) return self.number();

        return switch (char) {
            '(' => self.makeToken(.TOKEN_LEFT_PAREN),
            ')' => self.makeToken(.TOKEN_RIGHT_PAREN),
            '{' => self.makeToken(.TOKEN_LEFT_BRACE),
            '}' => self.makeToken(.TOKEN_RIGHT_BRACE),
            ';' => self.makeToken(.TOKEN_SEMICOLON),
            ',' => self.makeToken(.TOKEN_COMMA),
            '.' => self.makeToken(.TOKEN_DOT),
            '-' => self.makeToken(.TOKEN_MINUS),
            '+' => self.makeToken(.TOKEN_PLUS),
            '/' => self.makeToken(.TOKEN_SLASH),
            '*' => self.makeToken(.TOKEN_STAR),
            '!' => self.makeToken(if (self.match('=')) .TOKEN_BANG_EQUAL else .TOKEN_BANG),
            '=' => self.makeToken(if (self.match('=')) .TOKEN_EQUAL_EQUAL else .TOKEN_EQUAL),
            '<' => self.makeToken(if (self.match('=')) .TOKEN_LESS_EQUAL else .TOKEN_LESS),
            '>' => self.makeToken(if (self.match('=')) .TOKEN_GREATER_EQUAL else .TOKEN_GREATER),
            '"' => self.string(),
            else => self.errorToken("Unexpected character."),
        };
    }

    fn number(self: *Scanner) Token {
        while (isDigit(self.peek())) _ = self.advance();

        if (self.peek() == '.' and isDigit(self.peekNext())) {
            _ = self.advance();

            while (isDigit(self.peek())) _ = self.advance();
        }

        return self.makeToken(.TOKEN_NUMBER);
    }

    fn isDigit(char: u8) bool {
        return char >= '0' and char <= '9';
    }

    fn isAlpha(char: u8) bool {
        return (char >= 'a' and char <= 'z') or (char >= 'A' and char <= 'Z') or (char == '_');
    }

    fn string(self: *Scanner) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }
        if (self.isAtEnd()) return self.errorToken("Unterminated string.");
        _ = self.advance();
        return self.makeToken(.TOKEN_STRING);
    }

    fn identifier(self: *Scanner) Token {
        while (isAlpha(self.peek()) or isDigit(self.peek())) _ = self.advance();

        return self.makeToken(self.identifierType());
    }

    fn identifierType(self: *Scanner) TokenType {
        return switch (self.start[0]) {
            'a' => self.checkKeyword(1, 2, "nd", .TOKEN_AND),
            'c' => self.checkKeyword(1, 4, "lass", .TOKEN_CLASS),
            'e' => self.checkKeyword(1, 3, "lse", .TOKEN_ELSE),
            'f' => if (@ptrToInt(self.current) - @ptrToInt(self.start) > 1) blk: {
                switch (self.start[1]) {
                    'a' => break :blk self.checkKeyword(2, 3, "lse", .TOKEN_FALSE),
                    'o' => break :blk self.checkKeyword(2, 1, "r", .TOKEN_FOR),
                    'u' => break :blk self.checkKeyword(2, 1, "n", .TOKEN_FUN),
                    else => break :blk .TOKEN_IDENTIFIER,
                }
            } else .TOKEN_IDENTIFIER,
            'i' => self.checkKeyword(1, 1, "f", .TOKEN_IF),
            'n' => self.checkKeyword(1, 2, "il", .TOKEN_NIL),
            'o' => self.checkKeyword(1, 1, "r", .TOKEN_OR),
            'p' => self.checkKeyword(1, 4, "rint", .TOKEN_PRINT),
            'r' => self.checkKeyword(1, 5, "eturn", .TOKEN_AND),
            's' => self.checkKeyword(1, 4, "uper", .TOKEN_AND),
            't' => if (@ptrToInt(self.current) - @ptrToInt(self.start) > 1) blk: {
                switch (self.start[1]) {
                    'h' => break :blk self.checkKeyword(2, 2, "is", .TOKEN_THIS),
                    'r' => break :blk self.checkKeyword(2, 2, "ue", .TOKEN_TRUE),
                    else => break :blk .TOKEN_IDENTIFIER,
                }
            } else .TOKEN_IDENTIFIER,
            'v' => self.checkKeyword(1, 2, "ar", .TOKEN_VAR),
            'w' => self.checkKeyword(1, 4, "hile", .TOKEN_WHILE),
            else => .TOKEN_IDENTIFIER,
        };
    }

    fn checkKeyword(self: *Scanner, start: usize, length: usize, rest: []const u8, tokenType: TokenType) TokenType {
        return if ((@ptrToInt(self.current) - @ptrToInt(self.start) == start + length) and std.mem.eql(u8, self.start[start .. start + length], rest))
            tokenType
        else
            .TOKEN_IDENTIFIER;
    }

    fn match(self: *Scanner, char: u8) bool {
        if (self.isAtEnd() or self.current[0] != char) {
            return false;
        }

        self.current += 1;
        return true;
    }

    fn advance(self: *Scanner) u8 {
        const res = self.current[0];
        self.current += 1;
        return res;
    }

    fn isAtEnd(self: *Scanner) bool {
        return self.current[0] == 0;
    }

    fn makeToken(self: *Scanner, tokenType: TokenType) Token {
        return Token{
            .tokenType = tokenType,
            .literal = self.start[0 .. @ptrToInt(self.current) - @ptrToInt(self.start)],
            .line = self.line,
        };
    }

    fn errorToken(self: *Scanner, message: []const u8) Token {
        return Token{
            .tokenType = .TOKEN_ERROR,
            .literal = message,
            .line = self.line,
        };
    }

    fn skipWhitespace(self: *Scanner) void {
        while (true) {
            const char = self.peek();
            switch (char) {
                ' ', '\r', '\t' => _ = self.advance(),
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        // A comment goes until the end of the line.
                        while (self.peek() != '\n' and !self.isAtEnd())
                            _ = self.advance();
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    fn peek(self: *Scanner) u8 {
        return self.current[0];
    }

    fn peekNext(self: *Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.current[1];
    }
};
