const std = @import("std");
const print = std.debug.print;

usingnamespace @import("common.zig");
usingnamespace @import("value.zig");
usingnamespace @import("chunk.zig");
usingnamespace @import("scanner.zig");
usingnamespace @import("compiler.zig");
usingnamespace @import("vm.zig");

const Precedence = enum {
    PREC_NONE, //
    PREC_ASSIGNMENT, // =
    PREC_OR, // or
    PREC_AND, // and
    PREC_EQUALITY, // == !=
    PREC_COMPARISON, // < > <= >=
    PREC_TERM, // + -
    PREC_FACTOR, // * /
    PREC_UNARY, // ! -
    PREC_CALL, // . ()
    PREC_PRIMARY
};

const ParseFn = fn (self: *Parser, canAssign: bool) anyerror!void;

const ParseRule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precedence,

    const rules = [_]ParseRule{
        .{ .prefix = Parser.grouping, .infix = null, .precedence = .PREC_NONE }, //TOKEN_LEFT_PAREN
        .{ .prefix = null, .infix = null, .precedence = .PREC_NONE }, //TOKEN_RIGHT_PAREN
        .{ .prefix = null, .infix = null, .precedence = .PREC_NONE }, //TOKEN_LEFT_BRACE
        .{ .prefix = null, .infix = null, .precedence = .PREC_NONE }, //TOKEN_RIGHT_BRACE
        .{ .prefix = null, .infix = null, .precedence = .PREC_NONE }, //TOKEN_COMMA
        .{ .prefix = null, .infix = null, .precedence = .PREC_NONE }, //TOKEN_DOT
        .{ .prefix = Parser.unary, .infix = Parser.binary, .precedence = .PREC_TERM }, //TOKEN_MINUS
        .{ .prefix = null, .infix = Parser.binary, .precedence = .PREC_TERM }, //TOKEN_PLUS
        .{ .prefix = null, .infix = null, .precedence = .PREC_NONE }, //TOKEN_SEMICOLON
        .{ .prefix = null, .infix = Parser.binary, .precedence = .PREC_FACTOR }, //TOKEN_SLASH
        .{ .prefix = null, .infix = Parser.binary, .precedence = .PREC_FACTOR }, //TOKEN_STAR
        .{ .prefix = Parser.unary, .infix = null, .precedence = .PREC_NONE }, //TOKEN_BANG
        .{ .prefix = null, .infix = Parser.binary, .precedence = .PREC_EQUALITY }, //TOKEN_BANG_EQUAL
        .{ .prefix = null, .infix = null, .precedence = .PREC_NONE }, //TOKEN_EQUAL
        .{ .prefix = null, .infix = Parser.binary, .precedence = .PREC_EQUALITY }, //TOKEN_EQUAL_EQUAL
        .{ .prefix = null, .infix = Parser.binary, .precedence = .PREC_COMPARISON }, //TOKEN_GREATER
        .{ .prefix = null, .infix = Parser.binary, .precedence = .PREC_COMPARISON }, //TOKEN_GREATER_EQUAL
        .{ .prefix = null, .infix = Parser.binary, .precedence = .PREC_COMPARISON }, //TOKEN_LESS
        .{ .prefix = null, .infix = Parser.binary, .precedence = .PREC_COMPARISON }, //TOKEN_LESS_EQUAL
        .{ .prefix = Parser.variable, .infix = null, .precedence = .PREC_NONE }, //TOKEN_IDENTIFIER
        .{ .prefix = Parser.string, .infix = null, .precedence = .PREC_NONE }, //TOKEN_STRING
        .{ .prefix = Parser.number, .infix = null, .precedence = .PREC_NONE }, //TOKEN_NUMBER
        .{ .prefix = null, .infix = null, .precedence = .PREC_NONE }, //TOKEN_AND
        .{ .prefix = null, .infix = null, .precedence = .PREC_NONE }, //TOKEN_CLASS
        .{ .prefix = null, .infix = null, .precedence = .PREC_NONE }, //TOKEN_ELSE
        .{ .prefix = Parser.literal, .infix = null, .precedence = .PREC_NONE }, //TOKEN_FALSE
        .{ .prefix = null, .infix = null, .precedence = .PREC_NONE }, //TOKEN_FOR
        .{ .prefix = null, .infix = null, .precedence = .PREC_NONE }, //TOKEN_FUN
        .{ .prefix = null, .infix = null, .precedence = .PREC_NONE }, //TOKEN_IF
        .{ .prefix = Parser.literal, .infix = null, .precedence = .PREC_NONE }, //TOKEN_NIL
        .{ .prefix = null, .infix = null, .precedence = .PREC_NONE }, //TOKEN_OR
        .{ .prefix = null, .infix = null, .precedence = .PREC_NONE }, //TOKEN_PRINT
        .{ .prefix = null, .infix = null, .precedence = .PREC_NONE }, //TOKEN_RETURN
        .{ .prefix = null, .infix = null, .precedence = .PREC_NONE }, //TOKEN_SUPER
        .{ .prefix = null, .infix = null, .precedence = .PREC_NONE }, //TOKEN_THIS
        .{ .prefix = Parser.literal, .infix = null, .precedence = .PREC_NONE }, // TOKEN_TRUE
        .{ .prefix = null, .infix = null, .precedence = .PREC_NONE }, //TOKEN_VAR
        .{ .prefix = null, .infix = null, .precedence = .PREC_NONE }, //TOKEN_WHILE
        .{ .prefix = null, .infix = null, .precedence = .PREC_NONE }, //TOKEN_ERROR
        .{ .prefix = null, .infix = null, .precedence = .PREC_NONE }, //TOKEN_EOF
    };

    pub fn getRule(idx: TokenType) ParseRule {
        return rules[@enumToInt(idx)];
    }
};

pub const Parser = struct {
    compiler: *Compiler,
    scanner: *Scanner,

    current: Token,
    previous: Token,
    hadError: bool,
    panicMode: bool,
    vm: *VM,

    pub fn init(compiler: *Compiler, scanner: *Scanner) Parser {
        return Parser{
            .compiler = compiler,
            .scanner = scanner,
            .current = undefined,
            .previous = undefined,
            .hadError = false,
            .panicMode = false,
            .vm = compiler.vm,
        };
    }

    pub fn expression(self: *Parser) !void {
        try self.parsePrecedence(.PREC_ASSIGNMENT);
    }

    pub fn declaration(self: *Parser) !void {
        if (self.match(.TOKEN_VAR)) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }

        if (self.panicMode) self.synchronize();
    }

    fn statement(self: *Parser) !void {
        if (self.match(.TOKEN_PRINT)) {
            try self.printStatement();
        } else {
            try self.expressionStatement();
        }
    }

    fn expressionStatement(self: *Parser) !void {
        try self.expression();
        self.consume(.TOKEN_SEMICOLON, "Expect ';' after expression.");
        try self.emitCode(.OP_POP);
    }

    fn varDeclaration(self: *Parser) !void {
        const global = try self.parseVariable("Except variable name");

        if (self.match(.TOKEN_EQUAL)) {
            try self.expression();
        } else {
            try self.emitCode(.OP_NIL);
        }

        self.consume(.TOKEN_SEMICOLON, "Expect ';' after var declaration.");

        try self.defineVariable(global);
    }

    fn printStatement(self: *Parser) !void {
        try self.expression();
        self.consume(.TOKEN_SEMICOLON, "Exepect ';' after value.");
        try self.emitCode(.OP_PRINT);
    }

    fn namedVariable(self: *Parser, name: Token, canAssign: bool) !void {
        const arg = try self.identifierConstant(name);

        if (canAssign and self.match(.TOKEN_EQUAL)) {
            try self.expression();
            try self.emitBytes(@enumToInt(OpCode.OP_SET_GLOBAL), arg);
        } else {
            try self.emitBytes(@enumToInt(OpCode.OP_GET_GLOBAL), arg);
        }
    }

    fn parseVariable(self: *Parser, errorMessage: []const u8) !u8 {
        self.consume(.TOKEN_IDENTIFIER, errorMessage);
        return try self.identifierConstant(self.previous);
    }

    fn identifierConstant(self: *Parser, name: Token) !u8 {
        return try self.makeConstant(OBJ_STRING_VAL(try self.vm.heap.copyString(name.literal)));
    }

    fn defineVariable(self: *Parser, global: u8) !void {
        try self.emitBytes(@enumToInt(OpCode.OP_DEFINE_GLOBAL), global);
    }

    pub fn match(self: *Parser, tokenType: TokenType) bool {
        if (!self.check(tokenType)) return false;
        self.advance();
        return true;
    }

    fn check(self: *Parser, tokenType: TokenType) bool {
        return self.current.tokenType == tokenType;
    }

    pub fn advance(self: *Parser) void {
        self.previous = self.current;

        while (true) {
            self.current = self.scanner.scanToken();
            if (self.current.tokenType != .TOKEN_ERROR) break;

            self.errorAtCurrent(self.current.literal);
        }
    }

    pub fn consume(self: *Parser, tokenType: TokenType, message: []const u8) void {
        if (self.current.tokenType == tokenType) {
            _ = self.advance();
            return;
        }

        self.errorAtCurrent(message);
    }

    fn variable(self: *Parser, canAssign: bool) !void {
        try self.namedVariable(self.previous, canAssign);
    }

    fn literal(self: *Parser, canAssign: bool) !void {
        switch (self.previous.tokenType) {
            .TOKEN_FALSE => try self.emitCode(.OP_FALSE),
            .TOKEN_NIL => try self.emitCode(.OP_NIL),
            .TOKEN_TRUE => try self.emitCode(.OP_TRUE),
            else => unreachable,
        }
    }

    fn number(self: *Parser, canAssign: bool) !void {
        const value = try std.fmt.parseFloat(f64, self.previous.literal);
        try self.emitConstant(NUMBER_VAL(value));
    }

    fn string(self: *Parser, canAssign: bool) !void {
        try self.emitConstant(OBJ_VAL(@ptrCast(*Obj, try self.vm.heap.copyString(self.previous.literal[1 .. self.previous.literal.len - 1]))));
    }

    fn grouping(self: *Parser, canAssign: bool) !void {
        try self.expression();
        self.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
    }

    fn binary(self: *Parser, canAssign: bool) !void {
        // Remember the operator.
        const operatorType = self.previous.tokenType;

        // Compile the right operand.
        const rule = ParseRule.getRule(operatorType);
        try self.parsePrecedence(@intToEnum(Precedence, (@enumToInt(rule.precedence) + 1)));

        // Emit the operator instruction.
        switch (operatorType) {
            .TOKEN_PLUS => try self.emitCode(.OP_ADD),
            .TOKEN_MINUS => try self.emitCode(.OP_SUBTRACT),
            .TOKEN_STAR => try self.emitCode(.OP_MULTIPLY),
            .TOKEN_SLASH => try self.emitCode(.OP_DIVIDE),
            .TOKEN_BANG_EQUAL => try self.emitBytes(@enumToInt(OpCode.OP_EQUAL), @enumToInt(OpCode.OP_NOT)),
            .TOKEN_EQUAL_EQUAL => try self.emitCode(.OP_EQUAL),
            .TOKEN_GREATER => try self.emitCode(.OP_GREATER),
            .TOKEN_GREATER_EQUAL => try self.emitBytes(@enumToInt(OpCode.OP_LESS), @enumToInt(OpCode.OP_NOT)),
            .TOKEN_LESS => try self.emitCode(.OP_LESS),
            .TOKEN_LESS_EQUAL => try self.emitBytes(@enumToInt(OpCode.OP_GREATER), @enumToInt(OpCode.OP_NOT)),
            else => unreachable,
        }
    }

    fn unary(self: *Parser, canAssign: bool) !void {
        const operatorType = self.previous.tokenType;

        // Compile the operand.
        try self.parsePrecedence(.PREC_UNARY);

        // Emit the operator instruction.
        switch (operatorType) {
            .TOKEN_MINUS => try self.emitCode(.OP_NEGATE),
            .TOKEN_BANG => try self.emitCode(.OP_NOT),
            else => unreachable,
        }
    }

    pub fn endParse(self: *Parser) !void {
        try self.emitReturn();

        if (DEBUG_PRINT_CODE) {
            if (!self.hadError) {
                _ = self.currentChunk().disassemble("code");
            }
        }
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) !void {
        self.advance();
        const prefixRule = ParseRule.getRule(self.previous.tokenType).prefix;
        if (prefixRule) |prefixRuleFn| {
            const canAssign = @enumToInt(precedence) <= @enumToInt(Precedence.PREC_ASSIGNMENT);
            try prefixRuleFn(self, canAssign);

            while (@enumToInt(precedence) <= @enumToInt(ParseRule.getRule(self.current.tokenType).precedence)) {
                self.advance();
                const infixRule = ParseRule.getRule(self.previous.tokenType).infix.?;
                try infixRule(self, canAssign);
            }

            if (canAssign and self.match(.TOKEN_EQUAL)) {
                self.error0("Invaild assignment target.");
            }
        } else {
            self.error0("Expect expression.");
            return;
        }
    }

    fn emitReturn(self: *Parser) !void {
        try self.emitCode(.OP_RETURN);
    }

    fn emitByte(self: *Parser, byte: u8) !void {
        try self.currentChunk().writeByte(byte, self.previous.line);
    }

    fn emitBytes(self: *Parser, byte1: u8, byte2: u8) !void {
        try self.emitByte(byte1);
        try self.emitByte(byte2);
    }

    fn emitCode(self: *Parser, code: OpCode) !void {
        try self.currentChunk().writeCode(code, self.previous.line);
    }

    fn emitConstant(self: *Parser, value: Value) !void {
        try self.emitBytes(@enumToInt(OpCode.OP_CONSTANT), try self.makeConstant(value));
    }

    fn makeConstant(self: *Parser, value: Value) !u8 {
        const constant = try self.currentChunk().addConstant(value);
        if (constant > std.math.maxInt(u8)) {
            self.error0("Too many constants in one chunk.");
            return 0;
        }

        return constant;
    }

    fn currentChunk(self: *Parser) *Chunk {
        return self.compiler.chunk;
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) void {
        self.errorAt(&self.current, message);
    }

    fn error0(self: *Parser, message: []const u8) void {
        self.errorAt(&self.previous, message);
    }

    fn errorAt(self: *Parser, token: *Token, message: []const u8) void {
        if (self.panicMode) return;

        self.panicMode = true;

        print("[line {}] Error", .{token.line});

        if (token.tokenType == .TOKEN_EOF) {
            print(" at end", .{});
        } else if (token.tokenType == .TOKEN_ERROR) {
            // nothing
        } else {
            print(" at '{}'", .{token.literal});
        }

        print(": {}\n", .{message});
        self.hadError = true;
    }

    fn synchronize(self: *Parser) void {
        self.panicMode = false;

        while (self.current.tokenType != .TOKEN_EOF) {
            if (self.previous.tokenType == .TOKEN_SEMICOLON) return;

            switch (self.current.tokenType) {
                .TOKEN_CLASS,
                .TOKEN_FUN,
                .TOKEN_VAR,
                .TOKEN_FOR,
                .TOKEN_IF,
                .TOKEN_WHILE,
                .TOKEN_PRINT,
                .TOKEN_RETURN,
                => return,
                else => {},
            }

            self.advance();
        }
    }
};
