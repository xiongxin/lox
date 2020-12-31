const std = @import("std");
const print = std.debug.print;

usingnamespace @import("value.zig");
usingnamespace @import("chunk.zig");
usingnamespace @import("scanner.zig");
usingnamespace @import("compiler.zig");
usingnamespace @import("heap.zig");

pub var parser: Parser = undefined;

// Lox’s precedence levels in order from lowest to highest. 
const Precedence = enum {
    PREC_NONE,
    PREC_ASSIGNMENT,  // =
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_UNARY,       // ! -
    PREC_CALL,        // . ()
    PREC_PRIMARY
};

const ParseRule = struct {
    prefix: ?ParseFn,
    infix:  ?ParseFn,
    precedence: Precedence,
};

const ParseFn = fn(self: *Parser, canAssing: bool) anyerror!void;

pub const Parser = struct {
    current: Token,
    previous: Token,
    hadError: bool,
    panicMode: bool,

    pub fn init() Parser {
        return Parser {
            .current = undefined,
            .previous = undefined,
            .hadError = false,
            .panicMode = false,
        };
    }


    const rules = [_]ParseRule{
        .{ .prefix = grouping,  .infix = call,     .precedence =  .PREC_CALL },  // TOKEN_LEFT_PAREN
        .{ .prefix = null,      .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_RIGHT_PAREN
        .{ .prefix = null,      .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_LEFT_BRACE
        .{ .prefix = null,      .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_RIGHT_BRACE
        .{ .prefix = null,      .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_COMMA
        .{ .prefix = null,      .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_DOT
        .{ .prefix = unary,     .infix = binary,   .precedence =  .PREC_TERM }, // TOKEN_MINUS
        .{ .prefix = null,      .infix = binary,   .precedence =  .PREC_TERM }, // TOKEN_PLUS
        .{ .prefix = null,      .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_SEMICOLON
        .{ .prefix = null,      .infix = binary,   .precedence =  .PREC_FACTOR }, // TOKEN_SLASH
        .{ .prefix = null,      .infix = binary,   .precedence =  .PREC_FACTOR }, // TOKEN_STAR
        .{ .prefix = unary,     .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_BANG
        .{ .prefix = null,      .infix = binary,   .precedence =  .PREC_EQUALITY }, // TOKEN_BANG_EQUAL
        .{ .prefix = null,      .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_EQUAL
        .{ .prefix = null,      .infix = binary,   .precedence =  .PREC_EQUALITY }, // TOKEN_EQUAL_EQUAL
        .{ .prefix = null,      .infix = binary,   .precedence =  .PREC_COMPARISON }, // TOKEN_GREATER
        .{ .prefix = null,      .infix = binary,   .precedence =  .PREC_COMPARISON }, // TOKEN_GREATER_EQUAL
        .{ .prefix = null,      .infix = binary,   .precedence =  .PREC_COMPARISON }, // TOKEN_LESS
        .{ .prefix = null,      .infix = binary,   .precedence =  .PREC_COMPARISON }, // TOKEN_LESS_EQUAL
        .{ .prefix = variable,  .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_IDENTIFIER
        .{ .prefix = string,    .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_STRING
        .{ .prefix = number,    .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_NUMBER
        .{ .prefix = null,      .infix = and_,     .precedence =  .PREC_AND }, // TOKEN_AND
        .{ .prefix = null,      .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_CLASS
        .{ .prefix = null,      .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_ELSE
        .{ .prefix = literal,   .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_FALSE
        .{ .prefix = null,      .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_FOR
        .{ .prefix = null,      .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_FUN
        .{ .prefix = null,      .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_IF
        .{ .prefix = literal,   .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_NIL
        .{ .prefix = null,      .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_OR
        .{ .prefix = null,      .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_PRINT
        .{ .prefix = null,      .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_RETURN
        .{ .prefix = null,      .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_SUPER
        .{ .prefix = null,      .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_THIS
        .{ .prefix = literal,   .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_TRUE
        .{ .prefix = null,      .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_VAR
        .{ .prefix = null,      .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_WHILE
        .{ .prefix = null,      .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_ERROR
        .{ .prefix = null,      .infix = null,     .precedence =  .PREC_NONE }, // TOKEN_EOF
    };

    pub fn advance(self: *Parser) void {
        self.previous = self.current;

        while(true) {
            self.current = sacnner.scanToken();
            if (self.current.tokenType != TokenType.TOKEN_ERROR) {
                break;
            }

            self.errorAtCurrent(self.current.literal);
        }
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) void {
        self.errorAt(&self.current, message);
    }

    pub fn error0(self: *Parser, message: []const u8) void {
        self.errorAt(&self.previous, message);
    }

    fn errorAt(self: *Parser , token: *Token, message: []const u8) void {
        if (self.panicMode) {
            return;
        }
        print("[line {}] Error", .{token.line});

        if (token.tokenType == TokenType.TOKEN_EOF) {
            print(" at end", .{});
        } else if (token.tokenType == TokenType.TOKEN_ERROR) {
            // Nothing
        } else {
            print(" at '{}'", .{token.literal});
        }

        print(": {}\n", .{message});
        self.hadError = true;
    }


    pub fn consume(self: *Parser, tokenType: TokenType, message: []const u8) void {
        if (self.current.tokenType == tokenType) {
            self.advance();
            return;
        }

        self.errorAtCurrent(message);
    }

    fn check(self: *Parser, tt: TokenType) bool {
        return self.current.tokenType == tt;
    }

    pub fn match(self: *Parser, tt: TokenType) bool {
        if (!self.check(tt)) return false;
        self.advance();
        return true;
    }

    fn emitByte(self: *Parser, byte: u8) !void {
        try currentChunk().writeByte(byte, self.previous.line);
    }

    pub fn emitOpCode(self: *Parser, opCode: OpCode) !void {
        try self.emitByte(@enumToInt(opCode));
    }

    pub fn emitReturn(self: *Parser) !void {
        try self.emitOpCode(OpCode.OP_NIL);
        try self.emitOpCode(OpCode.OP_RETURN);
    }

    fn makeConstant(self: *Parser, value: Value) !u8 {
        const constant = try currentChunk().addConstant(value);
        
        if (constant > std.math.maxInt(u8)) {
            self.error0("Too many constant in one chunk.");
            return 0;
        }

        return @intCast(u8, constant);
    }

    fn emitConstant(self: *Parser, value: Value) !void {
        try self.emitOpCode(OpCode.OP_CONSTANT);
        const constant = try self.makeConstant(value);
        try self.emitByte(constant);
    }

    fn emitBytes(self: *Parser, byte1: u8, byte2: u8) !void {
        try self.emitByte(byte1);
        try self.emitByte(byte2);
    }

    fn emitOpCodes(self: *Parser, opCode1: OpCode, opCode2: OpCode) !void {
        try self.emitOpCode(opCode1);
        try self.emitOpCode(opCode2);
    }

    fn emitJump(self: *Parser, opCode: OpCode) !usize {
        try self.emitOpCode(opCode);
        try self.emitByte(0xff);
        try self.emitByte(0xff);

        return currentChunk().code.items.len - 2;
    }

    fn emitLoop(self: *Parser, loopStart: usize) !void {
        try self.emitOpCode(OpCode.OP_LOOP);

        const offset = currentChunk().code.items.len - loopStart + 2;
        if (offset > std.math.maxInt(u16)) self.error0("Loop body to large.");

        try self.emitByte(@intCast(u8, (offset >> 8) & 0xff));
        try self.emitByte(@intCast(u8, offset & 0xff));
    }

    // 写入需要跳过的字节码个数
    fn patchJump(self: *Parser, offset: usize) !void {
        const jump = currentChunk().code.items.len - offset - 2;
        if (jump > std.math.maxInt(u16)) {
            self.error0("Too much code jump over.");
        }

        currentChunk().code.items[offset] = @intCast(u8, (jump >> 8) & 0xff);
        currentChunk().code.items[offset + 1] = @intCast(u8, jump & 0xff);
    }

    fn binary(self: *Parser, canAssing: bool) !void {
        // Remember the operator
        const operatorType = self.previous.tokenType;

        // Compile the right operand.
        const rule = self.getRule(operatorType);
        try self.parsePrecedence(@intToEnum(Precedence, @enumToInt(rule.precedence) + 1));

        // Emit the operator instruction.
        switch (operatorType) {
            TokenType.TOKEN_MINUS         => try self.emitOpCode(OpCode.OP_SUBTRACT),
            TokenType.TOKEN_PLUS          => try self.emitOpCode(OpCode.OP_ADD),
            TokenType.TOKEN_STAR          => try self.emitOpCode(OpCode.OP_MULTIPLY),
            TokenType.TOKEN_SLASH         => try self.emitOpCode(OpCode.OP_DIVIDE),
            // 比较操作
            TokenType.TOKEN_BANG_EQUAL    => try self.emitOpCodes(OpCode.OP_EQUAL, OpCode.OP_NOT),
            TokenType.TOKEN_EQUAL_EQUAL   => try self.emitOpCode(OpCode.OP_EQUAL),
            TokenType.TOKEN_GREATER       => try self.emitOpCode(OpCode.OP_GREATER),
            TokenType.TOKEN_GREATER_EQUAL => try self.emitOpCodes(OpCode.OP_LESS, OpCode.OP_NOT),
            TokenType.TOKEN_LESS          => try self.emitOpCode(OpCode.OP_LESS),
            TokenType.TOKEN_LESS_EQUAL    => try self.emitOpCodes(OpCode.OP_GREATER, OpCode.OP_NOT),
            else => unreachable,
        }
    }

    fn call(self: *Parser, canAssign: bool) !void {
        const argCount = try self.argumentList();
        try self.emitBytes(@enumToInt(OpCode.OP_CALL), argCount);
    }

    fn literal(self: *Parser, canAssing: bool) !void {
        switch (parser.previous.tokenType) {
            TokenType.TOKEN_FALSE => try self.emitOpCode(OpCode.OP_FALSE),
            TokenType.TOKEN_TRUE  => try self.emitOpCode(OpCode.OP_TRUE),
            TokenType.TOKEN_NIL   => try self.emitOpCode(OpCode.OP_NIL),
            else  => unreachable,
        }
    }

    fn grouping(self: *Parser, canAssing: bool) !void {
        try self.expression();
        self.consume(TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
    }

    fn number(self: *Parser, canAssing: bool) !void {
        const value = try std.fmt.parseFloat(f64, self.previous.literal);
        try self.emitConstant(number2Value(value));
    }

    fn string(self: *Parser, canAssing: bool) !void {
        try self.emitConstant(objString2Value(try heap.copyString(parser.previous.literal[1 .. parser.previous.literal.len - 1])));
    }

    // 获取变量
    fn nameVariable(self: *Parser, name: Token, canAssing: bool) !void {
        var getOp: OpCode = undefined;
        var setOp: OpCode = undefined;

        var arg = current.?.resolveLocal(name);

        if (arg != -1) {
            getOp = OpCode.OP_GET_LOCAL;
            setOp = OpCode.OP_SET_LOCAL;
        } else {

            arg = try current.?.resolveUpvalue(name);
            if (arg != -1) {
                getOp = OpCode.OP_GET_UPVALUE;
                setOp = OpCode.OP_SET_UPVALUE;
            } else {
                arg = try self.identifierConstant(&name);
                getOp = OpCode.OP_GET_GLOBAL;
                setOp = OpCode.OP_SET_GLOBAL;
            }
        }

        if (self.match(TokenType.TOKEN_EQUAL) and canAssing) {
            try self.expression();
            try self.emitBytes(@enumToInt(setOp), @intCast(u8, arg));
        } else try self.emitBytes(@enumToInt(getOp), @intCast(u8, arg));
    }

    fn variable(self: *Parser, canAssing: bool) !void {
        try self.nameVariable(parser.previous, canAssing);
    }

    fn unary(self: *Parser, canAssing: bool) !void {
        const operatorType = parser.previous.tokenType;

        // Compile the operand.
        try self.parsePrecedence(Precedence.PREC_UNARY);

        // Emit the operator instruction.
        switch (operatorType) {
            TokenType.TOKEN_MINUS => try self.emitOpCode(OpCode.OP_NEGATE),
            TokenType.TOKEN_BANG  => try self.emitOpCode(OpCode.OP_NOT),
            else => unreachable,
        }
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) !void {
        self.advance();
        const prefixRule = self.getRule(self.previous.tokenType).prefix;
        if (prefixRule) |prefixFn| {

            const canAssign = @enumToInt(precedence) <= @enumToInt(Precedence.PREC_ASSIGNMENT);
            try prefixFn(self, canAssign);
            
            while (@enumToInt(precedence) <= @enumToInt(self.getRule(self.current.tokenType).precedence)) {
                self.advance();
                const infixRule = self.getRule(self.previous.tokenType).infix;
                if (infixRule) |infixFn| {
                    try infixFn(self, canAssign);
                } else {
                    self.error0("Expect expression");
                    return;
                }
            }

            if (canAssign and self.match(TokenType.TOKEN_EQUAL)) {
                self.error0("Invalid assignment target");
            }
        } else {
            self.error0("Expect expression");
            return;
        }
    }

    fn identifierConstant(self: *Parser, name: *const Token) !u8 {
        return try self.makeConstant(objString2Value(try heap.copyString(name.literal)));
    }

    // declare local variable
    fn declareVariable(self: *Parser) !void {
        if (current.?.scopeDepth == 0) return;
        const name = self.previous;

        if (current.?.locals.items.len > 0) {
            var i: usize = current.?.locals.items.len - 1;
            while ( i >= 0 ) {
                const local = current.?.locals.items[i];
                if (local.depth != -1 and local.depth < current.?.scopeDepth) {
                    break;
                }

                if (Token.equal(name, local.name)) {
                    self.error0("Already variable with this name in this scope");
                }

                if (@subWithOverflow(usize, i, 1, &i)) {
                    break;
                }
            }
        }

        try current.?.addLocal(name);
    }

    fn parseVariable(self: *Parser, errorMessage: []const u8) !u8 {
        self.consume(TokenType.TOKEN_IDENTIFIER, errorMessage);

        // declare local variable
        try self.declareVariable();
        // local aren't looked up by name
        if (current.?.scopeDepth > 0) return 0;

        return try self.identifierConstant(&self.previous);
    }

    fn defineVariable(self: *Parser, global: u8) !void {
        // 本地变量已经在栈上面了，不需要定义变量了
        if (current.?.scopeDepth > 0) {
            current.?.makeInitialized();
            return;
        }
        try self.emitBytes(@enumToInt(OpCode.OP_DEFINE_GLOBAL), global);
    }

    fn argumentList(self: *Parser) !u8 {
        var argCount: u8 = 0;
        if (!self.check(TokenType.TOKEN_RIGHT_PAREN)) {
            try self.expression();
            argCount += 1;

            while (self.match(TokenType.TOKEN_COMMA)) {
                try self.expression();
                argCount += 1;
            }
        }

        self.consume(TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after arguments");
        return argCount;
    }

    fn and_(self: *Parser, canAssign: bool) !void {
        const endJump = try self.emitJump(OpCode.OP_JUMP_IF_FALSE);

        try self.emitOpCode(OpCode.OP_POP);
        try self.parsePrecedence(Precedence.PREC_AND);

        try self.patchJump(endJump);
    }

    fn getRule(self: *Parser, tokenType: TokenType) *const ParseRule {
        return &rules[@enumToInt(tokenType)];
    }

    pub fn expression(self: *Parser) !void {
        try self.parsePrecedence(Precedence.PREC_ASSIGNMENT);
    }

    fn block(self: *Parser) !void {
        while (!self.check(TokenType.TOKEN_RIGHT_BRACE) and !self.check(TokenType.TOKEN_EOF)) {
            try self.declaration();
        }

        self.consume(TokenType.TOKEN_RIGHT_BRACE, "Expect '}' after block.");
    }

    fn functionParam(self: *Parser) !void {
        current.?.function.?.arity += 1;
        const paramConstant = try self.parseVariable("Expect parameter name.");
        try self.defineVariable(paramConstant);
    }

    fn function(self: *Parser, ft: FunctionType) !void {
        try initCompiler(heap.allocator, ft);
        const compiler = current;
        current.?.beginScope();
        // Compile the parameter list.
        self.consume(TokenType.TOKEN_LEFT_PAREN, "Expect '(' after function name.");
        if (!self.check(TokenType.TOKEN_RIGHT_PAREN)) {
            
            try self.functionParam();

            while (self.match(TokenType.TOKEN_COMMA)) {
                try self.functionParam();
            }
        }
        self.consume(TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");

        // The body.
        self.consume(TokenType.TOKEN_LEFT_BRACE, "Expect '{' before function body.");
        try self.block(); // all byte emit to fun's chunk

        // Create the function object
        const fun = try current.?.endCompiler(); // switch to current outside compiler
        try self.emitBytes(@enumToInt(OpCode.OP_CLOSURE), try self.makeConstant(objFunction2Value(fun)));
        var i : usize = 0;
        while (i < fun.upvalueCount) : (i+=1) {
            try self.emitByte(if (compiler.?.upvalues.items[i].isLocal) 1 else 0);
            try self.emitByte(@intCast(u8, compiler.?.upvalues.items[i].index));
        }
    }

    fn funDeclaration(self: *Parser) !void {
        const global = try self.parseVariable("Expect function name.");
        current.?.makeInitialized();
        try self.function(FunctionType.TYPE_FUNCTION);
        try self.defineVariable(global);
    }

    // 变量定义: 全局变量需要OpCode.OP_DEFINE_GLOBAL指令, 本地变量仅需要将Token存储到Complier locals变量中
    // 后面在需要使用的时候，根据locals中的索引直接从栈中,获取到对应位置的常量
    fn varDeclaration(self: *Parser) !void {
        const global = try self.parseVariable("Expect variable name");
        if (self.match(TokenType.TOKEN_EQUAL)) {
            try self.expression();
        } else {
            try self.emitOpCode(OpCode.OP_NIL);
        }
        self.consume(TokenType.TOKEN_SEMICOLON, "Expect ';' after variable declaration.");
        try self.defineVariable(global);
    }

    fn expressionStatement(self: *Parser) !void {
        try self.expression();
        self.consume(TokenType.TOKEN_SEMICOLON, "Expect ';' after expression.");
        try self.emitOpCode(OpCode.OP_POP);
    }

    fn ifStatement(self: *Parser) !void {
        self.consume(TokenType.TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
        try self.expression();
        self.consume(TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after condition.");
        const thenJump = try self.emitJump(OpCode.OP_JUMP_IF_FALSE);
        try self.emitOpCode(OpCode.OP_POP);
        try self.statement();
        const elseJump = try self.emitJump(OpCode.OP_JUMP);
        try self.patchJump(thenJump);
        try self.emitOpCode(OpCode.OP_POP);
        if (self.match(TokenType.TOKEN_ELSE)) try self.statement();
        try self.patchJump(elseJump);
    }

    fn printStatement(self: *Parser) !void {
        try self.expression();
        self.consume(TokenType.TOKEN_SEMICOLON, "Expect ';' after value.");
        try self.emitOpCode(OpCode.OP_PRINT);
    }

    fn returnStatement(self: *Parser) !void {
        if (self.match(TokenType.TOKEN_SEMICOLON)) {
            try self.emitReturn();
        } else {
            try self.expression();
            self.consume(TokenType.TOKEN_SEMICOLON, "Expect ';' after return value.");
            try self.emitOpCode(OpCode.OP_RETURN);
        }
    }

    fn whileStatement(self: *Parser) !void {
        const loopStart = currentChunk().code.items.len;

        self.consume(TokenType.TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
        try self.expression();
        self.consume(TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

        const exitJump = try self.emitJump(OpCode.OP_JUMP_IF_FALSE);

        try self.emitOpCode(OpCode.OP_POP);
        try self.statement();

        try self.emitLoop(loopStart);

        try self.patchJump(exitJump);
        try self.emitOpCode(OpCode.OP_POP);
    }

    fn synchronize(self: *Parser) void {
        self.panicMode = false;

        while (parser.current.tokenType != TokenType.TOKEN_EOF) {
            if (parser.previous.tokenType == TokenType.TOKEN_SEMICOLON) return;

            switch (parser.current.tokenType) {
                TokenType.TOKEN_CLASS, TokenType.TOKEN_FUN, TokenType.TOKEN_VAR,
                TokenType.TOKEN_FOR, TokenType.TOKEN_IF, TokenType.TOKEN_WHILE,
                TokenType.TOKEN_PRINT, TokenType.TOKEN_RETURN
                => return,
                else => break,
            }

            self.advance();
        }
    }

    pub fn declaration(self: *Parser) anyerror!void {
        if (self.match(TokenType.TOKEN_VAR)) {
            try self.varDeclaration();
        } else if (self.match(TokenType.TOKEN_FUN)) {
            try self.funDeclaration();
        } else {
            try self.statement();
        }

        if (self.panicMode) self.synchronize();
    }

    pub fn statement(self: *Parser) anyerror!void {
        if (self.match(TokenType.TOKEN_PRINT)) {
            try self.printStatement();
        } else if (self.match(TokenType.TOKEN_IF)) {
            try self.ifStatement();
        } else if (self.match(TokenType.TOKEN_RETURN)) {
            try self.returnStatement();
        } else if (self.match(TokenType.TOKEN_LEFT_BRACE)) {
            current.?.beginScope();
            try self.block();
            try current.?.endScope();
        } else if (self.match(TokenType.TOKEN_WHILE)) {
            try self.whileStatement();
        } else {
            try self.expressionStatement();
        }
    }
};