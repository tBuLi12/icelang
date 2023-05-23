#include "../../src/parser/parser.cpp"
#include <gtest/gtest.h>
#include <ranges>
#include <sstream>

using namespace parser;

#define PARSE(name, sourceCode, expectedDump)                                  \
    TEST(ParserTest, name) {                                                   \
        std::stringstream source{sourceCode};                                  \
        auto [program, log] = parseProgram(Source{source, ""});                \
        EXPECT_EQ(fmt::format("{}", program), expectedDump);                   \
        EXPECT_TRUE(log.errorsAreEmpty());                                   \
        log.printDiagnosticsTo(std::cout);            \
    }

#define PARSE_FAIL(name, sourceCode, expectedDump, ...)                        \
    TEST(ParserTest, name) {                                                   \
        std::stringstream source{sourceCode};                                  \
        auto [program, log] = parseProgram(Source{source, ""});                \
        EXPECT_EQ(fmt::format("{}", program), expectedDump);                   \
        EXPECT_TRUE(compareMessages(log.diagnostics, {__VA_ARGS__}));          \
    }

struct Message {
    Span span;
    std::string_view header;

    Message(
        size_t firstLine, size_t lastLine, size_t beginOffset,
        size_t beginHighlightOffset, size_t endHighlightOffset,
        std::string_view _header
    )
        : span{firstLine, lastLine, beginOffset, beginHighlightOffset, endHighlightOffset},
          header{_header} {}
};

bool compareMessages(
    std::vector<logs::SpannedMessage> const& logs,
    std::initializer_list<Message> messages
) {
    return std::ranges::equal(
        messages, logs,
        [](Message const& message, logs::SpannedMessage const& log) {
            std::cout << log.header << log.span << std::endl;
            return message.span == log.span && message.header == log.header;
        }
    );
}

// clang-format off

PARSE(Integer, "fun name() -> 3", "fun(name,,3)");
PARSE(Variable, "fun name() -> someVar", "fun(name,,someVar)");
PARSE(String, R"(fun name() -> "some string")", R"(fun(name,,"some string"))");
PARSE(NamedEmptyStruct, "fun name() -> data {}", "fun(name,,struct data())");
PARSE(NamedStruct, "fun name() -> data { key: 2 }", "fun(name,,struct data(key:2))");
PARSE(Vector, "fun name() -> [1, 2, 3]", "fun(name,,vector(1,2,3))");
PARSE(EmptyVector, "fun name() -> [:int]", "fun(name,,vector(:int))");
PARSE(Parenthases, "fun name() -> (someVar)", "fun(name,,someVar)");
PARSE(OneElementTuple, "fun name() -> (someVar,)", "fun(name,,tuple(someVar))");
PARSE(Tuple, "fun name() -> (someVar, otherVar)", "fun(name,,tuple(someVar,otherVar))");
PARSE(OpenTuple, "fun name() -> someVar, otherVar", "fun(name,,tuple(someVar,otherVar))");
PARSE(Match, "fun name() -> match stuff { 1 => 0, a => b }", "fun(name,,match(stuff,1=>0,a=>b))");
PARSE(Block, "fun name() -> { a(); b(); }", "fun(name,,block(call(a,),call(b,),))");
PARSE(BlockWithTrailingExpression, "fun name() -> { a(); b() }", "fun(name,,block(call(a,),call(b,)))");
PARSE(SingleIdentStruct, "fun name() -> { prop }", "fun(name,,struct(prop))");
PARSE(PropertyAccess, "fun name() -> expr.prop", "fun(name,,property(prop,expr))");
PARSE(FieldAccess, "fun name() -> expr.2", "fun(name,,field(2,expr))");
PARSE(NestedFieldAccess, "fun name() -> expr.2.4", "fun(name,,field(4,field(2,expr)))");
PARSE(IndexAccess, "fun name() -> expr[1, 2]", "fun(name,,index(tuple(1,2),expr))");
PARSE(Call, "fun name() -> expr()", "fun(name,,call(expr,))");
PARSE(GenericCall, "fun name() -> expr:<T>()", "fun(name,,call<T>(expr,))");
PARSE(MethodCall, "fun name() -> expr.func()", "fun(name,,call(property(func,expr),))");
PARSE(MethodCallWithArgument, "fun name() -> expr.func(arg)", "fun(name,,call(property(func,expr),arg))");
PARSE(MultipleArguments, "fun name() -> func(arg, other)", "fun(name,,call(func,arg,other))");
PARSE(MultipleArgumentsTrailingComma, "fun name() -> func(arg, other,)", "fun(name,,call(func,arg,other))");
PARSE(ExplicitTrait, "fun name() -> expr.<SomeTrait>method", "fun(name,,property(SomeTrait,method,expr))");
PARSE(Prefix, "fun name() -> -!expr.prop", "fun(name,,-(!(property(prop,expr))))");
PARSE(Cast, "fun name() -> !expr as float", "fun(name,,as(!(expr),float))");
PARSE(DoubleCast, "fun name() -> !expr as float as int", "fun(name,,as(as(!(expr),float),int))");
PARSE(Multiplication, "fun name() -> !expr * 2 as float", "fun(name,,*(!(expr),as(2,float)))");
PARSE(Addition, "fun name() -> 1 * 3 + 2 * 8 + 3", "fun(name,,+(+(*(1,3),*(2,8)),3))");
PARSE(Relational, "fun name() -> 1 * 3 > 2 < 1 + 4 <= 6", "fun(name,,<=(<(>(*(1,3),2),+(1,4)),6))");
PARSE(Equality, "fun name() -> 1 > 2 == 4 < 5", "fun(name,,==(>(1,2),<(4,5)))");
PARSE(And, "fun name() -> a + b && c == e", "fun(name,,&&(+(a,b),==(c,e)))");
PARSE(Or, "fun name() -> a && b || c || d && e", "fun(name,,||(||(&&(a,b),c),&&(d,e)))");
PARSE(Assignment, "fun name() -> a = e = b", "fun(name,,=(a,=(e,b)))");
PARSE(If, "fun name() -> !if (cond) yes", "fun(name,,!(if(cond,yes)))");
PARSE(IfElse, "fun name() -> if (cond) yes else no", "fun(name,,if(cond,yes,no))");
PARSE(NestedIf, "fun name() -> if (cond) if (other) 3 else 4 else 5", "fun(name,,if(cond,if(other,3,4),5))");
PARSE(MatchPattern, "fun name() -> match stuff { (1, x) if isEven(x) => b }", "fun(name,,match(stuff,guard(p-tuple(1,x),call(isEven,x))=>b))");
PARSE(MatchExitFlow, "fun name() -> match stuff { 1 => return, 2 => continue, n => break n, }", "fun(name,,match(stuff,1=>return,2=>continue,n=>break(n)))");
PARSE(LetPattern, "fun name() -> { let x, b = someTuple }", "fun(name,,block(let(p-tuple(x,b),someTuple)))");
PARSE(StructPattern, "fun name() -> { let { x > 4 + y, radius } = someStruct }", "fun(name,,block(let(p-struct(>(x,+(4,y)),radius),someStruct)))");
PARSE(StructPatternCallGuard, "fun name() -> { let { isEven(x), y } = someStruct }", "fun(name,,block(let(p-struct(call(isEven,x),y),someStruct)))");
PARSE(TypeDeclaration, "type Type float", "type(Type,float)");
PARSE(CompoundType, "type User { name: String, age: int }", "type(User,struct(name:String,age:int))");
PARSE(OneElementTupleType, "type Type (int)", "type(Type,tuple(int))");
PARSE(GenericType, "type Type<T> { value: T }", "type<T>(Type,struct(value:T))");
PARSE(TraitDeclaration, "trait Car { fun drive() fun stop() fun refuel(fuel: Fuel) }", "trait(Car,fun(drive,),fun(stop,),fun(refuel,fuel:Fuel))");
PARSE(GenericTrait, "trait Parse<T> { fun parse(): T }", "trait<T>(Parse,fun(parse:T,))");
PARSE(TraitImplementation, "def Audi as Car { fun drive() { this.startEngine(); } }", "def(Audi,Car,fun(drive,,block(call(property(startEngine,this),),)))");
PARSE(GenericImplementation, "def<T> [T] as Collection { fun length() -> this.length }", "def<T>(vector(T),Collection,fun(length,,property(length,this)))");

PARSE(
    Reverse,
    R"(
        fun reverse<T>(vector: [T]): [T] {
            var reversed = [];
            while (let [..rest, last] = vector) {
                reversed.push(last);
                rest
            };
            reversed
        }
    )",
    "fun<T>(reverse:vector(T),vector:vector(T),block(var(reversed,vector()),while(let(p-vector(..rest,last),vector),block(call(property(push,reversed),last),rest)),reversed))"
);

PARSE(
    GenericSum,
    R"(
        fun sum<T is Add<T>>(items: [T]): T {
            if (let [first, ..toSum] = items) {
                while(let total, [next, ..rest] = first, toSum) (sum + next, rest)
            }.0 else {
                panic("cannot sum an empty list")
            }
        }
    )",
    "fun<T:(Add<T>)>(sum:T,items:vector(T),block(if(let(p-vector(first,..toSum),items),field(0,block(while(let(p-tuple(total,p-vector(next,..rest)),tuple(first,toSum)),tuple(+(sum,next),rest)))),block(call(panic,\"cannot sum an empty list\")))))"
);

PARSE(
    CompareVectors,
    R"(
        fun compare<R, L is Equals<R>>(left: [L], right: [R]): bool {
            {
                while (let [x, ..leftRest], [x, ..rightRest] = left, right) (leftRest, rightRest)
            }.match {
                [], [] => true,
                _ => false,
            }
        }
    )",
    "fun<R,L:(Equals<R>)>(compare:bool,left:vector(L),right:vector(R),block(match(block(while(let(p-tuple(p-vector(x,..leftRest),p-vector(x,..rightRest)),tuple(left,right)),tuple(leftRest,rightRest))),p-tuple(p-vector(),p-vector())=>true,_=>false)))"
);


PARSE_FAIL(NoFun, "name() -> 3", "", Message(0, 0, 0, 0, 4, "expected a declaration"));
PARSE_FAIL(NoBody, "fun name() ->", "", Message(0, 0, 0, 12, 13, "expected an expression"));
PARSE_FAIL(NonBlockBody, "fun name() 3", "", Message(0, 0, 0, 11, 12, "expected { or ->"));
PARSE_FAIL(MissingSemicolon, "fun name() { invoke() 7 }", "fun(name,,block(call(invoke,)))", Message(0, 0, 0, 22, 23, "expected }"));
PARSE_FAIL(MissingColon, "fun name() -> { prop: value, second value }", "fun(name,,struct(prop:value,second))", Message(0, 0, 0, 36, 41, "expected }"));
PARSE_FAIL(MissingParenthases, "fun name() -> if condition) 3 else 5", "fun(name,,if(condition,3,5))", Message(0, 0, 0, 17, 26, "expected ("));
PARSE_FAIL(MatchStruct, "fun name() -> match name { prop: value } { a => b }", "", Message(0, 0, 0, 31, 32, "expected =>"));
PARSE_FAIL(StructBody, "fun name() { key: value }", "fun(name,,block(key))", Message(0, 0, 0, 16, 17, "expected }"));
PARSE_FAIL(
    BlockInGuard, 
    R"(
        fun name(user: User) {
            match user {
                { age == {
                    let ages = [12, 13, 15];
                    randomFrom(ages)
                } } => accept(user),
                _   => return,
            }
        }
    )", 
    "", 
    Message(3, 3, 57, 25, 26, "expected a guard")
);
PARSE_FAIL(
    ControlFlowOutsideBlock, 
    R"(
        fun getCount(): int {
            let count = 0;
            2 + return 4;
        }
    )", 
    "", 
    Message(3, 3, 58, 16, 22, "expected an expression")
);

TEST(ParserTest, Spans) {
    std::stringstream source{
        R"(
            fun example() {
                let result = if (condition) {
                    print("was true");
                    0
                } else 1;

                while (index < 10) {
                    print("loop");
                    index = index + 1;
                };

                let nums = [0, 0, 0, 1, 2, 3];
                let trimmed = while (let [0, ..rest] = vector) rest;

                let message = match car {
                    { fuel == 0, model } => model + " has no more fuel!",
                    { 0 < fuel < 0.2 } => "low on fuel",
                    { fuel }           => fuel as String + " fuel left",
                };
            }
        )"
    };
    auto [program, log] = parseProgram(Source{source, ""});
    EXPECT_TRUE(log.errorsAreEmpty());

    auto& function = program.functions.at(0);
    EXPECT_EQ(function.span, (Span{1, 20, 1, 12, 13}));
    EXPECT_EQ(function.name.span, (Span{1, 1, 1, 16, 23}));
    auto& block = std::get<ast::Block>(function.body.value);
    EXPECT_EQ(block.span, (Span{1, 20, 1, 26, 13}));
    auto& firstLet = std::get<ast::LetBinding>(block.items.at(0).value);
    EXPECT_EQ(firstLet.span, (Span{2, 5, 29, 16, 24}));
    auto& ifExpr = std::get<ast::If>(firstLet.initalValue.value);
    EXPECT_EQ(ifExpr.span, (Span{2, 5, 29, 29, 24}));
    EXPECT_EQ(std::get<ast::Variable>(std::get<ast::Expression>(ifExpr.condition->value).value).span, (Span{2, 2, 29, 33, 42}));
    auto& letTrimmed = std::get<ast::LetBinding>(block.items.at(3).value);
    EXPECT_EQ(letTrimmed.span, (Span{13, 13, 341, 16, 67}));
    auto& whileExpr = std::get<ast::While>(letTrimmed.initalValue.value);
    EXPECT_EQ(whileExpr.span, (Span{13, 13, 341, 30, 67}));
    EXPECT_EQ(std::get<ast::Variable>(whileExpr.body->value).span, (Span{13, 13, 341, 63, 67}));
    auto& match = std::get<ast::Match>(std::get<ast::LetBinding>(block.items.at(4).value).initalValue.value);
    EXPECT_EQ(match.span, (Span{15, 19, 411, 30, 17}));
    EXPECT_EQ(std::get<ast::Variable>(match.scrutinee->value).span, (Span{15, 15, 411, 36, 39}));
    auto& firstCase = match.body.at(0);
    EXPECT_EQ(firstCase.span, (Span{16, 16, 453, 20, 72}));
    auto& pattern = std::get<ast::DestructureStruct>(std::get<ast::Destructure>(firstCase.pattern.body.value).value);
    auto& arm = std::get<ast::Binary<"+">>(std::get<ast::Expression>(firstCase.value.value).value);
    EXPECT_EQ(pattern.span, (Span{16, 16, 453, 20, 40}));
    auto& firstProp = pattern.properties.at(0);
    EXPECT_EQ(std::get<ast::Binary<"==">>(firstProp.property.value).span, (Span{16, 16, 453, 22, 31}));
    EXPECT_EQ(arm.span, (Span{16, 16, 453, 44, 72}));
}

TEST(ParserTest, OrLambdaFolds) {
    
    auto folded = std::optional<std::variant<int, double>>{}
                || [] { return std::optional<double>{}; }
                || [] { return std::optional{3}; }
                || [] { return std::optional{0.7}; }
                || [] () -> std::optional<int> { throw ""; };
    EXPECT_EQ(folded, (std::optional<std::variant<int, double>>{3}));
}

TEST(ParserTest, AndLambdaFolds) {
    auto folded = std::optional{Spanned{Span{0, 0, 0, 1, 2}, std::tuple{3}}}
                && [] { return std::optional{Spanned{Span{0, 0, 0, 5, 7}, 0.3}}; }
                && [] { return std::optional{Spanned{Span{0, 0, 0, 8, 9}, 5}}; }
                && [] { return std::optional{Spanned{Span{0, 0, 0, 11, 14}, 0.9}}; };
    
    EXPECT_EQ(folded, (std::optional{Spanned{Span{0, 0, 0, 1, 14}, std::tuple{3, 0.3, 5, 0.9}}}));
}

TEST(ParserTest, AndLambdaFoldsFail) {
    auto folded = std::optional{Spanned{Span{}, std::tuple{3}}}
                && [] { return std::optional{Spanned{Span{}, 0.3}}; }
                && [] { return std::optional<Spanned<int>>{}; }
                && [] { return std::optional{Spanned{Span{}, 0.9}}; };
    
    EXPECT_EQ(folded, (std::optional<Spanned<std::tuple<int, double, int, double>>>{}));
}

TEST(ParserTest, ConcatResult) {
    auto result = concatResult(Spanned{Span{0, 0, 0, 1, 3}, std::tuple{1, 1.1}}, Spanned{Span{0, 0, 0, 5, 7}, 7});
    EXPECT_EQ(result, (Spanned{Span{0, 0, 0, 1, 7}, std::tuple{1, 1.1, 7}}));
}

TEST(ParserTest, ConcatResultKeyword) {
    auto result = concatResult(Spanned{Span{0, 0, 0, 1, 3}, std::tuple{1, 1.1}}, lexer::Keyword<"fun">{Span{1, 1, 4, 2, 5}});
    EXPECT_EQ(result, (Spanned{Span{0, 1, 0, 1, 5}, std::tuple{1, 1.1}}));
}

TEST(ParserTest, SpanOfToken) {
    lexer::Keyword<"fun"> keyword{Span{1, 1, 4, 2, 5}};
    EXPECT_EQ(spanOf(keyword), (Span{1, 1, 4, 2, 5}));
}

TEST(ParserTest, SpanOfSpanned) {
    Spanned spanned{Span{1, 1, 4, 6, 8}, 0};
    EXPECT_EQ(spanOf(spanned), (Span{1, 1, 4, 6, 8}));
}

TEST(ParserTest, MakeWithTuple) {
    Span span = make<Span>(static_cast<size_t>(0), std::tuple<size_t, size_t, size_t>{0, 1, 4}, static_cast<size_t>(8));
    EXPECT_EQ(span, (Span{0, 0, 1, 4, 8}));
}

TEST(ParserTest, MakeUpcastVariant) {
    auto result = make<std::variant<int, float, double>>(std::variant<double, int>{4});
    EXPECT_EQ(result, (std::variant<int, float, double>{4}));
}

TEST(ParserTest, MakeChooseContructor) {
    auto fromChar = make<std::string>(std::variant<std::string, char>{'o'});
    auto fromString = make<std::string>(std::variant<std::string, char>{"123"});
    EXPECT_EQ(fromChar, "o");
    EXPECT_EQ(fromString, "123");
}
