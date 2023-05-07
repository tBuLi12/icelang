#include "../../src/parser/parse.h"
#include <gtest/gtest.h>
#include <sstream>

#define PARSE(name, sourceCode, expectedDump)                                                   \
TEST(ParserTest, name) {                                                                        \
    std::stringstream source{sourceCode};                                                       \
    auto [program, log] = parseProgram(Source{source, ""});                                     \
    EXPECT_EQ(fmt::format("{}", program), expectedDump);                                        \
    EXPECT_TRUE(log.errorsAreEmpty());                                                          \
}

#define PARSE_FAIL(name, sourceCode, expectedDump)                                              \
TEST(ParserTest, name) {                                                                        \
    std::stringstream source{sourceCode};                                                       \
    auto [program, log] = parseProgram(Source{source, ""});                                     \
    EXPECT_EQ(fmt::format("{}", program), expectedDump);                                        \
    EXPECT_FALSE(log.errorsAreEmpty());                                                         \
}

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

PARSE_FAIL(NoFun, "name() -> 3", "");
PARSE_FAIL(NoBody, "fun name() ->", "");
PARSE_FAIL(NonBlockBody, "fun name() 3", "");
PARSE_FAIL(MissingSemicolon, "fun name() { invoke() 7 }", "fun(name,,block(call(invoke,),))");
PARSE_FAIL(MissingColon, "fun name() -> { prop: value, second value }", "fun(name,,struct(prop:value,second))");
PARSE_FAIL(MissingParenthases, "fun name() -> if condition) 3 else 5", "fun(name,,if(condition,3,5))");
PARSE_FAIL(MatchStruct, "fun name() -> match name { prop: value } { a => b }", "");

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
    auto& pattern = std::get<ast::DestructureStruct>(std::get<ast::GuardedPattern>(firstCase.pattern.value).pattern.value);
    auto& arm = std::get<ast::Binary<"+">>(std::get<ast::Expression>(firstCase.value.value).value);
    EXPECT_EQ(pattern.span, (Span{16, 16, 453, 20, 40}));
    auto& firstProp = pattern.properties.at(0);
    EXPECT_EQ(std::get<ast::Binary<"==">>(firstProp.property.value).span, (Span{16, 16, 453, 22, 31}));
    EXPECT_EQ(arm.span, (Span{16, 16, 453, 44, 72}));
}
