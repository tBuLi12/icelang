#include "iceStd.h"
#include "parse.h"
#include "compiler.h"
#include "KaleidoscopeJIT.h"
#include "jit.h"

Result jit(std::string_view sourceCode) {
    std::stringstream stdLib{stdIce};
    Source stdsource{stdLib, "std"};
    auto [program, log] = parseProgram(stdsource);
    auto mod = new Module{
            stdsource, std::move(program), "std"}; 

    std::stringstream code{std::string{sourceCode}};
    Source userSource{code, "anonymous"};
    auto [program2, log2] = parseProgram(userSource);
    if (!log2.errorsAreEmpty()) {
        return Result::DoesntParse;
    }
    auto mod2 = new Module{
            userSource, std::move(program2), "anonymous"};
        
    auto visibility = ast::Visibility{Span{}, ast::Visibility::Level::Private};
    mod2->imports["std"] = {0, &visibility};

    std::vector<Module*> modules{mod, mod2};
    auto result = compile(modules);
    if (!result) {
        return Result::DoesntCompile;
    }

    auto [llvmContext, llvmModule, _] = std::move(*result);

    std::unique_ptr<llvm::orc::KaleidoscopeJIT> TheJIT;

    auto exit = llvm::ExitOnError();

    TheJIT = exit(llvm::orc::KaleidoscopeJIT::Create());

    // Create a ResourceTracker to track JIT'd memory allocated to our
    // anonymous expression -- that way we can free it after executing.
    auto RT = TheJIT->getMainJITDylib().createResourceTracker();

    auto TSM = llvm::orc::ThreadSafeModule(std::move(llvmModule), std::move(llvmContext));
    exit(TheJIT->addModule(std::move(TSM), RT));

    // Search the JIT for the __anon_expr symbol.
    auto ExprSymbol = exit(TheJIT->lookup("main"));
    assert(ExprSymbol && "Function not found");

    // Get the symbol's address and cast it to the right type (takes no
    // arguments, returns a double) so we can call it as a native function.
    int32_t (*FP)() = reinterpret_cast<int32_t (*)()>(ExprSymbol.getAddress());
    // Delete the anonymous expression module from the JIT.
    int32_t exitCode = FP();
    exit(RT->remove());
    return exitCode == 0 ? Result::Ok : Result::Failed;
}
