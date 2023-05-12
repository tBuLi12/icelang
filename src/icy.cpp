#include "./IREmmiter/IREmmiter.cpp"
#include "./parser/parse.h"
#include <fstream>

int main(int argc, char* argv[]) {
    auto ctx = std::make_unique<llvm::LLVMContext>();
    auto mod = std::make_unique<llvm::Module>("my cool jit", *ctx);
    auto builder = std::make_unique<llvm::IRBuilder<>>(*ctx);

    auto intt = llvm::Type::getInt32Ty(*ctx);
    auto nullt = llvm::StructType::create(*ctx, {}, "null");
    auto boolt = llvm::Type::getInt1Ty(*ctx);
    auto ptr = llvm::PointerType::get(*ctx, 0);

    std::unordered_map<std::string, type::BuiltIn*> builtInTypes{
        {"int", new type::BuiltIn{intt, "int"}},
        {"null", new type::BuiltIn{nullt, "null"}},
        {"bool", new type::BuiltIn{boolt, "bool"}},
        {"ptr", new type::BuiltIn{ptr, "ptr"}},
    };

    std::ifstream sourcef{};
    sourcef.open("test.ice", std::ios::binary);
    std::cout << "b4 parsing done" << std::endl;
    Source source{sourcef, "test.ice"};
    auto [program, log] = parseProgram(source);
    std::cout << "parsing done" << std::endl;
    if (!log.errorsAreEmpty()) {
        log.printDiagnosticsTo(std::cout);
        sourcef.close();
        return 1;
    }

    TypeChecker tc{
        builtInTypes, collectNamedTypes(program), collectFunctions(program),
        collectTraitDeclarations(program)};
    std::cout << "parsing done" << std::endl;

    tc.check(program);

    IRVisitor ir{source,
                 std::move(ctx),
                 std::move(builder),
                 std::move(mod),
                 builtInTypes,
                 ImplScope{std::move(tc.implementationScope)}};

    ir.vecDeclaration = tc.typeScope.at("Vector");
    fmt::println("here");
    ir.allocFunc = Function{tc.funcScope.at("rtAlloc"), {}};
    ir.sliceFunc = Function{tc.funcScope.at("rtSlice"), {}};
    ir.freeFunc = Function{tc.funcScope.at("rtFree"), {}};
    fmt::println("aaaa");
    ir.copyDeclaration = tc.traitScope.at("Copy");
    ir.dropDeclaration = tc.traitScope.at("Drop");

    auto targetTriple = llvm::sys::getDefaultTargetTriple();

    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    std::string targetErr;
    auto target = llvm::TargetRegistry::lookupTarget(targetTriple, targetErr);
    if (!target) {
        std::cerr << targetErr;
        return 1;
    }

    auto targetMachine =
        target->createTargetMachine(targetTriple, "generic", "", {}, {});

    ir.currentModule->setDataLayout(targetMachine->createDataLayout());
    ir.currentModule->setTargetTriple(targetTriple);

    ir.emitMain(std::move(tc.funcScope["main"]->body));
    sourcef.close();

    llvm::SMDiagnostic err;
    auto stlmod = llvm::parseIRFile("stl.ll", err, *ir.context);
    if (!stlmod) {
        err.print("stl", llvm::outs());
        return 1;
    }

    llvm::Linker::linkModules(*ir.currentModule, std::move(stlmod));
    ir.currentModule->print(llvm::outs(), nullptr);

    llvm::legacy::PassManager passManager{};

    std::error_code errCode;
    llvm::raw_fd_ostream outputFile{"iout.o", errCode, llvm::sys::fs::OF_None};
    if (errCode) {
        std::cerr << "could not open out file: " << errCode << std::endl;
        return 1;
    }
    if (targetMachine->addPassesToEmitFile(
            passManager, outputFile, nullptr, llvm::CGFT_ObjectFile
        )) {
        std::cerr << "can't emit" << std::endl;
        return 1;
    }

    passManager.run(*ir.currentModule);
    outputFile.flush();

    return 0;
}
